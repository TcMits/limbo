use super::dbsp::{RowKeyStream, RowKeyZSet};
use super::operator::{
    AggregateFunction, Delta, FilterOperator, FilterPredicate, ProjectColumn, ProjectOperator,
};
use crate::schema::{BTreeTable, Column, Schema};
use crate::types::{IOResult, Value};
use crate::util::{extract_column_name_from_expr, extract_view_columns};
use crate::{LimboError, Result, Statement};
use fallible_iterator::FallibleIterator;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;
use turso_sqlite3_parser::{
    ast::{Cmd, Stmt},
    lexer::sql::Parser,
};

/// State machine for populating a view from its source table
pub enum PopulateState {
    /// Initial state - need to prepare the query
    Start,
    /// Actively processing rows from the query
    Processing {
        stmt: Box<Statement>,
        rows_processed: usize,
    },
    /// Population complete
    Done,
}

impl fmt::Debug for PopulateState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PopulateState::Start => write!(f, "Start"),
            PopulateState::Processing { rows_processed, .. } => f
                .debug_struct("Processing")
                .field("rows_processed", rows_processed)
                .finish(),
            PopulateState::Done => write!(f, "Done"),
        }
    }
}

/// Per-connection transaction state for incremental views
#[derive(Debug, Clone, Default)]
pub struct ViewTransactionState {
    // Per-connection delta for uncommitted changes (contains both weights and values)
    pub delta: Delta,
}

/// Incremental view that maintains a stream of row keys using DBSP-style computation
/// The actual row data is stored as transformed Values
///
/// This version keeps everything in-memory. This is acceptable for small views, since DBSP
/// doesn't have to track the history of changes. Still for very large views (think of the result
/// of create view v as select * from tbl where x > 1; and that having 1B values.
///
/// We should have a version of this that materializes the results. Materializing will also be good
/// for large aggregations, because then we don't have to re-compute when opening the database
/// again.
///
/// Right now we are supporting the simplest views by keeping the operators in the view and
/// applying them in a sane order. But the general solution would turn this into a DBSP circuit.
#[derive(Debug)]
pub struct IncrementalView {
    // Stream of row keys for this view
    stream: RowKeyStream,
    name: String,
    // Store the actual row data as Values, keyed by row_key
    // Using BTreeMap for ordered iteration
    pub records: BTreeMap<i64, Vec<Value>>,
    // WHERE clause predicate for filtering (kept for compatibility)
    pub where_predicate: FilterPredicate,
    // The SELECT statement that defines how to transform input data
    pub select_stmt: Box<turso_sqlite3_parser::ast::Select>,

    // Internal filter operator for predicate evaluation
    filter_operator: Option<FilterOperator>,
    // Internal project operator for value transformation
    project_operator: Option<ProjectOperator>,
    // Tables referenced by this view (extracted from FROM clause and JOINs)
    base_table: Arc<BTreeTable>,
    // The view's output columns with their types
    pub columns: Vec<Column>,
    // State machine for population
    populate_state: PopulateState,
}

impl IncrementalView {
    /// Validate that a CREATE VIEW statement can be handled by IncrementalView
    /// This should be called early, before updating sqlite_master
    pub fn can_create_view(
        select: &turso_sqlite3_parser::ast::Select,
        schema: &Schema,
    ) -> Result<()> {
        // Check for aggregations
        let (group_by_columns, aggregate_functions, _) = Self::extract_aggregation_info(select);
        if !group_by_columns.is_empty() || !aggregate_functions.is_empty() {
            return Err(LimboError::ParseError(
                "aggregations in views are not yet supported".to_string(),
            ));
        }

        // Check for JOINs
        let (join_tables, join_condition) = Self::extract_join_info(select);
        if join_tables.is_some() || join_condition.is_some() {
            return Err(LimboError::ParseError(
                "JOINs in views are not yet supported".to_string(),
            ));
        }

        // Check that we have a base table
        let base_table_name = Self::extract_base_table(select).ok_or_else(|| {
            LimboError::ParseError("views without a base table not supported yet".to_string())
        })?;

        // Get the base table
        let base_table = schema.get_btree_table(&base_table_name).ok_or_else(|| {
            LimboError::ParseError(format!("Table '{base_table_name}' not found in schema"))
        })?;

        // Get base table column names for validation
        let base_table_column_names: Vec<String> = base_table
            .columns
            .iter()
            .enumerate()
            .map(|(i, col)| col.name.clone().unwrap_or_else(|| format!("column_{i}")))
            .collect();

        // Validate columns are a strict subset
        Self::validate_view_columns(select, &base_table_column_names)?;

        Ok(())
    }

    /// Get an iterator over column names, using enumerated naming for unnamed columns
    pub fn column_names(&self) -> impl Iterator<Item = String> + '_ {
        self.columns.iter().enumerate().map(|(i, col)| {
            col.name
                .clone()
                .unwrap_or_else(|| format!("column{}", i + 1))
        })
    }

    /// Check if this view has the same SQL definition as the provided SQL string
    pub fn has_same_sql(&self, sql: &str) -> bool {
        // Parse the SQL to extract just the SELECT statement
        if let Ok(Some(Cmd::Stmt(Stmt::CreateView { select, .. }))) =
            Parser::new(sql.as_bytes()).next()
        {
            // Compare the SELECT statements as SQL strings
            use turso_sqlite3_parser::ast::fmt::ToTokens;

            // Format both SELECT statements and compare
            if let (Ok(current_sql), Ok(provided_sql)) =
                (self.select_stmt.format(), select.format())
            {
                return current_sql == provided_sql;
            }
        }
        false
    }

    /// Apply filter operator to check if values pass the view's WHERE clause
    fn apply_filter(&self, values: &[Value]) -> bool {
        if let Some(ref filter_op) = self.filter_operator {
            filter_op.evaluate_predicate(values)
        } else {
            true
        }
    }
    pub fn from_sql(sql: &str, schema: &Schema) -> Result<Self> {
        let mut parser = Parser::new(sql.as_bytes());
        let cmd = parser.next()?;
        let cmd = cmd.expect("View is an empty statement");
        match cmd {
            Cmd::Stmt(Stmt::CreateView {
                temporary: _,
                if_not_exists: _,
                view_name,
                columns: _,
                select,
            }) => IncrementalView::from_stmt(view_name, select, schema),
            _ => Err(LimboError::ParseError(format!(
                "View is not a CREATE VIEW statement: {sql}"
            ))),
        }
    }

    pub fn from_stmt(
        view_name: turso_sqlite3_parser::ast::QualifiedName,
        select: Box<turso_sqlite3_parser::ast::Select>,
        schema: &Schema,
    ) -> Result<Self> {
        let name = view_name.name.as_str().to_string();

        let where_predicate = FilterPredicate::from_select(&select);

        // Extract output columns using the shared function
        let view_columns = extract_view_columns(&select, schema);

        // Extract GROUP BY columns and aggregate functions
        let (group_by_columns, aggregate_functions, _old_output_names) =
            Self::extract_aggregation_info(&select);

        if !group_by_columns.is_empty() || !aggregate_functions.is_empty() {
            return Err(LimboError::ParseError(
                "aggregations in views are not yet supported".to_string(),
            ));
        }

        let (join_tables, join_condition) = Self::extract_join_info(&select);
        if join_tables.is_some() || join_condition.is_some() {
            return Err(LimboError::ParseError(
                "JOINs in views are not yet supported".to_string(),
            ));
        }

        // Get the base table from FROM clause (when no joins)
        let base_table = if let Some(base_table_name) = Self::extract_base_table(&select) {
            if let Some(table) = schema.get_btree_table(&base_table_name) {
                table.clone()
            } else {
                return Err(LimboError::ParseError(format!(
                    "Table '{base_table_name}' not found in schema"
                )));
            }
        } else {
            return Err(LimboError::ParseError(
                "views without a base table not supported yet".to_string(),
            ));
        };

        let base_table_column_names = base_table
            .columns
            .iter()
            .enumerate()
            .map(|(i, col)| col.name.clone().unwrap_or_else(|| format!("column_{i}")))
            .collect();

        Ok(Self::new(
            name,
            Vec::new(), // Empty initial data
            where_predicate,
            select.clone(),
            base_table,
            base_table_column_names,
            view_columns,
        ))
    }

    pub fn new(
        name: String,
        initial_data: Vec<(i64, Vec<Value>)>,
        where_predicate: FilterPredicate,
        select_stmt: Box<turso_sqlite3_parser::ast::Select>,
        base_table: Arc<BTreeTable>,
        base_table_column_names: Vec<String>,
        columns: Vec<Column>,
    ) -> Self {
        let mut records = BTreeMap::new();

        for (row_key, values) in initial_data {
            records.insert(row_key, values);
        }

        // Create initial stream with row keys
        let mut zset = RowKeyZSet::new();
        for (row_key, values) in &records {
            use crate::incremental::hashable_row::HashableRow;
            let row = HashableRow::new(*row_key, values.clone());
            zset.insert(row, 1);
        }

        // Create filter operator if we have a predicate
        let filter_operator = if !matches!(where_predicate, FilterPredicate::None) {
            Some(FilterOperator::new(
                where_predicate.clone(),
                base_table_column_names.clone(),
            ))
        } else {
            None
        };

        let project_operator = {
            let columns = Self::extract_project_columns(&select_stmt, &base_table_column_names)
                .unwrap_or_else(|| {
                    // If we can't extract columns, default to projecting all columns
                    base_table_column_names
                        .iter()
                        .map(|name| ProjectColumn::Column(name.to_string()))
                        .collect()
                });
            Some(ProjectOperator::new(
                columns,
                base_table_column_names.clone(),
            ))
        };

        Self {
            stream: RowKeyStream::from_zset(zset),
            name,
            records,
            where_predicate,
            select_stmt,
            filter_operator,
            project_operator,
            base_table,
            columns,
            populate_state: PopulateState::Start,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get all table names referenced by this view
    pub fn get_referenced_table_names(&self) -> Vec<String> {
        vec![self.base_table.name.clone()]
    }

    /// Get all tables referenced by this view
    pub fn get_referenced_tables(&self) -> Vec<Arc<BTreeTable>> {
        vec![self.base_table.clone()]
    }

    /// Validate that view columns are a strict subset of the base table columns
    /// No duplicates, no complex expressions, only simple column references
    fn validate_view_columns(
        select: &turso_sqlite3_parser::ast::Select,
        base_table_column_names: &[String],
    ) -> Result<()> {
        if let turso_sqlite3_parser::ast::OneSelect::Select(ref select_stmt) = &*select.body.select
        {
            let mut seen_columns = std::collections::HashSet::new();

            for result_col in &select_stmt.columns {
                match result_col {
                    turso_sqlite3_parser::ast::ResultColumn::Expr(
                        turso_sqlite3_parser::ast::Expr::Id(name),
                        _,
                    ) => {
                        let col_name = name.as_str();

                        // Check for duplicates
                        if !seen_columns.insert(col_name) {
                            return Err(LimboError::ParseError(format!(
                                "Duplicate column '{col_name}' in view. Views must have columns as a strict subset of the base table (no duplicates)"
                            )));
                        }

                        // Check that column exists in base table
                        if !base_table_column_names.iter().any(|n| n == col_name) {
                            return Err(LimboError::ParseError(format!(
                                "Column '{col_name}' not found in base table. Views must have columns as a strict subset of the base table"
                            )));
                        }
                    }
                    turso_sqlite3_parser::ast::ResultColumn::Star => {
                        // SELECT * is allowed - it's the full set
                    }
                    _ => {
                        // Any other expression is not allowed
                        return Err(LimboError::ParseError("Complex expressions, functions, or computed columns are not supported in views. Views must have columns as a strict subset of the base table".to_string()));
                    }
                }
            }
        }
        Ok(())
    }

    /// Extract the base table name from a SELECT statement (for non-join cases)
    fn extract_base_table(select: &turso_sqlite3_parser::ast::Select) -> Option<String> {
        if let turso_sqlite3_parser::ast::OneSelect::Select(ref select_stmt) = &*select.body.select
        {
            if let Some(ref from) = &select_stmt.from {
                if let Some(ref select_table) = &from.select {
                    if let turso_sqlite3_parser::ast::SelectTable::Table(name, _, _) =
                        &**select_table
                    {
                        return Some(name.name.as_str().to_string());
                    }
                }
            }
        }
        None
    }

    /// Generate the SQL query for populating the view from its source table
    fn sql_for_populate(&self) -> crate::Result<String> {
        // Get the base table from referenced tables
        let table = &self.base_table;

        // Build column list for SELECT clause
        let select_columns = if let Some(ref project_op) = self.project_operator {
            // Get the columns used by the projection operator
            let mut columns = Vec::new();
            for col in project_op.columns() {
                match col {
                    ProjectColumn::Column(name) => {
                        columns.push(name.clone());
                    }
                    ProjectColumn::Expression { .. } => {
                        // For expressions, we need all columns (for now)
                        columns.clear();
                        columns.push("*".to_string());
                        break;
                    }
                }
            }
            if columns.is_empty() || columns.contains(&"*".to_string()) {
                "*".to_string()
            } else {
                // Add the columns and always include rowid
                columns.join(", ").to_string()
            }
        } else {
            // No projection, use all columns
            "*".to_string()
        };

        // Build WHERE clause from filter operator
        let where_clause = if let Some(ref filter_op) = self.filter_operator {
            self.build_where_clause(filter_op.predicate())?
        } else {
            String::new()
        };

        // Construct the final query
        let query = if where_clause.is_empty() {
            format!("SELECT {}, rowid FROM {}", select_columns, table.name)
        } else {
            format!(
                "SELECT {}, rowid FROM {} WHERE {}",
                select_columns, table.name, where_clause
            )
        };
        Ok(query)
    }

    /// Build a WHERE clause from a FilterPredicate
    fn build_where_clause(&self, predicate: &FilterPredicate) -> crate::Result<String> {
        match predicate {
            FilterPredicate::None => Ok(String::new()),
            FilterPredicate::Equals { column, value } => {
                Ok(format!("{} = {}", column, self.value_to_sql(value)))
            }
            FilterPredicate::NotEquals { column, value } => {
                Ok(format!("{} != {}", column, self.value_to_sql(value)))
            }
            FilterPredicate::GreaterThan { column, value } => {
                Ok(format!("{} > {}", column, self.value_to_sql(value)))
            }
            FilterPredicate::GreaterThanOrEqual { column, value } => {
                Ok(format!("{} >= {}", column, self.value_to_sql(value)))
            }
            FilterPredicate::LessThan { column, value } => {
                Ok(format!("{} < {}", column, self.value_to_sql(value)))
            }
            FilterPredicate::LessThanOrEqual { column, value } => {
                Ok(format!("{} <= {}", column, self.value_to_sql(value)))
            }
            FilterPredicate::And(left, right) => {
                let left_clause = self.build_where_clause(left)?;
                let right_clause = self.build_where_clause(right)?;
                Ok(format!("({left_clause} AND {right_clause})"))
            }
            FilterPredicate::Or(left, right) => {
                let left_clause = self.build_where_clause(left)?;
                let right_clause = self.build_where_clause(right)?;
                Ok(format!("({left_clause} OR {right_clause})"))
            }
        }
    }

    /// Convert a Value to SQL literal representation
    fn value_to_sql(&self, value: &Value) -> String {
        match value {
            Value::Null => "NULL".to_string(),
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Text(t) => format!("'{}'", t.as_str().replace('\'', "''")),
            Value::Blob(_) => "NULL".to_string(), // Blob literals not supported in WHERE clause yet
        }
    }

    /// Populate the view by scanning the source table using a state machine
    /// This can be called multiple times and will resume from where it left off
    pub fn populate_from_table(
        &mut self,
        conn: &std::sync::Arc<crate::Connection>,
    ) -> crate::Result<IOResult<()>> {
        // If already populated, return immediately
        if matches!(self.populate_state, PopulateState::Done) {
            return Ok(IOResult::Done(()));
        }

        const BATCH_SIZE: usize = 100; // Process 100 rows at a time before yielding

        loop {
            match &mut self.populate_state {
                PopulateState::Start => {
                    // Generate the SQL query for populating the view
                    // It is best to use a standard query than a cursor for two reasons:
                    // 1) Using a sql query will allow us to be much more efficient in cases where we only want
                    //    some rows, in particular for indexed filters
                    // 2) There are two types of cursors: index and table. In some situations (like for example
                    //    if the table has an integer primary key), the key will be exclusively in the index
                    //    btree and not in the table btree. Using cursors would force us to be aware of this
                    //    distinction (and others), and ultimately lead to reimplementing the whole query
                    //    machinery (next step is which index is best to use, etc)
                    let query = self.sql_for_populate()?;

                    // Prepare the statement
                    let stmt = conn.prepare(&query)?;

                    self.populate_state = PopulateState::Processing {
                        stmt: Box::new(stmt),
                        rows_processed: 0,
                    };
                    // Continue to next state
                }

                PopulateState::Processing {
                    stmt,
                    rows_processed,
                } => {
                    // Process rows in batches to allow for IO interruption
                    let mut batch_count = 0;

                    loop {
                        if batch_count >= BATCH_SIZE {
                            // Yield control after processing a batch
                            // The statement maintains its position, so we'll resume from here
                            return Ok(IOResult::IO);
                        }

                        // This step() call resumes from where the statement left off
                        match stmt.step()? {
                            crate::vdbe::StepResult::Row => {
                                // Get the row
                                let row = stmt.row().unwrap();

                                // Extract values from the row
                                let all_values: Vec<crate::types::Value> =
                                    row.get_values().cloned().collect();

                                // The last value should be the rowid
                                let rowid = match all_values.last() {
                                    Some(crate::types::Value::Integer(id)) => *id,
                                    _ => {
                                        // This shouldn't happen - rowid must be an integer
                                        *rows_processed += 1;
                                        batch_count += 1;
                                        continue;
                                    }
                                };

                                // Get all values except the rowid
                                let values = all_values[..all_values.len() - 1].to_vec();

                                // Apply filter if we have one
                                // Pure DBSP would ingest the entire stream and then apply filter operators.
                                // However, for initial population, we adopt a hybrid approach where we filter at
                                // the query result level for efficiency. This avoids reading millions of rows just
                                // to filter them down to a few. We only do this optimization for filters, not for
                                // other operators like projections or aggregations.
                                // TODO: We should further optimize by pushing the filter into the SQL WHERE clause.

                                // Check filter first (we need to do this before accessing self mutably)
                                let passes_filter =
                                    if let Some(ref filter_op) = self.filter_operator {
                                        filter_op.evaluate_predicate(&values)
                                    } else {
                                        true
                                    };

                                if passes_filter {
                                    // Store the row with its original rowid
                                    self.records.insert(rowid, values.clone());

                                    // Update the ZSet stream with weight +1
                                    let mut delta = RowKeyZSet::new();
                                    use crate::incremental::hashable_row::HashableRow;
                                    let row = HashableRow::new(rowid, values);
                                    delta.insert(row, 1);
                                    self.stream.apply_delta(&delta);
                                }

                                *rows_processed += 1;
                                batch_count += 1;
                            }
                            crate::vdbe::StepResult::Done => {
                                // All rows processed, move to Done state
                                self.populate_state = PopulateState::Done;
                                return Ok(IOResult::Done(()));
                            }
                            crate::vdbe::StepResult::Interrupt | crate::vdbe::StepResult::Busy => {
                                return Err(LimboError::Busy);
                            }
                            crate::vdbe::StepResult::IO => {
                                // The Statement needs to wait for IO
                                // When we return here, the Statement maintains its position
                                return Ok(IOResult::IO);
                            }
                        }
                    }
                }

                PopulateState::Done => {
                    // Already populated
                    return Ok(IOResult::Done(()));
                }
            }
        }
    }

    /// Extract GROUP BY columns and aggregate functions from SELECT statement
    fn extract_aggregation_info(
        select: &turso_sqlite3_parser::ast::Select,
    ) -> (Vec<String>, Vec<AggregateFunction>, Vec<String>) {
        use turso_sqlite3_parser::ast::*;

        let mut group_by_columns = Vec::new();
        let mut aggregate_functions = Vec::new();
        let mut output_column_names = Vec::new();

        if let OneSelect::Select(ref select_stmt) = &*select.body.select {
            // Extract GROUP BY columns
            if let Some(ref group_by) = select_stmt.group_by {
                for expr in &group_by.exprs {
                    if let Some(col_name) = extract_column_name_from_expr(expr) {
                        group_by_columns.push(col_name);
                    }
                }
            }

            // Extract aggregate functions and column names/aliases from SELECT list
            for result_col in &select_stmt.columns {
                match result_col {
                    ResultColumn::Expr(expr, alias) => {
                        // Extract aggregate functions
                        let mut found_aggregates = Vec::new();
                        Self::extract_aggregates_from_expr(expr, &mut found_aggregates);

                        // Determine the output column name
                        let col_name = if let Some(As::As(alias_name)) = alias {
                            // Use the provided alias
                            alias_name.as_str().to_string()
                        } else if !found_aggregates.is_empty() {
                            // Use the default name from the aggregate function
                            found_aggregates[0].default_output_name()
                        } else if let Some(name) = extract_column_name_from_expr(expr) {
                            // Use the column name
                            name
                        } else {
                            // Fallback to a generic name
                            format!("column{}", output_column_names.len() + 1)
                        };

                        output_column_names.push(col_name);
                        aggregate_functions.extend(found_aggregates);
                    }
                    ResultColumn::Star => {
                        // For SELECT *, we'd need to know the base table columns
                        // This is handled elsewhere
                    }
                    ResultColumn::TableStar(_) => {
                        // Similar to Star, but for a specific table
                    }
                }
            }
        }

        (group_by_columns, aggregate_functions, output_column_names)
    }

    /// Recursively extract aggregate functions from an expression
    fn extract_aggregates_from_expr(
        expr: &turso_sqlite3_parser::ast::Expr,
        aggregate_functions: &mut Vec<AggregateFunction>,
    ) {
        use crate::function::Func;
        use turso_sqlite3_parser::ast::*;

        match expr {
            // Handle COUNT(*) and similar aggregate functions with *
            Expr::FunctionCallStar { name, .. } => {
                // FunctionCallStar is typically COUNT(*), which has 0 args
                if let Ok(func) = Func::resolve_function(name.as_str(), 0) {
                    // Use the centralized mapping from operator.rs
                    // For COUNT(*), we pass None as the input column
                    if let Some(agg_func) = AggregateFunction::from_sql_function(&func, None) {
                        aggregate_functions.push(agg_func);
                    }
                }
            }
            Expr::FunctionCall { name, args, .. } => {
                // Regular function calls with arguments
                let arg_count = args.as_ref().map_or(0, |a| a.len());

                if let Ok(func) = Func::resolve_function(name.as_str(), arg_count) {
                    // Extract the input column if there's an argument
                    let input_column = if arg_count > 0 {
                        args.as_ref()
                            .and_then(|args| args.first())
                            .and_then(extract_column_name_from_expr)
                    } else {
                        None
                    };

                    // Use the centralized mapping from operator.rs
                    if let Some(agg_func) =
                        AggregateFunction::from_sql_function(&func, input_column)
                    {
                        aggregate_functions.push(agg_func);
                    }
                }
            }
            // Recursively check binary expressions, etc.
            Expr::Binary(left, _, right) => {
                Self::extract_aggregates_from_expr(left, aggregate_functions);
                Self::extract_aggregates_from_expr(right, aggregate_functions);
            }
            _ => {}
        }
    }

    /// Extract JOIN information from SELECT statement
    #[allow(clippy::type_complexity)]
    pub fn extract_join_info(
        select: &turso_sqlite3_parser::ast::Select,
    ) -> (Option<(String, String)>, Option<(String, String)>) {
        use turso_sqlite3_parser::ast::*;

        if let OneSelect::Select(ref select_stmt) = &*select.body.select {
            if let Some(ref from) = &select_stmt.from {
                // Check if there are any joins
                if let Some(ref joins) = &from.joins {
                    if !joins.is_empty() {
                        // Get the first (left) table name
                        let left_table = if let Some(ref select_table) = &from.select {
                            match &**select_table {
                                SelectTable::Table(name, _, _) => {
                                    Some(name.name.as_str().to_string())
                                }
                                _ => None,
                            }
                        } else {
                            None
                        };

                        // Get the first join (right) table and condition
                        if let Some(first_join) = joins.first() {
                            let right_table = match &first_join.table {
                                SelectTable::Table(name, _, _) => {
                                    Some(name.name.as_str().to_string())
                                }
                                _ => None,
                            };

                            // Extract join condition (simplified - assumes single equality)
                            let join_condition =
                                if let Some(ref constraint) = &first_join.constraint {
                                    match constraint {
                                        JoinConstraint::On(expr) => {
                                            Self::extract_join_columns_from_expr(expr)
                                        }
                                        _ => None,
                                    }
                                } else {
                                    None
                                };

                            if let (Some(left), Some(right)) = (left_table, right_table) {
                                return (Some((left, right)), join_condition);
                            }
                        }
                    }
                }
            }
        }

        (None, None)
    }

    /// Extract join column names from a join condition expression
    fn extract_join_columns_from_expr(
        expr: &turso_sqlite3_parser::ast::Expr,
    ) -> Option<(String, String)> {
        use turso_sqlite3_parser::ast::*;

        // Look for expressions like: t1.col = t2.col
        if let Expr::Binary(left, op, right) = expr {
            if matches!(op, Operator::Equals) {
                // Extract column names from both sides
                let left_col = match &**left {
                    Expr::Qualified(name, _) => Some(name.as_str().to_string()),
                    Expr::Id(name) => Some(name.as_str().to_string()),
                    _ => None,
                };

                let right_col = match &**right {
                    Expr::Qualified(name, _) => Some(name.as_str().to_string()),
                    Expr::Id(name) => Some(name.as_str().to_string()),
                    _ => None,
                };

                if let (Some(l), Some(r)) = (left_col, right_col) {
                    return Some((l, r));
                }
            }
        }

        None
    }

    /// Extract projection columns from SELECT statement
    fn extract_project_columns(
        select: &turso_sqlite3_parser::ast::Select,
        column_names: &[String],
    ) -> Option<Vec<ProjectColumn>> {
        use turso_sqlite3_parser::ast::*;

        if let OneSelect::Select(ref select_stmt) = &*select.body.select {
            let mut columns = Vec::new();

            for result_col in &select_stmt.columns {
                match result_col {
                    ResultColumn::Expr(Expr::Id(name), _) => {
                        columns.push(ProjectColumn::Column(name.as_str().to_string()));
                    }
                    ResultColumn::Star => {
                        // Select all columns
                        for name in column_names {
                            columns.push(ProjectColumn::Column(name.as_str().to_string()));
                        }
                    }
                    _ => {
                        // For now, skip complex expressions
                        // Could be extended to handle more cases
                    }
                }
            }

            if !columns.is_empty() {
                return Some(columns);
            }
        }

        None
    }

    /// Get the current records as an iterator - for cursor-based access
    pub fn iter(&self) -> impl Iterator<Item = (i64, Vec<Value>)> + '_ {
        self.stream.to_vec().into_iter().filter_map(move |row| {
            self.records
                .get(&row.rowid)
                .map(|values| (row.rowid, values.clone()))
        })
    }

    /// Get current data merged with transaction state
    pub fn current_data(&self, tx_state: Option<&ViewTransactionState>) -> Vec<(i64, Vec<Value>)> {
        // Start with committed records

        if let Some(tx_state) = tx_state {
            // processed_delta = input delta for now. Need to apply operations
            let processed_delta = &tx_state.delta;

            // For non-aggregation views, merge the processed delta with committed records
            let mut result_map: BTreeMap<i64, Vec<Value>> = self.records.clone();

            for (row, weight) in &processed_delta.changes {
                if *weight > 0 && self.apply_filter(&row.values) {
                    result_map.insert(row.rowid, row.values.clone());
                } else if *weight < 0 {
                    result_map.remove(&row.rowid);
                }
            }

            result_map.into_iter().collect()
        } else {
            // No transaction state: return committed records
            self.records.clone().into_iter().collect()
        }
    }

    /// Merge a delta of changes into the view's current state
    pub fn merge_delta(&mut self, delta: &Delta) {
        // Create a Z-set of changes to apply to the stream
        let mut zset_delta = RowKeyZSet::new();

        // Apply the delta changes to the records
        for (row, weight) in &delta.changes {
            if *weight > 0 {
                // Insert
                if self.apply_filter(&row.values) {
                    self.records.insert(row.rowid, row.values.clone());
                    zset_delta.insert(row.clone(), 1);
                }
            } else if *weight < 0 {
                // Delete
                if self.records.remove(&row.rowid).is_some() {
                    zset_delta.insert(row.clone(), -1);
                }
            }
        }

        // Apply all changes to the stream at once
        self.stream.apply_delta(&zset_delta);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::incremental::operator::{Delta, IncrementalOperator};
    use crate::schema::{BTreeTable, Column, Schema, Type};
    use crate::types::Value;
    use std::sync::Arc;
    fn create_test_schema() -> Schema {
        let mut schema = Schema::new(false);
        let table = BTreeTable {
            root_page: 1,
            name: "t".to_string(),
            columns: vec![
                Column {
                    name: Some("a".to_string()),
                    ty: Type::Integer,
                    ty_str: "INTEGER".to_string(),
                    primary_key: false,
                    is_rowid_alias: false,
                    notnull: false,
                    default: None,
                    unique: false,
                    collation: None,
                    hidden: false,
                },
                Column {
                    name: Some("b".to_string()),
                    ty: Type::Integer,
                    ty_str: "INTEGER".to_string(),
                    primary_key: false,
                    is_rowid_alias: false,
                    notnull: false,
                    default: None,
                    unique: false,
                    collation: None,
                    hidden: false,
                },
                Column {
                    name: Some("c".to_string()),
                    ty: Type::Integer,
                    ty_str: "INTEGER".to_string(),
                    primary_key: false,
                    is_rowid_alias: false,
                    notnull: false,
                    default: None,
                    unique: false,
                    collation: None,
                    hidden: false,
                },
            ],
            primary_key_columns: vec![],
            has_rowid: true,
            is_strict: false,
            unique_sets: None,
        };
        schema.add_btree_table(Arc::new(table));
        schema
    }

    #[test]
    fn test_projection_simple_columns() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT a, b FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(10), Value::Integer(20), Value::Integer(30)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(output.values, vec![Value::Integer(10), Value::Integer(20)]);
    }

    #[test]
    fn test_projection_arithmetic_expression() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT a * 2 as doubled FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(4), Value::Integer(2), Value::Integer(0)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(output.values, vec![Value::Integer(8)]);
    }

    #[test]
    fn test_projection_multiple_expressions() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT a + b as sum, a - b as diff, c FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(10), Value::Integer(3), Value::Integer(7)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(
            output.values,
            vec![Value::Integer(13), Value::Integer(7), Value::Integer(7),]
        );
    }

    #[test]
    fn test_projection_function_call() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT hex(a) as hex_a, b FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(255), Value::Integer(20), Value::Integer(30)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(
            output.values,
            vec![Value::Text("FF".into()), Value::Integer(20),]
        );
    }

    #[test]
    fn test_projection_mixed_columns_and_expressions() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT a, b * 2 as doubled, c, a + b + c as total FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(1), Value::Integer(5), Value::Integer(3)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(
            output.values,
            vec![
                Value::Integer(1),
                Value::Integer(10),
                Value::Integer(3),
                Value::Integer(9),
            ]
        );
    }

    #[test]
    fn test_projection_complex_expression() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT (a * 2) + (b * 3) as weighted, c / 2 as half FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(5), Value::Integer(2), Value::Integer(10)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(output.values, vec![Value::Integer(16), Value::Integer(5),]);
    }

    #[test]
    fn test_projection_with_where_clause() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT a, a * 2 as doubled FROM t WHERE b > 2";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        assert!(view.filter_operator.is_some());

        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(4), Value::Integer(3), Value::Integer(0)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        assert_eq!(output.values, vec![Value::Integer(4), Value::Integer(8),]);
    }

    #[test]
    fn test_projection_more_output_columns_than_input() {
        let schema = create_test_schema();
        let sql = "CREATE VIEW v AS SELECT a, b, a * 2 as doubled_a, b * 3 as tripled_b, a + b as sum, hex(c) as hex_c FROM t";

        let view = IncrementalView::from_sql(sql, &schema).unwrap();

        assert!(view.project_operator.is_some());
        let project_op = view.project_operator.as_ref().unwrap();

        let mut delta = Delta::new();
        delta.insert(
            1,
            vec![Value::Integer(5), Value::Integer(2), Value::Integer(15)],
        );

        let mut temp_project = project_op.clone();
        temp_project.initialize(delta);
        let result = temp_project.get_current_state();

        let (output, _weight) = result.changes.first().unwrap();
        // 3 input columns -> 6 output columns
        assert_eq!(
            output.values,
            vec![
                Value::Integer(5),       // a
                Value::Integer(2),       // b
                Value::Integer(10),      // a * 2
                Value::Integer(6),       // b * 3
                Value::Integer(7),       // a + b
                Value::Text("F".into()), // hex(15)
            ]
        );
    }
}
