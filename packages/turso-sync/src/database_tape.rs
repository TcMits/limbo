use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};

use crate::{
    types::{
        DatabaseChange, DatabaseTapeOperation, DatabaseTapeRowChange, DatabaseTapeRowChangeType,
    },
    Result,
};

/// Simple wrapper over [turso::Database] which extends its intereface with few methods
/// to collect changes made to the database and apply/revert arbitrary changes to the database
pub struct DatabaseTape {
    inner: turso::Database,
    cdc_table: Arc<String>,
    pragma_query: String,
}

const DEFAULT_CDC_TABLE_NAME: &str = "turso_cdc";
const DEFAULT_CDC_MODE: &str = "full";
const DEFAULT_CHANGES_BATCH_SIZE: usize = 100;
const CDC_PRAGMA_NAME: &str = "unstable_capture_data_changes_conn";

#[derive(Debug)]
pub struct DatabaseTapeOpts {
    pub cdc_table: Option<String>,
    pub cdc_mode: Option<String>,
}

impl DatabaseTape {
    pub fn new(database: turso::Database) -> Self {
        let opts = DatabaseTapeOpts {
            cdc_table: None,
            cdc_mode: None,
        };
        Self::new_with_opts(database, opts)
    }
    pub fn new_with_opts(database: turso::Database, opts: DatabaseTapeOpts) -> Self {
        tracing::debug!("create local sync database with options {:?}", opts);
        let cdc_table = opts.cdc_table.unwrap_or(DEFAULT_CDC_TABLE_NAME.to_string());
        let cdc_mode = opts.cdc_mode.unwrap_or(DEFAULT_CDC_MODE.to_string());
        let pragma_query = format!("PRAGMA {CDC_PRAGMA_NAME}('{cdc_mode},{cdc_table}')");
        Self {
            inner: database,
            cdc_table: Arc::new(cdc_table.to_string()),
            pragma_query,
        }
    }
    pub async fn connect(&self) -> Result<turso::Connection> {
        let connection = self.inner.connect()?;
        tracing::debug!("set '{CDC_PRAGMA_NAME}' for new connection");
        connection.execute(&self.pragma_query, ()).await?;
        Ok(connection)
    }
    /// Builds an iterator which emits [DatabaseTapeOperation] by extracting data from CDC table
    pub async fn iterate_changes(
        &self,
        opts: DatabaseChangesIteratorOpts,
    ) -> Result<DatabaseChangesIterator> {
        tracing::debug!("opening changes iterator with options {:?}", opts);
        let conn = self.inner.connect()?;
        let query = opts.mode.query(&self.cdc_table, opts.batch_size);
        let query_stmt = conn.prepare(&query).await?;
        Ok(DatabaseChangesIterator {
            first_change_id: opts.first_change_id,
            batch: VecDeque::with_capacity(opts.batch_size),
            query_stmt,
            txn_boundary_returned: false,
            mode: opts.mode,
            ignore_schema_changes: opts.ignore_schema_changes,
        })
    }
    /// Start replay session which can apply [DatabaseTapeOperation] from [Self::iterate_changes]
    pub async fn start_tape_session(&self) -> Result<DatabaseReplaySession> {
        tracing::debug!("opening replay session");
        Ok(DatabaseReplaySession {
            conn: self.connect().await?,
            cached_delete_stmt: HashMap::new(),
            cached_insert_stmt: HashMap::new(),
            in_txn: false,
        })
    }
}

#[derive(Debug)]
pub enum DatabaseChangesIteratorMode {
    Apply,
    Revert,
}

impl DatabaseChangesIteratorMode {
    pub fn query(&self, table_name: &str, limit: usize) -> String {
        let (operation, order) = match self {
            DatabaseChangesIteratorMode::Apply => (">=", "ASC"),
            DatabaseChangesIteratorMode::Revert => ("<=", "DESC"),
        };
        format!(
            "SELECT * FROM {table_name} WHERE change_id {operation} ? ORDER BY change_id {order} LIMIT {limit}",
        )
    }
    pub fn first_id(&self) -> i64 {
        match self {
            DatabaseChangesIteratorMode::Apply => -1,
            DatabaseChangesIteratorMode::Revert => i64::MAX,
        }
    }
    pub fn next_id(&self, id: i64) -> i64 {
        match self {
            DatabaseChangesIteratorMode::Apply => id + 1,
            DatabaseChangesIteratorMode::Revert => id - 1,
        }
    }
}

#[derive(Debug)]
pub struct DatabaseChangesIteratorOpts {
    pub first_change_id: Option<i64>,
    pub batch_size: usize,
    pub mode: DatabaseChangesIteratorMode,
    pub ignore_schema_changes: bool,
}

impl Default for DatabaseChangesIteratorOpts {
    fn default() -> Self {
        Self {
            first_change_id: None,
            batch_size: DEFAULT_CHANGES_BATCH_SIZE,
            mode: DatabaseChangesIteratorMode::Apply,
            ignore_schema_changes: true,
        }
    }
}

pub struct DatabaseChangesIterator {
    query_stmt: turso::Statement,
    first_change_id: Option<i64>,
    batch: VecDeque<DatabaseTapeRowChange>,
    txn_boundary_returned: bool,
    mode: DatabaseChangesIteratorMode,
    ignore_schema_changes: bool,
}

impl DatabaseChangesIterator {
    pub async fn next(&mut self) -> Result<Option<DatabaseTapeOperation>> {
        if self.batch.is_empty() {
            self.refill().await?;
        }
        // todo(sivukhin): iterator must be more clever about transaction boundaries - but for that we need to extend CDC table
        // for now, if iterator reach the end of CDC table - we are sure that this is a transaction boundary
        loop {
            let next = if let Some(change) = self.batch.pop_front() {
                self.txn_boundary_returned = false;
                Some(DatabaseTapeOperation::RowChange(change))
            } else if !self.txn_boundary_returned {
                self.txn_boundary_returned = true;
                Some(DatabaseTapeOperation::Commit)
            } else {
                None
            };
            if let Some(DatabaseTapeOperation::RowChange(change)) = &next {
                if self.ignore_schema_changes && change.table_name == "sqlite_schema" {
                    continue;
                }
            }
            return Ok(next);
        }
    }
    async fn refill(&mut self) -> Result<()> {
        let change_id_filter = self.first_change_id.unwrap_or(self.mode.first_id());
        self.query_stmt.reset();

        let mut rows = self.query_stmt.query((change_id_filter,)).await?;
        while let Some(row) = rows.next().await? {
            let database_change: DatabaseChange = row.try_into()?;
            let tape_change = match self.mode {
                DatabaseChangesIteratorMode::Apply => database_change.into_apply()?,
                DatabaseChangesIteratorMode::Revert => database_change.into_revert()?,
            };
            self.batch.push_back(tape_change);
        }
        let batch_len = self.batch.len();
        if batch_len > 0 {
            self.first_change_id = Some(self.mode.next_id(self.batch[batch_len - 1].change_id));
        }
        Ok(())
    }
}

pub struct DatabaseReplaySession {
    conn: turso::Connection,
    cached_delete_stmt: HashMap<String, turso::Statement>,
    cached_insert_stmt: HashMap<(String, usize), turso::Statement>,
    in_txn: bool,
}

impl DatabaseReplaySession {
    pub async fn replay(&mut self, operation: DatabaseTapeOperation) -> Result<()> {
        match operation {
            DatabaseTapeOperation::Commit => {
                tracing::trace!("replay: commit replayed changes after transaction boundary");
                if self.in_txn {
                    self.conn.execute("COMMIT", ()).await?;
                    self.in_txn = false;
                }
            }
            DatabaseTapeOperation::RowChange(change) => {
                if !self.in_txn {
                    tracing::trace!("replay: start txn for replaying changes");
                    self.conn.execute("BEGIN", ()).await?;
                    self.in_txn = true;
                }
                tracing::trace!("replay: change={:?}", change);
                let table_name = &change.table_name;
                match change.change {
                    DatabaseTapeRowChangeType::Delete => {
                        self.replay_delete(table_name, change.id).await?
                    }
                    DatabaseTapeRowChangeType::Update { bin_record } => {
                        self.replay_delete(table_name, change.id).await?;
                        let values = parse_bin_record(bin_record)?;
                        self.replay_insert(table_name, change.id, values).await?;
                    }
                    DatabaseTapeRowChangeType::Insert { bin_record } => {
                        let values = parse_bin_record(bin_record)?;
                        self.replay_insert(table_name, change.id, values).await?;
                    }
                }
            }
        }
        Ok(())
    }
    async fn replay_delete(&mut self, table_name: &str, id: i64) -> Result<()> {
        let stmt = self.cached_delete_stmt(table_name).await?;
        stmt.execute((id,)).await?;
        Ok(())
    }
    async fn replay_insert(
        &mut self,
        table_name: &str,
        id: i64,
        mut values: Vec<turso::Value>,
    ) -> Result<()> {
        let columns = values.len();
        let stmt = self.cached_insert_stmt(table_name, columns).await?;

        values.push(turso::Value::Integer(id));
        let params = turso::params::Params::Positional(values);

        stmt.execute(params).await?;
        Ok(())
    }
    async fn cached_delete_stmt(&mut self, table_name: &str) -> Result<&mut turso::Statement> {
        if !self.cached_delete_stmt.contains_key(table_name) {
            tracing::trace!("prepare delete statement for replay: table={}", table_name);
            let query = format!("DELETE FROM {table_name} WHERE rowid = ?");
            let stmt = self.conn.prepare(&query).await?;
            self.cached_delete_stmt.insert(table_name.to_string(), stmt);
        }
        tracing::trace!(
            "ready to use prepared delete statement for replay: table={}",
            table_name
        );
        Ok(self.cached_delete_stmt.get_mut(table_name).unwrap())
    }
    async fn cached_insert_stmt(
        &mut self,
        table_name: &str,
        columns: usize,
    ) -> Result<&mut turso::Statement> {
        let key = (table_name.to_string(), columns);
        if !self.cached_insert_stmt.contains_key(&key) {
            tracing::trace!(
                "prepare insert statement for replay: table={}, columns={}",
                table_name,
                columns
            );
            let mut table_info = self
                .conn
                .query(
                    &format!("SELECT name FROM pragma_table_info('{table_name}')"),
                    (),
                )
                .await?;

            let mut column_names = Vec::with_capacity(columns + 1);
            while let Some(table_info_row) = table_info.next().await? {
                let value = table_info_row.get_value(0)?;
                column_names.push(value.as_text().expect("must be text").to_string());
            }
            column_names.push("rowid".to_string());

            let placeholders = ["?"].repeat(columns + 1).join(",");
            let column_names = column_names.join(", ");
            let query = format!("INSERT INTO {table_name}({column_names}) VALUES ({placeholders})");
            let stmt = self.conn.prepare(&query).await?;
            self.cached_insert_stmt.insert(key.clone(), stmt);
        }
        tracing::trace!(
            "ready to use prepared insert statement for replay: table={}, columns={}",
            table_name,
            columns
        );
        Ok(self.cached_insert_stmt.get_mut(&key).unwrap())
    }
}

fn parse_bin_record(bin_record: Vec<u8>) -> Result<Vec<turso::Value>> {
    let record = turso_core::types::ImmutableRecord::from_bin_record(bin_record);
    let mut cursor = turso_core::types::RecordCursor::new();
    let columns = cursor.count(&record);
    let mut values = Vec::with_capacity(columns);
    for i in 0..columns {
        let value = cursor.get_value(&record, i)?;
        values.push(value.to_owned().into());
    }
    Ok(values)
}

#[cfg(test)]
mod tests {
    use tempfile::NamedTempFile;
    use turso::Value;

    use crate::{
        database_tape::{DatabaseChangesIteratorOpts, DatabaseTape},
        types::DatabaseTapeOperation,
    };

    async fn fetch_rows(conn: &turso::Connection, query: &str) -> Vec<Vec<turso::Value>> {
        let mut rows = vec![];
        let mut iterator = conn.query(query, ()).await.unwrap();
        while let Some(row) = iterator.next().await.unwrap() {
            let mut row_values = vec![];
            for i in 0..row.column_count() {
                row_values.push(row.get_value(i).unwrap());
            }
            rows.push(row_values);
        }
        rows
    }

    #[tokio::test]
    async fn test_database_cdc_single_iteration() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();

        let temp_file2 = NamedTempFile::new().unwrap();
        let db_path2 = temp_file2.path().to_str().unwrap();

        let db1 = turso::Builder::new_local(db_path1).build().await.unwrap();
        let db1 = DatabaseTape::new(db1);
        let conn1 = db1.connect().await.unwrap();

        let db2 = turso::Builder::new_local(db_path2).build().await.unwrap();
        let db2 = DatabaseTape::new(db2);
        let conn2 = db2.connect().await.unwrap();

        conn1
            .execute("CREATE TABLE a(x INTEGER PRIMARY KEY, y);", ())
            .await
            .unwrap();
        conn1
            .execute("CREATE TABLE b(x INTEGER PRIMARY KEY, y, z);", ())
            .await
            .unwrap();
        conn2
            .execute("CREATE TABLE a(x INTEGER PRIMARY KEY, y);", ())
            .await
            .unwrap();
        conn2
            .execute("CREATE TABLE b(x INTEGER PRIMARY KEY, y, z);", ())
            .await
            .unwrap();

        conn1
            .execute("INSERT INTO a VALUES (1, 'hello'), (2, 'turso')", ())
            .await
            .unwrap();

        conn1
            .execute(
                "INSERT INTO b VALUES (3, 'bye', 0.1), (4, 'limbo', 0.2)",
                (),
            )
            .await
            .unwrap();

        let mut iterator = db1.iterate_changes(Default::default()).await.unwrap();
        {
            let mut replay = db2.start_tape_session().await.unwrap();
            while let Some(change) = iterator.next().await.unwrap() {
                replay.replay(change).await.unwrap();
            }
        }
        assert_eq!(
            fetch_rows(&conn2, "SELECT * FROM a").await,
            vec![
                vec![
                    turso::Value::Integer(1),
                    turso::Value::Text("hello".to_string())
                ],
                vec![
                    turso::Value::Integer(2),
                    turso::Value::Text("turso".to_string())
                ],
            ]
        );
        assert_eq!(
            fetch_rows(&conn2, "SELECT * FROM b").await,
            vec![
                vec![
                    turso::Value::Integer(3),
                    turso::Value::Text("bye".to_string()),
                    turso::Value::Real(0.1)
                ],
                vec![
                    turso::Value::Integer(4),
                    turso::Value::Text("limbo".to_string()),
                    turso::Value::Real(0.2)
                ],
            ]
        );

        conn1
            .execute("DELETE FROM b WHERE y = 'limbo'", ())
            .await
            .unwrap();

        {
            let mut replay = db2.start_tape_session().await.unwrap();
            while let Some(change) = iterator.next().await.unwrap() {
                replay.replay(change).await.unwrap();
            }
        }

        assert_eq!(
            fetch_rows(&conn2, "SELECT * FROM a").await,
            vec![
                vec![
                    turso::Value::Integer(1),
                    turso::Value::Text("hello".to_string())
                ],
                vec![
                    turso::Value::Integer(2),
                    turso::Value::Text("turso".to_string())
                ],
            ]
        );
        assert_eq!(
            fetch_rows(&conn2, "SELECT * FROM b").await,
            vec![vec![
                turso::Value::Integer(3),
                turso::Value::Text("bye".to_string()),
                turso::Value::Real(0.1)
            ],]
        );

        conn1
            .execute("UPDATE b SET y = x'deadbeef' WHERE x = 3", ())
            .await
            .unwrap();

        {
            let mut replay = db2.start_tape_session().await.unwrap();
            while let Some(change) = iterator.next().await.unwrap() {
                replay.replay(change).await.unwrap();
            }
        }

        assert_eq!(
            fetch_rows(&conn2, "SELECT * FROM a").await,
            vec![
                vec![
                    turso::Value::Integer(1),
                    turso::Value::Text("hello".to_string())
                ],
                vec![
                    turso::Value::Integer(2),
                    turso::Value::Text("turso".to_string())
                ],
            ]
        );
        assert_eq!(
            fetch_rows(&conn2, "SELECT * FROM b").await,
            vec![vec![
                turso::Value::Integer(3),
                turso::Value::Blob(vec![0xde, 0xad, 0xbe, 0xef]),
                turso::Value::Real(0.1)
            ]]
        );
    }

    #[tokio::test]
    async fn test_database_cdc_multiple_iterations() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();

        let temp_file2 = NamedTempFile::new().unwrap();
        let db_path2 = temp_file2.path().to_str().unwrap();

        let db1 = turso::Builder::new_local(db_path1).build().await.unwrap();
        let db1 = DatabaseTape::new(db1);
        let conn1 = db1.connect().await.unwrap();

        let db2 = turso::Builder::new_local(db_path2).build().await.unwrap();
        let db2 = DatabaseTape::new(db2);
        let conn2 = db2.connect().await.unwrap();

        conn1
            .execute("CREATE TABLE a(x INTEGER PRIMARY KEY, y);", ())
            .await
            .unwrap();
        conn2
            .execute("CREATE TABLE a(x INTEGER PRIMARY KEY, y);", ())
            .await
            .unwrap();

        let mut next_change_id = None;
        let mut expected = Vec::new();
        for i in 0..10 {
            conn1
                .execute("INSERT INTO a VALUES (?, 'hello')", (i,))
                .await
                .unwrap();
            expected.push(vec![
                Value::Integer(i as i64),
                Value::Text("hello".to_string()),
            ]);

            let mut iterator = db1
                .iterate_changes(DatabaseChangesIteratorOpts {
                    first_change_id: next_change_id,
                    ..Default::default()
                })
                .await
                .unwrap();
            {
                let mut replay = db2.start_tape_session().await.unwrap();
                while let Some(change) = iterator.next().await.unwrap() {
                    if let DatabaseTapeOperation::RowChange(change) = &change {
                        next_change_id = Some(change.change_id + 1);
                    }
                    replay.replay(change).await.unwrap();
                }
            }
            let conn2 = db2.connect().await.unwrap();
            assert_eq!(fetch_rows(&conn2, "SELECT * FROM a").await, expected);
        }
    }
}
