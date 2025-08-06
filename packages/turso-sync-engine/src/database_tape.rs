use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};

use turso_core::{types::WalFrameInfo, StepResult};

use crate::{
    database_sync_operations::WAL_FRAME_HEADER,
    errors::Error,
    types::{
        Coro, DatabaseChange, DatabaseTapeOperation, DatabaseTapeRowChange,
        DatabaseTapeRowChangeType, ProtocolCommand,
    },
    wal_session::WalSession,
    Result,
};

/// Simple wrapper over [turso::Database] which extends its intereface with few methods
/// to collect changes made to the database and apply/revert arbitrary changes to the database
pub struct DatabaseTape {
    inner: Arc<turso_core::Database>,
    cdc_table: Arc<String>,
    pragma_query: String,
}

const DEFAULT_CDC_TABLE_NAME: &str = "turso_cdc";
const DEFAULT_CDC_MODE: &str = "full";
const DEFAULT_CHANGES_BATCH_SIZE: usize = 100;
const CDC_PRAGMA_NAME: &str = "unstable_capture_data_changes_conn";

#[derive(Debug, Clone)]
pub struct DatabaseTapeOpts {
    pub cdc_table: Option<String>,
    pub cdc_mode: Option<String>,
}

pub(crate) async fn run_stmt<'a>(
    coro: &'_ Coro,
    stmt: &'a mut turso_core::Statement,
) -> Result<Option<&'a turso_core::Row>> {
    loop {
        match stmt.step()? {
            StepResult::IO => {
                coro.yield_(ProtocolCommand::IO).await?.none();
            }
            StepResult::Done => {
                return Ok(None);
            }
            StepResult::Interrupt => {
                return Err(Error::DatabaseTapeError(
                    "statement was interrupted".to_string(),
                ))
            }
            StepResult::Busy => {
                return Err(Error::DatabaseTapeError("database is busy".to_string()))
            }
            StepResult::Row => return Ok(Some(stmt.row().unwrap())),
        }
    }
}

pub(crate) async fn exec_stmt<'a>(
    coro: &'_ Coro,
    stmt: &'a mut turso_core::Statement,
) -> Result<()> {
    loop {
        match stmt.step()? {
            StepResult::IO => {
                coro.yield_(ProtocolCommand::IO).await?.none();
            }
            StepResult::Done => {
                return Ok(());
            }
            StepResult::Interrupt => {
                return Err(Error::DatabaseTapeError(
                    "statement was interrupted".to_string(),
                ))
            }
            StepResult::Busy => {
                return Err(Error::DatabaseTapeError("database is busy".to_string()))
            }
            StepResult::Row => panic!("statement should not return any rows"),
        }
    }
}

impl DatabaseTape {
    pub fn new(database: Arc<turso_core::Database>) -> Self {
        let opts = DatabaseTapeOpts {
            cdc_table: None,
            cdc_mode: None,
        };
        Self::new_with_opts(database, opts)
    }
    pub fn new_with_opts(database: Arc<turso_core::Database>, opts: DatabaseTapeOpts) -> Self {
        tracing::debug!("create local sync database with options {:?}", opts);
        let cdc_table_name = opts.cdc_table.unwrap_or(DEFAULT_CDC_TABLE_NAME.to_string());
        let cdc_mode = opts.cdc_mode.unwrap_or(DEFAULT_CDC_MODE.to_string());
        let pragma_query = format!("PRAGMA {CDC_PRAGMA_NAME}('{cdc_mode},{cdc_table_name}')");
        Self {
            inner: database,
            cdc_table: Arc::new(cdc_table_name.to_string()),
            pragma_query,
        }
    }
    pub(crate) fn connect_untracked(&self) -> Result<Arc<turso_core::Connection>> {
        let connection = self.inner.connect()?;
        Ok(connection)
    }
    pub async fn connect(&self, coro: &Coro) -> Result<Arc<turso_core::Connection>> {
        let connection = self.inner.connect()?;
        tracing::debug!("set '{CDC_PRAGMA_NAME}' for new connection");
        let mut stmt = connection.prepare(&self.pragma_query)?;
        run_stmt(&coro, &mut stmt).await?;
        Ok(connection)
    }
    /// Builds an iterator which emits [DatabaseTapeOperation] by extracting data from CDC table
    pub fn iterate_changes(
        &self,
        opts: DatabaseChangesIteratorOpts,
    ) -> Result<DatabaseChangesIterator> {
        tracing::debug!("opening changes iterator with options {:?}", opts);
        let conn = self.inner.connect()?;
        let query = opts.mode.query(&self.cdc_table, opts.batch_size);
        let query_stmt = conn.prepare(&query)?;
        Ok(DatabaseChangesIterator {
            first_change_id: opts.first_change_id,
            batch: VecDeque::with_capacity(opts.batch_size),
            query_stmt,
            txn_boundary_returned: false,
            mode: opts.mode,
            ignore_schema_changes: opts.ignore_schema_changes,
        })
    }
    /// Start raw WAL edit session which can append or rollback pages directly in the current WAL
    pub async fn start_wal_session(&self, coro: &Coro) -> Result<DatabaseWalSession> {
        let conn = self.connect(coro).await?;
        let mut wal_session = WalSession::new(conn);
        wal_session.begin()?;
        Ok(DatabaseWalSession::new(coro, wal_session).await?)
    }

    /// Start replay session which can apply [DatabaseTapeOperation] from [Self::iterate_changes]
    pub async fn start_replay_session(
        &self,
        coro: &Coro,
        opts: DatabaseReplaySessionOpts,
    ) -> Result<DatabaseReplaySession> {
        tracing::debug!("opening replay session");
        Ok(DatabaseReplaySession {
            conn: self.connect(&coro).await?,
            cached_delete_stmt: HashMap::new(),
            cached_insert_stmt: HashMap::new(),
            in_txn: false,
            opts,
        })
    }
}

pub struct DatabaseWalSession {
    page_size: usize,
    next_wal_frame_no: u64,
    wal_session: WalSession,
    prepared_frame: Option<(u32, Vec<u8>)>,
}

impl DatabaseWalSession {
    pub async fn new(coro: &Coro, wal_session: WalSession) -> Result<Self> {
        let conn = wal_session.conn();
        let frames_count = conn.wal_frame_count()?;
        let mut page_size_stmt = conn.prepare("PRAGMA page_size")?;
        let Some(row) = run_stmt(coro, &mut page_size_stmt).await? else {
            return Err(Error::DatabaseTapeError(
                "unable to get database page size".to_string(),
            ));
        };
        if row.len() != 1 {
            return Err(Error::DatabaseTapeError(
                "unexpected columns count for PRAGMA page_size query".to_string(),
            ));
        }
        let turso_core::Value::Integer(page_size) = row.get_value(0) else {
            return Err(Error::DatabaseTapeError(
                "unexpected column type for PRAGMA page_size query".to_string(),
            ));
        };
        let page_size = *page_size;
        let None = run_stmt(coro, &mut page_size_stmt).await? else {
            return Err(Error::DatabaseTapeError(
                "page size pragma returned multiple rows".to_string(),
            ));
        };

        Ok(Self {
            page_size: page_size as usize,
            next_wal_frame_no: frames_count + 1,
            wal_session,
            prepared_frame: None,
        })
    }

    pub fn frames_count(&self) -> Result<u64> {
        Ok(self.wal_session.conn().wal_frame_count()?)
    }

    pub fn append_page(&mut self, page_no: u32, page: &[u8]) -> Result<()> {
        if page.len() != self.page_size {
            return Err(Error::DatabaseTapeError(format!(
                "page.len() must be equal to page_size: {} != {}",
                page.len(),
                self.page_size
            )));
        }
        self.flush_prepared_frame(0)?;

        let mut frame = vec![0u8; WAL_FRAME_HEADER + self.page_size];
        frame[WAL_FRAME_HEADER..].copy_from_slice(page);
        self.prepared_frame = Some((page_no, frame));

        Ok(())
    }

    pub fn rollback_page(&mut self, page_no: u32, frame_watermark: u64) -> Result<()> {
        self.flush_prepared_frame(0)?;

        let conn = self.wal_session.conn();
        let mut frame = vec![0u8; WAL_FRAME_HEADER + self.page_size];
        if conn.try_wal_watermark_read_page(
            page_no,
            &mut frame[WAL_FRAME_HEADER..],
            Some(frame_watermark),
        )? {
            tracing::trace!("rollback page {}", page_no);
            self.prepared_frame = Some((page_no, frame));
        } else {
            tracing::trace!(
                "skip rollback page {} as no page existed with given watermark",
                page_no
            );
        }

        Ok(())
    }

    pub fn rollback_changes_after(&mut self, frame_watermark: u64) -> Result<()> {
        let conn = self.wal_session.conn();
        let pages = conn.wal_changed_pages_after(frame_watermark)?;
        for page_no in pages {
            self.rollback_page(page_no, frame_watermark)?;
        }
        Ok(())
    }

    pub fn db_size(&self) -> Result<u32> {
        let frames_count = self.frames_count()?;
        let conn = self.wal_session.conn();
        let mut page = vec![0u8; self.page_size];
        assert!(conn.try_wal_watermark_read_page(1, &mut page, Some(frames_count))?);
        let db_size = u32::from_be_bytes(page[28..32].try_into().unwrap());
        Ok(db_size)
    }

    pub fn commit(&mut self, db_size: u32) -> Result<()> {
        self.flush_prepared_frame(db_size)
    }

    fn flush_prepared_frame(&mut self, db_size: u32) -> Result<()> {
        let Some((page_no, mut frame)) = self.prepared_frame.take() else {
            return Ok(());
        };

        let frame_info = WalFrameInfo { db_size, page_no };
        frame_info.put_to_frame_header(&mut frame);

        let frame_no = self.next_wal_frame_no;
        tracing::trace!(
            "flush prepared frame {:?} as frame_no {}",
            frame_info,
            frame_no
        );
        self.wal_session.conn().wal_insert_frame(frame_no, &frame)?;
        self.next_wal_frame_no += 1;

        Ok(())
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
    query_stmt: turso_core::Statement,
    first_change_id: Option<i64>,
    batch: VecDeque<DatabaseTapeRowChange>,
    txn_boundary_returned: bool,
    mode: DatabaseChangesIteratorMode,
    ignore_schema_changes: bool,
}

impl DatabaseChangesIterator {
    pub async fn next(&mut self, coro: &Coro) -> Result<Option<DatabaseTapeOperation>> {
        if self.batch.is_empty() {
            self.refill(coro).await?;
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
    async fn refill(&mut self, coro: &Coro) -> Result<()> {
        let change_id_filter = self.first_change_id.unwrap_or(self.mode.first_id());
        self.query_stmt.reset();
        self.query_stmt.bind_at(
            1.try_into().unwrap(),
            turso_core::Value::Integer(change_id_filter),
        );

        while let Some(row) = run_stmt(coro, &mut self.query_stmt).await? {
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

#[derive(Debug)]
pub struct DatabaseReplaySessionOpts {
    pub use_implicit_rowid: bool,
}

struct DeleteCachedStmt {
    stmt: turso_core::Statement,
    pk_column_indices: Option<Vec<usize>>, // if None - use rowid instead
}

pub struct DatabaseReplaySession {
    conn: Arc<turso_core::Connection>,
    cached_delete_stmt: HashMap<String, DeleteCachedStmt>,
    cached_insert_stmt: HashMap<(String, usize), turso_core::Statement>,
    in_txn: bool,
    opts: DatabaseReplaySessionOpts,
}

impl DatabaseReplaySession {
    pub fn conn(&self) -> Arc<turso_core::Connection> {
        self.conn.clone()
    }
    pub async fn replay(&mut self, coro: &Coro, operation: DatabaseTapeOperation) -> Result<()> {
        match operation {
            DatabaseTapeOperation::Commit => {
                tracing::debug!("replay: commit replayed changes after transaction boundary");
                if self.in_txn {
                    self.conn.execute("COMMIT")?;
                    self.in_txn = false;
                }
            }
            DatabaseTapeOperation::RowChange(change) => {
                if !self.in_txn {
                    tracing::trace!("replay: start txn for replaying changes");
                    self.conn.execute("BEGIN")?;
                    self.in_txn = true;
                }
                tracing::trace!("replay: change={:?}", change);
                let table_name = &change.table_name;
                match change.change {
                    DatabaseTapeRowChangeType::Delete { before } => {
                        let before = parse_bin_record(before)?;
                        self.replay_delete(coro, table_name, change.id, before)
                            .await?
                    }
                    DatabaseTapeRowChangeType::Update { before, after } => {
                        let before = parse_bin_record(before)?;
                        self.replay_delete(coro, table_name, change.id, before)
                            .await?;
                        let after = parse_bin_record(after)?;
                        self.replay_insert(coro, table_name, change.id, after)
                            .await?;
                    }
                    DatabaseTapeRowChangeType::Insert { after } => {
                        let values = parse_bin_record(after)?;
                        self.replay_insert(coro, table_name, change.id, values)
                            .await?;
                    }
                }
            }
        }
        Ok(())
    }
    async fn replay_delete(
        &mut self,
        coro: &Coro,
        table_name: &str,
        id: i64,
        mut values: Vec<turso_core::Value>,
    ) -> Result<()> {
        let cached = self.cached_delete_stmt(coro, table_name).await?;
        if let Some(pk_column_indices) = &cached.pk_column_indices {
            for (i, pk_column) in pk_column_indices.iter().enumerate() {
                let value = std::mem::replace(&mut values[*pk_column], turso_core::Value::Null);
                cached.stmt.bind_at((i + 1).try_into().unwrap(), value);
            }
        } else {
            let value = turso_core::Value::Integer(id);
            cached.stmt.bind_at(1.try_into().unwrap(), value);
        }
        exec_stmt(coro, &mut cached.stmt).await?;
        Ok(())
    }
    async fn replay_insert(
        &mut self,
        coro: &Coro,
        table_name: &str,
        id: i64,
        values: Vec<turso_core::Value>,
    ) -> Result<()> {
        let columns = values.len();
        let use_implicit_rowid = self.opts.use_implicit_rowid;
        let stmt = self.cached_insert_stmt(coro, table_name, columns).await?;
        stmt.reset();

        for (i, value) in values.into_iter().enumerate() {
            stmt.bind_at((i + 1).try_into().unwrap(), value);
        }
        if use_implicit_rowid {
            stmt.bind_at(
                (columns + 1).try_into().unwrap(),
                turso_core::Value::Integer(id),
            );
        }
        exec_stmt(coro, stmt).await?;
        Ok(())
    }
    async fn cached_delete_stmt(
        &mut self,
        coro: &Coro,
        table_name: &str,
    ) -> Result<&mut DeleteCachedStmt> {
        if !self.cached_delete_stmt.contains_key(table_name) {
            tracing::trace!("prepare delete statement for replay: table={}", table_name);
            let stmt = self.delete_query(coro, table_name).await?;
            self.cached_delete_stmt.insert(table_name.to_string(), stmt);
        }
        tracing::trace!(
            "ready to use prepared delete statement for replay: table={}",
            table_name
        );
        let cached = self.cached_delete_stmt.get_mut(table_name).unwrap();
        cached.stmt.reset();
        Ok(cached)
    }
    async fn cached_insert_stmt(
        &mut self,
        coro: &Coro,
        table_name: &str,
        columns: usize,
    ) -> Result<&mut turso_core::Statement> {
        let key = (table_name.to_string(), columns);
        if !self.cached_insert_stmt.contains_key(&key) {
            tracing::trace!(
                "prepare insert statement for replay: table={}, columns={}",
                table_name,
                columns
            );
            let stmt = self.insert_query(coro, table_name, columns).await?;
            self.cached_insert_stmt.insert(key.clone(), stmt);
        }
        tracing::trace!(
            "ready to use prepared insert statement for replay: table={}, columns={}",
            table_name,
            columns
        );
        let stmt = self.cached_insert_stmt.get_mut(&key).unwrap();
        stmt.reset();
        Ok(stmt)
    }

    async fn insert_query(
        &self,
        coro: &Coro,
        table_name: &str,
        columns: usize,
    ) -> Result<turso_core::Statement> {
        let query = if !self.opts.use_implicit_rowid {
            let placeholders = ["?"].repeat(columns).join(",");
            format!("INSERT INTO {table_name} VALUES ({placeholders})")
        } else {
            let mut table_info_stmt = self.conn.prepare(&format!(
                "SELECT name FROM pragma_table_info('{table_name}')"
            ))?;
            let mut column_names = Vec::with_capacity(columns + 1);
            while let Some(column) = run_stmt(&coro, &mut table_info_stmt).await? {
                let turso_core::Value::Text(text) = column.get_value(0) else {
                    return Err(Error::DatabaseTapeError(
                        "unexpected column type for pragma_table_info query".to_string(),
                    ));
                };
                column_names.push(text.to_string());
            }
            column_names.push("rowid".to_string());

            let placeholders = ["?"].repeat(columns + 1).join(",");
            let column_names = column_names.join(", ");
            format!("INSERT INTO {table_name}({column_names}) VALUES ({placeholders})")
        };
        Ok(self.conn.prepare(&query)?)
    }

    async fn delete_query(&self, coro: &Coro, table_name: &str) -> Result<DeleteCachedStmt> {
        let (query, pk_column_indices) = if self.opts.use_implicit_rowid {
            (format!("DELETE FROM {table_name} WHERE rowid = ?"), None)
        } else {
            let mut pk_info_stmt = self.conn.prepare(&format!(
                "SELECT cid, name FROM pragma_table_info('{table_name}') WHERE pk = 1"
            ))?;
            let mut pk_predicates = Vec::with_capacity(1);
            let mut pk_column_indices = Vec::with_capacity(1);
            while let Some(column) = run_stmt(&coro, &mut pk_info_stmt).await? {
                let turso_core::Value::Integer(column_id) = column.get_value(0) else {
                    return Err(Error::DatabaseTapeError(
                        "unexpected column type for pragma_table_info query".to_string(),
                    ));
                };
                let turso_core::Value::Text(name) = column.get_value(1) else {
                    return Err(Error::DatabaseTapeError(
                        "unexpected column type for pragma_table_info query".to_string(),
                    ));
                };
                pk_predicates.push(format!("{name} = ?"));
                pk_column_indices.push(*column_id as usize);
            }

            if pk_column_indices.is_empty() {
                (format!("DELETE FROM {table_name} WHERE rowid = ?"), None)
            } else {
                let pk_predicates = pk_predicates.join(" AND ");
                let query = format!("DELETE FROM {table_name} WHERE {pk_predicates}");
                (query, Some(pk_column_indices))
            }
        };
        let use_implicit_rowid = self.opts.use_implicit_rowid;
        tracing::trace!("delete_query: table_name={table_name}, query={query}, use_implicit_rowid={use_implicit_rowid}");
        let stmt = self.conn.prepare(&query)?;
        Ok(DeleteCachedStmt {
            stmt,
            pk_column_indices,
        })
    }
}

fn parse_bin_record(bin_record: Vec<u8>) -> Result<Vec<turso_core::Value>> {
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
    use std::sync::Arc;

    use tempfile::NamedTempFile;

    use crate::{
        database_tape::{run_stmt, DatabaseReplaySessionOpts, DatabaseTape},
        types::{
            DatabaseTapeOperation, DatabaseTapeRowChange, DatabaseTapeRowChangeType,
            ProtocolResponse,
        },
    };

    #[test]
    pub fn test_database_tape_connect() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();

        let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
        let db1 = turso_core::Database::open_file(io.clone(), db_path1, false, false).unwrap();
        let db1 = Arc::new(DatabaseTape::new(db1));
        let mut gen = genawaiter::sync::Gen::new({
            let db1 = db1.clone();
            |coro| async move {
                let conn = db1.connect(&coro).await.unwrap();
                let mut stmt = conn.prepare("SELECT * FROM turso_cdc").unwrap();
                let mut rows = Vec::new();
                while let Some(row) = run_stmt(&coro, &mut stmt).await.unwrap() {
                    rows.push(row.get_values().cloned().collect::<Vec<_>>());
                }
                rows
            }
        });
        let rows = loop {
            match gen.resume_with(Ok(ProtocolResponse::None)) {
                genawaiter::GeneratorState::Yielded(..) => io.run_once().unwrap(),
                genawaiter::GeneratorState::Complete(result) => break result,
            }
        };
        assert_eq!(rows, vec![] as Vec<Vec<turso_core::Value>>);
    }

    #[test]
    pub fn test_database_tape_iterate_changes() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();

        let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
        let db1 = turso_core::Database::open_file(io.clone(), db_path1, false, false).unwrap();
        let db1 = Arc::new(DatabaseTape::new(db1));

        let mut gen = genawaiter::sync::Gen::new({
            let db1 = db1.clone();
            |coro| async move {
                let conn = db1.connect(&coro).await.unwrap();
                conn.execute("CREATE TABLE t(x)").unwrap();
                conn.execute("INSERT INTO t VALUES (1), (2), (3)").unwrap();
                let opts = Default::default();
                let mut iterator = db1.iterate_changes(opts).unwrap();
                let mut changes = Vec::new();
                while let Some(change) = iterator.next(&coro).await.unwrap() {
                    changes.push(change);
                }
                changes
            }
        });
        let changes = loop {
            match gen.resume_with(Ok(ProtocolResponse::None)) {
                genawaiter::GeneratorState::Yielded(..) => io.run_once().unwrap(),
                genawaiter::GeneratorState::Complete(result) => break result,
            }
        };
        tracing::info!("changes: {:?}", changes);
        assert_eq!(changes.len(), 4);
        assert!(matches!(
            changes[0],
            DatabaseTapeOperation::RowChange(DatabaseTapeRowChange {
                change_id: 2,
                id: 1,
                ref table_name,
                change: DatabaseTapeRowChangeType::Insert { .. },
                ..
            }) if table_name == "t"
        ));
        assert!(matches!(
            changes[1],
            DatabaseTapeOperation::RowChange(DatabaseTapeRowChange {
                change_id: 3,
                id: 2,
                ref table_name,
                change: DatabaseTapeRowChangeType::Insert { .. },
                ..
            }) if table_name == "t"
        ));
        assert!(matches!(
            changes[2],
            DatabaseTapeOperation::RowChange(DatabaseTapeRowChange {
                change_id: 4,
                id: 3,
                ref table_name,
                change: DatabaseTapeRowChangeType::Insert { .. },
                ..
            }) if table_name == "t"
        ));
        assert!(matches!(changes[3], DatabaseTapeOperation::Commit));
    }

    #[test]
    pub fn test_database_tape_replay_changes_preserve_rowid() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();
        let temp_file2 = NamedTempFile::new().unwrap();
        let db_path2 = temp_file2.path().to_str().unwrap();

        let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
        let db1 = turso_core::Database::open_file(io.clone(), db_path1, false, false).unwrap();
        let db1 = Arc::new(DatabaseTape::new(db1));

        let db2 = turso_core::Database::open_file(io.clone(), db_path2, false, false).unwrap();
        let db2 = Arc::new(DatabaseTape::new(db2));

        let mut gen = genawaiter::sync::Gen::new({
            let db1 = db1.clone();
            let db2 = db2.clone();
            |coro| async move {
                let conn1 = db1.connect(&coro).await.unwrap();
                conn1.execute("CREATE TABLE t(x)").unwrap();
                conn1
                    .execute("INSERT INTO t(rowid, x) VALUES (10, 1), (20, 2)")
                    .unwrap();
                let conn2 = db2.connect(&coro).await.unwrap();
                conn2.execute("CREATE TABLE t(x)").unwrap();
                conn2
                    .execute("INSERT INTO t(rowid, x) VALUES (1, -1), (2, -2)")
                    .unwrap();

                {
                    let opts = DatabaseReplaySessionOpts {
                        use_implicit_rowid: true,
                    };
                    let mut session = db2.start_replay_session(&coro, opts).await.unwrap();
                    let opts = Default::default();
                    let mut iterator = db1.iterate_changes(opts).unwrap();
                    while let Some(operation) = iterator.next(&coro).await.unwrap() {
                        session.replay(&coro, operation).await.unwrap();
                    }
                }
                let mut stmt = conn2.prepare("SELECT rowid, x FROM t").unwrap();
                let mut rows = Vec::new();
                while let Some(row) = run_stmt(&coro, &mut stmt).await.unwrap() {
                    rows.push(row.get_values().cloned().collect::<Vec<_>>());
                }
                rows
            }
        });
        let rows = loop {
            match gen.resume_with(Ok(ProtocolResponse::None)) {
                genawaiter::GeneratorState::Yielded(..) => io.run_once().unwrap(),
                genawaiter::GeneratorState::Complete(rows) => break rows,
            }
        };
        tracing::info!("rows: {:?}", rows);
        assert_eq!(
            rows,
            vec![
                vec![
                    turso_core::Value::Integer(1),
                    turso_core::Value::Integer(-1)
                ],
                vec![
                    turso_core::Value::Integer(2),
                    turso_core::Value::Integer(-2)
                ],
                vec![
                    turso_core::Value::Integer(10),
                    turso_core::Value::Integer(1)
                ],
                vec![
                    turso_core::Value::Integer(20),
                    turso_core::Value::Integer(2)
                ]
            ]
        );
    }

    #[test]
    pub fn test_database_tape_replay_changes_do_not_preserve_rowid() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();
        let temp_file2 = NamedTempFile::new().unwrap();
        let db_path2 = temp_file2.path().to_str().unwrap();

        let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
        let db1 = turso_core::Database::open_file(io.clone(), db_path1, false, false).unwrap();
        let db1 = Arc::new(DatabaseTape::new(db1));

        let db2 = turso_core::Database::open_file(io.clone(), db_path2, false, false).unwrap();
        let db2 = Arc::new(DatabaseTape::new(db2));

        let mut gen = genawaiter::sync::Gen::new({
            let db1 = db1.clone();
            let db2 = db2.clone();
            |coro| async move {
                let conn1 = db1.connect(&coro).await.unwrap();
                conn1.execute("CREATE TABLE t(x)").unwrap();
                conn1
                    .execute("INSERT INTO t(rowid, x) VALUES (10, 1), (20, 2)")
                    .unwrap();
                let conn2 = db2.connect(&coro).await.unwrap();
                conn2.execute("CREATE TABLE t(x)").unwrap();
                conn2
                    .execute("INSERT INTO t(rowid, x) VALUES (1, -1), (2, -2)")
                    .unwrap();

                {
                    let opts = DatabaseReplaySessionOpts {
                        use_implicit_rowid: false,
                    };
                    let mut session = db2.start_replay_session(&coro, opts).await.unwrap();
                    let opts = Default::default();
                    let mut iterator = db1.iterate_changes(opts).unwrap();
                    while let Some(operation) = iterator.next(&coro).await.unwrap() {
                        session.replay(&coro, operation).await.unwrap();
                    }
                }
                let mut stmt = conn2.prepare("SELECT rowid, x FROM t").unwrap();
                let mut rows = Vec::new();
                while let Some(row) = run_stmt(&coro, &mut stmt).await.unwrap() {
                    rows.push(row.get_values().cloned().collect::<Vec<_>>());
                }
                rows
            }
        });
        let rows = loop {
            match gen.resume_with(Ok(ProtocolResponse::None)) {
                genawaiter::GeneratorState::Yielded(..) => io.run_once().unwrap(),
                genawaiter::GeneratorState::Complete(rows) => break rows,
            }
        };
        tracing::info!("rows: {:?}", rows);
        assert_eq!(
            rows,
            vec![
                vec![
                    turso_core::Value::Integer(1),
                    turso_core::Value::Integer(-1)
                ],
                vec![
                    turso_core::Value::Integer(2),
                    turso_core::Value::Integer(-2)
                ],
                vec![turso_core::Value::Integer(3), turso_core::Value::Integer(1)],
                vec![turso_core::Value::Integer(4), turso_core::Value::Integer(2)]
            ]
        );
    }

    #[test]
    pub fn test_database_tape_replay_changes_delete() {
        let temp_file1 = NamedTempFile::new().unwrap();
        let db_path1 = temp_file1.path().to_str().unwrap();
        let temp_file2 = NamedTempFile::new().unwrap();
        let db_path2 = temp_file2.path().to_str().unwrap();

        let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
        let db1 = turso_core::Database::open_file(io.clone(), db_path1, false, true).unwrap();
        let db1 = Arc::new(DatabaseTape::new(db1));

        let db2 = turso_core::Database::open_file(io.clone(), db_path2, false, true).unwrap();
        let db2 = Arc::new(DatabaseTape::new(db2));

        let mut gen = genawaiter::sync::Gen::new({
            let db1 = db1.clone();
            let db2 = db2.clone();
            |coro| async move {
                let conn1 = db1.connect(&coro).await.unwrap();
                conn1.execute("CREATE TABLE t(x TEXT PRIMARY KEY)").unwrap();
                conn1.execute("INSERT INTO t(x) VALUES ('a')").unwrap();
                conn1.execute("DELETE FROM t").unwrap();
                let conn2 = db2.connect(&coro).await.unwrap();
                conn2.execute("CREATE TABLE t(x TEXT PRIMARY KEY)").unwrap();
                conn2.execute("INSERT INTO t(x) VALUES ('b')").unwrap();

                {
                    let opts = DatabaseReplaySessionOpts {
                        use_implicit_rowid: false,
                    };
                    let mut session = db2.start_replay_session(&coro, opts).await.unwrap();
                    let opts = Default::default();
                    let mut iterator = db1.iterate_changes(opts).unwrap();
                    while let Some(operation) = iterator.next(&coro).await.unwrap() {
                        session.replay(&coro, operation).await.unwrap();
                    }
                }
                let mut stmt = conn2.prepare("SELECT rowid, x FROM t").unwrap();
                let mut rows = Vec::new();
                while let Some(row) = run_stmt(&coro, &mut stmt).await.unwrap() {
                    rows.push(row.get_values().cloned().collect::<Vec<_>>());
                }
                rows
            }
        });
        let rows = loop {
            match gen.resume_with(Ok(ProtocolResponse::None)) {
                genawaiter::GeneratorState::Yielded(..) => io.run_once().unwrap(),
                genawaiter::GeneratorState::Complete(rows) => break rows,
            }
        };
        tracing::info!("rows: {:?}", rows);
        assert_eq!(
            rows,
            vec![vec![
                turso_core::Value::Integer(1),
                turso_core::Value::Text(turso_core::types::Text::new("b"))
            ]]
        );
    }
}
