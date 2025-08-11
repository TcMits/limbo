use std::sync::Arc;

use crate::{
    database_sync_operations::{
        db_bootstrap, reset_wal_file, transfer_logical_changes, transfer_physical_changes,
        wait_full_body, wal_pull, wal_push, WalPullResult,
    },
    errors::Error,
    io_operations::IoOperations,
    protocol_io::ProtocolIO,
    types::{Coro, DatabaseMetadata},
    wal_session::WalSession,
    Result,
};

#[derive(Debug)]
pub struct DatabaseSyncEngineOpts {
    pub client_name: String,
    pub wal_pull_batch_size: u64,
}

pub struct DatabaseSyncEngine<P: ProtocolIO> {
    io: Arc<dyn turso_core::IO>,
    protocol: Arc<P>,
    draft_path: String,
    synced_path: String,
    meta_path: String,
    opts: DatabaseSyncEngineOpts,
    meta: Option<DatabaseMetadata>,
    // we remember information if Synced DB is dirty - which will make Database to reset it in case of any sync attempt
    // this bit is set to false when we properly reset Synced DB
    // this bit is set to true when we transfer changes from Draft to Synced or on initialization
    synced_is_dirty: bool,
}

async fn update_meta<IO: ProtocolIO>(
    coro: &Coro,
    io: &IO,
    meta_path: &str,
    orig: &mut Option<DatabaseMetadata>,
    update: impl FnOnce(&mut DatabaseMetadata),
) -> Result<()> {
    let mut meta = orig.as_ref().unwrap().clone();
    update(&mut meta);
    tracing::debug!("update_meta: {meta:?}");
    let completion = io.full_write(meta_path, meta.dump()?)?;
    // todo: what happen if we will actually update the metadata on disk but fail and so in memory state will not be updated
    wait_full_body(coro, &completion).await?;
    *orig = Some(meta);
    Ok(())
}

async fn set_meta<IO: ProtocolIO>(
    coro: &Coro,
    io: &IO,
    meta_path: &str,
    orig: &mut Option<DatabaseMetadata>,
    meta: DatabaseMetadata,
) -> Result<()> {
    tracing::debug!("set_meta: {meta:?}");
    let completion = io.full_write(meta_path, meta.dump()?)?;
    // todo: what happen if we will actually update the metadata on disk but fail and so in memory state will not be updated
    wait_full_body(coro, &completion).await?;
    *orig = Some(meta);
    Ok(())
}

impl<C: ProtocolIO> DatabaseSyncEngine<C> {
    /// Creates new instance of SyncEngine and initialize it immediately if no consistent local data exists
    pub async fn new(
        coro: &Coro,
        io: Arc<dyn turso_core::IO>,
        protocol: Arc<C>,
        path: &str,
        opts: DatabaseSyncEngineOpts,
    ) -> Result<Self> {
        let mut db = Self {
            io,
            protocol,
            draft_path: format!("{path}-draft"),
            synced_path: format!("{path}-synced"),
            meta_path: format!("{path}-info"),
            opts,
            meta: None,
            synced_is_dirty: true,
        };
        db.init(coro).await?;
        Ok(db)
    }

    /// Create database connection and appropriately configure it before use
    pub async fn connect(&self, coro: &Coro) -> Result<Arc<turso_core::Connection>> {
        let db = self.io.open_tape(&self.draft_path, true)?;
        db.connect(coro).await
    }

    /// Sync all new changes from remote DB and apply them locally
    /// This method will **not** send local changed to the remote
    /// This method will block writes for the period of pull
    pub async fn pull(&mut self, coro: &Coro) -> Result<()> {
        tracing::debug!(
            "pull: draft={}, synced={}",
            self.draft_path,
            self.synced_path
        );

        // reset Synced DB if it wasn't properly cleaned-up on previous "sync-method" attempt
        self.reset_synced_if_dirty(coro).await?;

        // update Synced DB with fresh changes from remote
        self.pull_synced_from_remote(coro).await?;

        // we will "replay" Synced WAL to the Draft WAL later without pushing it to the remote
        // so, we pass 'capture: true' as we need to preserve all changes for future push of WAL
        let draft = self.io.open_tape(&self.draft_path, true)?;
        let synced = self.io.open_tape(&self.synced_path, true)?;

        {
            // we will start wal write session for Draft DB in order to hold write lock during transfer of changes
            let mut draft_session = WalSession::new(draft.connect(coro).await?);
            draft_session.begin()?;

            // mark Synced as dirty as we will start transfer of logical changes there and if we will fail in the middle - we will need to cleanup Synced db
            self.synced_is_dirty = true;

            // transfer logical changes to the Synced DB in order to later execute physical "rebase" operation
            let client_id = &self.meta().client_unique_id;
            transfer_logical_changes(coro, &draft, &synced, client_id, true).await?;

            // now we are ready to do the rebase: let's transfer physical changes from Synced to Draft
            let synced_wal_watermark = self.meta().synced_wal_match_watermark;
            let synced_sync_watermark = self.meta().synced_frame_no.expect(
                "synced_frame_no must be set as we call pull_synced_from_remote before that",
            );
            let draft_wal_watermark = self.meta().draft_wal_match_watermark;
            let draft_sync_watermark = transfer_physical_changes(
                coro,
                &synced,
                draft_session,
                synced_wal_watermark,
                synced_sync_watermark,
                draft_wal_watermark,
            )
            .await?;
            update_meta(
                coro,
                self.protocol.as_ref(),
                &self.meta_path,
                &mut self.meta,
                |m| {
                    m.draft_wal_match_watermark = draft_sync_watermark;
                    m.synced_wal_match_watermark = synced_sync_watermark;
                },
            )
            .await?;
        }

        // Synced DB is 100% dirty now - let's reset it
        assert!(self.synced_is_dirty);
        self.reset_synced_if_dirty(coro).await?;

        Ok(())
    }

    /// Sync local changes to remote DB
    /// This method will **not** pull remote changes to the local DB
    /// This method will **not** block writes for the period of sync
    pub async fn push(&mut self, coro: &Coro) -> Result<()> {
        tracing::debug!(
            "push: draft={}, synced={}",
            self.draft_path,
            self.synced_path
        );

        // reset Synced DB if it wasn't properly cleaned-up on previous "sync-method" attempt
        self.reset_synced_if_dirty(coro).await?;

        // update Synced DB with fresh changes from remote in order to avoid WAL frame conflicts
        self.pull_synced_from_remote(coro).await?;

        // we will push Synced WAL to the remote
        // so, we pass 'capture: false' as we don't need to preserve changes made to Synced WAL in turso_cdc
        let draft = self.io.open_tape(&self.draft_path, true)?;
        let synced = self.io.open_tape(&self.synced_path, false)?;

        // mark Synced as dirty as we will start transfer of logical changes there and if we will fail in the middle - we will need to cleanup Synced db
        self.synced_is_dirty = true;

        let client_id = &self.meta().client_unique_id;
        transfer_logical_changes(coro, &draft, &synced, client_id, false).await?;

        self.push_synced_to_remote(coro).await?;
        Ok(())
    }

    /// Sync local changes to remote DB and bring new changes from remote to local
    /// This method will block writes for the period of sync
    pub async fn sync(&mut self, coro: &Coro) -> Result<()> {
        // todo(sivukhin): this is bit suboptimal as both 'push' and 'pull' will call pull_synced_from_remote
        // but for now - keep it simple
        self.push(coro).await?;
        self.pull(coro).await?;
        Ok(())
    }

    async fn init(&mut self, coro: &Coro) -> Result<()> {
        tracing::debug!(
            "initialize sync engine: draft={}, synced={}, opts={:?}",
            self.draft_path,
            self.synced_path,
            self.opts,
        );

        let completion = self.protocol.full_read(&self.meta_path)?;
        let data = wait_full_body(coro, &completion).await?;
        let meta = if data.is_empty() {
            None
        } else {
            Some(DatabaseMetadata::load(&data)?)
        };

        match meta {
            Some(meta) => {
                self.meta = Some(meta);
            }
            None => {
                let meta = self.bootstrap_db_files(coro).await?;
                tracing::debug!("write meta after successful bootstrap: meta={meta:?}");
                set_meta(
                    coro,
                    self.protocol.as_ref(),
                    &self.meta_path,
                    &mut self.meta,
                    meta,
                )
                .await?;
            }
        };

        let draft_exists = self.io.try_open(&self.draft_path)?.is_some();
        let synced_exists = self.io.try_open(&self.synced_path)?.is_some();
        if !draft_exists || !synced_exists {
            let error = "Draft or Synced files doesn't exists, but metadata is".to_string();
            return Err(Error::DatabaseSyncEngineError(error));
        }

        if self.meta().synced_frame_no.is_none() {
            // sync WAL from the remote in case of bootstrap - all subsequent initializations will be fast
            self.pull(coro).await?;
        }
        Ok(())
    }

    async fn pull_synced_from_remote(&mut self, coro: &Coro) -> Result<()> {
        tracing::debug!(
            "pull_synced_from_remote: draft={:?}, synced={:?}",
            self.draft_path,
            self.synced_path,
        );
        let synced = self.io.open_tape(&self.synced_path, false)?;
        let synced_conn = synced.connect(coro).await?;
        let mut wal = WalSession::new(synced_conn);

        let generation = self.meta().synced_generation;
        let mut start_frame = self.meta().synced_frame_no.unwrap_or(0) + 1;
        loop {
            let end_frame = start_frame + self.opts.wal_pull_batch_size;
            let update = async |coro, frame_no| {
                update_meta(
                    coro,
                    self.protocol.as_ref(),
                    &self.meta_path,
                    &mut self.meta,
                    |m| m.synced_frame_no = Some(frame_no),
                )
                .await
            };
            match wal_pull(
                coro,
                self.protocol.as_ref(),
                &mut wal,
                generation,
                start_frame,
                end_frame,
                update,
            )
            .await?
            {
                WalPullResult::Done => return Ok(()),
                WalPullResult::PullMore => {
                    start_frame = end_frame;
                    continue;
                }
                WalPullResult::NeedCheckpoint => {
                    return Err(Error::DatabaseSyncEngineError(
                        "checkpoint is temporary not supported".to_string(),
                    ));
                }
            }
        }
    }

    async fn push_synced_to_remote(&mut self, coro: &Coro) -> Result<()> {
        tracing::debug!(
            "push_synced_to_remote: draft={}, synced={}, id={}",
            self.draft_path,
            self.synced_path,
            self.meta().client_unique_id
        );
        let synced = self.io.open_tape(&self.synced_path, false)?;
        let synced_conn = synced.connect(coro).await?;

        let mut wal = WalSession::new(synced_conn);
        wal.begin()?;

        // todo(sivukhin): push frames in multiple batches
        let generation = self.meta().synced_generation;
        let start_frame = self.meta().synced_frame_no.unwrap_or(0) + 1;
        let end_frame = wal.conn().wal_frame_count()? + 1;
        match wal_push(
            coro,
            self.protocol.as_ref(),
            &mut wal,
            None,
            generation,
            start_frame,
            end_frame,
        )
        .await
        {
            Ok(_) => {
                update_meta(
                    coro,
                    self.protocol.as_ref(),
                    &self.meta_path,
                    &mut self.meta,
                    |m| m.synced_frame_no = Some(end_frame - 1),
                )
                .await?;
                self.synced_is_dirty = false;
                Ok(())
            }
            Err(err) => {
                tracing::info!("push_synced_to_remote: failed: err={err}");
                Err(err)
            }
        }
    }

    async fn bootstrap_db_files(&mut self, coro: &Coro) -> Result<DatabaseMetadata> {
        assert!(
            self.meta.is_none(),
            "bootstrap_db_files must be called only when meta is not set"
        );
        tracing::debug!(
            "bootstrap_db_files: draft={}, synced={}",
            self.draft_path,
            self.synced_path,
        );

        let start_time = std::time::Instant::now();
        // cleanup all files left from previous attempt to bootstrap
        // we shouldn't write any WAL files - but let's truncate them too for safety
        if let Some(file) = self.io.try_open(&self.draft_path)? {
            self.io.truncate(coro, file, 0).await?;
        }
        if let Some(file) = self.io.try_open(&self.synced_path)? {
            self.io.truncate(coro, file, 0).await?;
        }
        if let Some(file) = self.io.try_open(&format!("{}-wal", self.draft_path))? {
            self.io.truncate(coro, file, 0).await?;
        }
        if let Some(file) = self.io.try_open(&format!("{}-wal", self.synced_path))? {
            self.io.truncate(coro, file, 0).await?;
        }

        let files = &[
            self.io.create(&self.draft_path)?,
            self.io.create(&self.synced_path)?,
        ];
        let db_info = db_bootstrap(coro, self.protocol.as_ref(), files).await?;

        let elapsed = std::time::Instant::now().duration_since(start_time);
        tracing::debug!(
            "bootstrap_db_files: finished draft={:?}, synced={:?}: elapsed={:?}",
            self.draft_path,
            self.synced_path,
            elapsed
        );

        Ok(DatabaseMetadata {
            client_unique_id: format!("{}-{}", self.opts.client_name, uuid::Uuid::new_v4()),
            synced_generation: db_info.current_generation,
            synced_frame_no: None,
            draft_wal_match_watermark: 0,
            synced_wal_match_watermark: 0,
        })
    }

    /// Reset WAL of Synced database which potentially can have some local changes
    async fn reset_synced_if_dirty(&mut self, coro: &Coro) -> Result<()> {
        tracing::debug!(
            "reset_synced: synced_path={:?}, synced_is_dirty={}",
            self.synced_path,
            self.synced_is_dirty
        );
        // if we know that Synced DB is not dirty - let's skip this phase completely
        if !self.synced_is_dirty {
            return Ok(());
        }
        if let Some(synced_wal) = self.io.try_open(&format!("{}-wal", self.synced_path))? {
            reset_wal_file(coro, synced_wal, self.meta().synced_frame_no.unwrap_or(0)).await?;
        }
        self.synced_is_dirty = false;
        Ok(())
    }

    fn meta(&self) -> &DatabaseMetadata {
        self.meta.as_ref().expect("metadata must be set")
    }
}

#[cfg(test)]
pub mod tests {
    use std::{collections::BTreeMap, sync::Arc};

    use rand::RngCore;

    use crate::{
        database_sync_engine::DatabaseSyncEngineOpts,
        test_context::{FaultInjectionStrategy, TestContext},
        test_protocol_io::TestProtocolIo,
        test_sync_server::convert_rows,
        tests::{deterministic_runtime, seed_u64, TestRunner},
        Result,
    };

    async fn query_rows(conn: &turso::Connection, sql: &str) -> Result<Vec<Vec<turso::Value>>> {
        let mut rows = conn.query(sql, ()).await?;
        convert_rows(&mut rows).await
    }

    #[test]
    pub fn test_sync_single_db_simple() {
        deterministic_runtime(async || {
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();
            let mut runner = TestRunner::new(ctx.clone(), io, protocol.clone());
            let local_path = dir.path().join("local.db");
            let opts = DatabaseSyncEngineOpts {
                client_name: "id-1".to_string(),
                wal_pull_batch_size: 1,
            };
            runner.init(local_path, opts).await.unwrap();

            protocol
                .server
                .execute("CREATE TABLE t(x INTEGER PRIMARY KEY)", ())
                .await
                .unwrap();
            protocol
                .server
                .execute("INSERT INTO t VALUES (1)", ())
                .await
                .unwrap();

            let conn = runner.connect().await.unwrap();

            // no table in schema before sync from remote (as DB was initialized when remote was empty)
            assert!(matches!(
                query_rows(&conn, "SELECT * FROM t").await,
                Err(x) if x.to_string().contains("no such table: t")
            ));

            // 1 rows synced
            runner.pull().await.unwrap();
            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![vec![turso::Value::Integer(1)]]
            );

            protocol
                .server
                .execute("INSERT INTO t VALUES (2)", ())
                .await
                .unwrap();

            conn.execute("INSERT INTO t VALUES (3)", ()).await.unwrap();

            // changes are synced from the remote - but remote changes are not propagated locally
            runner.push().await.unwrap();
            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(3)]
                ]
            );

            let server_db = protocol.server.db();
            let server_conn = server_db.connect().unwrap();
            assert_eq!(
                convert_rows(&mut server_conn.query("SELECT * FROM t", ()).await.unwrap())
                    .await
                    .unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)],
                    vec![turso::Value::Integer(3)],
                ]
            );

            conn.execute("INSERT INTO t VALUES (4)", ()).await.unwrap();
            runner.push().await.unwrap();
            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(3)],
                    vec![turso::Value::Integer(4)]
                ]
            );

            assert_eq!(
                convert_rows(&mut server_conn.query("SELECT * FROM t", ()).await.unwrap())
                    .await
                    .unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)],
                    vec![turso::Value::Integer(3)],
                    vec![turso::Value::Integer(4)],
                ]
            );

            runner.pull().await.unwrap();
            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)],
                    vec![turso::Value::Integer(3)],
                    vec![turso::Value::Integer(4)]
                ]
            );

            assert_eq!(
                convert_rows(&mut server_conn.query("SELECT * FROM t", ()).await.unwrap())
                    .await
                    .unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)],
                    vec![turso::Value::Integer(3)],
                    vec![turso::Value::Integer(4)],
                ]
            );
        });
    }

    #[test]
    pub fn test_sync_single_db_many_pulls_big_payloads() {
                deterministic_runtime(async || {
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();
            let mut runner = TestRunner::new(ctx.clone(), io, protocol.clone());
            let local_path = dir.path().join("local.db");
            let opts = DatabaseSyncEngineOpts {
                client_name: "id-1".to_string(),
                wal_pull_batch_size: 1,
            };

            runner.init(local_path, opts).await.unwrap();

            protocol
                .server
                .execute("CREATE TABLE t(x INTEGER PRIMARY KEY, y)", ())
                .await
                .unwrap();

            runner.pull().await.unwrap();

            // create connection in outer scope in order to prevent Database from being dropped in between of pull operations
            let conn = runner.connect().await.unwrap();

            let mut expected = BTreeMap::new();
            for attempt in 0..10 {
                for _ in 0..5 {
                    let key = ctx.rng().await.next_u32();
                    let length = ctx.rng().await.next_u32() % (10 * 4096);
                    protocol
                        .server
                        .execute("INSERT INTO t VALUES (?, randomblob(?))", (key, length))
                        .await
                        .unwrap();
                    expected.insert(key as i64, length as i64);
                }

                tracing::info!("pull attempt={}", attempt);
                runner.pull().await.unwrap();

                let expected = expected
                    .iter()
                    .map(|(x, y)| vec![turso::Value::Integer(*x), turso::Value::Integer(*y)])
                    .collect::<Vec<_>>();
                assert_eq!(
                    query_rows(&conn, "SELECT x, length(y) FROM t")
                        .await
                        .unwrap(),
                    expected
                );
            }
        });
    }
    
    #[test]
    pub fn test_sync_single_db_checkpoint() {
        deterministic_runtime(async || {
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();
            let mut runner = TestRunner::new(ctx.clone(), io, protocol.clone());
            let local_path = dir.path().join("local.db");
            let opts = DatabaseSyncEngineOpts {
                client_name: "id-1".to_string(),
                wal_pull_batch_size: 1,
            };
            runner.init(local_path, opts).await.unwrap();

            protocol
                .server
                .execute("CREATE TABLE t(x INTEGER PRIMARY KEY)", ())
                .await
                .unwrap();
            protocol
                .server
                .execute("INSERT INTO t VALUES (1)", ())
                .await
                .unwrap();
            protocol.server.checkpoint().await.unwrap();
            protocol
                .server
                .execute("INSERT INTO t VALUES (2)", ())
                .await
                .unwrap();

            let conn = runner.connect().await.unwrap();

            runner.pull().await.unwrap();

            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)]
                ]
            );
        });
    }

    #[test]
    pub fn test_sync_single_db_full_syncs() {
        deterministic_runtime(async || {
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let server = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();
            let mut runner = TestRunner::new(ctx.clone(), io.clone(), server.clone());
            let local_path = dir.path().join("local.db");
            let opts = DatabaseSyncEngineOpts {
                client_name: "id-1".to_string(),
                wal_pull_batch_size: 1,
            };
            runner.init(local_path, opts).await.unwrap();

            server
                .server
                .execute("CREATE TABLE t(x INTEGER PRIMARY KEY)", ())
                .await
                .unwrap();
            server
                .server
                .execute("INSERT INTO t VALUES (1)", ())
                .await
                .unwrap();

            let conn = runner.connect().await.unwrap();

            // no table in schema before sync from remote (as DB was initialized when remote was empty)
            assert!(matches!(
                query_rows(&conn, "SELECT * FROM t").await,
                Err(x) if x.to_string().contains("no such table: t")
            ));

            runner.sync().await.unwrap();

            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![vec![turso::Value::Integer(1)]]
            );

            conn.execute("INSERT INTO t VALUES (2)", ()).await.unwrap();
            runner.sync().await.unwrap();

            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)]
                ]
            );

            conn.execute("INSERT INTO t VALUES (3)", ()).await.unwrap();
            runner.sync().await.unwrap();
            assert_eq!(
                query_rows(&conn, "SELECT * FROM t").await.unwrap(),
                vec![
                    vec![turso::Value::Integer(1)],
                    vec![turso::Value::Integer(2)],
                    vec![turso::Value::Integer(3)]
                ]
            );
        });
    }

    #[test]
    pub fn test_sync_multiple_dbs_conflict() {
        deterministic_runtime(async || {
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();
            let mut dbs = Vec::new();
            const CLIENTS: usize = 8;
            for i in 0..CLIENTS {
                let mut runner = TestRunner::new(ctx.clone(), io.clone(), protocol.clone());
                let local_path = dir.path().join(format!("local-{i}.db"));
                let opts = DatabaseSyncEngineOpts {
                    client_name: format!("id-{i}"),
                    wal_pull_batch_size: 1,
                };
                runner.init(local_path, opts).await.unwrap();
                dbs.push(runner);
            }

            protocol
                .server
                .execute("CREATE TABLE t(x INTEGER PRIMARY KEY)", ())
                .await
                .unwrap();

            for db in &mut dbs {
                db.pull().await.unwrap();
            }
            for (i, db) in dbs.iter().enumerate() {
                let conn = db.connect().await.unwrap();
                conn.execute("INSERT INTO t VALUES (?)", (i as i32,))
                    .await
                    .unwrap();
            }

            let try_sync = || async {
                let mut tasks = Vec::new();
                for db in &dbs {
                    tasks.push(async move { db.push().await });
                }
                futures::future::join_all(tasks).await
            };
            for attempt in 0..CLIENTS {
                let results = try_sync().await;
                tracing::info!("attempt #{}: {:?}", attempt, results);
                assert!(results.iter().filter(|x| x.is_ok()).count() > attempt);
            }
        });
    }

    #[test]
    pub fn test_sync_multiple_clients_no_conflicts_synchronized() {
        deterministic_runtime(async || {
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();

            protocol
                .server
                .execute("CREATE TABLE t(k INTEGER PRIMARY KEY, v)", ())
                .await
                .unwrap();

            let sync_lock = Arc::new(tokio::sync::Mutex::new(()));
            let mut clients = Vec::new();
            const CLIENTS: usize = 10;
            let mut expected_rows = Vec::new();
            for i in 0..CLIENTS {
                let mut queries = Vec::new();
                let cnt = ctx.rng().await.next_u32() % CLIENTS as u32 + 1;
                for q in 0..cnt {
                    let key = i * CLIENTS + q as usize;
                    let length = ctx.rng().await.next_u32() % 4096;
                    queries.push(format!(
                        "INSERT INTO t VALUES ({key}, randomblob({length}))",
                    ));
                    expected_rows.push(vec![
                        turso::Value::Integer(key as i64),
                        turso::Value::Integer(length as i64),
                    ]);
                }
                clients.push({
                    let io = io.clone();
                    let dir = dir.path().to_path_buf().clone();
                    let ctx = ctx.clone();
                    let server = protocol.clone();
                    let sync_lock = sync_lock.clone();
                    async move {
                        let mut runner = TestRunner::new(ctx.clone(), io.clone(), server.clone());
                        let local_path = dir.join(format!("local-{i}.db"));
                        let opts = DatabaseSyncEngineOpts {
                            client_name: format!("id-{i}"),
                            wal_pull_batch_size: 1,
                        };
                        runner.init(local_path, opts).await.unwrap();
                        runner.pull().await.unwrap();
                        let conn = runner.connect().await.unwrap();
                        for query in queries {
                            conn.execute(&query, ()).await.unwrap();
                        }
                        let guard = sync_lock.lock().await;
                        runner.push().await.unwrap();
                        drop(guard);
                    }
                });
            }
            for client in clients {
                client.await;
            }
            let db = protocol.server.db();
            let conn = db.connect().unwrap();
            let mut result = conn.query("SELECT k, length(v) FROM t", ()).await.unwrap();
            let rows = convert_rows(&mut result).await.unwrap();
            assert_eq!(rows, expected_rows);
        });
    }

    #[test]
    pub fn test_sync_single_db_sync_from_remote_nothing_single_failure() {
        deterministic_runtime(async || {
            let dir = tempfile::TempDir::new().unwrap();
            let server_path = dir.path().join("server.db");
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let ctx = Arc::new(TestContext::new(seed_u64()));
            let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                .await
                .unwrap();

            protocol
                .server
                .execute("CREATE TABLE t(x)", ())
                .await
                .unwrap();
            protocol
                .server
                .execute("INSERT INTO t VALUES (1), (2), (3)", ())
                .await
                .unwrap();

            let mut session = ctx.fault_session();
            let mut it = 0;
            while let Some(strategy) = session.next().await {
                it += 1;

                let mut runner = TestRunner::new(ctx.clone(), io.clone(), protocol.clone());
                let local_path = dir.path().join(format!("local-{it}.db"));
                let opts = DatabaseSyncEngineOpts {
                    client_name: format!("id-{it}"),
                    wal_pull_batch_size: 1,
                };
                runner.init(local_path, opts).await.unwrap();

                let has_fault = matches!(strategy, FaultInjectionStrategy::Enabled { .. });

                ctx.switch_mode(strategy).await;
                let result = runner.pull().await;
                ctx.switch_mode(FaultInjectionStrategy::Disabled).await;

                if !has_fault {
                    result.unwrap();
                } else {
                    let err = result.err().unwrap();
                    tracing::info!("error after fault injection: {}", err);
                }

                let conn = runner.connect().await.unwrap();
                let rows = query_rows(&conn, "SELECT * FROM t").await.unwrap();
                assert_eq!(
                    rows,
                    vec![
                        vec![turso::Value::Integer(1)],
                        vec![turso::Value::Integer(2)],
                        vec![turso::Value::Integer(3)],
                    ]
                );

                runner.pull().await.unwrap();

                let rows = query_rows(&conn, "SELECT * FROM t").await.unwrap();
                assert_eq!(
                    rows,
                    vec![
                        vec![turso::Value::Integer(1)],
                        vec![turso::Value::Integer(2)],
                        vec![turso::Value::Integer(3)],
                    ]
                );
            }
        });
    }

    #[test]
    pub fn test_sync_single_db_sync_from_remote_single_failure() {
        deterministic_runtime(async || {
            let dir = tempfile::TempDir::new().unwrap();
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let ctx = Arc::new(TestContext::new(seed_u64()));

            let mut session = ctx.fault_session();
            let mut it = 0;
            while let Some(strategy) = session.next().await {
                it += 1;

                let server_path = dir.path().join(format!("server-{it}.db"));
                let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                    .await
                    .unwrap();

                protocol
                    .server
                    .execute("CREATE TABLE t(x)", ())
                    .await
                    .unwrap();

                let mut runner = TestRunner::new(ctx.clone(), io.clone(), protocol.clone());
                let local_path = dir.path().join(format!("local-{it}.db"));
                let opts = DatabaseSyncEngineOpts {
                    client_name: format!("id-{it}"),
                    wal_pull_batch_size: 1,
                };
                runner.init(local_path, opts).await.unwrap();

                protocol
                    .server
                    .execute("INSERT INTO t VALUES (1), (2), (3)", ())
                    .await
                    .unwrap();

                let has_fault = matches!(strategy, FaultInjectionStrategy::Enabled { .. });

                ctx.switch_mode(strategy).await;
                let result = runner.pull().await;
                ctx.switch_mode(FaultInjectionStrategy::Disabled).await;

                if !has_fault {
                    result.unwrap();
                } else {
                    let err = result.err().unwrap();
                    tracing::info!("error after fault injection: {}", err);
                }

                let conn = runner.connect().await.unwrap();
                let rows = query_rows(&conn, "SELECT * FROM t").await.unwrap();
                assert!(rows.len() <= 3);

                runner.pull().await.unwrap();

                let rows = query_rows(&conn, "SELECT * FROM t").await.unwrap();
                assert_eq!(
                    rows,
                    vec![
                        vec![turso::Value::Integer(1)],
                        vec![turso::Value::Integer(2)],
                        vec![turso::Value::Integer(3)],
                    ]
                );
            }
        });
    }

    #[test]
    pub fn test_sync_single_db_sync_to_remote_single_failure() {
        deterministic_runtime(async || {
            let dir = tempfile::TempDir::new().unwrap();
            let io: Arc<dyn turso_core::IO> = Arc::new(turso_core::PlatformIO::new().unwrap());
            let ctx = Arc::new(TestContext::new(seed_u64()));

            let mut session = ctx.fault_session();
            let mut it = 0;
            while let Some(strategy) = session.next().await {
                it += 1;

                let server_path = dir.path().join(format!("server-{it}.db"));
                let protocol = TestProtocolIo::new(ctx.clone(), &server_path)
                    .await
                    .unwrap();

                protocol
                    .server
                    .execute("CREATE TABLE t(x INTEGER PRIMARY KEY)", ())
                    .await
                    .unwrap();

                protocol
                    .server
                    .execute("INSERT INTO t VALUES (1)", ())
                    .await
                    .unwrap();

                let mut runner = TestRunner::new(ctx.clone(), io.clone(), protocol.clone());
                let local_path = dir.path().join(format!("local-{it}.db"));
                let opts = DatabaseSyncEngineOpts {
                    client_name: format!("id-{it}"),
                    wal_pull_batch_size: 1,
                };
                runner.init(local_path, opts).await.unwrap();

                let conn = runner.connect().await.unwrap();

                conn.execute("INSERT INTO t VALUES (2), (3)", ())
                    .await
                    .unwrap();

                let has_fault = matches!(strategy, FaultInjectionStrategy::Enabled { .. });

                ctx.switch_mode(strategy).await;
                let result = runner.push().await;
                ctx.switch_mode(FaultInjectionStrategy::Disabled).await;

                if !has_fault {
                    result.unwrap();
                } else {
                    let err = result.err().unwrap();
                    tracing::info!("error after fault injection: {}", err);
                }

                let server_db = protocol.server.db();
                let server_conn = server_db.connect().unwrap();
                let rows =
                    convert_rows(&mut server_conn.query("SELECT * FROM t", ()).await.unwrap())
                        .await
                        .unwrap();
                assert!(rows.len() <= 3);

                runner.push().await.unwrap();

                let rows =
                    convert_rows(&mut server_conn.query("SELECT * FROM t", ()).await.unwrap())
                        .await
                        .unwrap();
                assert_eq!(
                    rows,
                    vec![
                        vec![turso::Value::Integer(1)],
                        vec![turso::Value::Integer(2)],
                        vec![turso::Value::Integer(3)],
                    ]
                );
            }
        });
    }
}
