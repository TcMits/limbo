use std::{collections::HashMap, path::Path, sync::Arc};

use tokio::sync::Mutex;

use crate::{
    errors::Error,
    test_context::TestContext,
    test_protocol_io::TestDataCompletion,
    types::{DbSyncInfo, DbSyncStatus},
    Result,
};

const PAGE_SIZE: usize = 4096;
const FRAME_SIZE: usize = 24 + PAGE_SIZE;

struct Generation {
    snapshot: Vec<u8>,
    frames: Vec<Vec<u8>>,
}

#[derive(Clone)]
struct SyncSession {
    baton: String,
    conn: turso::Connection,
    in_txn: bool,
}

struct TestSyncServerState {
    generation: u64,
    generations: HashMap<u64, Generation>,
    sessions: HashMap<String, SyncSession>,
}

#[derive(Clone)]
pub struct TestSyncServer {
    ctx: Arc<TestContext>,
    db: turso::Database,
    state: Arc<Mutex<TestSyncServerState>>,
}

impl TestSyncServer {
    pub async fn new(ctx: Arc<TestContext>, path: &Path) -> Result<Self> {
        let mut generations = HashMap::new();
        generations.insert(
            1,
            Generation {
                snapshot: EMPTY_WAL_MODE_DB.to_vec(),
                frames: Vec::new(),
            },
        );
        Ok(Self {
            ctx,
            db: turso::Builder::new_local(path.to_str().unwrap())
                .build()
                .await?,
            state: Arc::new(Mutex::new(TestSyncServerState {
                generation: 1,
                generations,
                sessions: HashMap::new(),
            })),
        })
    }
    pub async fn db_info(&self, completion: TestDataCompletion) -> Result<()> {
        tracing::debug!("db_info");
        self.ctx.faulty_call("db_info_start").await?;

        let state = self.state.lock().await;
        let result = DbSyncInfo {
            current_generation: state.generation,
        };

        completion.set_status(200);
        self.ctx.faulty_call("db_info_status").await?;

        completion.push_data(serde_json::to_vec(&result)?);
        self.ctx.faulty_call("db_info_data").await?;

        completion.set_done();

        Ok(())
    }

    pub async fn db_export(
        &self,
        completion: TestDataCompletion,
        generation_id: u64,
    ) -> Result<()> {
        tracing::debug!("db_export: {}", generation_id);
        self.ctx.faulty_call("db_export_start").await?;

        let state = self.state.lock().await;
        let Some(generation) = state.generations.get(&generation_id) else {
            return Err(Error::DatabaseSyncEngineError(
                "generation not found".to_string(),
            ));
        };
        completion.set_status(200);
        self.ctx.faulty_call("db_export_status").await?;

        completion.push_data(generation.snapshot.clone());
        self.ctx.faulty_call("db_export_push").await?;

        completion.set_done();

        Ok(())
    }

    pub async fn wal_pull(
        &self,
        completion: TestDataCompletion,
        generation_id: u64,
        start_frame: u64,
        end_frame: u64,
    ) -> Result<()> {
        tracing::debug!("wal_pull: {}/{}/{}", generation_id, start_frame, end_frame);
        self.ctx.faulty_call("wal_pull_start").await?;

        let state = self.state.lock().await;
        let Some(generation) = state.generations.get(&generation_id) else {
            return Err(Error::DatabaseSyncEngineError(
                "generation not found".to_string(),
            ));
        };
        let mut data = Vec::new();
        for frame_no in start_frame..end_frame {
            let frame_idx = frame_no - 1;
            let Some(frame) = generation.frames.get(frame_idx as usize) else {
                break;
            };
            data.extend_from_slice(frame);
        }
        if data.is_empty() {
            let last_generation = state.generations.get(&state.generation).unwrap();

            let status = DbSyncStatus {
                baton: None,
                status: "checkpoint_needed".to_string(),
                generation: state.generation,
                max_frame_no: last_generation.frames.len() as u64,
            };
            completion.set_status(400);
            self.ctx.faulty_call("wal_pull_400_status").await?;

            completion.push_data(serde_json::to_vec(&status)?);
            self.ctx.faulty_call("wal_pull_400_push").await?;

            completion.set_done();
        } else {
            completion.set_status(200);
            self.ctx.faulty_call("wal_pull_200_status").await?;

            completion.push_data(data);
            self.ctx.faulty_call("wal_pull_200_push").await?;

            completion.set_done();
        };

        Ok(())
    }

    pub async fn wal_push(
        &self,
        completion: TestDataCompletion,
        mut baton: Option<String>,
        generation_id: u64,
        start_frame: u64,
        end_frame: u64,
        frames: Vec<u8>,
    ) -> Result<()> {
        tracing::debug!(
            "wal_push: {}/{}/{}/{:?}",
            generation_id,
            start_frame,
            end_frame,
            baton
        );
        self.ctx.faulty_call("wal_push_start").await?;

        let mut session = {
            let mut state = self.state.lock().await;
            if state.generation != generation_id {
                return Err(Error::DatabaseSyncEngineError(
                    "generation id mismatch".to_string(),
                ));
            }
            let baton_str = baton.unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
            let session = match state.sessions.get(&baton_str) {
                Some(session) => session.clone(),
                None => {
                    let session = SyncSession {
                        baton: baton_str.clone(),
                        conn: self.db.connect()?,
                        in_txn: false,
                    };
                    state.sessions.insert(baton_str.clone(), session.clone());
                    session
                }
            };
            baton = Some(baton_str.clone());
            session
        };

        let conflict = 'conflict: {
            let mut offset = 0;
            for frame_no in start_frame..end_frame {
                if offset + FRAME_SIZE > frames.len() {
                    return Err(Error::DatabaseSyncEngineError(
                        "unexpected length of frames data".to_string(),
                    ));
                }
                if !session.in_txn {
                    session.conn.wal_insert_begin()?;
                    session.in_txn = true;
                }
                let frame = &frames[offset..offset + FRAME_SIZE];
                match session.conn.wal_insert_frame(frame_no, frame) {
                    Ok(info) => {
                        if info.is_commit_frame() {
                            if session.in_txn {
                                session.conn.wal_insert_end()?;
                                session.in_txn = false;
                            }
                            self.sync_frames_from_conn(&session.conn).await?;
                        }
                    }
                    Err(turso::Error::WalOperationError(err)) if err.contains("Conflict") => {
                        session.conn.wal_insert_end()?;
                        break 'conflict true;
                    }
                    Err(err) => {
                        session.conn.wal_insert_end()?;
                        return Err(err.into());
                    }
                }
                offset += FRAME_SIZE;
            }
            false
        };
        let mut state = self.state.lock().await;
        state
            .sessions
            .insert(baton.clone().unwrap(), session.clone());
        let status = DbSyncStatus {
            baton: Some(session.baton.clone()),
            status: if conflict { "conflict" } else { "ok" }.into(),
            generation: state.generation,
            max_frame_no: session.conn.wal_frame_count()?,
        };

        let status = serde_json::to_vec(&status)?;

        completion.set_status(200);
        self.ctx.faulty_call("wal_push_status").await?;

        completion.push_data(status);
        self.ctx.faulty_call("wal_push_push").await?;

        completion.set_done();

        Ok(())
    }

    pub fn db(&self) -> turso::Database {
        self.db.clone()
    }
    pub async fn execute(&self, sql: &str, params: impl turso::IntoParams) -> Result<()> {
        let conn = self.db.connect()?;
        conn.execute(sql, params).await?;
        self.sync_frames_from_conn(&conn).await?;
        Ok(())
    }
    async fn sync_frames_from_conn(&self, conn: &turso::Connection) -> Result<()> {
        let mut state = self.state.lock().await;
        let generation = state.generation;
        let generation = state.generations.get_mut(&generation).unwrap();
        let last_frame = generation.frames.len() + 1;
        let mut frame = [0u8; FRAME_SIZE];
        let wal_frame_count = conn.wal_frame_count()?;
        tracing::debug!("conn frames count: {}", wal_frame_count);
        for frame_no in last_frame..=wal_frame_count as usize {
            conn.wal_get_frame(frame_no as u64, &mut frame)?;
            tracing::debug!("push local frame {}", frame_no);
            generation.frames.push(frame.to_vec());
        }
        Ok(())
    }
}

// empty DB with single 4096-byte page and WAL mode (PRAGMA journal_mode=WAL)
// see test test_empty_wal_mode_db_content which validates asset content
pub const EMPTY_WAL_MODE_DB: &[u8] = include_bytes!("test_empty.db");

pub async fn convert_rows(rows: &mut turso::Rows) -> Result<Vec<Vec<turso::Value>>> {
    let mut rows_values = vec![];
    while let Some(row) = rows.next().await? {
        let mut row_values = vec![];
        for i in 0..row.column_count() {
            row_values.push(row.get_value(i)?);
        }
        rows_values.push(row_values);
    }
    Ok(rows_values)
}
