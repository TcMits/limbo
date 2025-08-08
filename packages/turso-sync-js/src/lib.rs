#![deny(clippy::all)]

pub mod generator;
pub mod js_protocol_io;

use std::sync::{Arc, Mutex};

use napi::bindgen_prelude::AsyncTask;
use napi_derive::napi;
use turso_node::IoLoopTask;
use turso_sync_engine::{
    database_sync_engine::{DatabaseSyncEngine, DatabaseSyncEngineOpts},
    types::Coro,
};

use crate::{
    generator::GeneratorHolder,
    js_protocol_io::{JsProtocolIo, JsProtocolRequestData},
};

#[napi(object)]
pub struct DatabaseOpts {
    pub path: String,
}

#[napi]
pub struct SyncEngine {
    path: String,
    client_name: String,
    wal_pull_batch_size: u32,
    io: Arc<dyn turso_core::IO>,
    protocol: Arc<JsProtocolIo>,
    sync_engine: Arc<Mutex<Option<DatabaseSyncEngine<JsProtocolIo>>>>,
    opened: Arc<Mutex<Option<turso_node::Database>>>,
}

#[napi(object)]
pub struct SyncEngineOpts {
    pub path: String,
    pub client_name: Option<String>,
    pub wal_pull_batch_size: Option<u32>,
}

#[napi]
impl SyncEngine {
    #[napi(constructor)]
    pub fn new(opts: SyncEngineOpts) -> napi::Result<Self> {
        let is_memory = opts.path == ":memory:";
        let io: Arc<dyn turso_core::IO> = if is_memory {
            Arc::new(turso_core::MemoryIO::new())
        } else {
            Arc::new(turso_core::PlatformIO::new().map_err(|e| {
                napi::Error::new(
                    napi::Status::GenericFailure,
                    format!("Failed to create IO: {e}"),
                )
            })?)
        };
        Ok(SyncEngine {
            path: opts.path,
            client_name: opts.client_name.unwrap_or("turso-sync-js".to_string()),
            wal_pull_batch_size: opts.wal_pull_batch_size.unwrap_or(100),
            sync_engine: Arc::new(Mutex::new(None)),
            io,
            protocol: Arc::new(JsProtocolIo::default()),
            #[allow(clippy::arc_with_non_send_sync)]
            opened: Arc::new(Mutex::new(None)),
        })
    }

    #[napi]
    pub fn init(&self) -> GeneratorHolder {
        let opts = DatabaseSyncEngineOpts {
            client_name: self.client_name.clone(),
            wal_pull_batch_size: self.wal_pull_batch_size as u64,
        };

        let protocol = self.protocol.clone();
        let sync_engine = self.sync_engine.clone();
        let io = self.io.clone();
        let opened = self.opened.clone();
        let path = self.path.clone();
        let generator = genawaiter::sync::Gen::new(|coro| async move {
            let initialized =
                DatabaseSyncEngine::new(&coro, io.clone(), protocol, &path, opts).await?;
            let connection = initialized.connect(&coro).await?;
            let db = turso_node::Database::create(None, io.clone(), connection, false);

            *sync_engine.lock().unwrap() = Some(initialized);
            *opened.lock().unwrap() = Some(db);
            Ok(())
        });
        GeneratorHolder {
            inner: Box::new(Mutex::new(generator)),
        }
    }

    #[napi]
    pub fn io_loop_sync(&self) -> napi::Result<()> {
        self.io.run_once().map_err(|e| {
            napi::Error::new(napi::Status::GenericFailure, format!("IO error: {e}"))
        })?;
        Ok(())
    }

    /// Runs the I/O loop asynchronously, returning a Promise.
    #[napi(ts_return_type = "Promise<void>")]
    pub fn io_loop_async(&self) -> AsyncTask<IoLoopTask> {
        let io = self.io.clone();
        AsyncTask::new(IoLoopTask { io })
    }

    #[napi]
    pub fn protocol_io(&self) -> Option<JsProtocolRequestData> {
        self.protocol.take_request()
    }

    #[napi]
    pub fn sync(&self) -> GeneratorHolder {
        self.run(async move |coro, sync_engine| sync_engine.sync(coro).await)
    }

    #[napi]
    pub fn push(&self) -> GeneratorHolder {
        self.run(async move |coro, sync_engine| sync_engine.push(coro).await)
    }

    #[napi]
    pub fn pull(&self) -> GeneratorHolder {
        self.run(async move |coro, sync_engine| sync_engine.pull(coro).await)
    }

    #[napi]
    pub fn open(&self) -> napi::Result<turso_node::Database> {
        let opened = self.opened.lock().unwrap();
        let Some(opened) = opened.as_ref() else {
            return Err(napi::Error::new(
                napi::Status::GenericFailure,
                "sync_engine must be initialized".to_string(),
            ));
        };
        Ok(opened.clone())
    }

    fn run(
        &self,
        f: impl AsyncFnOnce(
                &Coro,
                &mut DatabaseSyncEngine<JsProtocolIo>,
            ) -> turso_sync_engine::Result<()>
            + 'static,
    ) -> GeneratorHolder {
        let sync_engine = self.sync_engine.clone();
        #[allow(clippy::await_holding_lock)]
        let generator = genawaiter::sync::Gen::new(|coro| async move {
            let Ok(mut sync_engine) = sync_engine.try_lock() else {
                let nasty_error = "sync_engine is busy".to_string();
                return Err(turso_sync_engine::errors::Error::DatabaseSyncEngineError(
                    nasty_error,
                ));
            };
            let Some(sync_engine) = sync_engine.as_mut() else {
                let error = "sync_engine must be initialized".to_string();
                return Err(turso_sync_engine::errors::Error::DatabaseSyncEngineError(
                    error,
                ));
            };
            f(&coro, sync_engine).await?;
            Ok(())
        });
        GeneratorHolder {
            inner: Box::new(Mutex::new(generator)),
        }
    }
}
