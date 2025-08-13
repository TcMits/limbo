pub mod database_sync_engine;
pub mod database_sync_operations;
pub mod database_tape;
pub mod errors;
pub mod io_operations;
pub mod protocol_io;
pub mod types;
pub mod wal_session;

#[cfg(test)]
pub mod test_context;
#[cfg(test)]
pub mod test_protocol_io;
#[cfg(test)]
pub mod test_sync_server;

pub type Result<T> = std::result::Result<T, errors::Error>;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use tokio::{select, sync::Mutex};
    use tracing_subscriber::EnvFilter;
    use turso_core::IO;

    use crate::{
        database_sync_engine::{DatabaseSyncEngine, DatabaseSyncEngineOpts},
        errors::Error,
        test_context::TestContext,
        test_protocol_io::TestProtocolIo,
        types::{Coro, ProtocolCommand},
        Result,
    };

    #[ctor::ctor]
    fn init() {
        tracing_subscriber::fmt()
            .with_env_filter(EnvFilter::from_default_env())
            .with_ansi(false)
            .init();
    }

    pub fn seed_u64() -> u64 {
        seed().parse().unwrap_or(0)
    }

    pub fn seed() -> String {
        std::env::var("SEED").unwrap_or("0".to_string())
    }

    pub fn deterministic_runtime_from_seed<F: std::future::Future<Output = ()>>(
        seed: &[u8],
        f: impl Fn() -> F,
    ) {
        let seed = tokio::runtime::RngSeed::from_bytes(seed);
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_time()
            .start_paused(true)
            .rng_seed(seed)
            .build_local(Default::default())
            .unwrap();
        runtime.block_on(f());
    }

    pub fn deterministic_runtime<F: std::future::Future<Output = ()>>(f: impl Fn() -> F) {
        let seed = seed();
        deterministic_runtime_from_seed(seed.as_bytes(), f);
    }

    pub struct TestRunner {
        pub ctx: Arc<TestContext>,
        pub io: Arc<dyn IO>,
        pub sync_server: TestProtocolIo,
        db: Option<Arc<Mutex<DatabaseSyncEngine<TestProtocolIo>>>>,
    }

    impl TestRunner {
        pub fn new(ctx: Arc<TestContext>, io: Arc<dyn IO>, sync_server: TestProtocolIo) -> Self {
            Self {
                ctx,
                io,
                sync_server,
                db: None,
            }
        }
        pub async fn init(&mut self, local_path: &str, opts: DatabaseSyncEngineOpts) -> Result<()> {
            let io = self.io.clone();
            let server = self.sync_server.clone();
            let db = self
                .run(genawaiter::sync::Gen::new(|coro| async move {
                    DatabaseSyncEngine::new(&coro, io, Arc::new(server), local_path, opts).await
                }))
                .await
                .unwrap();
            self.db = Some(Arc::new(Mutex::new(db)));
            Ok(())
        }
        pub async fn connect(&self) -> Result<turso::Connection> {
            self.run_db_fn(self.db.as_ref().unwrap(), async move |coro, db| {
                Ok(turso::Connection::create(db.connect(coro).await?))
            })
            .await
        }
        pub async fn pull(&self) -> Result<()> {
            self.run_db_fn(self.db.as_ref().unwrap(), async move |coro, db| {
                db.pull(coro).await
            })
            .await
        }
        pub async fn push(&self) -> Result<()> {
            self.run_db_fn(self.db.as_ref().unwrap(), async move |coro, db| {
                db.push(coro).await
            })
            .await
        }
        pub async fn sync(&self) -> Result<()> {
            self.run_db_fn(self.db.as_ref().unwrap(), async move |coro, db| {
                db.sync(coro).await
            })
            .await
        }
        pub async fn run_db_fn<T>(
            &self,
            db: &Arc<Mutex<DatabaseSyncEngine<TestProtocolIo>>>,
            f: impl AsyncFn(&Coro, &mut DatabaseSyncEngine<TestProtocolIo>) -> Result<T>,
        ) -> Result<T> {
            let g = genawaiter::sync::Gen::new({
                let db = db.clone();
                |coro| async move {
                    let mut db = db.lock().await;
                    f(&coro, &mut db).await
                }
            });
            self.run(g).await
        }
        pub async fn run<T, F: std::future::Future<Output = Result<T>>>(
            &self,
            mut g: genawaiter::sync::Gen<ProtocolCommand, Result<()>, F>,
        ) -> Result<T> {
            let mut response = Ok(());
            loop {
                // we must drive internal tokio clocks on every iteration - otherwise one TestRunner without work can block everything
                // if other TestRunner sleeping - as time will "freeze" in this case
                self.ctx.random_sleep().await;

                match g.resume_with(response) {
                    genawaiter::GeneratorState::Complete(result) => return result,
                    genawaiter::GeneratorState::Yielded(ProtocolCommand::IO) => {
                        let drained = {
                            let mut requests = self.sync_server.requests.lock().unwrap();
                            requests.drain(..).collect::<Vec<_>>()
                        };
                        for mut request in drained {
                            select! {
                                value = &mut request => { value.unwrap(); },
                                _ = self.ctx.random_sleep() => { self.sync_server.requests.lock().unwrap().push(request); }
                            };
                        }
                        response =
                            self.io.run_once().map(|_| ()).map_err(|e| {
                                Error::DatabaseSyncEngineError(format!("io error: {e}"))
                            });
                    }
                }
            }
        }
    }
}
