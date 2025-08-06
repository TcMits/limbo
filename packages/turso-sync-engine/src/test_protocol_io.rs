use std::{
    collections::{HashMap, VecDeque},
    path::Path,
    pin::Pin,
    sync::Arc,
};

use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    errors::Error,
    protocol_io::{DataCompletion, DataPollResult, ProtocolIO},
    test_context::TestContext,
    test_sync_server::TestSyncServer,
    Result,
};

#[derive(Clone)]
pub struct TestProtocolIo {
    pub requests: Arc<std::sync::Mutex<Vec<Pin<Box<JoinHandle<()>>>>>>,
    pub server: TestSyncServer,
    ctx: Arc<TestContext>,
    files: Arc<Mutex<HashMap<String, Vec<u8>>>>,
}

pub struct TestDataPollResult(Vec<u8>);

impl DataPollResult for TestDataPollResult {
    fn data(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Clone)]
pub struct TestDataCompletion {
    status: Arc<std::sync::Mutex<Option<u16>>>,
    chunks: Arc<std::sync::Mutex<VecDeque<Vec<u8>>>>,
    done: Arc<std::sync::Mutex<bool>>,
    poisoned: Arc<std::sync::Mutex<Option<String>>>,
}

impl TestDataCompletion {
    pub fn new() -> Self {
        Self {
            status: Arc::new(std::sync::Mutex::new(None)),
            chunks: Arc::new(std::sync::Mutex::new(VecDeque::new())),
            done: Arc::new(std::sync::Mutex::new(false)),
            poisoned: Arc::new(std::sync::Mutex::new(None)),
        }
    }
    pub fn set_status(&self, status: u16) {
        *self.status.lock().unwrap() = Some(status);
    }

    pub fn push_data(&self, data: Vec<u8>) {
        let mut chunks = self.chunks.lock().unwrap();
        chunks.push_back(data);
    }

    pub fn set_done(&self) {
        *self.done.lock().unwrap() = true;
    }

    pub fn poison(&self, err: &str) {
        *self.poisoned.lock().unwrap() = Some(err.to_string());
    }
}

impl DataCompletion for TestDataCompletion {
    type HttpPollResult = TestDataPollResult;

    fn status(&self) -> Result<Option<u16>> {
        let poison = self.poisoned.lock().unwrap();
        if poison.is_some() {
            return Err(Error::DatabaseSyncEngineError(format!(
                "status error: {poison:?}"
            )));
        }
        Ok(self.status.lock().unwrap().clone())
    }

    fn poll_data(&self) -> Result<Option<Self::HttpPollResult>> {
        let poison = self.poisoned.lock().unwrap();
        if poison.is_some() {
            return Err(Error::DatabaseSyncEngineError(format!(
                "poll_data error: {poison:?}"
            )));
        }
        let mut chunks = self.chunks.lock().unwrap();
        Ok(chunks.pop_front().map(|x| TestDataPollResult(x)))
    }

    fn is_done(&self) -> Result<bool> {
        let poison = self.poisoned.lock().unwrap();
        if poison.is_some() {
            return Err(Error::DatabaseSyncEngineError(format!(
                "is_done error: {poison:?}"
            )));
        }
        Ok(*self.done.lock().unwrap())
    }
}

impl TestProtocolIo {
    pub async fn new(ctx: Arc<TestContext>, path: &Path) -> Result<Self> {
        Ok(Self {
            ctx: ctx.clone(),
            requests: Arc::new(std::sync::Mutex::new(Vec::new())),
            server: TestSyncServer::new(ctx, path).await?,
            files: Arc::new(Mutex::new(HashMap::new())),
        })
    }
    fn schedule<
        Fut: std::future::Future<Output = Result<()>> + Send + 'static,
        F: FnOnce(TestSyncServer, TestDataCompletion) -> Fut + Send + 'static,
    >(
        &self,
        completion: TestDataCompletion,
        f: F,
    ) {
        let server = self.server.clone();
        let mut requests = self.requests.lock().unwrap();
        requests.push(Box::pin(tokio::spawn(async move {
            if let Err(err) = f(server, completion.clone()).await {
                tracing::info!("poison completion: {}", err);
                completion.poison(&err.to_string());
            }
        })));
    }
}

impl ProtocolIO for TestProtocolIo {
    type DataCompletion = TestDataCompletion;
    fn http(
        &self,
        method: http::Method,
        path: String,
        data: Option<Vec<u8>>,
    ) -> Result<TestDataCompletion> {
        let completion = TestDataCompletion::new();
        {
            let completion = completion.clone();
            let path = &path[1..].split("/").collect::<Vec<_>>();
            match (method.as_str(), path.as_slice()) {
                ("GET", ["info"]) => {
                    self.schedule(completion, |s, c| async move { s.db_info(c).await });
                }
                ("GET", ["export", generation]) => {
                    let generation = generation.parse().unwrap();
                    self.schedule(completion, async move |s, c| {
                        s.db_export(c, generation).await
                    });
                }
                ("GET", ["sync", generation, start, end]) => {
                    let generation = generation.parse().unwrap();
                    let start = start.parse().unwrap();
                    let end = end.parse().unwrap();
                    self.schedule(completion, async move |s, c| {
                        s.wal_pull(c, generation, start, end).await
                    });
                }
                ("POST", ["sync", generation, start, end]) => {
                    let generation = generation.parse().unwrap();
                    let start = start.parse().unwrap();
                    let end = end.parse().unwrap();
                    let data = data.unwrap();
                    self.schedule(completion, async move |s, c| {
                        s.wal_push(c, None, generation, start, end, data).await
                    });
                }
                ("POST", ["sync", generation, start, end, baton]) => {
                    let baton = baton.to_string();
                    let generation = generation.parse().unwrap();
                    let start = start.parse().unwrap();
                    let end = end.parse().unwrap();
                    let data = data.unwrap();
                    self.schedule(completion, async move |s, c| {
                        s.wal_push(c, Some(baton), generation, start, end, data)
                            .await
                    });
                }
                _ => panic!("unexpected sync server request: {} {:?}", method, path),
            };
        }
        Ok(completion)
    }

    fn full_read(&self, path: &str) -> Result<Self::DataCompletion> {
        let completion = TestDataCompletion::new();
        let ctx = self.ctx.clone();
        let files = self.files.clone();
        let path = path.to_string();
        self.schedule(completion.clone(), async move |_, c| {
            ctx.faulty_call("full_read_start").await?;
            let files = files.lock().await;
            let result = files.get(&path);
            c.push_data(result.cloned().unwrap_or(Vec::new()));
            ctx.faulty_call("full_read_end").await?;
            c.set_done();
            Ok(())
        });
        Ok(completion)
    }

    fn full_write(&self, path: &str, content: Vec<u8>) -> Result<Self::DataCompletion> {
        let completion = TestDataCompletion::new();
        let ctx = self.ctx.clone();
        let files = self.files.clone();
        let path = path.to_string();
        self.schedule(completion.clone(), async move |_, c| {
            ctx.faulty_call("full_write_start").await?;
            let mut files = files.lock().await;
            files.insert(path, content);
            ctx.faulty_call("full_write_end").await?;
            c.set_done();
            Ok(())
        });
        Ok(completion)
    }
}
