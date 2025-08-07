use napi_derive::napi;
use std::{future::Future, sync::Mutex};

use turso_sync_engine::types::ProtocolCommand;

pub const GENERATOR_RESUME_IO: u32 = 0;
pub const GENERATOR_RESUME_DONE: u32 = 1;

pub trait Generator {
    fn resume(&mut self, result: Option<String>) -> napi::Result<u32>;
}

impl<F: Future<Output = turso_sync_engine::Result<()>>> Generator
    for genawaiter::sync::Gen<ProtocolCommand, turso_sync_engine::Result<()>, F>
{
    fn resume(&mut self, error: Option<String>) -> napi::Result<u32> {
        let result = match error {
            Some(err) => Err(turso_sync_engine::errors::Error::DatabaseSyncEngineError(
                format!("JsProtocolIo error: {err}"),
            )),
            None => Ok(()),
        };
        match self.resume_with(result) {
            genawaiter::GeneratorState::Yielded(ProtocolCommand::IO) => Ok(GENERATOR_RESUME_IO),
            genawaiter::GeneratorState::Complete(Ok(())) => Ok(GENERATOR_RESUME_DONE),
            genawaiter::GeneratorState::Complete(Err(err)) => Err(napi::Error::new(
                napi::Status::GenericFailure,
                format!("sync engine operation failed: {err}"),
            )),
        }
    }
}

#[napi]
pub struct GeneratorHolder {
    pub(crate) inner: Box<Mutex<dyn Generator>>,
}

#[napi]
impl GeneratorHolder {
    #[napi]
    pub fn resume(&self, error: Option<String>) -> napi::Result<u32> {
        self.inner.lock().unwrap().resume(error)
    }
}
