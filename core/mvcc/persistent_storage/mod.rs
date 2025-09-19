use std::cell::RefCell;
use std::fmt::Debug;
use std::sync::Arc;

mod logical_log;
use crate::mvcc::database::LogRecord;
use crate::mvcc::persistent_storage::logical_log::LogicalLog;
use crate::types::IOResult;
use crate::{File, Result};

pub struct Storage {
    logical_log: RefCell<LogicalLog>,
}

impl Storage {
    pub fn new(file: Arc<dyn File>) -> Self {
        Self {
            logical_log: RefCell::new(LogicalLog::new(file)),
        }
    }
}

impl Storage {
    pub fn log_tx(&self, m: &LogRecord) -> Result<IOResult<()>> {
        self.logical_log.borrow_mut().log_tx(m)
    }

    pub fn read_tx_log(&self) -> Result<Vec<LogRecord>> {
        todo!()
    }

    pub fn is_logical_log(&self) -> bool {
        true
    }

    pub fn sync(&self) -> Result<IOResult<()>> {
        self.logical_log.borrow_mut().sync()
    }
}

impl Debug for Storage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LogicalLog {{ logical_log }}")
    }
}
