use crate::Result;

pub struct WalSession<'a> {
    conn: &'a turso::Connection,
    in_txn: bool,
}

impl<'a> WalSession<'a> {
    pub fn new(conn: &'a turso::Connection) -> Self {
        Self {
            conn,
            in_txn: false,
        }
    }
    pub fn begin(&mut self) -> Result<()> {
        assert!(!self.in_txn);
        self.conn.wal_insert_begin()?;
        self.in_txn = true;
        Ok(())
    }
    pub fn end(&mut self) -> Result<()> {
        assert!(self.in_txn);
        self.conn.wal_insert_end()?;
        self.in_txn = false;
        Ok(())
    }
    pub fn in_txn(&self) -> bool {
        self.in_txn
    }
}

impl<'a> Drop for WalSession<'a> {
    fn drop(&mut self) {
        if self.in_txn {
            let _ = self
                .end()
                .inspect_err(|e| tracing::error!("failed to close WAL session: {}", e));
        }
    }
}
