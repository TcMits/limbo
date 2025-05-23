use limbo_core::{CheckpointStatus, Connection, Database, IO};
use rand::{rng, RngCore};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use tempfile::TempDir;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

#[allow(dead_code)]
pub struct TempDatabase {
    pub path: PathBuf,
    pub io: Arc<dyn IO + Send>,
}
unsafe impl Send for TempDatabase {}

#[allow(dead_code, clippy::arc_with_non_send_sync)]
impl TempDatabase {
    pub fn new_empty() -> Self {
        Self::new(&format!("test-{}.db", rng().next_u32()))
    }

    pub fn new(db_name: &str) -> Self {
        let mut path = TempDir::new().unwrap().into_path();
        path.push(db_name);
        let io: Arc<dyn IO + Send> = Arc::new(limbo_core::PlatformIO::new().unwrap());
        Self { path, io }
    }

    pub fn new_with_rusqlite(table_sql: &str) -> Self {
        let mut path = TempDir::new().unwrap().into_path();
        path.push("test.db");
        {
            let connection = rusqlite::Connection::open(&path).unwrap();
            connection
                .pragma_update(None, "journal_mode", "wal")
                .unwrap();
            connection.execute(table_sql, ()).unwrap();
        }
        let io: Arc<dyn limbo_core::IO> = Arc::new(limbo_core::PlatformIO::new().unwrap());

        Self { path, io }
    }

    pub fn connect_limbo(&self) -> Rc<limbo_core::Connection> {
        log::debug!("conneting to limbo");
        let db = Database::open_file(self.io.clone(), self.path.to_str().unwrap(), false).unwrap();

        let conn = db.connect().unwrap();
        log::debug!("connected to limbo");
        conn
    }

    pub fn limbo_database(&self) -> Arc<limbo_core::Database> {
        log::debug!("conneting to limbo");
        Database::open_file(self.io.clone(), self.path.to_str().unwrap(), false).unwrap()
    }
}

pub(crate) fn do_flush(conn: &Rc<Connection>, tmp_db: &TempDatabase) -> anyhow::Result<()> {
    loop {
        match conn.cacheflush()? {
            CheckpointStatus::Done(_) => {
                break;
            }
            CheckpointStatus::IO => {
                tmp_db.io.run_once()?;
            }
        }
    }
    Ok(())
}

pub(crate) fn compare_string(a: impl AsRef<str>, b: impl AsRef<str>) {
    let a = a.as_ref();
    let b = b.as_ref();

    assert_eq!(a.len(), b.len(), "Strings are not equal in size!");

    let a = a.as_bytes();
    let b = b.as_bytes();

    let len = a.len();
    for i in 0..len {
        if a[i] != b[i] {
            println!(
                "Bytes differ \n\t at index: dec -> {} hex -> {:#02x} \n\t values dec -> {}!={} hex -> {:#02x}!={:#02x}",
                i, i, a[i], b[i], a[i], b[i]
            );
            break;
        }
    }
}

pub fn maybe_setup_tracing() {
    let _ = tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(false)
                .with_line_number(true)
                .with_thread_ids(true),
        )
        .with(EnvFilter::from_default_env())
        .try_init();
}
#[cfg(test)]
mod tests {
    use super::TempDatabase;

    #[test]
    fn test_statement_columns() -> anyhow::Result<()> {
        let _ = env_logger::try_init();
        let tmp_db = TempDatabase::new_with_rusqlite(
            "create table test (foo integer, bar integer, baz integer);",
        );
        let conn = tmp_db.connect_limbo();

        let stmt = conn.prepare("select * from test;")?;

        let columns = stmt.num_columns();
        assert_eq!(columns, 3);
        assert_eq!(stmt.get_column_name(0), "foo");
        assert_eq!(stmt.get_column_name(1), "bar");
        assert_eq!(stmt.get_column_name(2), "baz");

        let stmt = conn.prepare("select foo, bar from test;")?;

        let columns = stmt.num_columns();
        assert_eq!(columns, 2);
        assert_eq!(stmt.get_column_name(0), "foo");
        assert_eq!(stmt.get_column_name(1), "bar");

        let stmt = conn.prepare("delete from test;")?;
        let columns = stmt.num_columns();
        assert_eq!(columns, 0);

        let stmt = conn.prepare("insert into test (foo, bar, baz) values (1, 2, 3);")?;
        let columns = stmt.num_columns();
        assert_eq!(columns, 0);

        let stmt = conn.prepare("delete from test where foo = 1")?;
        let columns = stmt.num_columns();
        assert_eq!(columns, 0);

        Ok(())
    }
}
