import { run, bench, group, baseline } from 'mitata';

import Database from 'better-sqlite3';

const db = new Database(':memory:');

db.exec("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, email TEXT)");
db.exec("INSERT INTO users (id, name, email) VALUES (1, 'Alice', 'alice@example.org')");

const stmt = db.prepare("SELECT * FROM users WHERE id = ?");
const rawStmt = db.prepare("SELECT * FROM users WHERE id = ?").raw();

group('Statement', () => {
  bench('Statement.get() with bind parameters [expanded]', () => {
    stmt.get(1);
  });
  bench('Statement.get() with bind parameters [raw]', () => {
    rawStmt.get(1);
  });
});


await run({
  units: false,
  silent: false,
  avg: true,
  json: false,
  colors: true,
  min_max: true,
  percentiles: true,
});
