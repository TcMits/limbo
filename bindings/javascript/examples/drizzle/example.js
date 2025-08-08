import { drizzle } from "drizzle-orm/better-sqlite3";
import { sql } from "drizzle-orm";
import { Database } from '@tursodatabase/database';

const sqlite = new Database('sqlite.db');
const db = drizzle({ client: sqlite });
const result = await db.all(sql`select 1`);
console.log(result);
