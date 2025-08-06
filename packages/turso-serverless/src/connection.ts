import { Session, type SessionConfig } from './session.js';
import { Statement } from './statement.js';

/**
 * Configuration options for connecting to a Turso database.
 */
export interface Config extends SessionConfig {}

/**
 * A connection to a Turso database.
 * 
 * Provides methods for executing SQL statements and managing prepared statements.
 * Uses the SQL over HTTP protocol with streaming cursor support for optimal performance.
 */
export class Connection {
  private config: Config;
  private session: Session;
  private isOpen: boolean = true;

  constructor(config: Config) {
    if (!config.url) {
      throw new Error("invalid config: url is required");
    }
    this.config = config;
    this.session = new Session(config);
  }

  /**
   * Prepare a SQL statement for execution.
   * 
   * Each prepared statement gets its own session to avoid conflicts during concurrent execution.
   * 
   * @param sql - The SQL statement to prepare
   * @returns A Statement object that can be executed multiple ways
   * 
   * @example
   * ```typescript
   * const stmt = client.prepare("SELECT * FROM users WHERE id = ?");
   * const user = await stmt.get([123]);
   * const allUsers = await stmt.all();
   * ```
   */
  prepare(sql: string): Statement {
    if (!this.isOpen) {
      throw new TypeError("The database connection is not open");
    }
    return new Statement(this.config, sql);
  }

  /**
   * Execute a SQL statement and return all results.
   * 
   * @param sql - The SQL statement to execute
   * @returns Promise resolving to the complete result set
   * 
   * @example
   * ```typescript
   * const result = await client.execute("SELECT * FROM users");
   * console.log(result.rows);
   * ```
   */
  async exec(sql: string): Promise<any> {
    return this.session.sequence(sql);
  }


  /**
   * Execute multiple SQL statements in a batch.
   * 
   * @param statements - Array of SQL statements to execute
   * @param mode - Optional transaction mode (currently unused)
   * @returns Promise resolving to batch execution results
   * 
   * @example
   * ```typescript
   * await client.batch([
   *   "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)",
   *   "INSERT INTO users (name) VALUES ('Alice')",
   *   "INSERT INTO users (name) VALUES ('Bob')"
   * ]);
   * ```
   */
  async batch(statements: string[], mode?: string): Promise<any> {
    return this.session.batch(statements);
  }

  /**
   * Execute a pragma.
   * 
   * @param pragma - The pragma to execute
   * @returns Promise resolving to the result of the pragma
   */
  async pragma(pragma: string): Promise<any> {
    const sql = `PRAGMA ${pragma}`;
    return this.session.execute(sql);
  }

  /**
   * Close the connection.
   * 
   * This sends a close request to the server to properly clean up the stream.
   */
  async close(): Promise<void> {
    this.isOpen = false;
    await this.session.close();
  }
}

/**
 * Create a new connection to a Turso database.
 * 
 * @param config - Configuration object with database URL and auth token
 * @returns A new Connection instance
 * 
 * @example
 * ```typescript
 * import { connect } from "@tursodatabase/serverless";
 * 
 * const client = connect({
 *   url: process.env.TURSO_DATABASE_URL,
 *   authToken: process.env.TURSO_AUTH_TOKEN
 * });
 * ```
 */
export function connect(config: Config): Connection {
  return new Connection(config);
}
