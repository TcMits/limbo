import { 
  decodeValue, 
  type CursorEntry
} from './protocol.js';
import { Session, type SessionConfig } from './session.js';
import { DatabaseError } from './error.js';

/**
 * A prepared SQL statement that can be executed in multiple ways.
 * 
 * Each statement has its own session to avoid conflicts during concurrent execution.
 * Provides three execution modes:
 * - `get(args?)`: Returns the first row or null
 * - `all(args?)`: Returns all rows as an array
 * - `iterate(args?)`: Returns an async iterator for streaming results
 */
export class Statement {
  private session: Session;
  private sql: string;
  private presentationMode: 'expanded' | 'raw' | 'pluck' = 'expanded';

  constructor(sessionConfig: SessionConfig, sql: string) {
    this.session = new Session(sessionConfig);
    this.sql = sql;
  }

  /**
   * Toggle raw mode.
   * 
   * @param raw Enable or disable raw mode. If you don't pass the parameter, raw mode is enabled.
   * @returns This statement instance for chaining
   * 
   * @example
   * ```typescript
   * const stmt = client.prepare("SELECT * FROM users WHERE id = ?");
   * const row = await stmt.raw().get([1]);
   * console.log(row); // [1, "Alice", "alice@example.org"]
   * ```
   */
  raw(raw?: boolean): Statement {
    this.presentationMode = raw === false ? 'expanded' : 'raw';
    return this;
  }

  /**
   * Executes the prepared statement.
   * 
   * @param args - Optional array of parameter values or object with named parameters
   * @returns Promise resolving to the result of the statement
   * 
   * @example
   * ```typescript
   * const stmt = client.prepare("INSERT INTO users (name, email) VALUES (?, ?)");
   * const result = await stmt.run(['John Doe', 'john.doe@example.com']);
   * console.log(`Inserted user with ID ${result.lastInsertRowid}`);
   * ```
   */
  async run(args: any[] | Record<string, any> = []): Promise<any> {
    const result = await this.session.execute(this.sql, args);
    return { changes: result.rowsAffected, lastInsertRowid: result.lastInsertRowid };
  }

  /**
   * Execute the statement and return the first row.
   * 
   * @param args - Optional array of parameter values or object with named parameters
   * @returns Promise resolving to the first row or undefined if no results
   * 
   * @example
   * ```typescript
   * const stmt = client.prepare("SELECT * FROM users WHERE id = ?");
   * const user = await stmt.get([123]);
   * if (user) {
   *   console.log(user.name);
   * }
   * ```
   */
  async get(args: any[] | Record<string, any> = []): Promise<any> {
    const result = await this.session.execute(this.sql, args);
    const row = result.rows[0];
    if (!row) {
      return undefined;
    }
    
    if (this.presentationMode === 'raw') {
      // In raw mode, return the row as a plain array (it already is one)
      // The row object is already an array with column properties added
      return [...row];
    }
    
    return row;
  }

  /**
   * Execute the statement and return all rows.
   * 
   * @param args - Optional array of parameter values or object with named parameters
   * @returns Promise resolving to an array of all result rows
   * 
   * @example
   * ```typescript
   * const stmt = client.prepare("SELECT * FROM users WHERE active = ?");
   * const activeUsers = await stmt.all([true]);
   * console.log(`Found ${activeUsers.length} active users`);
   * ```
   */
  async all(args: any[] | Record<string, any> = []): Promise<any[]> {
    const result = await this.session.execute(this.sql, args);
    
    if (this.presentationMode === 'raw') {
      // In raw mode, return arrays of values
      // Each row is already an array with column properties added
      return result.rows.map((row: any) => [...row]);
    }
    
    return result.rows;
  }

  /**
   * Execute the statement and return an async iterator for streaming results.
   * 
   * This method provides memory-efficient processing of large result sets
   * by streaming rows one at a time instead of loading everything into memory.
   * 
   * @param args - Optional array of parameter values or object with named parameters
   * @returns AsyncGenerator that yields individual rows
   * 
   * @example
   * ```typescript
   * const stmt = client.prepare("SELECT * FROM large_table WHERE category = ?");
   * for await (const row of stmt.iterate(['electronics'])) {
   *   // Process each row individually
   *   console.log(row.id, row.name);
   * }
   * ```
   */
  async *iterate(args: any[] | Record<string, any> = []): AsyncGenerator<any> {
    const { response, entries } = await this.session.executeRaw(this.sql, args);
    
    let columns: string[] = [];
    
    for await (const entry of entries) {
      switch (entry.type) {
        case 'step_begin':
          if (entry.cols) {
            columns = entry.cols.map(col => col.name);
          }
          break;
        case 'row':
          if (entry.row) {
            const decodedRow = entry.row.map(decodeValue);
            if (this.presentationMode === 'raw') {
              // In raw mode, yield arrays of values
              yield decodedRow;
            } else {
              const rowObject = this.session.createRowObject(decodedRow, columns);
              yield rowObject;
            }
          }
          break;
        case 'step_error':
        case 'error':
          throw new DatabaseError(entry.error?.message || 'SQL execution failed');
      }
    }
  }

}
