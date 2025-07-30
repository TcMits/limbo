import { 
  executeCursor, 
  executePipeline,
  encodeValue, 
  decodeValue, 
  type CursorRequest,
  type CursorResponse,
  type CursorEntry,
  type PipelineRequest,
  type SequenceRequest,
  type CloseRequest,
  type NamedArg,
  type Value
} from './protocol.js';
import { DatabaseError } from './error.js';

/**
 * Configuration options for a session.
 */
export interface SessionConfig {
  /** Database URL */
  url: string;
  /** Authentication token */
  authToken: string;
}

function normalizeUrl(url: string): string {
  return url.replace(/^libsql:\/\//, 'https://');
}

function isValidIdentifier(str: string): boolean {
  return /^[a-zA-Z_$][a-zA-Z0-9_$]*$/.test(str);
}

/**
 * A database session that manages the connection state and baton.
 * 
 * Each session maintains its own connection state and can execute SQL statements
 * independently without interfering with other sessions.
 */
export class Session {
  private config: SessionConfig;
  private baton: string | null = null;
  private baseUrl: string;

  constructor(config: SessionConfig) {
    this.config = config;
    this.baseUrl = normalizeUrl(config.url);
  }

  /**
   * Execute a SQL statement and return all results.
   * 
   * @param sql - The SQL statement to execute
   * @param args - Optional array of parameter values or object with named parameters
   * @returns Promise resolving to the complete result set
   */
  async execute(sql: string, args: any[] | Record<string, any> = []): Promise<any> {
    const { response, entries } = await this.executeRaw(sql, args);
    const result = await this.processCursorEntries(entries);
    return result;
  }

  /**
   * Execute a SQL statement and return the raw response and entries.
   * 
   * @param sql - The SQL statement to execute
   * @param args - Optional array of parameter values or object with named parameters
   * @returns Promise resolving to the raw response and cursor entries
   */
  async executeRaw(sql: string, args: any[] | Record<string, any> = []): Promise<{ response: CursorResponse; entries: AsyncGenerator<CursorEntry> }> {
    let positionalArgs: Value[] = [];
    let namedArgs: NamedArg[] = [];

    if (Array.isArray(args)) {
      positionalArgs = args.map(encodeValue);
    } else {
      // Convert object with named parameters to NamedArg array
      namedArgs = Object.entries(args).map(([name, value]) => ({
        name,
        value: encodeValue(value)
      }));
    }

    const request: CursorRequest = {
      baton: this.baton,
      batch: {
        steps: [{
          stmt: {
            sql,
            args: positionalArgs,
            named_args: namedArgs,
            want_rows: true
          }
        }]
      }
    };

    const { response, entries } = await executeCursor(this.baseUrl, this.config.authToken, request);
    
    this.baton = response.baton;
    if (response.base_url) {
      this.baseUrl = response.base_url;
    }

    return { response, entries };
  }

  /**
   * Process cursor entries into a structured result.
   * 
   * @param entries - Async generator of cursor entries
   * @returns Promise resolving to the processed result
   */
  async processCursorEntries(entries: AsyncGenerator<CursorEntry>): Promise<any> {
    let columns: string[] = [];
    let columnTypes: string[] = [];
    let rows: any[] = [];
    let rowsAffected = 0;
    let lastInsertRowid: number | undefined;

    for await (const entry of entries) {
      switch (entry.type) {
        case 'step_begin':
          if (entry.cols) {
            columns = entry.cols.map(col => col.name);
            columnTypes = entry.cols.map(col => col.decltype || '');
          }
          break;
        case 'row':
          if (entry.row) {
            const decodedRow = entry.row.map(decodeValue);
            const rowObject = this.createRowObject(decodedRow, columns);
            rows.push(rowObject);
          }
          break;
        case 'step_end':
          if (entry.affected_row_count !== undefined) {
            rowsAffected = entry.affected_row_count;
          }
          if (entry.last_insert_rowid) {
            lastInsertRowid = parseInt(entry.last_insert_rowid, 10);
          }
          break;
        case 'step_error':
        case 'error':
          throw new DatabaseError(entry.error?.message || 'SQL execution failed');
      }
    }

    return {
      columns,
      columnTypes,
      rows,
      rowsAffected,
      lastInsertRowid
    };
  }

  /**
   * Create a row object with both array and named property access.
   * 
   * @param values - Array of column values
   * @param columns - Array of column names
   * @returns Row object with dual access patterns
   */
  createRowObject(values: any[], columns: string[]): any {
    const row = [...values];
    
    // Add column name properties to the array as non-enumerable
    // Only add valid identifier names to avoid conflicts
    columns.forEach((column, index) => {
      if (column && isValidIdentifier(column)) {
        Object.defineProperty(row, column, {
          value: values[index],
          enumerable: false,
          writable: false,
          configurable: true
        });
      }
    });
    
    return row;
  }

  /**
   * Execute multiple SQL statements in a batch.
   * 
   * @param statements - Array of SQL statements to execute
   * @returns Promise resolving to batch execution results
   */
  async batch(statements: string[]): Promise<any> {
    const request: CursorRequest = {
      baton: this.baton,
      batch: {
        steps: statements.map(sql => ({
          stmt: {
            sql,
            args: [],
            named_args: [],
            want_rows: false
          }
        }))
      }
    };

    const { response, entries } = await executeCursor(this.baseUrl, this.config.authToken, request);
    
    this.baton = response.baton;
    if (response.base_url) {
      this.baseUrl = response.base_url;
    }

    let totalRowsAffected = 0;
    let lastInsertRowid: number | undefined;

    for await (const entry of entries) {
      switch (entry.type) {
        case 'step_end':
          if (entry.affected_row_count !== undefined) {
            totalRowsAffected += entry.affected_row_count;
          }
          if (entry.last_insert_rowid) {
            lastInsertRowid = parseInt(entry.last_insert_rowid, 10);
          }
          break;
        case 'step_error':
        case 'error':
          throw new DatabaseError(entry.error?.message || 'Batch execution failed');
      }
    }

    return {
      rowsAffected: totalRowsAffected,
      lastInsertRowid
    };
  }

  /**
   * Execute a sequence of SQL statements separated by semicolons.
   * 
   * @param sql - SQL string containing multiple statements separated by semicolons
   * @returns Promise resolving when all statements are executed
   */
  async sequence(sql: string): Promise<void> {
    const request: PipelineRequest = {
      baton: this.baton,
      requests: [{
        type: "sequence",
        sql: sql
      } as SequenceRequest]
    };

    const response = await executePipeline(this.baseUrl, this.config.authToken, request);
    
    this.baton = response.baton;
    if (response.base_url) {
      this.baseUrl = response.base_url;
    }

    // Check for errors in the response
    if (response.results && response.results[0]) {
      const result = response.results[0];
      if (result.type === "error") {
        throw new DatabaseError(result.error?.message || 'Sequence execution failed');
      }
    }
  }

  /**
   * Close the session.
   * 
   * This sends a close request to the server to properly clean up the stream
   * before resetting the local state.
   */
  async close(): Promise<void> {
    // Only send close request if we have an active baton
    if (this.baton) {
      try {
        const request: PipelineRequest = {
          baton: this.baton,
          requests: [{
            type: "close"
          } as CloseRequest]
        };

        await executePipeline(this.baseUrl, this.config.authToken, request);
      } catch (error) {
        // Ignore errors during close, as the connection might already be closed
        console.error('Error closing session:', error);
      }
    }

    // Reset local state
    this.baton = null;
    this.baseUrl = '';
  }
}