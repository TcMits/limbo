import { DatabaseError } from './error.js';

export interface Value {
  type: 'null' | 'integer' | 'float' | 'text' | 'blob';
  value?: string | number;
  base64?: string;
}

export interface Column {
  name: string;
  decltype: string;
}

export interface ExecuteResult {
  cols: Column[];
  rows: Value[][];
  affected_row_count: number;
  last_insert_rowid?: string;
}

export interface NamedArg {
  name: string;
  value: Value;
}

export interface ExecuteRequest {
  type: 'execute';
  stmt: {
    sql: string;
    args: Value[];
    named_args: NamedArg[];
    want_rows: boolean;
  };
}

export interface BatchStep {
  stmt: {
    sql: string;
    args: Value[];
    named_args?: NamedArg[];
    want_rows: boolean;
  };
  condition?: {
    type: 'ok';
    step: number;
  };
}

export interface BatchRequest {
  type: 'batch';
  batch: {
    steps: BatchStep[];
  };
}

export interface SequenceRequest {
  type: 'sequence';
  sql: string;
}

export interface CloseRequest {
  type: 'close';
}

export interface PipelineRequest {
  baton: string | null;
  requests: (ExecuteRequest | BatchRequest | SequenceRequest | CloseRequest)[];
}

export interface PipelineResponse {
  baton: string | null;
  base_url: string | null;
  results: Array<{
    type: 'ok' | 'error';
    response?: {
      type: 'execute' | 'batch' | 'sequence' | 'close';
      result?: ExecuteResult;
    };
    error?: {
      message: string;
      code: string;
    };
  }>;
}

export function encodeValue(value: any): Value {
  if (value === null || value === undefined) {
    return { type: 'null' };
  }
  
  if (typeof value === 'number') {
    if (Number.isInteger(value)) {
      return { type: 'integer', value: value.toString() };
    }
    return { type: 'float', value };
  }
  
  if (typeof value === 'string') {
    return { type: 'text', value };
  }
  
  if (value instanceof ArrayBuffer || value instanceof Uint8Array) {
    const base64 = btoa(String.fromCharCode(...new Uint8Array(value)));
    return { type: 'blob', base64 };
  }
  
  return { type: 'text', value: String(value) };
}

export function decodeValue(value: Value): any {
  switch (value.type) {
    case 'null':
      return null;
    case 'integer':
      return parseInt(value.value as string, 10);
    case 'float':
      return value.value as number;
    case 'text':
      return value.value as string;
    case 'blob':
      if (value.base64) {
        const binaryString = atob(value.base64);
        const bytes = new Uint8Array(binaryString.length);
        for (let i = 0; i < binaryString.length; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        return bytes;
      }
      return null;
    default:
      return null;
  }
}

export interface CursorRequest {
  baton: string | null;
  batch: {
    steps: BatchStep[];
  };
}

export interface CursorResponse {
  baton: string | null;
  base_url: string | null;
}

export interface CursorEntry {
  type: 'step_begin' | 'step_end' | 'step_error' | 'row' | 'error';
  step?: number;
  cols?: Column[];
  row?: Value[];
  affected_row_count?: number;
  last_insert_rowid?: string;
  error?: {
    message: string;
    code: string;
  };
}

export async function executeCursor(
  url: string,
  authToken: string,
  request: CursorRequest
): Promise<{ response: CursorResponse; entries: AsyncGenerator<CursorEntry> }> {
  const response = await fetch(`${url}/v3/cursor`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${authToken}`,
    },
    body: JSON.stringify(request),
  });

  if (!response.ok) {
    let errorMessage = `HTTP error! status: ${response.status}`;
    try {
      const errorBody = await response.text();
      const errorData = JSON.parse(errorBody);
      if (errorData.message) {
        errorMessage = errorData.message;
      }
    } catch {
      // If we can't parse the error body, use the default HTTP error message
    }
    throw new DatabaseError(errorMessage);
  }

  const reader = response.body?.getReader();
  if (!reader) {
    throw new DatabaseError('No response body');
  }

  const decoder = new TextDecoder();
  let buffer = '';
  let isFirstLine = true;
  let cursorResponse: CursorResponse;

  async function* parseEntries(): AsyncGenerator<CursorEntry> {
    try {
      while (true) {
        const { done, value } = await reader!.read();
        if (done) break;

        buffer += decoder.decode(value, { stream: true });
        
        let newlineIndex;
        while ((newlineIndex = buffer.indexOf('\n')) !== -1) {
          const line = buffer.slice(0, newlineIndex).trim();
          buffer = buffer.slice(newlineIndex + 1);
          
          if (line) {
            if (isFirstLine) {
              cursorResponse = JSON.parse(line);
              isFirstLine = false;
            } else {
              yield JSON.parse(line) as CursorEntry;
            }
          }
        }
      }
    } finally {
      reader!.releaseLock();
    }
  }

  const entries = parseEntries();
  
  // Get the first entry to parse the cursor response
  const firstEntry = await entries.next();
  if (!firstEntry.done) {
    // Put the first entry back
    const generator = (async function* () {
      yield firstEntry.value;
      yield* entries;
    })();
    
    return { response: cursorResponse!, entries: generator };
  }
  
  return { response: cursorResponse!, entries };
}

export async function executePipeline(
  url: string,
  authToken: string,
  request: PipelineRequest
): Promise<PipelineResponse> {
  const response = await fetch(`${url}/v3/pipeline`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${authToken}`,
    },
    body: JSON.stringify(request),
  });

  if (!response.ok) {
    throw new DatabaseError(`HTTP error! status: ${response.status}`);
  }

  return response.json();
}
