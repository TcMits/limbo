"use strict";

import { SyncEngine } from '#entry-point';
import { Database } from '@tursodatabase/turso';

const GENERATOR_RESUME_IO = 0;
const GENERATOR_RESUME_DONE = 1;

async function read(opts, path: string): Promise<Buffer | Uint8Array | null> {
    if (opts.isMemory) {
        return opts.value;
    }
    if (typeof window === 'undefined') {
        const { promises } = await import('node:fs');
        try {
            return await promises.readFile(path);
        } catch (error) {
            if (error.code === 'ENOENT') {
                return null;
            }
            throw error;
        }
    } else {
        const data = localStorage.getItem(path);
        if (data != null) {
            return new TextEncoder().encode(data);
        } else {
            return null;
        }
    }
}

async function write(opts, path: string, content: number[]): Promise<void> {
    if (opts.isMemory) {
        opts.value = content;
        return;
    }
    const data = new Uint8Array(content);
    if (typeof window === 'undefined') {
        const { promises } = await import('node:fs');
        const unix = Math.floor(Date.now() / 1000);
        const nonce = Math.floor(Math.random() * 1000000000);
        const tmp = `${path}.tmp.${unix}.${nonce}`;
        await promises.writeFile(tmp, data);
        await promises.rename(tmp, path);
    } else {
        localStorage.setItem(path, new TextDecoder().decode(data));
    }
}

async function process(opts, request) {
    const requestType = request.request();
    const completion = request.completion();
    if (requestType.type == 'Http') {
        try {
            const response = await fetch(`${opts.url}${requestType.path}`, {
                method: requestType.method,
                headers: opts.headers,
                body: requestType.body
            });
            completion.status(response.status);
            const reader = response.body.getReader();
            while (true) {
                const { done, value } = await reader.read();
                if (done) {
                    completion.done();
                    break;
                }
                completion.push(value);
            }
        } catch (error) {
            completion.poison(`fetch error: ${error}`);
        }
    } else if (requestType.type == 'FullRead') {
        try {
            const metadata = await read(opts.metadata, requestType.path);
            if (metadata != null) {
                completion.push(metadata);
            }
            completion.done();
        } catch (error) {
            completion.poison(`metadata read error: ${error}`);
        }
    } else if (requestType.type == 'FullWrite') {
        try {
            await write(opts.metadata, requestType.path, requestType.content);
            completion.done();
        } catch (error) {
            completion.poison(`metadata write error: ${error}`);
        }
    }
}

async function run(httpOpts, engine, generator) {
    while (generator.resume(null) !== GENERATOR_RESUME_DONE) {
        const tasks = [engine.ioLoopAsync()];
        for (let request = engine.protocolIo(); request != null; request = engine.protocolIo()) {
            tasks.push(process(httpOpts, request));
        }
        await Promise.all(tasks);
    }
}

interface ConnectOpts {
    path: string;
    clientName?: string;
    url: string;
    authToken?: string;
    encryptionKey?: string;
}

interface Sync {
    sync(): Promise<void>;
    push(): Promise<void>;
    pull(): Promise<void>;
}

export async function connect(opts: ConnectOpts): Database & Sync {
    const engine = new SyncEngine({ path: opts.path, clientName: opts.clientName });
    const httpOpts = {
        url: opts.url,
        headers: {
            ...(opts.authToken != null && { "Authorization": `Bearer ${opts.authToken}` }),
            ...(opts.encryptionKey != null && { "x-turso-encryption-key": opts.encryptionKey })
        },
        metadata: opts.path == ':memory:' ? { isMemory: true, value: null } : { isMemory: false }
    };
    await run(httpOpts, engine, engine.init());
    const nativeDb = engine.open();
    const db = Database.create();
    db.initialize(nativeDb, opts.path, false);
    db.sync = async function () { await run(httpOpts, engine, engine.sync()); }
    db.pull = async function () { await run(httpOpts, engine, engine.pull()); }
    db.push = async function () { await run(httpOpts, engine, engine.push()); }
    return db;
}
