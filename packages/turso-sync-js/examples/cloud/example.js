import { connect } from '@tursodatabase/sync';

const db = await connect({
    path: 'local.db',
    url: process.env.TURSO_URL,
    authToken: process.env.TURSO_AUTH_TOKEN,
    clientName: 'turso-sync-example'
});

await db.sync();

console.info("database initialized and ready to accept writes")

{
    console.info("data from remote")
    let stmt = await db.prepare('SELECT * FROM users');
    console.info(await stmt.all());
}


for (let i = 0; i < 2; i++) {
    let id = Math.ceil(Math.random() * 100000);
    await db.exec(`INSERT INTO users VALUES (${id}, 'random-name-${id}')`);
}

{
    console.info("data after local insert")
    let stmt = await db.prepare('SELECT * FROM users');
    console.info(await stmt.all());
}

console.info("sync changes with the remote")
await db.sync();