# turso-sync-js package

> [!WARNING]
> **`@tursodatabase/sync`** is in a **very experimental** stage.
> It may cause **data corruption** in **both local and remote databases**.
>
> We are actively working to make it a **production-grade** package, but **it is not safe for critical data yet**.

## Usage

```
npm i @tursodatabase/sync
```

Example usage with remote DB hosting at [Turso Cloud](https://turso.tech)

```js
import { connect } from '@tursodatabase/sync';

const db = await connect({
    path: 'local.db',                // path used as a prefix for local files created by sync-engine
    url: 'https://<db>.turso.io',    // URL of the remote database: turso db show <db>
    authToken: '...',                // auth token issued from the Turso Cloud: turso db tokens create <db>
    clientName: 'turso-sync-example' // arbitrary client name
});

// db has same functions as Database class from @tursodatabase/database package but adds few more methods for sync:
await db.pull(); // pull changes from the remote
await db.push(); // push changes to the remote
await db.sync(); // pull & push changes
```
