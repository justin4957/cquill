# Adapters

Adapters in cquill provide the abstraction layer between your queries and the underlying database. This separation enables testing with in-memory stores and deploying with real databases using identical application code.

## What is an Adapter?

An adapter translates cquill queries into backend-specific operations:

```
┌─────────────────────┐
│   cquill Query      │  query.from("users") |> query.where(...)
└─────────────────────┘
          │
          ▼
┌─────────────────────┐
│      Adapter        │  Translates to backend-specific format
└─────────────────────┘
          │
          ▼
┌─────────────────────┐
│     Backend         │  Memory / PostgreSQL / SQLite / etc.
└─────────────────────┘
```

## Available Adapters

### Memory Adapter

The in-memory adapter stores data in Gleam data structures. Perfect for:

- Unit testing
- Development without a database
- Prototyping

```gleam
import cquill/adapter/memory

// Create a new store
let store = memory.new_store()

// Create tables
let store = memory.create_table(store, "users", ["id", "email", "name"])

// Insert data
let store = memory.insert_row(store, "users", dict.from_list([
  #("id", ast.IntValue(1)),
  #("email", ast.StringValue("alice@example.com")),
  #("name", ast.StringValue("Alice")),
]))

// Execute queries
let result = memory.execute_select(store, query)
```

#### Memory Adapter Features

| Feature | Supported |
|---------|-----------|
| SELECT queries | Yes |
| INSERT | Yes |
| UPDATE | Yes |
| DELETE | Yes |
| Transactions | Yes |
| Savepoints | Yes |
| Batch operations | Yes |
| JOINs | Yes |
| ORDER BY | Yes |
| LIMIT/OFFSET | Yes |
| Aggregations | Partial |

### PostgreSQL Adapter

The PostgreSQL adapter connects to real PostgreSQL databases:

```gleam
import cquill/adapter/postgres

// Configuration
let config = postgres.Config(
  host: "localhost",
  port: 5432,
  database: "myapp_dev",
  user: "postgres",
  password: "password",
  pool_size: 10,
)

// Connect
use pool <- result.try(postgres.connect(config))

// Execute queries
let result = postgres.execute_select(pool, query)
```

## Adapter Protocol

All adapters implement the same interface, defined by the adapter behavior:

```gleam
// Core operations every adapter must implement
pub type AdapterBehaviour {
  // Query execution
  execute_select: fn(Adapter, Query) -> Result(List(Row), AdapterError)
  execute_insert: fn(Adapter, InsertQuery) -> Result(List(Row), AdapterError)
  execute_update: fn(Adapter, UpdateQuery) -> Result(List(Row), AdapterError)
  execute_delete: fn(Adapter, DeleteQuery) -> Result(Int, AdapterError)

  // Transaction support
  begin_transaction: fn(Adapter) -> Result(Transaction, AdapterError)
  commit_transaction: fn(Transaction) -> Result(Nil, AdapterError)
  rollback_transaction: fn(Transaction) -> Result(Nil, AdapterError)

  // Savepoint support
  create_savepoint: fn(Transaction, String) -> Result(Savepoint, AdapterError)
  release_savepoint: fn(Savepoint) -> Result(Nil, AdapterError)
  rollback_to_savepoint: fn(Savepoint) -> Result(Nil, AdapterError)
}
```

## Using Adapters

### Consistent API

The same code works with any adapter:

```gleam
import cquill/repo

// Function works with any adapter
pub fn get_active_users(adapter) {
  let query =
    query.from("users")
    |> query.where(query.eq("active", ast.BoolValue(True)))

  repo.all(adapter, query)
}

// In tests - use memory adapter
let store = memory.new_store() |> setup_test_data()
get_active_users(store)

// In production - use PostgreSQL
use pool <- result.try(postgres.connect(config))
get_active_users(pool)
```

### Adapter-Specific Configuration

Each adapter has its own configuration:

```gleam
// Memory adapter - no configuration needed
let memory_adapter = memory.new_store()

// PostgreSQL - connection configuration
let pg_config = postgres.Config(
  host: env.get("DB_HOST"),
  port: env.get_int("DB_PORT"),
  database: env.get("DB_NAME"),
  user: env.get("DB_USER"),
  password: env.get("DB_PASSWORD"),
  pool_size: env.get_int("DB_POOL_SIZE"),
  ssl: env.get_bool("DB_SSL"),
  timeout: env.get_int("DB_TIMEOUT"),
)
```

## Writing Custom Adapters

You can create adapters for other databases by implementing the adapter protocol.

### Basic Structure

```gleam
// my_adapter.gleam
import cquill/error.{type AdapterError}
import cquill/query/ast.{type Query, type InsertQuery, type UpdateQuery, type DeleteQuery}

pub opaque type MyAdapter {
  MyAdapter(connection: SomeConnection)
}

pub fn connect(config: MyConfig) -> Result(MyAdapter, AdapterError) {
  // Initialize connection
}

pub fn execute_select(
  adapter: MyAdapter,
  query: Query(a),
) -> Result(List(Row), AdapterError) {
  // Compile query to backend format
  let sql = compile_select(query)

  // Execute and convert results
  case execute(adapter.connection, sql) {
    Ok(rows) -> Ok(convert_rows(rows))
    Error(e) -> Error(convert_error(e))
  }
}

// Implement other operations...
```

### Query Compilation

Convert cquill AST to your backend's format:

```gleam
fn compile_select(query: Query(a)) -> String {
  let table = case query.source {
    ast.TableSource(table, None) -> table
    ast.TableSource(table, Some(schema)) -> schema <> "." <> table
  }

  let select = case query.select {
    ast.SelectAll -> "*"
    ast.SelectFields(fields) -> string.join(fields, ", ")
  }

  let where_clause = compile_wheres(query.wheres)
  let order_clause = compile_order_by(query.order_bys)
  let limit_clause = compile_limit(query.limit, query.offset)

  "SELECT " <> select <> " FROM " <> table
    <> where_clause <> order_clause <> limit_clause
}
```

### Error Conversion

Map backend errors to cquill errors:

```gleam
fn convert_error(backend_error: BackendError) -> AdapterError {
  case backend_error {
    BackendNotFound -> error.NotFound
    BackendTimeout -> error.Timeout
    BackendDuplicate(key) -> error.UniqueViolation(key, "")
    BackendForeignKey(key) -> error.ForeignKeyViolation(key, "")
    BackendQuery(msg, code) -> error.QueryFailed(msg, Some(code))
    _ -> error.AdapterSpecific("unknown", backend_error.message)
  }
}
```

## Testing with Adapters

### Unit Tests with Memory Adapter

```gleam
import gleeunit/should
import cquill/adapter/memory

pub fn test_user_creation() {
  // Setup
  let store = memory.new_store()
    |> memory.create_table("users", ["id", "email", "name"])

  // Test
  let store = memory.insert_row(store, "users", dict.from_list([
    #("id", ast.IntValue(1)),
    #("email", ast.StringValue("test@example.com")),
    #("name", ast.StringValue("Test")),
  ]))

  // Assert
  let query = query.from("users")
    |> query.where(query.eq("id", ast.IntValue(1)))

  memory.execute_select(store, query)
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)
}
```

### Integration Tests with Real Database

```gleam
pub fn test_user_creation_postgres() {
  use pool <- setup_test_database()

  // Test runs against real PostgreSQL
  let result = repo.insert(pool, create_user_query())

  result
  |> should.be_ok()
}

fn setup_test_database() -> fn(fn(Pool) -> a) -> a {
  fn(test_fn) {
    let pool = postgres.connect(test_config())
    postgres.begin_transaction(pool)

    let result = test_fn(pool)

    // Rollback to keep test database clean
    postgres.rollback_transaction(pool)
    result
  }
}
```

## Best Practices

### 1. Abstract Adapter Access

```gleam
// Good: Pass adapter as parameter
pub fn get_user(adapter, id: Int) {
  repo.one(adapter, user_by_id_query(id))
}

// Avoid: Hardcode adapter choice
pub fn get_user_bad(id: Int) {
  repo.one(postgres.global_pool(), user_by_id_query(id))
}
```

### 2. Use Memory Adapter for Unit Tests

```gleam
// Unit tests should be fast and isolated
pub fn test_business_logic() {
  let adapter = memory.new_store() |> setup_fixtures()

  // Test your logic without database I/O
  my_business_function(adapter)
  |> should.be_ok()
}
```

### 3. Integration Tests for Adapter-Specific Behavior

```gleam
// Test real database behavior separately
pub fn test_postgres_specific() {
  use pool <- setup_postgres()

  // Test things like constraints, indexes, etc.
  repo.insert(pool, duplicate_key_insert())
  |> should.equal(Error(error.UniqueViolation(_, _)))
}
```

## Next Steps

- See the [Adapter Contract](../ADAPTER_CONTRACT.md) for detailed specifications
- Learn about [Testing](../guides/testing.md) strategies
- Read the [Memory Adapter API](../reference/api.md#memory-adapter)
