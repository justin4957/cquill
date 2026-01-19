# Repository Pattern

The Repo module is cquill's public API for all database operations. It provides a unified interface for executing queries against any adapter.

## What is the Repo?

The Repo is the single entry point for database I/O in cquill. It:

- Executes queries against adapters
- Manages transactions
- Handles error conversion
- Provides a consistent API across different backends

```gleam
import cquill/repo
import cquill/adapter/memory

// All database operations go through the repo
let result = repo.all(adapter, query)
let result = repo.one(adapter, query)
let result = repo.insert(adapter, insert_query)
let result = repo.update(adapter, update_query)
let result = repo.delete(adapter, delete_query)
```

## Core Operations

### Reading Data

#### `repo.all` - Fetch Multiple Rows

Returns all rows matching the query:

```gleam
import cquill/query
import cquill/query/ast

let query =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.order_by_asc("name")

case repo.all(adapter, query) {
  Ok(rows) -> {
    // rows is List(Dict(String, ast.Value))
    list.each(rows, fn(row) {
      io.println("User: " <> string.inspect(row))
    })
  }
  Error(e) -> handle_error(e)
}
```

#### `repo.one` - Fetch Single Row

Returns exactly one row, or an error:

```gleam
let query =
  query.from("users")
  |> query.where(query.eq("id", ast.IntValue(1)))

case repo.one(adapter, query) {
  Ok(row) -> {
    // row is Dict(String, ast.Value)
    io.println("Found user: " <> string.inspect(row))
  }
  Error(error.NotFound) -> {
    io.println("User not found")
  }
  Error(error.TooManyRows(expected, got)) -> {
    io.println("Expected 1 row, got " <> int.to_string(got))
  }
  Error(e) -> handle_error(e)
}
```

#### `repo.exists` - Check Existence

Returns whether any rows match:

```gleam
let query =
  query.from("users")
  |> query.where(query.eq("email", ast.StringValue("test@example.com")))

case repo.exists(adapter, query) {
  Ok(True) -> io.println("Email already taken")
  Ok(False) -> io.println("Email available")
  Error(e) -> handle_error(e)
}
```

### Writing Data

#### `repo.insert` - Create Records

Insert new records:

```gleam
import cquill/query/ast

let insert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values([[
    ast.StringValue("alice@example.com"),
    ast.StringValue("Alice"),
  ]])
  |> ast.returning(["id"])

case repo.insert(adapter, insert) {
  Ok(rows) -> {
    case list.first(rows) {
      Ok(row) -> {
        let id = dict.get(row, "id")
        io.println("Created user with ID: " <> string.inspect(id))
      }
      Error(_) -> io.println("Insert succeeded but no rows returned")
    }
  }
  Error(error.UniqueViolation(constraint, detail)) -> {
    io.println("Email already exists")
  }
  Error(e) -> handle_error(e)
}
```

#### `repo.insert_all` - Bulk Insert

Insert multiple records efficiently:

```gleam
let users = [
  [ast.StringValue("alice@example.com"), ast.StringValue("Alice")],
  [ast.StringValue("bob@example.com"), ast.StringValue("Bob")],
  [ast.StringValue("carol@example.com"), ast.StringValue("Carol")],
]

let insert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values(users)

case repo.insert_all(adapter, insert) {
  Ok(count) -> io.println("Inserted " <> int.to_string(count) <> " users")
  Error(e) -> handle_error(e)
}
```

#### `repo.update` - Modify Records

Update existing records:

```gleam
let update =
  ast.new_update("users")
  |> ast.set("name", ast.StringValue("Alice Smith"))
  |> ast.update_where(query.eq("id", ast.IntValue(1)))
  |> ast.returning(["id", "name"])

case repo.update(adapter, update) {
  Ok(rows) -> {
    io.println("Updated " <> int.to_string(list.length(rows)) <> " rows")
  }
  Error(error.NotFound) -> {
    io.println("No matching rows to update")
  }
  Error(e) -> handle_error(e)
}
```

#### `repo.delete` - Remove Records

Delete records:

```gleam
let delete =
  ast.new_delete("users")
  |> ast.delete_where(query.eq("id", ast.IntValue(1)))

case repo.delete(adapter, delete) {
  Ok(count) -> io.println("Deleted " <> int.to_string(count) <> " rows")
  Error(e) -> handle_error(e)
}
```

### Transactions

#### `repo.transaction` - Atomic Operations

Execute multiple operations atomically:

```gleam
repo.transaction(adapter, fn(tx) {
  // All operations use the transaction context
  use user <- result.try(repo.insert(tx, create_user))
  use _profile <- result.try(repo.insert(tx, create_profile(user)))
  use _settings <- result.try(repo.insert(tx, create_settings(user)))

  Ok(user)
})
|> result.map(fn(user) {
  io.println("Created user with profile and settings")
})
|> result.map_error(fn(e) {
  io.println("Transaction failed, all changes rolled back")
})
```

#### Savepoints

For partial rollback within transactions:

```gleam
repo.transaction(adapter, fn(tx) {
  use order <- result.try(repo.insert(tx, create_order))

  // Create a savepoint
  use sp <- repo.savepoint(tx, "process_items")

  // Try to process items
  case process_items(tx, order) {
    Ok(items) -> Ok(#(order, items))
    Error(_) -> {
      // Roll back just the items, keep the order
      repo.rollback_to_savepoint(tx, sp)
      Ok(#(order, []))  // Order without items
    }
  }
})
```

## Error Handling

All repo operations return `Result` with specific error types:

```gleam
import cquill/error

case repo.one(adapter, query) {
  Ok(row) -> handle_success(row)

  // Not found errors
  Error(error.NotFound) -> handle_not_found()
  Error(error.TooManyRows(expected, got)) -> handle_too_many()

  // Connection errors
  Error(error.ConnectionFailed(reason)) -> handle_connection_error()
  Error(error.ConnectionTimeout) -> handle_timeout()
  Error(error.PoolExhausted) -> handle_pool_exhausted()

  // Query errors
  Error(error.QueryFailed(message, code)) -> handle_query_error()
  Error(error.Timeout) -> handle_query_timeout()

  // Constraint violations
  Error(error.UniqueViolation(constraint, detail)) -> handle_duplicate()
  Error(error.ForeignKeyViolation(constraint, detail)) -> handle_fk_error()
  Error(error.NotNullViolation(column)) -> handle_null_error()

  // Catch-all
  Error(e) -> handle_unknown_error(e)
}
```

## Adapter Independence

The repo works identically with different adapters:

```gleam
// Development/testing with memory adapter
let dev_adapter = memory.new_store()
let result = repo.all(dev_adapter, query)

// Production with PostgreSQL adapter
let prod_adapter = postgres.connect(config)
let result = repo.all(prod_adapter, query)

// Same query, same repo API, different backend
```

## Best Practices

### 1. Use Transactions for Related Operations

```gleam
// Good: Atomic operation
repo.transaction(adapter, fn(tx) {
  use order <- result.try(repo.insert(tx, create_order))
  use _ <- result.try(repo.update(tx, update_inventory))
  Ok(order)
})

// Bad: Separate operations that should be atomic
let order = repo.insert(adapter, create_order)  // What if this fails?
let _ = repo.update(adapter, update_inventory)   // Inventory updated but order failed
```

### 2. Handle Specific Errors

```gleam
// Good: Handle specific cases
case repo.insert(adapter, insert) {
  Ok(row) -> Ok(row)
  Error(error.UniqueViolation(_, _)) -> Error("Email already exists")
  Error(e) -> Error("Unexpected error: " <> error.format(e))
}

// Bad: Generic error handling
case repo.insert(adapter, insert) {
  Ok(row) -> Ok(row)
  Error(_) -> Error("Something went wrong")
}
```

### 3. Use `one` vs `all` Appropriately

```gleam
// Use `one` when expecting exactly one result
let user = repo.one(adapter, get_user_by_id(id))

// Use `all` with limit(1) when you want at most one
let user = repo.all(adapter, get_users() |> query.limit(1))
```

## Next Steps

- Learn about [Adapters](./adapters.md) for backend specifics
- See [Transactions](../guides/transactions.md) for advanced patterns
- Read about [Error Handling](../reference/errors.md)
