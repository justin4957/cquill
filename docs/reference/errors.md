# Errors Reference

Complete reference for all error types in cquill with causes and solutions.

## AdapterError

The main error type returned by all adapter operations.

```gleam
import cquill/error

case repo.all(adapter, query) {
  Ok(rows) -> handle_success(rows)
  Error(e) -> {
    io.println("Error: " <> error.format(e))
    handle_error(e)
  }
}
```

## Not Found Errors

### NotFound

A query expected to return a row returned none.

```gleam
Error(error.NotFound)
```

**When it occurs:**
- `repo.one()` with no matching rows
- Getting a record by ID that doesn't exist

**How to handle:**

```gleam
case repo.one(adapter, user_by_id(id)) {
  Ok(user) -> Ok(user)
  Error(error.NotFound) -> {
    io.println("User " <> int.to_string(id) <> " not found")
    Error("User not found")
  }
  Error(e) -> Error(error.format(e))
}
```

### TooManyRows

A query expected one row but returned multiple.

```gleam
Error(error.TooManyRows(expected: Int, got: Int))
```

**When it occurs:**
- `repo.one()` returns more than one row

**How to handle:**

```gleam
case repo.one(adapter, query) {
  Ok(row) -> Ok(row)
  Error(error.TooManyRows(expected, got)) -> {
    io.println("Expected " <> int.to_string(expected) <> ", got " <> int.to_string(got))
    // Usually indicates a bug in query or data
    Error("Multiple matches found")
  }
  Error(e) -> Error(error.format(e))
}
```

## Connection Errors

### ConnectionFailed

Failed to establish a database connection.

```gleam
Error(error.ConnectionFailed(reason: String))
```

**When it occurs:**
- Database server is down
- Invalid connection credentials
- Network issues

**How to handle:**

```gleam
case postgres.connect(config) {
  Ok(pool) -> use_pool(pool)
  Error(error.ConnectionFailed(reason)) -> {
    io.println_error("Cannot connect to database: " <> reason)
    // Retry with backoff or fail gracefully
  }
  Error(e) -> handle_error(e)
}
```

### ConnectionTimeout

Connection attempt timed out.

```gleam
Error(error.ConnectionTimeout)
```

**When it occurs:**
- Database is slow to respond
- Network latency issues

**How to handle:**

```gleam
case result {
  Error(error.ConnectionTimeout) -> {
    // Retry with increased timeout or fail
    io.println_error("Connection timeout - database may be overloaded")
  }
  _ -> ...
}
```

### PoolExhausted

All connections in the pool are in use.

```gleam
Error(error.PoolExhausted)
```

**When it occurs:**
- Too many concurrent database operations
- Pool size too small for workload
- Long-running queries holding connections

**How to handle:**

```gleam
case result {
  Error(error.PoolExhausted) -> {
    io.println_error("Connection pool exhausted")
    // Consider: increase pool size, add connection timeout
    // Or: implement request queuing/backpressure
  }
  _ -> ...
}
```

### ConnectionLost

Connection was lost during operation.

```gleam
Error(error.ConnectionLost(reason: String))
```

**When it occurs:**
- Database server restarted
- Network interruption
- Idle connection timeout

**How to handle:**

```gleam
case result {
  Error(error.ConnectionLost(reason)) -> {
    io.println_error("Connection lost: " <> reason)
    // Retry the operation with a new connection
  }
  _ -> ...
}
```

## Query Errors

### QueryFailed

Query execution failed.

```gleam
Error(error.QueryFailed(message: String, code: Option(String)))
```

**When it occurs:**
- SQL syntax error
- Invalid table or column name
- Invalid data type

**How to handle:**

```gleam
case result {
  Error(error.QueryFailed(message, Some(code))) -> {
    io.println_error("Query failed [" <> code <> "]: " <> message)
  }
  Error(error.QueryFailed(message, None)) -> {
    io.println_error("Query failed: " <> message)
  }
  _ -> ...
}
```

### DecodeFailed

Failed to decode a result row.

```gleam
Error(error.DecodeFailed(
  row: Int,
  column: String,
  expected: String,
  got: String,
))
```

**When it occurs:**
- Column type mismatch
- Unexpected NULL value
- Data corruption

**How to handle:**

```gleam
case result {
  Error(error.DecodeFailed(row, column, expected, got)) -> {
    io.println_error(
      "Decode error at row " <> int.to_string(row)
      <> ", column '" <> column <> "': expected " <> expected <> ", got " <> got
    )
  }
  _ -> ...
}
```

### Timeout

Operation timed out.

```gleam
Error(error.Timeout)
```

**When it occurs:**
- Query took too long to execute
- Statement timeout exceeded

**How to handle:**

```gleam
case result {
  Error(error.Timeout) -> {
    io.println_error("Query timeout - consider optimizing the query")
  }
  _ -> ...
}
```

## Constraint Violations

### UniqueViolation

Unique constraint violated.

```gleam
Error(error.UniqueViolation(constraint: String, detail: String))
```

**When it occurs:**
- Inserting duplicate value in unique column
- Updating to create duplicate

**How to handle:**

```gleam
case repo.insert(adapter, user_insert) {
  Ok(rows) -> Ok(rows)
  Error(error.UniqueViolation(constraint, detail)) -> {
    case constraint {
      "users_email_key" -> Error("Email already in use")
      "users_username_key" -> Error("Username already taken")
      _ -> Error("Duplicate value: " <> detail)
    }
  }
  Error(e) -> Error(error.format(e))
}
```

### ForeignKeyViolation

Foreign key constraint violated.

```gleam
Error(error.ForeignKeyViolation(constraint: String, detail: String))
```

**When it occurs:**
- Inserting with non-existent foreign key
- Deleting record referenced by others

**How to handle:**

```gleam
case result {
  Error(error.ForeignKeyViolation(constraint, detail)) -> {
    case constraint {
      "orders_customer_id_fkey" -> Error("Customer does not exist")
      "order_items_order_id_fkey" -> Error("Order does not exist")
      _ -> Error("Foreign key violation: " <> detail)
    }
  }
  _ -> ...
}
```

### CheckViolation

Check constraint violated.

```gleam
Error(error.CheckViolation(constraint: String, detail: String))
```

**When it occurs:**
- Value doesn't satisfy CHECK constraint

**How to handle:**

```gleam
case result {
  Error(error.CheckViolation(constraint, detail)) -> {
    case constraint {
      "positive_price" -> Error("Price must be positive")
      "valid_age" -> Error("Age must be between 0 and 150")
      _ -> Error("Validation failed: " <> detail)
    }
  }
  _ -> ...
}
```

### NotNullViolation

NOT NULL constraint violated.

```gleam
Error(error.NotNullViolation(column: String))
```

**When it occurs:**
- Inserting NULL into NOT NULL column
- Updating to NULL

**How to handle:**

```gleam
case result {
  Error(error.NotNullViolation(column)) -> {
    Error("Missing required field: " <> column)
  }
  _ -> ...
}
```

### ConstraintViolation

Generic constraint violation.

```gleam
Error(error.ConstraintViolation(constraint: String, detail: String))
```

**When it occurs:**
- Any constraint not covered by specific types

## Data Errors

### StaleData

Optimistic locking conflict.

```gleam
Error(error.StaleData(expected_version: String, actual_version: String))
```

**When it occurs:**
- Concurrent modification detected
- Version mismatch in optimistic locking

**How to handle:**

```gleam
case result {
  Error(error.StaleData(expected, actual)) -> {
    io.println("Conflict: expected version " <> expected <> ", found " <> actual)
    // Reload data and retry, or ask user to resolve
    Error("Data was modified by another user")
  }
  _ -> ...
}
```

### DataIntegrityError

Data integrity error.

```gleam
Error(error.DataIntegrityError(message: String))
```

**When it occurs:**
- Invalid data state
- Inconsistent data detected

## Capability Errors

### NotSupported

Adapter doesn't support this operation.

```gleam
Error(error.NotSupported(operation: String))
```

**When it occurs:**
- Using feature not supported by adapter
- PostgreSQL-specific feature with memory adapter

**How to handle:**

```gleam
case result {
  Error(error.NotSupported(operation)) -> {
    io.println_error("Operation not supported: " <> operation)
    // Fall back to alternative approach
  }
  _ -> ...
}
```

## Adapter-Specific Errors

### AdapterSpecific

Errors specific to an adapter.

```gleam
Error(error.AdapterSpecific(code: String, message: String))
```

**When it occurs:**
- Backend-specific error
- Errors not mapped to standard types

**How to handle:**

```gleam
case result {
  Error(error.AdapterSpecific(code, message)) -> {
    io.println_error("Adapter error [" <> code <> "]: " <> message)
  }
  _ -> ...
}
```

## Error Formatting

### `error.format(e: AdapterError) -> String`

Format error for display.

```gleam
case result {
  Error(e) -> {
    let message = error.format(e)
    io.println_error(message)
  }
  _ -> ...
}
```

### `error.is_recoverable(e: AdapterError) -> Bool`

Check if error is recoverable (retry may help).

```gleam
case result {
  Error(e) if error.is_recoverable(e) -> {
    // Retry with backoff
    retry_operation()
  }
  Error(e) -> {
    // Don't retry
    fail_permanently(e)
  }
  Ok(v) -> Ok(v)
}
```

### `error.to_http_status(e: AdapterError) -> Int`

Map error to HTTP status code (useful for APIs).

```gleam
case result {
  Error(e) -> {
    let status = error.to_http_status(e)
    let body = error.format(e)
    http.response(status, body)
  }
  Ok(data) -> http.response(200, encode(data))
}
```

## Best Practices

### 1. Handle Specific Errors First

```gleam
case result {
  // Handle specific cases
  Error(error.NotFound) -> handle_not_found()
  Error(error.UniqueViolation(_, _)) -> handle_duplicate()
  Error(error.ForeignKeyViolation(_, _)) -> handle_invalid_reference()
  // Then catch-all
  Error(e) -> handle_unexpected(e)
  Ok(v) -> handle_success(v)
}
```

### 2. Log Errors with Context

```gleam
case repo.insert(adapter, insert) {
  Error(e) -> {
    io.println_error("[UserService.create] Failed to create user: " <> error.format(e))
    Error(e)
  }
  Ok(v) -> Ok(v)
}
```

### 3. Return User-Friendly Messages

```gleam
fn user_friendly_error(e: error.AdapterError) -> String {
  case e {
    error.UniqueViolation("users_email_key", _) -> "This email is already registered"
    error.NotNullViolation("name") -> "Name is required"
    error.ForeignKeyViolation(_, _) -> "Invalid reference"
    _ -> "An unexpected error occurred"
  }
}
```
