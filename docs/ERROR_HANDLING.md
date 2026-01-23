# Error Handling Guide

This guide covers cquill's error handling system, including error types, formatting, and best practices for handling database errors in your application.

## Error Design Principles

cquill's error system is designed around these principles:

1. **Clear**: Error messages explain what went wrong
2. **Actionable**: Users know what to do to fix the issue
3. **Contextual**: Includes relevant details (table, field, value)
4. **No jargon**: Avoids database-specific terminology where possible
5. **No sensitive data**: Doesn't leak passwords, connection strings, etc.

## Error Types

### AdapterError

The primary error type for database operations:

```gleam
import cquill/error

// Not Found Errors
error.NotFound                    // Record not found
error.TooManyRows(expected, got)  // Too many rows returned

// Connection Errors
error.ConnectionFailed(reason)    // Failed to connect
error.ConnectionTimeout           // Connection timed out
error.PoolExhausted              // All connections in use
error.ConnectionLost(reason)      // Connection lost during operation

// Query Errors
error.QueryFailed(message, code)  // Query execution failed
error.DecodeFailed(row, col, expected, got)  // Result decoding failed
error.Timeout                     // Operation timed out

// Constraint Violations
error.UniqueViolation(constraint, detail)     // Duplicate value
error.ForeignKeyViolation(constraint, detail) // Reference missing
error.CheckViolation(constraint, detail)      // Check constraint failed
error.NotNullViolation(column)               // Required field missing
error.ConstraintViolation(constraint, detail) // Generic constraint

// Data Errors
error.StaleData(expected, actual)  // Optimistic locking conflict
error.DataIntegrityError(message)  // Data integrity issue

// Capability Errors
error.NotSupported(operation)      // Operation not supported
error.AdapterSpecific(code, msg)   // Adapter-specific error
```

### TransactionError

Wraps errors that occur during transactions:

```gleam
error.UserError(e)                // User callback returned error
error.AdapterTransactionError(err) // Adapter error during transaction
error.BeginFailed(reason)         // Could not start transaction
error.CommitFailed(reason)        // Could not commit
error.RolledBack                  // Transaction was rolled back
error.TransactionRollback(reason) // Explicit rollback with reason
error.TransactionConnectionLost   // Connection lost during transaction
error.NestedTransactionError      // Nested transactions not supported
error.TransactionTimeout          // Transaction timed out
error.SerializationFailure        // Concurrent transaction conflict
```

### SavepointError

Errors for savepoint (partial rollback) operations:

```gleam
error.SavepointNotFound(name)        // Savepoint doesn't exist
error.SavepointAdapterError(err)     // Adapter error
error.SavepointUserError(e)          // User callback error
error.SavepointCreationFailed(reason) // Could not create savepoint
error.SavepointReleaseFailed(reason)  // Could not release savepoint
error.SavepointNoTransaction         // Not in a transaction
```

## Formatting Errors

### Detailed Format (with hints)

Use `format_error` for user-facing error messages:

```gleam
import cquill/error

let err = error.UniqueViolation(
  "users_email_key",
  "Key (email)=(test@example.com) already exists"
)

error.format_error(err)
// Output:
// Unique constraint violation on users_email_key
//   Key (email)=(test@example.com) already exists
//
// Hint: Check if a record with this value exists before inserting,
//       or use an upsert operation for insert-or-update semantics.
```

### Compact Format (for logging)

Use `format_error_compact` for logging or when hints aren't needed:

```gleam
import cquill/error

let err = error.NotFound

error.format_error_compact(err)
// Output: "Record not found"
```

## Error Classification

Helper functions to classify errors:

```gleam
import cquill/error

// Check error type
error.is_not_found(err)           // True for NotFound
error.is_constraint_violation(err) // True for any constraint error
error.is_unique_violation(err)     // True for UniqueViolation
error.is_foreign_key_violation(err) // True for ForeignKeyViolation
error.is_connection_error(err)     // True for connection issues
error.is_query_error(err)          // True for query/decode errors
error.is_recoverable(err)          // True if retry might help
```

## Handling Specific Errors

### Unique Constraint Violations

```gleam
import cquill/error

case repo.insert(user_record) {
  Ok(user) -> Ok(user)
  Error(err) if error.is_unique_violation(err) -> {
    // Handle duplicate - maybe fetch existing record
    case repo.get_by(users, email: user.email) {
      Ok(existing) -> Ok(existing)
      Error(_) -> Error(err)
    }
  }
  Error(err) -> Error(err)
}
```

### Connection Errors with Retry

```gleam
import cquill/error

fn with_retry(operation, max_attempts) {
  do_retry(operation, max_attempts, 1)
}

fn do_retry(operation, max_attempts, attempt) {
  case operation() {
    Ok(result) -> Ok(result)
    Error(err) if error.is_recoverable(err) && attempt < max_attempts -> {
      // Wait and retry
      process.sleep(attempt * 100)
      do_retry(operation, max_attempts, attempt + 1)
    }
    Error(err) -> Error(err)
  }
}
```

### Transaction Serialization Failures

```gleam
import cquill/error

fn transfer_funds(from, to, amount) {
  case repo.transaction(fn(tx) {
    // Perform transfer...
  }) {
    Ok(result) -> Ok(result)
    Error(error.SerializationFailure) -> {
      // Safe to retry - another transaction modified the data
      transfer_funds(from, to, amount)
    }
    Error(err) -> Error(err)
  }
}
```

## Error Examples

### Good Error Messages

| Error | Message |
|-------|---------|
| Unique violation | `Unique constraint violation on users_email_key`<br>`Key (email)=(test@example.com) already exists`<br><br>`Hint: Check if a record with this value exists before inserting, or use an upsert operation for insert-or-update semantics.` |
| Not null | `NOT NULL violation on column 'email'`<br><br>`Hint: Provide a value for this required field, or update the schema to allow NULL.` |
| Connection | `Connection failed: Unable to reach database server`<br><br>`Hint: Check that the database server is running and accessible.` |
| Pool exhausted | `Connection pool exhausted: all connections are in use`<br><br>`Hint: Increase the pool size, reduce long-running queries, or check for connection leaks.` |
| Stale data | `Stale data detected: expected version 3, found version 5`<br><br>`Hint: The record was modified by another process. Reload the data and retry your operation.` |

### Avoiding Bad Patterns

**Bad:** Generic database error
```
Error: pgo error: unique_violation
```

**Good:** Contextual with actionable hint
```
Unique constraint violation on users_email_key
  Key (email)=(test@example.com) already exists

Hint: Check if a record with this value exists before inserting,
      or use an upsert operation for insert-or-update semantics.
```

## Database Error Mapping

cquill maps database-specific error codes to unified error types:

### PostgreSQL

| Code | Error Type |
|------|------------|
| 23505 | UniqueViolation |
| 23503 | ForeignKeyViolation |
| 23514 | CheckViolation |
| 23502 | NotNullViolation |
| 08000, 08003, 08006 | ConnectionFailed/Lost |
| 42P01 | QueryFailed (undefined table) |
| 42703 | QueryFailed (undefined column) |
| 57014 | Timeout |
| 53300 | PoolExhausted |

### MySQL

| Code | Error Type |
|------|------------|
| 1062 | UniqueViolation |
| 1452 | ForeignKeyViolation |
| 1048, 1364 | NotNullViolation |
| 3819 | CheckViolation |
| 2002, 2003 | ConnectionFailed |
| 2006, 2013 | ConnectionLost |

### SQLite

| Code | Error Type |
|------|------------|
| 19 | Constraint violation (parsed from message) |
| 5 | ConnectionFailed (database locked) |
| 6 | ConnectionFailed (table locked) |
| 1 | QueryFailed |

## Security Considerations

cquill automatically sanitizes error messages to prevent sensitive data leakage:

```gleam
// If connection string accidentally passed:
let err = error.ConnectionFailed("postgres://user:password@host/db")
error.format_error(err)
// Output: "Connection failed: [connection details redacted]"
```

Patterns that are automatically redacted:
- Connection strings (`postgres://`, `mysql://`, `sqlite://`)
- Passwords (`password=`, `pwd=`)
- Secrets (`secret=`, `api_key=`)

## Best Practices

1. **Use detailed format for user-facing errors** - The hints help users understand what to do
2. **Use compact format for logging** - Keeps logs concise and parseable
3. **Check error classification before handling** - Use helper functions like `is_recoverable`
4. **Retry recoverable errors** - Connection timeouts, pool exhaustion, and serialization failures can often be retried
5. **Never expose raw database errors** - Always use cquill's error formatting
6. **Log original errors for debugging** - Use compact format in logs while showing detailed format to users
