// cquill Unified Error Types
//
// This module defines the error types used throughout cquill.
// All adapter implementations should map their internal errors to these types.
//
// Design principles:
// 1. Specific over generic - use the most specific error variant
// 2. Include context - error messages should include relevant details
// 3. Preserve codes - adapter-specific error codes are preserved
// 4. Recoverable flag - errors indicate if retry might help

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// ADAPTER ERROR TYPE
// ============================================================================

/// Unified error type for all adapter operations.
/// Adapters should map their internal errors to these types.
pub type AdapterError {

  // -------------------------------------------------------------------------
  // Not Found Errors
  // -------------------------------------------------------------------------
  /// Record not found when one was expected
  NotFound

  /// Too many rows returned when expecting one
  TooManyRows(expected: Int, got: Int)

  // -------------------------------------------------------------------------
  // Connection Errors
  // -------------------------------------------------------------------------
  /// Failed to establish database connection
  ConnectionFailed(reason: String)

  /// Connection attempt timed out
  ConnectionTimeout

  /// Connection pool exhausted (all connections in use)
  PoolExhausted

  /// Connection lost during operation
  ConnectionLost(reason: String)

  // -------------------------------------------------------------------------
  // Query Errors
  // -------------------------------------------------------------------------
  /// Query execution failed (syntax error, invalid query, etc.)
  QueryFailed(message: String, code: Option(String))

  /// Failed to decode a result row
  DecodeFailed(row: Int, column: String, expected: String, got: String)

  /// Operation timed out
  Timeout

  // -------------------------------------------------------------------------
  // Constraint Violations
  // -------------------------------------------------------------------------
  /// Unique constraint violated
  UniqueViolation(constraint: String, detail: String)

  /// Foreign key constraint violated
  ForeignKeyViolation(constraint: String, detail: String)

  /// Check constraint violated
  CheckViolation(constraint: String, detail: String)

  /// NOT NULL constraint violated
  NotNullViolation(column: String)

  /// Generic constraint violation (when type cannot be determined)
  ConstraintViolation(constraint: String, detail: String)

  // -------------------------------------------------------------------------
  // Data Errors
  // -------------------------------------------------------------------------
  /// Optimistic locking conflict - record was modified by another process
  StaleData(expected_version: String, actual_version: String)

  /// Data integrity error (invalid data state)
  DataIntegrityError(message: String)

  // -------------------------------------------------------------------------
  // Capability Errors
  // -------------------------------------------------------------------------
  /// Adapter does not support this operation
  NotSupported(operation: String)

  // -------------------------------------------------------------------------
  // Adapter-Specific Errors
  // -------------------------------------------------------------------------
  /// Generic error for adapter-specific failures
  AdapterSpecific(code: String, message: String)
}

// ============================================================================
// TRANSACTION ERROR TYPE
// ============================================================================

/// Transaction-specific error wrapper
pub type TransactionError(e) {
  /// The user-provided function returned an error
  UserError(e)

  /// Error from the adapter/database during transaction
  AdapterTransactionError(AdapterError)

  /// Transaction could not be started
  BeginFailed(reason: String)

  /// Transaction commit failed (may have been rolled back)
  CommitFailed(reason: String)

  /// Explicit rollback was requested
  RolledBack

  /// Transaction was explicitly rolled back with a reason
  TransactionRollback(reason: String)

  /// Connection was lost during transaction
  TransactionConnectionLost

  /// Nested transaction attempted (not supported)
  NestedTransactionError

  /// Transaction timed out
  TransactionTimeout

  /// Serialization failure (retry may succeed) - for serializable isolation
  SerializationFailure
}

// ============================================================================
// SAVEPOINT ERROR TYPE
// ============================================================================

/// Savepoint-specific error wrapper for partial rollback operations
pub type SavepointError(e) {
  /// Savepoint with the given name was not found
  SavepointNotFound(name: String)

  /// Error from the adapter/database during savepoint operation
  SavepointAdapterError(AdapterError)

  /// The user-provided function returned an error
  SavepointUserError(e)

  /// Savepoint creation failed
  SavepointCreationFailed(reason: String)

  /// Savepoint release failed
  SavepointReleaseFailed(reason: String)

  /// Not in a transaction (savepoints require an active transaction)
  SavepointNoTransaction
}

// ============================================================================
// ERROR CLASSIFICATION
// ============================================================================

/// Check if an error indicates the record was not found
pub fn is_not_found(error: AdapterError) -> Bool {
  case error {
    NotFound -> True
    _ -> False
  }
}

/// Check if an error is any kind of constraint violation
pub fn is_constraint_violation(error: AdapterError) -> Bool {
  case error {
    UniqueViolation(..) -> True
    ForeignKeyViolation(..) -> True
    CheckViolation(..) -> True
    NotNullViolation(..) -> True
    ConstraintViolation(..) -> True
    _ -> False
  }
}

/// Check if an error is a unique constraint violation
pub fn is_unique_violation(error: AdapterError) -> Bool {
  case error {
    UniqueViolation(..) -> True
    _ -> False
  }
}

/// Check if an error is a foreign key violation
pub fn is_foreign_key_violation(error: AdapterError) -> Bool {
  case error {
    ForeignKeyViolation(..) -> True
    _ -> False
  }
}

/// Check if an error is a connection-related error
pub fn is_connection_error(error: AdapterError) -> Bool {
  case error {
    ConnectionFailed(_) -> True
    ConnectionTimeout -> True
    PoolExhausted -> True
    ConnectionLost(_) -> True
    _ -> False
  }
}

/// Check if an error is recoverable (can retry)
pub fn is_recoverable(error: AdapterError) -> Bool {
  case error {
    // Connection issues are often transient
    ConnectionFailed(_) -> True
    ConnectionTimeout -> True
    PoolExhausted -> True
    ConnectionLost(_) -> True
    // Timeouts can be retried
    Timeout -> True
    // Stale data can be retried with fresh data
    StaleData(..) -> True
    // Most other errors are not recoverable
    _ -> False
  }
}

/// Check if an error is a query/execution error
pub fn is_query_error(error: AdapterError) -> Bool {
  case error {
    QueryFailed(..) -> True
    DecodeFailed(..) -> True
    Timeout -> True
    _ -> False
  }
}

// ============================================================================
// ERROR FORMATTING
// ============================================================================

/// Format an error for display
pub fn format_error(error: AdapterError) -> String {
  case error {
    NotFound -> "Record not found"
    TooManyRows(expected, got) ->
      "Too many rows: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)

    ConnectionFailed(reason) -> "Connection failed: " <> reason
    ConnectionTimeout -> "Connection timed out"
    PoolExhausted -> "Connection pool exhausted"
    ConnectionLost(reason) -> "Connection lost: " <> reason

    QueryFailed(message, None) -> "Query failed: " <> message
    QueryFailed(message, Some(code)) ->
      "Query failed [" <> code <> "]: " <> message
    DecodeFailed(row, column, expected, got) ->
      "Decode failed at row "
      <> int.to_string(row)
      <> ", column "
      <> column
      <> ": expected "
      <> expected
      <> ", got "
      <> got
    Timeout -> "Operation timed out"

    UniqueViolation(constraint, detail) ->
      "Unique constraint violation: " <> constraint <> " - " <> detail
    ForeignKeyViolation(constraint, detail) ->
      "Foreign key violation: " <> constraint <> " - " <> detail
    CheckViolation(constraint, detail) ->
      "Check constraint violation: " <> constraint <> " - " <> detail
    NotNullViolation(column) -> "NOT NULL violation on column: " <> column
    ConstraintViolation(constraint, detail) ->
      "Constraint violation: " <> constraint <> " - " <> detail

    StaleData(expected, actual) ->
      "Stale data: expected version " <> expected <> ", found " <> actual
    DataIntegrityError(message) -> "Data integrity error: " <> message

    NotSupported(operation) -> "Operation not supported: " <> operation

    AdapterSpecific(code, message) ->
      "Adapter error [" <> code <> "]: " <> message
  }
}

/// Format a transaction error for display
pub fn format_transaction_error(error: TransactionError(e)) -> String {
  case error {
    UserError(_) -> "Transaction aborted: user error"
    AdapterTransactionError(adapter_err) ->
      "Transaction aborted: " <> format_error(adapter_err)
    BeginFailed(reason) -> "Failed to begin transaction: " <> reason
    CommitFailed(reason) -> "Failed to commit transaction: " <> reason
    RolledBack -> "Transaction was rolled back"
    TransactionRollback(reason) -> "Transaction rolled back: " <> reason
    TransactionConnectionLost -> "Connection lost during transaction"
    NestedTransactionError -> "Nested transactions are not supported"
    TransactionTimeout -> "Transaction timed out"
    SerializationFailure ->
      "Serialization failure: concurrent transaction conflict (retry may succeed)"
  }
}

/// Format a savepoint error for display
pub fn format_savepoint_error(error: SavepointError(e)) -> String {
  case error {
    SavepointNotFound(name) -> "Savepoint not found: " <> name
    SavepointAdapterError(adapter_err) ->
      "Savepoint operation failed: " <> format_error(adapter_err)
    SavepointUserError(_) -> "Savepoint aborted: user error"
    SavepointCreationFailed(reason) -> "Failed to create savepoint: " <> reason
    SavepointReleaseFailed(reason) -> "Failed to release savepoint: " <> reason
    SavepointNoTransaction -> "Cannot use savepoint outside of a transaction"
  }
}

// ============================================================================
// ERROR MAPPING (Postgres-specific)
// ============================================================================

/// Map a PostgreSQL error code to an AdapterError.
///
/// PostgreSQL error codes are 5-character strings where the first two
/// characters indicate the error class. See:
/// https://www.postgresql.org/docs/current/errcodes-appendix.html
///
/// Common codes:
/// - 23505: unique_violation
/// - 23503: foreign_key_violation
/// - 23514: check_violation
/// - 23502: not_null_violation
/// - 23000: integrity_constraint_violation
/// - 08000: connection_exception
/// - 08003: connection_does_not_exist
/// - 08006: connection_failure
/// - 57P01: admin_shutdown
/// - 42P01: undefined_table
/// - 42703: undefined_column
pub fn from_postgres_error(
  code: String,
  message: String,
  detail: String,
) -> AdapterError {
  case code {
    // Constraint violations (Class 23)
    "23505" -> UniqueViolation(extract_constraint_name(detail), detail)
    "23503" -> ForeignKeyViolation(extract_constraint_name(detail), detail)
    "23514" -> CheckViolation(extract_constraint_name(detail), detail)
    "23502" -> NotNullViolation(extract_column_name(message))
    "23000" -> ConstraintViolation(extract_constraint_name(detail), detail)

    // Connection exceptions (Class 08)
    "08000" -> ConnectionFailed(message)
    "08003" -> ConnectionLost("Connection does not exist")
    "08006" -> ConnectionFailed("Connection failure: " <> message)

    // Operator intervention (Class 57)
    "57P01" -> ConnectionLost("Server shutdown")
    "57P02" -> ConnectionLost("Crash shutdown")
    "57P03" -> ConnectionFailed("Cannot connect now")

    // Invalid catalog name (Class 3D)
    "3D000" -> QueryFailed(message, Some(code))

    // Invalid schema name (Class 3F)
    "3F000" -> QueryFailed(message, Some(code))

    // Syntax error or access rule violation (Class 42)
    "42P01" -> QueryFailed("Undefined table: " <> message, Some(code))
    "42703" -> QueryFailed("Undefined column: " <> message, Some(code))
    "42601" -> QueryFailed("Syntax error: " <> message, Some(code))
    "42501" -> QueryFailed("Insufficient privilege: " <> message, Some(code))

    // Query canceled (Class 57)
    "57014" -> Timeout

    // Insufficient resources (Class 53)
    "53300" -> PoolExhausted
    "53400" ->
      QueryFailed("Configuration limit exceeded: " <> message, Some(code))

    // Default: preserve as adapter-specific
    _ -> AdapterSpecific(code, message)
  }
}

/// Map a MySQL error code to an AdapterError.
///
/// Common MySQL error codes:
/// - 1062: Duplicate entry (unique violation)
/// - 1452: Foreign key constraint fails
/// - 1364: No default value (not null violation)
/// - 1048: Column cannot be null
/// - 2002: Connection refused
/// - 2003: Can't connect to server
/// - 2006: Server has gone away
/// - 2013: Lost connection during query
pub fn from_mysql_error(code: Int, message: String) -> AdapterError {
  case code {
    // Constraint violations
    1062 -> UniqueViolation(extract_constraint_name(message), message)
    1452 -> ForeignKeyViolation(extract_constraint_name(message), message)
    1364 -> NotNullViolation(extract_column_name(message))
    1048 -> NotNullViolation(extract_column_name(message))
    3819 -> CheckViolation(extract_constraint_name(message), message)

    // Connection errors
    2002 -> ConnectionFailed("Connection refused")
    2003 -> ConnectionFailed("Can't connect to server")
    2006 -> ConnectionLost("Server has gone away")
    2013 -> ConnectionLost("Lost connection during query")

    // Query errors
    1064 -> QueryFailed("Syntax error: " <> message, Some(int.to_string(code)))
    1146 ->
      QueryFailed("Table doesn't exist: " <> message, Some(int.to_string(code)))
    1054 ->
      QueryFailed("Unknown column: " <> message, Some(int.to_string(code)))

    // Default: preserve as adapter-specific
    _ -> AdapterSpecific(int.to_string(code), message)
  }
}

/// Map a SQLite error code to an AdapterError.
///
/// SQLite uses integer result codes. Extended result codes provide more detail.
/// See: https://www.sqlite.org/rescode.html
pub fn from_sqlite_error(code: Int, message: String) -> AdapterError {
  case code {
    // Constraint violations (SQLITE_CONSTRAINT = 19)
    19 -> classify_sqlite_constraint(message)

    // SQLITE_BUSY (5) - database is locked
    5 -> ConnectionFailed("Database is locked")

    // SQLITE_LOCKED (6) - table is locked
    6 -> ConnectionFailed("Table is locked")

    // SQLITE_NOTADB (26) - not a database
    26 -> ConnectionFailed("Not a database file")

    // SQLITE_ERROR (1) - SQL error or missing database
    1 -> QueryFailed(message, Some(int.to_string(code)))

    // Default
    _ -> AdapterSpecific(int.to_string(code), message)
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Classify a SQLite constraint violation based on the error message.
///
/// SQLite's SQLITE_CONSTRAINT error (code 19) requires parsing the error
/// message to determine the specific constraint type. This function uses
/// a declarative approach with a list of patterns for better readability.
fn classify_sqlite_constraint(message: String) -> AdapterError {
  let lower_message = string.lowercase(message)

  // Define constraint patterns and their corresponding error constructors
  // Order matters: more specific patterns should come first
  let constraint_patterns = [
    #("unique", fn(msg: String) {
      UniqueViolation(extract_constraint_name(msg), msg)
    }),
    #("foreign key", fn(msg: String) {
      ForeignKeyViolation(extract_constraint_name(msg), msg)
    }),
    #("not null", fn(msg: String) { NotNullViolation(extract_column_name(msg)) }),
    #("check", fn(msg: String) {
      CheckViolation(extract_constraint_name(msg), msg)
    }),
  ]

  constraint_patterns
  |> list.find(fn(pattern) { string.contains(lower_message, pattern.0) })
  |> result.map(fn(pattern) { pattern.1(message) })
  |> result.unwrap(ConstraintViolation("unknown", message))
}

/// Extract constraint name from error detail/message
fn extract_constraint_name(text: String) -> String {
  // Common patterns:
  // PostgreSQL: "Key (email)=(test@example.com) already exists."
  // PostgreSQL: violates foreign key constraint "posts_user_id_fkey"
  // MySQL: for key 'users.email'
  case string.split(text, "constraint \"") {
    [_, rest] ->
      case string.split(rest, "\"") {
        [name, ..] -> name
        _ -> "unknown"
      }
    _ ->
      case string.split(text, "for key '") {
        [_, rest] ->
          case string.split(rest, "'") {
            [name, ..] -> name
            _ -> "unknown"
          }
        _ -> "unknown"
      }
  }
}

/// Extract column name from error message
fn extract_column_name(text: String) -> String {
  // Common patterns:
  // PostgreSQL: null value in column "email" violates not-null constraint
  // MySQL: Field 'email' doesn't have a default value
  case string.split(text, "column \"") {
    [_, rest] ->
      case string.split(rest, "\"") {
        [name, ..] -> name
        _ -> "unknown"
      }
    _ ->
      case string.split(text, "Field '") {
        [_, rest] ->
          case string.split(rest, "'") {
            [name, ..] -> name
            _ -> "unknown"
          }
        _ -> "unknown"
      }
  }
}

// ============================================================================
// ERROR CONSTRUCTORS (Convenience)
// ============================================================================

/// Create a NotFound error
pub fn not_found() -> AdapterError {
  NotFound
}

/// Create a QueryFailed error
pub fn query_failed(message: String) -> AdapterError {
  QueryFailed(message, None)
}

/// Create a QueryFailed error with code
pub fn query_failed_with_code(message: String, code: String) -> AdapterError {
  QueryFailed(message, Some(code))
}

/// Create a ConnectionFailed error
pub fn connection_failed(reason: String) -> AdapterError {
  ConnectionFailed(reason)
}

/// Create a Timeout error
pub fn timeout() -> AdapterError {
  Timeout
}

/// Create a UniqueViolation error
pub fn unique_violation(constraint: String, detail: String) -> AdapterError {
  UniqueViolation(constraint, detail)
}

/// Create a NotSupported error
pub fn not_supported(operation: String) -> AdapterError {
  NotSupported(operation)
}

/// Create an AdapterSpecific error
pub fn adapter_specific(code: String, message: String) -> AdapterError {
  AdapterSpecific(code, message)
}
