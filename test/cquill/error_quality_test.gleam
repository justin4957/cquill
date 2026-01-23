// Error Message Quality Validation Tests
//
// This test suite validates that all error messages meet quality criteria:
// 1. Clear: Error message explains what went wrong
// 2. Actionable: User knows what to do to fix it
// 3. Contextual: Includes relevant context (table, field, value)
// 4. No jargon: Avoids database-specific terminology where possible
// 5. No sensitive data: Doesn't leak passwords, connection strings, etc.

import cquill/error.{
  type AdapterError, type SavepointError, type TransactionError, AdapterSpecific,
  AdapterTransactionError, BeginFailed, CheckViolation, CommitFailed,
  ConnectionFailed, ConnectionLost, ConnectionTimeout, ConstraintViolation,
  DataIntegrityError, DecodeFailed, ForeignKeyViolation, NestedTransactionError,
  NotFound, NotNullViolation, NotSupported, PoolExhausted, QueryFailed,
  RolledBack, SavepointAdapterError, SavepointCreationFailed,
  SavepointNoTransaction, SavepointNotFound, SavepointReleaseFailed,
  SavepointUserError, SerializationFailure, StaleData, Timeout, TooManyRows,
  TransactionConnectionLost, TransactionRollback, TransactionTimeout,
  UniqueViolation, UserError,
}
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// ERROR MESSAGE QUALITY CRITERIA
// ============================================================================

/// Validates that an error message is clear and explains what went wrong.
/// A clear message should be understandable without database expertise.
fn is_clear_message(message: String) -> Bool {
  // Message should not be empty
  let is_not_empty = string.length(message) > 0
  // Message should not be just a single word (too vague)
  let has_context = string.length(message) > 10
  // Message should not contain only error codes
  let is_not_just_code =
    !{ string.starts_with(message, "[") && string.length(message) < 20 }

  is_not_empty && has_context && is_not_just_code
}

/// Validates that an error message includes actionable hints.
fn has_hint(message: String) -> Bool {
  string.contains(message, "Hint:")
}

/// Validates that an error message provides actionable information.
/// For recoverable errors, the message should hint at what can be done.
fn has_actionable_info(
  error: AdapterError,
  message: String,
  is_recoverable: Bool,
) -> Bool {
  case is_recoverable {
    // Recoverable errors should hint at retry or alternative action
    True ->
      case error {
        // Connection errors should indicate the issue
        ConnectionFailed(_) -> string.contains(message, "Connection")
        ConnectionTimeout -> string.contains(message, "timed out")
        PoolExhausted -> string.contains(message, "pool")
        ConnectionLost(_) -> string.contains(message, "lost")
        Timeout -> string.contains(message, "timed out")
        StaleData(_, _) -> string.contains(message, "version")
        _ -> True
      }
    // Non-recoverable errors need clear explanation
    False ->
      case error {
        NotFound -> string.contains(message, "not found")
        UniqueViolation(_, _) ->
          string.contains(message, "constraint")
          || string.contains(message, "violation")
        ForeignKeyViolation(_, _) -> string.contains(message, "key")
        CheckViolation(_, _) -> string.contains(message, "constraint")
        NotNullViolation(_) ->
          string.contains(message, "NULL")
          || string.contains(message, "null")
          || string.contains(message, "Null")
        QueryFailed(_, _) ->
          string.contains(message, "failed")
          || string.contains(message, "error")
          || string.contains(message, "Error")
        _ -> True
      }
  }
}

/// Validates that an error message includes relevant context.
/// Constraint violations should mention the constraint/column name.
fn has_context(error: AdapterError, message: String) -> Bool {
  case error {
    // Constraint errors should include constraint/column names
    UniqueViolation(constraint, _) ->
      constraint != "" && string.contains(message, constraint)
    ForeignKeyViolation(constraint, _) ->
      constraint != "" && string.contains(message, constraint)
    CheckViolation(constraint, _) ->
      constraint != "" && string.contains(message, constraint)
    NotNullViolation(column) -> column != "" && string.contains(message, column)
    ConstraintViolation(constraint, _) ->
      constraint != "" && string.contains(message, constraint)
    // Decode errors should include row/column info
    DecodeFailed(_, column, _, _) -> string.contains(message, column)
    // TooManyRows should include counts
    TooManyRows(_, _) ->
      string.contains(message, "expected") && string.contains(message, "got")
    // Stale data should include version info
    StaleData(_, _) ->
      string.contains(message, "version")
      || string.contains(message, "expected")
    _ -> True
  }
}

/// Validates that an error message does not contain sensitive data.
fn does_not_leak_sensitive_data(message: String) -> Bool {
  let lower_message = string.lowercase(message)
  // Should not contain passwords
  let no_password =
    !string.contains(lower_message, "password=")
    && !string.contains(lower_message, "pwd=")
  // Should not contain connection strings
  let no_connection_string =
    !string.contains(lower_message, "postgres://")
    && !string.contains(lower_message, "mysql://")
    && !string.contains(lower_message, "sqlite://")
    && !string.contains(lower_message, "host=")
  // Should not contain API keys
  let no_api_keys =
    !string.contains(lower_message, "api_key=")
    && !string.contains(lower_message, "apikey=")
    && !string.contains(lower_message, "secret=")

  no_password && no_connection_string && no_api_keys
}

// ============================================================================
// CONNECTION ERROR TESTS
// ============================================================================

pub fn error_connection_failed_is_clear_test() {
  let error = ConnectionFailed("Unable to reach database server")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_actionable_info(error, message, True) |> should.be_true
  does_not_leak_sensitive_data(message) |> should.be_true
  has_hint(message) |> should.be_true
}

pub fn error_connection_timeout_is_clear_test() {
  let error = ConnectionTimeout
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_actionable_info(error, message, True) |> should.be_true
  has_hint(message) |> should.be_true
}

pub fn error_pool_exhausted_is_clear_test() {
  let error = PoolExhausted
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_actionable_info(error, message, True) |> should.be_true
  has_hint(message) |> should.be_true
}

pub fn error_connection_lost_is_clear_test() {
  let error = ConnectionLost("Server terminated the connection")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_actionable_info(error, message, True) |> should.be_true
  does_not_leak_sensitive_data(message) |> should.be_true
  has_hint(message) |> should.be_true
}

pub fn error_connection_failed_does_not_leak_credentials_test() {
  // Even if someone accidentally passes credentials, the error should not
  // expose them in the formatted message (this tests the principle)
  let reason = "Failed to authenticate"
  let error = ConnectionFailed(reason)
  let message = error.format_error(error)

  does_not_leak_sensitive_data(message) |> should.be_true
}

pub fn error_connection_sanitizes_password_test() {
  // Connection messages with passwords should be redacted
  let error = ConnectionFailed("postgres://user:password@host/db")
  let message = error.format_error(error)

  // Should not contain the original connection string
  string.contains(message, "postgres://") |> should.be_false
  string.contains(message, "redacted") |> should.be_true
}

pub fn error_connection_sanitizes_secret_test() {
  // Connection messages with secrets should be redacted
  let error = ConnectionFailed("Authentication failed with secret=abc123")
  let message = error.format_error(error)

  string.contains(message, "secret=") |> should.be_false
  string.contains(message, "redacted") |> should.be_true
}

// ============================================================================
// QUERY ERROR TESTS
// ============================================================================

pub fn error_query_failed_is_clear_test() {
  let error = QueryFailed("Syntax error at position 42", Some("42601"))
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  // Should include error code for debugging
  string.contains(message, "42601") |> should.be_true
}

pub fn error_query_failed_without_code_is_clear_test() {
  let error = QueryFailed("Table 'users' does not exist", None)
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "users") |> should.be_true
}

pub fn error_decode_failed_has_context_test() {
  let error = DecodeFailed(5, "age", "Int", "String")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  // Should include all relevant context
  string.contains(message, "5") |> should.be_true
  string.contains(message, "age") |> should.be_true
  string.contains(message, "Int") |> should.be_true
  string.contains(message, "String") |> should.be_true
}

pub fn error_timeout_is_clear_test() {
  let error = Timeout
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_actionable_info(error, message, True) |> should.be_true
}

// ============================================================================
// CONSTRAINT VIOLATION TESTS
// ============================================================================

pub fn error_unique_violation_has_context_test() {
  let error =
    UniqueViolation(
      "users_email_key",
      "Key (email)=(test@example.com) already exists",
    )
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  has_actionable_info(error, message, False) |> should.be_true
  // Should include constraint name
  string.contains(message, "users_email_key") |> should.be_true
}

pub fn error_foreign_key_violation_has_context_test() {
  let error =
    ForeignKeyViolation(
      "posts_user_id_fkey",
      "Key (user_id)=(999) is not present in table \"users\"",
    )
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  string.contains(message, "posts_user_id_fkey") |> should.be_true
}

pub fn error_check_violation_has_context_test() {
  let error =
    CheckViolation("users_age_check", "new row violates check constraint")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  string.contains(message, "users_age_check") |> should.be_true
}

pub fn error_not_null_violation_has_context_test() {
  let error = NotNullViolation("email")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  has_actionable_info(error, message, False) |> should.be_true
  // Should include column name
  string.contains(message, "email") |> should.be_true
}

pub fn error_constraint_violation_has_context_test() {
  let error =
    ConstraintViolation("custom_constraint", "Constraint check failed")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  string.contains(message, "custom_constraint") |> should.be_true
}

// ============================================================================
// NOT FOUND / ROW COUNT TESTS
// ============================================================================

pub fn error_not_found_is_clear_test() {
  let error = NotFound
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_actionable_info(error, message, False) |> should.be_true
}

pub fn error_too_many_rows_has_context_test() {
  let error = TooManyRows(1, 5)
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  // Should include both counts
  string.contains(message, "1") |> should.be_true
  string.contains(message, "5") |> should.be_true
}

// ============================================================================
// DATA ERROR TESTS
// ============================================================================

pub fn error_stale_data_has_context_test() {
  let error = StaleData("3", "5")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
  // Should include version info
  string.contains(message, "3") |> should.be_true
  string.contains(message, "5") |> should.be_true
}

pub fn error_data_integrity_error_is_clear_test() {
  let error = DataIntegrityError("Orphaned record detected")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "Orphaned") |> should.be_true
}

// ============================================================================
// CAPABILITY ERROR TESTS
// ============================================================================

pub fn error_not_supported_is_clear_test() {
  let error = NotSupported("UPSERT")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "UPSERT") |> should.be_true
}

pub fn error_adapter_specific_is_clear_test() {
  let error = AdapterSpecific("CUSTOM_001", "Adapter-specific failure")
  let message = error.format_error(error)

  is_clear_message(message) |> should.be_true
  // Should preserve code for debugging
  string.contains(message, "CUSTOM_001") |> should.be_true
}

// ============================================================================
// TRANSACTION ERROR TESTS
// ============================================================================

pub fn error_transaction_user_error_is_clear_test() {
  let error: TransactionError(String) = UserError("Invalid operation")
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
}

pub fn error_transaction_adapter_error_is_clear_test() {
  let error: TransactionError(String) =
    AdapterTransactionError(ConnectionLost("Server disconnected"))
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  // Should include underlying error details
  string.contains(message, "Connection") |> should.be_true
}

pub fn error_begin_failed_is_clear_test() {
  let error: TransactionError(String) = BeginFailed("Unable to acquire lock")
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "begin") |> should.be_true
}

pub fn error_commit_failed_is_clear_test() {
  let error: TransactionError(String) =
    CommitFailed("Constraint violation during commit")
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "commit") |> should.be_true
}

pub fn error_rolled_back_is_clear_test() {
  let error: TransactionError(String) = RolledBack
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "rolled back") |> should.be_true
}

pub fn error_transaction_rollback_with_reason_test() {
  let error: TransactionError(String) = TransactionRollback("Validation failed")
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "Validation") |> should.be_true
}

pub fn error_transaction_connection_lost_is_clear_test() {
  let error: TransactionError(String) = TransactionConnectionLost
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  let has_connection =
    string.contains(message, "Connection")
    || string.contains(message, "connection")
  has_connection |> should.be_true
}

pub fn error_nested_transaction_is_clear_test() {
  let error: TransactionError(String) = NestedTransactionError
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "Nested") |> should.be_true
}

pub fn error_transaction_timeout_is_clear_test() {
  let error: TransactionError(String) = TransactionTimeout
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "timed out") |> should.be_true
}

pub fn error_serialization_failure_is_clear_and_actionable_test() {
  let error: TransactionError(String) = SerializationFailure
  let message = error.format_transaction_error(error)

  is_clear_message(message) |> should.be_true
  // This error is recoverable - should mention retry
  let mentions_retry =
    string.contains(message, "retry") || string.contains(message, "retried")
  mentions_retry |> should.be_true
}

// ============================================================================
// SAVEPOINT ERROR TESTS
// ============================================================================

pub fn error_savepoint_not_found_has_context_test() {
  let error: SavepointError(String) = SavepointNotFound("my_savepoint")
  let message = error.format_savepoint_error(error)

  is_clear_message(message) |> should.be_true
  // Should include savepoint name
  string.contains(message, "my_savepoint") |> should.be_true
}

pub fn error_savepoint_adapter_error_is_clear_test() {
  let error: SavepointError(String) =
    SavepointAdapterError(ConnectionLost("Network error"))
  let message = error.format_savepoint_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "Connection") |> should.be_true
}

pub fn error_savepoint_user_error_is_clear_test() {
  let error: SavepointError(String) = SavepointUserError("Operation failed")
  let message = error.format_savepoint_error(error)

  is_clear_message(message) |> should.be_true
}

pub fn error_savepoint_creation_failed_is_clear_test() {
  let error: SavepointError(String) = SavepointCreationFailed("Out of memory")
  let message = error.format_savepoint_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "create") |> should.be_true
}

pub fn error_savepoint_release_failed_is_clear_test() {
  let error: SavepointError(String) =
    SavepointReleaseFailed("Savepoint already released")
  let message = error.format_savepoint_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "release") |> should.be_true
}

pub fn error_savepoint_no_transaction_is_clear_test() {
  let error: SavepointError(String) = SavepointNoTransaction
  let message = error.format_savepoint_error(error)

  is_clear_message(message) |> should.be_true
  string.contains(message, "transaction") |> should.be_true
}

// ============================================================================
// DATABASE ERROR MAPPING TESTS
// ============================================================================

pub fn postgres_unique_violation_mapping_test() {
  let error =
    error.from_postgres_error(
      "23505",
      "duplicate key value violates unique constraint",
      "Key (email)=(test@example.com) already exists",
    )

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
}

pub fn postgres_not_null_violation_mapping_test() {
  let error =
    error.from_postgres_error(
      "23502",
      "null value in column \"email\" violates not-null constraint",
      "",
    )

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  // Should extract column name from message
  string.contains(message, "email") |> should.be_true
}

pub fn postgres_connection_failure_mapping_test() {
  let error =
    error.from_postgres_error("08006", "could not connect to server", "")

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  does_not_leak_sensitive_data(message) |> should.be_true
}

pub fn postgres_undefined_table_mapping_test() {
  let error =
    error.from_postgres_error(
      "42P01",
      "relation \"nonexistent_table\" does not exist",
      "",
    )

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  let has_table =
    string.contains(message, "table") || string.contains(message, "Table")
  has_table |> should.be_true
}

pub fn postgres_undefined_column_mapping_test() {
  let error =
    error.from_postgres_error(
      "42703",
      "column \"nonexistent_column\" does not exist",
      "",
    )

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  let has_column =
    string.contains(message, "column") || string.contains(message, "Column")
  has_column |> should.be_true
}

pub fn mysql_unique_violation_mapping_test() {
  let error =
    error.from_mysql_error(
      1062,
      "Duplicate entry 'test@example.com' for key 'users.email'",
    )

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
}

pub fn mysql_connection_refused_mapping_test() {
  let error = error.from_mysql_error(2002, "Connection refused")

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  does_not_leak_sensitive_data(message) |> should.be_true
}

pub fn sqlite_unique_violation_mapping_test() {
  let error =
    error.from_sqlite_error(19, "UNIQUE constraint failed: users.email")

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
  has_context(error, message) |> should.be_true
}

pub fn sqlite_database_locked_mapping_test() {
  let error = error.from_sqlite_error(5, "database is locked")

  let message = error.format_error(error)
  is_clear_message(message) |> should.be_true
}

// ============================================================================
// ERROR CLASSIFICATION TESTS
// ============================================================================

pub fn classification_is_not_found_test() {
  error.is_not_found(NotFound) |> should.be_true
  error.is_not_found(Timeout) |> should.be_false
}

pub fn classification_is_constraint_violation_test() {
  error.is_constraint_violation(UniqueViolation("test", "detail"))
  |> should.be_true
  error.is_constraint_violation(ForeignKeyViolation("test", "detail"))
  |> should.be_true
  error.is_constraint_violation(CheckViolation("test", "detail"))
  |> should.be_true
  error.is_constraint_violation(NotNullViolation("col")) |> should.be_true
  error.is_constraint_violation(ConstraintViolation("test", "detail"))
  |> should.be_true
  error.is_constraint_violation(NotFound) |> should.be_false
}

pub fn classification_is_connection_error_test() {
  error.is_connection_error(ConnectionFailed("reason")) |> should.be_true
  error.is_connection_error(ConnectionTimeout) |> should.be_true
  error.is_connection_error(PoolExhausted) |> should.be_true
  error.is_connection_error(ConnectionLost("reason")) |> should.be_true
  error.is_connection_error(NotFound) |> should.be_false
}

pub fn classification_is_recoverable_test() {
  error.is_recoverable(ConnectionFailed("reason")) |> should.be_true
  error.is_recoverable(ConnectionTimeout) |> should.be_true
  error.is_recoverable(PoolExhausted) |> should.be_true
  error.is_recoverable(Timeout) |> should.be_true
  error.is_recoverable(StaleData("1", "2")) |> should.be_true
  // Non-recoverable
  error.is_recoverable(NotFound) |> should.be_false
  error.is_recoverable(UniqueViolation("test", "detail")) |> should.be_false
}

pub fn classification_is_query_error_test() {
  error.is_query_error(QueryFailed("msg", None)) |> should.be_true
  error.is_query_error(DecodeFailed(1, "col", "Int", "String"))
  |> should.be_true
  error.is_query_error(Timeout) |> should.be_true
  error.is_query_error(NotFound) |> should.be_false
}

// ============================================================================
// CONVENIENCE CONSTRUCTOR TESTS
// ============================================================================

pub fn constructor_not_found_test() {
  let error = error.not_found()
  error.is_not_found(error) |> should.be_true
}

pub fn constructor_query_failed_test() {
  let error = error.query_failed("Test message")
  case error {
    QueryFailed(msg, None) -> msg |> should.equal("Test message")
    _ -> should.fail()
  }
}

pub fn constructor_query_failed_with_code_test() {
  let error = error.query_failed_with_code("Test message", "42P01")
  case error {
    QueryFailed(msg, Some(code)) -> {
      msg |> should.equal("Test message")
      code |> should.equal("42P01")
    }
    _ -> should.fail()
  }
}

pub fn constructor_connection_failed_test() {
  let error = error.connection_failed("Server unreachable")
  case error {
    ConnectionFailed(reason) -> reason |> should.equal("Server unreachable")
    _ -> should.fail()
  }
}

pub fn constructor_timeout_test() {
  let error = error.timeout()
  case error {
    Timeout -> Nil
    _ -> should.fail()
  }
}

pub fn constructor_unique_violation_test() {
  let error = error.unique_violation("constraint_name", "detail info")
  case error {
    UniqueViolation(constraint, detail) -> {
      constraint |> should.equal("constraint_name")
      detail |> should.equal("detail info")
    }
    _ -> should.fail()
  }
}

pub fn constructor_not_supported_test() {
  let error = error.not_supported("OPERATION")
  case error {
    NotSupported(op) -> op |> should.equal("OPERATION")
    _ -> should.fail()
  }
}

pub fn constructor_adapter_specific_test() {
  let error = error.adapter_specific("CODE", "message")
  case error {
    AdapterSpecific(code, message) -> {
      code |> should.equal("CODE")
      message |> should.equal("message")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// COMPACT FORMAT TESTS
// ============================================================================

pub fn compact_format_no_hints_test() {
  let error = NotFound
  let message = error.format_error_compact(error)

  // Compact format should not include hints
  string.contains(message, "Hint:") |> should.be_false
  // But should still be clear
  is_clear_message(message) |> should.be_true
}

pub fn compact_format_connection_error_test() {
  let error = ConnectionFailed("Server unreachable")
  let compact = error.format_error_compact(error)
  let detailed = error.format_error(error)

  // Compact should be shorter
  { string.length(compact) < string.length(detailed) } |> should.be_true
  // Compact should not have hints
  string.contains(compact, "Hint:") |> should.be_false
}

pub fn compact_format_unique_violation_test() {
  let error = UniqueViolation("users_email_key", "duplicate value")
  let compact = error.format_error_compact(error)

  // Should still have context
  string.contains(compact, "users_email_key") |> should.be_true
  // But no hints
  string.contains(compact, "Hint:") |> should.be_false
}

pub fn compact_transaction_format_no_hints_test() {
  let error: TransactionError(String) = SerializationFailure
  let compact = error.format_transaction_error_compact(error)

  string.contains(compact, "Hint:") |> should.be_false
  is_clear_message(compact) |> should.be_true
}

pub fn compact_savepoint_format_no_hints_test() {
  let error: SavepointError(String) = SavepointNoTransaction
  let compact = error.format_savepoint_error_compact(error)

  string.contains(compact, "Hint:") |> should.be_false
  is_clear_message(compact) |> should.be_true
}

// ============================================================================
// HINT PRESENCE TESTS
// ============================================================================

pub fn error_not_found_has_hint_test() {
  let message = error.format_error(NotFound)
  has_hint(message) |> should.be_true
}

pub fn error_too_many_rows_has_hint_test() {
  let message = error.format_error(TooManyRows(1, 5))
  has_hint(message) |> should.be_true
}

pub fn error_timeout_has_hint_test() {
  let message = error.format_error(Timeout)
  has_hint(message) |> should.be_true
}

pub fn error_unique_violation_has_hint_test() {
  let message =
    error.format_error(UniqueViolation("test_key", "duplicate value"))
  has_hint(message) |> should.be_true
}

pub fn error_foreign_key_violation_has_hint_test() {
  let message =
    error.format_error(ForeignKeyViolation("fk_test", "reference missing"))
  has_hint(message) |> should.be_true
}

pub fn error_check_violation_has_hint_test() {
  let message = error.format_error(CheckViolation("check_test", "failed"))
  has_hint(message) |> should.be_true
}

pub fn error_not_null_violation_has_hint_test() {
  let message = error.format_error(NotNullViolation("email"))
  has_hint(message) |> should.be_true
}

pub fn error_stale_data_has_hint_test() {
  let message = error.format_error(StaleData("1", "2"))
  has_hint(message) |> should.be_true
}

pub fn error_decode_failed_has_hint_test() {
  let message = error.format_error(DecodeFailed(1, "age", "Int", "String"))
  has_hint(message) |> should.be_true
}

pub fn error_not_supported_has_hint_test() {
  let message = error.format_error(NotSupported("UPSERT"))
  has_hint(message) |> should.be_true
}

pub fn error_data_integrity_has_hint_test() {
  let message = error.format_error(DataIntegrityError("orphaned record"))
  has_hint(message) |> should.be_true
}

// ============================================================================
// TRANSACTION HINT TESTS
// ============================================================================

pub fn transaction_user_error_has_hint_test() {
  let error: TransactionError(String) = UserError("test")
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_begin_failed_has_hint_test() {
  let error: TransactionError(String) = BeginFailed("reason")
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_commit_failed_has_hint_test() {
  let error: TransactionError(String) = CommitFailed("reason")
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_rolled_back_has_hint_test() {
  let error: TransactionError(String) = RolledBack
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_connection_lost_has_hint_test() {
  let error: TransactionError(String) = TransactionConnectionLost
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_nested_error_has_hint_test() {
  let error: TransactionError(String) = NestedTransactionError
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_timeout_has_hint_test() {
  let error: TransactionError(String) = TransactionTimeout
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

pub fn transaction_serialization_has_hint_test() {
  let error: TransactionError(String) = SerializationFailure
  let message = error.format_transaction_error(error)
  has_hint(message) |> should.be_true
}

// ============================================================================
// SAVEPOINT HINT TESTS
// ============================================================================

pub fn savepoint_not_found_has_hint_test() {
  let error: SavepointError(String) = SavepointNotFound("sp1")
  let message = error.format_savepoint_error(error)
  has_hint(message) |> should.be_true
}

pub fn savepoint_user_error_has_hint_test() {
  let error: SavepointError(String) = SavepointUserError("test")
  let message = error.format_savepoint_error(error)
  has_hint(message) |> should.be_true
}

pub fn savepoint_creation_failed_has_hint_test() {
  let error: SavepointError(String) = SavepointCreationFailed("reason")
  let message = error.format_savepoint_error(error)
  has_hint(message) |> should.be_true
}

pub fn savepoint_release_failed_has_hint_test() {
  let error: SavepointError(String) = SavepointReleaseFailed("reason")
  let message = error.format_savepoint_error(error)
  has_hint(message) |> should.be_true
}

pub fn savepoint_no_transaction_has_hint_test() {
  let error: SavepointError(String) = SavepointNoTransaction
  let message = error.format_savepoint_error(error)
  has_hint(message) |> should.be_true
}
