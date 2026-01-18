// cquill Rich Error Formatting
//
// This module provides rich, actionable error messages with:
// - Visual indicators of what went wrong
// - Context about where the error occurred
// - Explanations of why the error happened
// - Actionable hints to resolve the issue
//
// Error messages follow the principle of being helpful to developers
// by providing enough context to quickly diagnose and fix issues.

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
import cquill/query/ast.{
  type Value, BoolValue, FloatValue, IntValue, ListValue, NullValue, ParamValue,
  StringValue,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// ERROR CONTEXT TYPES
// ============================================================================

/// Context about where an error occurred, for enriched error messages
pub type ErrorContext {
  ErrorContext(
    /// The SQL query that failed (if available)
    query: Option(String),
    /// The query parameters (if available)
    params: Option(List(Value)),
    /// Source location in user code (if available)
    source_location: Option(SourceLocation),
    /// The table being operated on (if known)
    table: Option(String),
    /// The operation being performed
    operation: Option(String),
  )
}

/// Source location in user code where the error originated
pub type SourceLocation {
  SourceLocation(file: String, line: Int, function: String)
}

/// Connection configuration for connection error context
pub type ConnectionConfig {
  ConnectionConfig(host: String, port: Int, database: String, user: String)
}

// ============================================================================
// CONTEXT CONSTRUCTORS
// ============================================================================

/// Create an empty error context
pub fn empty_context() -> ErrorContext {
  ErrorContext(
    query: None,
    params: None,
    source_location: None,
    table: None,
    operation: None,
  )
}

/// Create a context with query information
pub fn with_query(context: ErrorContext, query: String) -> ErrorContext {
  ErrorContext(..context, query: Some(query))
}

/// Create a context with query parameters
pub fn with_params(context: ErrorContext, params: List(Value)) -> ErrorContext {
  ErrorContext(..context, params: Some(params))
}

/// Create a context with source location
pub fn with_source_location(
  context: ErrorContext,
  file: String,
  line: Int,
  function: String,
) -> ErrorContext {
  ErrorContext(
    ..context,
    source_location: Some(SourceLocation(file:, line:, function:)),
  )
}

/// Create a context with table name
pub fn with_table(context: ErrorContext, table: String) -> ErrorContext {
  ErrorContext(..context, table: Some(table))
}

/// Create a context with operation name
pub fn with_operation(context: ErrorContext, operation: String) -> ErrorContext {
  ErrorContext(..context, operation: Some(operation))
}

// ============================================================================
// VALUE FORMATTING
// ============================================================================

/// Format a Value for display in error messages
pub fn format_value(value: Value) -> String {
  case value {
    IntValue(i) -> int.to_string(i)
    FloatValue(f) -> format_float(f)
    StringValue(s) -> "\"" <> mask_sensitive_value(s) <> "\""
    BoolValue(True) -> "true"
    BoolValue(False) -> "false"
    NullValue -> "NULL"
    ParamValue(pos) -> "$" <> int.to_string(pos)
    ListValue(values) ->
      "[" <> string.join(list.map(values, format_value), ", ") <> "]"
  }
}

/// Format a float value for display
fn format_float(f: Float) -> String {
  // Gleam doesn't have a direct float to string, use inspect
  string.inspect(f)
}

/// Mask potentially sensitive values (emails, etc.)
fn mask_sensitive_value(value: String) -> String {
  let length = string.length(value)
  case length {
    l if l <= 3 -> value
    l if l <= 8 -> {
      let visible = string.slice(value, 0, 2)
      visible <> string.repeat("*", l - 2)
    }
    _ -> {
      let visible_start = string.slice(value, 0, 3)
      let visible_end = string.slice(value, length - 2, 2)
      visible_start <> "***" <> visible_end
    }
  }
}

// ============================================================================
// CONSTRAINT VIOLATION FORMATTERS
// ============================================================================

/// Format a unique constraint violation with rich context
pub fn format_unique_violation(
  constraint: String,
  table: String,
  column: String,
  value: Value,
) -> String {
  let value_str = format_value(value)
  let column_underline = string.repeat("^", string.length(column))

  string.join(
    [
      "Error: UniqueConstraintViolation",
      "",
      "  INSERT INTO "
        <> table
        <> " ("
        <> column
        <> ", ...) VALUES ("
        <> value_str
        <> ", ...)",
      "                " <> column_underline,
      "",
      "  Unique constraint \"" <> constraint <> "\" violated.",
      "",
      "  A record with " <> column <> " = " <> value_str <> " already exists.",
      "",
      "  Hint: Use `on_conflict_do_nothing()` to ignore duplicates,",
      "        or check existence first with `repo.exists(...)`.",
      "",
    ],
    "\n",
  )
}

/// Format a unique violation from an AdapterError
pub fn format_unique_violation_error(
  constraint: String,
  detail: String,
  context: ErrorContext,
) -> String {
  let table = option.unwrap(context.table, "table")
  let column = extract_column_from_unique_detail(detail)
  let value_str = extract_value_from_unique_detail(detail)

  string.join(
    [
      "Error: UniqueConstraintViolation",
      "",
      format_operation_line(context),
      "",
      "  Unique constraint \"" <> constraint <> "\" violated.",
      "",
      "  Detail: " <> detail,
      "",
      "  A record with "
        <> column
        <> " = "
        <> value_str
        <> " already exists in "
        <> table
        <> ".",
      "",
      "  Hint: Use `on_conflict_do_nothing()` to ignore duplicates,",
      "        or check existence first with `repo.exists(...)`.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a foreign key constraint violation with rich context
pub fn format_foreign_key_violation(
  constraint: String,
  table: String,
  column: String,
  references_table: String,
  references_column: String,
) -> String {
  string.join(
    [
      "Error: ForeignKeyViolation",
      "",
      "  Foreign key constraint \"" <> constraint <> "\" violated.",
      "",
      "  The value in "
        <> table
        <> "."
        <> column
        <> " does not exist in "
        <> references_table
        <> "."
        <> references_column
        <> ".",
      "",
      "  Hint: Ensure the referenced record exists before inserting,",
      "        or use ON DELETE SET NULL if the reference is optional.",
      "",
    ],
    "\n",
  )
}

/// Format a foreign key violation from an AdapterError
pub fn format_foreign_key_violation_error(
  constraint: String,
  detail: String,
  context: ErrorContext,
) -> String {
  let table = option.unwrap(context.table, "table")

  string.join(
    [
      "Error: ForeignKeyViolation",
      "",
      format_operation_line(context),
      "",
      "  Foreign key constraint \"" <> constraint <> "\" violated.",
      "",
      "  Detail: " <> detail,
      "",
      "  The referenced record in the parent table does not exist.",
      "",
      "  Hint: Ensure the referenced record exists before inserting into "
        <> table
        <> ",",
      "        or verify the foreign key value is correct.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a check constraint violation
pub fn format_check_violation(
  constraint: String,
  detail: String,
  context: ErrorContext,
) -> String {
  string.join(
    [
      "Error: CheckConstraintViolation",
      "",
      format_operation_line(context),
      "",
      "  Check constraint \"" <> constraint <> "\" violated.",
      "",
      "  Detail: " <> detail,
      "",
      "  The value did not satisfy the check constraint condition.",
      "",
      "  Hint: Review the constraint definition and ensure",
      "        the data meets all validation requirements.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a NOT NULL constraint violation
pub fn format_not_null_violation(
  column: String,
  context: ErrorContext,
) -> String {
  let table = option.unwrap(context.table, "table")

  string.join(
    [
      "Error: NotNullViolation",
      "",
      format_operation_line(context),
      "",
      "  Column \""
        <> column
        <> "\" in table \""
        <> table
        <> "\" cannot be NULL.",
      "",
      "  Hint: Provide a value for the \"" <> column <> "\" column,",
      "        or alter the column to allow NULL values.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

// ============================================================================
// QUERY ERROR FORMATTERS
// ============================================================================

/// Format a decode error with detailed context
pub fn format_decode_error(
  row: Int,
  column: String,
  expected: String,
  found: String,
) -> String {
  string.join(
    [
      "Error: DecodeError",
      "",
      "  Failed to decode row "
        <> int.to_string(row)
        <> ", column \""
        <> column
        <> "\".",
      "",
      "  Expected: " <> expected,
      "  Found:    " <> found,
      "",
      "  Hint: Check that your decoder matches the database column type.",
      "        The schema might be out of sync — verify column types match.",
      "",
    ],
    "\n",
  )
}

/// Format a query failed error with context
pub fn format_query_failed(
  message: String,
  code: Option(String),
  context: ErrorContext,
) -> String {
  let code_str = case code {
    Some(c) -> " [" <> c <> "]"
    None -> ""
  }

  string.join(
    [
      "Error: QueryFailed" <> code_str,
      "",
      format_query_context(context),
      "",
      "  " <> message,
      "",
      format_query_hint(message, code),
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a "not found" error
pub fn format_not_found(context: ErrorContext) -> String {
  let table = option.unwrap(context.table, "the table")

  string.join(
    [
      "Error: RecordNotFound",
      "",
      format_operation_line(context),
      "",
      "  No record was found in " <> table <> " matching the query.",
      "",
      "  Hint: Verify the query conditions are correct,",
      "        or use `repo.one_or_none(...)` if the record may not exist.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a "too many rows" error
pub fn format_too_many_rows(
  expected: Int,
  got: Int,
  context: ErrorContext,
) -> String {
  string.join(
    [
      "Error: TooManyRows",
      "",
      format_operation_line(context),
      "",
      "  Expected "
        <> int.to_string(expected)
        <> " row(s), but found "
        <> int.to_string(got)
        <> ".",
      "",
      "  Hint: Add a LIMIT clause or use more specific conditions,",
      "        or use `repo.all(...)` if multiple results are expected.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a timeout error
pub fn format_timeout(context: ErrorContext) -> String {
  string.join(
    [
      "Error: Timeout",
      "",
      format_query_context(context),
      "",
      "  The operation timed out waiting for a response.",
      "",
      "  Hint: The query may be too slow — consider adding indexes,",
      "        optimizing the query, or increasing the timeout.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

// ============================================================================
// CONNECTION ERROR FORMATTERS
// ============================================================================

/// Format a connection failed error with troubleshooting hints
pub fn format_connection_failed(
  reason: String,
  config: ConnectionConfig,
) -> String {
  let host_str = config.host <> ":" <> int.to_string(config.port)

  string.join(
    [
      "Error: ConnectionFailed",
      "",
      "  Could not connect to database at " <> host_str,
      "",
      "  Reason: " <> reason,
      "",
      "  Check:",
      "  - Is the database server running?",
      "  - Is the database \"" <> config.database <> "\" created?",
      "  - Are the credentials for user \"" <> config.user <> "\" correct?",
      "  - Is port "
        <> int.to_string(config.port)
        <> " accessible (firewall/network)?",
      "  - Is the connection string/host correct?",
      "",
    ],
    "\n",
  )
}

/// Format a connection failed error with minimal context
pub fn format_connection_failed_simple(reason: String) -> String {
  string.join(
    [
      "Error: ConnectionFailed",
      "",
      "  Could not establish database connection.",
      "",
      "  Reason: " <> reason,
      "",
      "  Check:",
      "  - Is the database server running?",
      "  - Is the database created?",
      "  - Are the credentials correct?",
      "  - Is the host and port accessible?",
      "",
    ],
    "\n",
  )
}

/// Format a connection timeout error
pub fn format_connection_timeout(config: Option(ConnectionConfig)) -> String {
  let host_info = case config {
    Some(c) -> " to " <> c.host <> ":" <> int.to_string(c.port)
    None -> ""
  }

  string.join(
    [
      "Error: ConnectionTimeout",
      "",
      "  Connection attempt" <> host_info <> " timed out.",
      "",
      "  Possible causes:",
      "  - Database server is not responding",
      "  - Network connectivity issues",
      "  - Firewall blocking the connection",
      "  - Server is overloaded",
      "",
      "  Hint: Check network connectivity and server status.",
      "",
    ],
    "\n",
  )
}

/// Format a pool exhausted error
pub fn format_pool_exhausted() -> String {
  string.join(
    [
      "Error: PoolExhausted",
      "",
      "  All database connections in the pool are in use.",
      "",
      "  Possible causes:",
      "  - Too many concurrent requests",
      "  - Long-running queries holding connections",
      "  - Connection leaks (connections not being returned)",
      "",
      "  Hint: Increase pool size, optimize slow queries,",
      "        or check for connection leaks in your application.",
      "",
    ],
    "\n",
  )
}

/// Format a connection lost error
pub fn format_connection_lost(reason: String) -> String {
  string.join(
    [
      "Error: ConnectionLost",
      "",
      "  The database connection was lost during the operation.",
      "",
      "  Reason: " <> reason,
      "",
      "  Possible causes:",
      "  - Database server was restarted",
      "  - Network interruption",
      "  - Server terminated the connection (idle timeout)",
      "",
      "  Hint: This error is often transient — retry the operation.",
      "        If persistent, check database server logs.",
      "",
    ],
    "\n",
  )
}

// ============================================================================
// TRANSACTION ERROR FORMATTERS
// ============================================================================

/// Format a transaction error with rich context
pub fn format_rich_transaction_error(
  error: TransactionError(e),
  context: ErrorContext,
) -> String {
  case error {
    UserError(_) ->
      string.join(
        [
          "Error: TransactionAborted",
          "",
          "  Transaction was aborted due to a user error.",
          "",
          "  The transaction function returned an error and changes",
          "  were rolled back automatically.",
          "",
          format_source_location(context.source_location),
        ],
        "\n",
      )

    AdapterTransactionError(adapter_err) ->
      string.join(
        [
          "Error: TransactionAdapterError",
          "",
          format_operation_line(context),
          "",
          "  " <> error.format_error(adapter_err),
          "",
          format_source_location(context.source_location),
        ],
        "\n",
      )

    BeginFailed(reason) ->
      string.join(
        [
          "Error: TransactionBeginFailed",
          "",
          "  Failed to begin transaction: " <> reason,
          "",
          "  Hint: Check database connectivity and permissions.",
          "",
        ],
        "\n",
      )

    CommitFailed(reason) ->
      string.join(
        [
          "Error: TransactionCommitFailed",
          "",
          "  Failed to commit transaction: " <> reason,
          "",
          "  Changes may have been rolled back.",
          "",
          "  Hint: The transaction may have been invalidated by a constraint",
          "        violation or other error during execution.",
          "",
        ],
        "\n",
      )

    RolledBack ->
      string.join(
        [
          "Error: TransactionRolledBack",
          "",
          "  Transaction was explicitly rolled back.",
          "",
        ],
        "\n",
      )

    TransactionRollback(reason) ->
      string.join(
        [
          "Error: TransactionRolledBack",
          "",
          "  Transaction was rolled back: " <> reason,
          "",
        ],
        "\n",
      )

    TransactionConnectionLost ->
      string.join(
        [
          "Error: TransactionConnectionLost",
          "",
          "  The database connection was lost during the transaction.",
          "",
          "  All changes in this transaction were rolled back.",
          "",
          "  Hint: This error is often transient — retry the entire transaction.",
          "",
        ],
        "\n",
      )

    NestedTransactionError ->
      string.join(
        [
          "Error: NestedTransactionError",
          "",
          "  Attempted to start a nested transaction.",
          "",
          "  Nested transactions are not supported by this adapter.",
          "",
          "  Hint: Use savepoints instead with `repo.savepoint(...)` for",
          "        partial rollback capabilities within a transaction.",
          "",
        ],
        "\n",
      )

    TransactionTimeout ->
      string.join(
        [
          "Error: TransactionTimeout",
          "",
          "  The transaction timed out.",
          "",
          "  Possible causes:",
          "  - Long-running queries within the transaction",
          "  - Deadlock with another transaction",
          "  - Lock contention",
          "",
          "  Hint: Optimize queries, reduce transaction scope,",
          "        or increase the timeout if appropriate.",
          "",
        ],
        "\n",
      )

    SerializationFailure ->
      string.join(
        [
          "Error: SerializationFailure",
          "",
          "  Transaction could not serialize access due to concurrent updates.",
          "",
          "  Another transaction modified data that this transaction was",
          "  reading or writing.",
          "",
          "  Hint: This error indicates a conflict — retry the transaction.",
          "        For frequently conflicting operations, consider using",
          "        explicit locking or adjusting the isolation level.",
          "",
        ],
        "\n",
      )
  }
}

// ============================================================================
// SAVEPOINT ERROR FORMATTERS
// ============================================================================

/// Format a savepoint error with rich context
pub fn format_rich_savepoint_error(
  error: SavepointError(e),
  context: ErrorContext,
) -> String {
  case error {
    SavepointNotFound(name) ->
      string.join(
        [
          "Error: SavepointNotFound",
          "",
          "  Savepoint \"" <> name <> "\" does not exist.",
          "",
          "  Hint: Ensure the savepoint was created with `repo.savepoint(...)`",
          "        and has not been released or rolled back.",
          "",
          format_source_location(context.source_location),
        ],
        "\n",
      )

    SavepointAdapterError(adapter_err) ->
      string.join(
        [
          "Error: SavepointAdapterError",
          "",
          "  " <> error.format_error(adapter_err),
          "",
          format_source_location(context.source_location),
        ],
        "\n",
      )

    SavepointUserError(_) ->
      string.join(
        [
          "Error: SavepointAborted",
          "",
          "  Savepoint was aborted due to a user error.",
          "",
          "  Changes since the savepoint were rolled back.",
          "",
          format_source_location(context.source_location),
        ],
        "\n",
      )

    SavepointCreationFailed(reason) ->
      string.join(
        [
          "Error: SavepointCreationFailed",
          "",
          "  Failed to create savepoint: " <> reason,
          "",
          "  Hint: Ensure you are within an active transaction.",
          "",
        ],
        "\n",
      )

    SavepointReleaseFailed(reason) ->
      string.join(
        [
          "Error: SavepointReleaseFailed",
          "",
          "  Failed to release savepoint: " <> reason,
          "",
        ],
        "\n",
      )

    SavepointNoTransaction ->
      string.join(
        [
          "Error: SavepointNoTransaction",
          "",
          "  Cannot use savepoint outside of a transaction.",
          "",
          "  Savepoints require an active transaction context.",
          "",
          "  Hint: Wrap your code in `repo.transaction(...)` first,",
          "        then use `repo.savepoint(...)` within it.",
          "",
        ],
        "\n",
      )
  }
}

// ============================================================================
// MAIN ERROR FORMATTER
// ============================================================================

/// Format any AdapterError with rich context
pub fn format_rich_error(error: AdapterError, context: ErrorContext) -> String {
  case error {
    NotFound -> format_not_found(context)

    TooManyRows(expected, got) -> format_too_many_rows(expected, got, context)

    ConnectionFailed(reason) -> format_connection_failed_simple(reason)

    ConnectionTimeout -> format_connection_timeout(None)

    PoolExhausted -> format_pool_exhausted()

    ConnectionLost(reason) -> format_connection_lost(reason)

    QueryFailed(message, code) -> format_query_failed(message, code, context)

    DecodeFailed(row, column, expected, got) ->
      format_decode_error(row, column, expected, got)

    Timeout -> format_timeout(context)

    UniqueViolation(constraint, detail) ->
      format_unique_violation_error(constraint, detail, context)

    ForeignKeyViolation(constraint, detail) ->
      format_foreign_key_violation_error(constraint, detail, context)

    CheckViolation(constraint, detail) ->
      format_check_violation(constraint, detail, context)

    NotNullViolation(column) -> format_not_null_violation(column, context)

    ConstraintViolation(constraint, detail) ->
      format_generic_constraint_violation(constraint, detail, context)

    StaleData(expected, actual) -> format_stale_data(expected, actual, context)

    DataIntegrityError(message) -> format_data_integrity_error(message, context)

    NotSupported(operation) -> format_not_supported(operation)

    AdapterSpecific(code, message) ->
      format_adapter_specific(code, message, context)
  }
}

// ============================================================================
// ADDITIONAL FORMATTERS
// ============================================================================

/// Format a generic constraint violation
fn format_generic_constraint_violation(
  constraint: String,
  detail: String,
  context: ErrorContext,
) -> String {
  string.join(
    [
      "Error: ConstraintViolation",
      "",
      format_operation_line(context),
      "",
      "  Constraint \"" <> constraint <> "\" violated.",
      "",
      "  Detail: " <> detail,
      "",
      "  Hint: Review the constraint definition and ensure",
      "        the data meets all requirements.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a stale data error
fn format_stale_data(
  expected: String,
  actual: String,
  context: ErrorContext,
) -> String {
  string.join(
    [
      "Error: StaleData",
      "",
      format_operation_line(context),
      "",
      "  The record was modified by another process.",
      "",
      "  Expected version: " <> expected,
      "  Actual version:   " <> actual,
      "",
      "  Hint: Refresh the record and retry the operation,",
      "        or use optimistic locking with retry logic.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a data integrity error
fn format_data_integrity_error(message: String, context: ErrorContext) -> String {
  string.join(
    [
      "Error: DataIntegrityError",
      "",
      format_operation_line(context),
      "",
      "  " <> message,
      "",
      "  The database detected an integrity issue.",
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

/// Format a not supported error
fn format_not_supported(operation: String) -> String {
  string.join(
    [
      "Error: NotSupported",
      "",
      "  Operation \"" <> operation <> "\" is not supported by this adapter.",
      "",
      "  Hint: Check the adapter documentation for supported operations,",
      "        or use a different adapter that supports this feature.",
      "",
    ],
    "\n",
  )
}

/// Format an adapter-specific error
fn format_adapter_specific(
  code: String,
  message: String,
  context: ErrorContext,
) -> String {
  string.join(
    [
      "Error: AdapterSpecific [" <> code <> "]",
      "",
      format_operation_line(context),
      "",
      "  " <> message,
      "",
      format_source_location(context.source_location),
    ],
    "\n",
  )
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Format the operation line from context
fn format_operation_line(context: ErrorContext) -> String {
  case context.operation, context.table {
    Some(op), Some(table) -> "  During: " <> op <> " on " <> table
    Some(op), None -> "  During: " <> op
    None, Some(table) -> "  Table: " <> table
    None, None -> ""
  }
}

/// Format query context (query + params)
fn format_query_context(context: ErrorContext) -> String {
  case context.query {
    Some(query) -> {
      let params_str = case context.params {
        Some(params) ->
          "\n  Params: ["
          <> string.join(list.map(params, format_value), ", ")
          <> "]"
        None -> ""
      }
      "  Query: " <> truncate_query(query) <> params_str
    }
    None -> format_operation_line(context)
  }
}

/// Format source location if available
fn format_source_location(location: Option(SourceLocation)) -> String {
  case location {
    Some(loc) ->
      "  at "
      <> loc.file
      <> ":"
      <> int.to_string(loc.line)
      <> " in "
      <> loc.function
      <> "()"
    None -> ""
  }
}

/// Generate a hint based on the query error message and code
fn format_query_hint(message: String, code: Option(String)) -> String {
  let lower_message = string.lowercase(message)

  case code {
    Some("42P01") ->
      "  Hint: The table does not exist. Check the table name spelling,\n"
      <> "        run migrations, or verify the schema."

    Some("42703") ->
      "  Hint: The column does not exist. Check the column name spelling\n"
      <> "        or verify the schema definition."

    Some("42601") ->
      "  Hint: There is a syntax error in the query. Check for typos,\n"
      <> "        missing commas, or incorrect SQL syntax."

    Some("42501") ->
      "  Hint: Insufficient privileges. Check that the database user\n"
      <> "        has the required permissions for this operation."

    _ -> get_hint_from_message(lower_message)
  }
}

/// Get hint based on error message content
fn get_hint_from_message(lower_message: String) -> String {
  case string.contains(lower_message, "syntax") {
    True -> "  Hint: Check the query syntax for errors."
    False ->
      case
        string.contains(lower_message, "permission")
        || string.contains(lower_message, "privilege")
      {
        True -> "  Hint: Check database user permissions."
        False ->
          case string.contains(lower_message, "table") {
            True -> "  Hint: Verify the table exists and is accessible."
            False ->
              case string.contains(lower_message, "column") {
                True -> "  Hint: Verify the column name is correct."
                False -> "  Hint: Check the query and parameters for errors."
              }
          }
      }
  }
}

/// Truncate a long query for display
fn truncate_query(query: String) -> String {
  let max_length = 200
  case string.length(query) > max_length {
    True -> string.slice(query, 0, max_length) <> "..."
    False -> query
  }
}

/// Extract column name from unique constraint detail
fn extract_column_from_unique_detail(detail: String) -> String {
  // Pattern: "Key (column)=(value) already exists."
  case string.split(detail, "(") {
    [_, rest, ..] ->
      case string.split(rest, ")") {
        [column, ..] -> column
        _ -> "column"
      }
    _ -> "column"
  }
}

/// Extract value from unique constraint detail
fn extract_value_from_unique_detail(detail: String) -> String {
  // Pattern: "Key (column)=(value) already exists."
  case string.split(detail, "=(") {
    [_, rest, ..] ->
      case string.split(rest, ")") {
        [value, ..] -> mask_sensitive_value(value)
        _ -> "(value)"
      }
    _ -> "(value)"
  }
}
