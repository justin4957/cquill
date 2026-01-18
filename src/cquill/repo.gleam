// cquill Repo Module
//
// The Repo module is the unified public API for all database operations.
// It provides a clean, consistent interface that delegates to adapters.
//
// This is the ONLY module users need to import for database operations.
// It handles:
// - Adapter abstraction
// - Error transformation to user-friendly forms
// - Convenience functions for common operations
//
// Example:
// ```gleam
// import cquill/repo
// import cquill/query
// import cquill/adapter/postgres
//
// pub fn get_active_users(conn: PostgresConnection) {
//   let adapter = postgres.postgres_adapter()
//   query.from(user_schema)
//   |> query.where(query.eq_bool("active", True))
//   |> repo.all(adapter, conn, _, user_decoder)
// }
// ```

import cquill/adapter.{
  type Adapter, type AdapterCapabilities, type AdapterError, type CompiledQuery,
  type QueryParam,
}
import cquill/error
import cquill/schema.{type Schema}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// REPO ERROR TYPES
// ============================================================================

/// User-friendly error type for repository operations.
/// These errors are designed for application code to handle.
pub type RepoError {
  /// Record not found when one was expected
  NotFound(schema: String, query_description: String)

  /// Multiple records found when expecting at most one
  TooManyRows(expected: Int, got: Int)

  /// Constraint violation during mutation
  ConstraintError(kind: ConstraintKind, detail: String)

  /// Database connection error
  ConnectionError(reason: String)

  /// Query execution error
  QueryError(message: String)

  /// Validation errors (for future changeset integration)
  ValidationErrors(errors: List(ValidationError))

  /// Operation not supported by the adapter
  NotSupported(operation: String)

  /// Timeout during query execution
  Timeout
}

/// Specific constraint violation types
pub type ConstraintKind {
  /// Unique constraint (e.g., duplicate email)
  UniqueConstraint(field: String)

  /// Foreign key constraint (e.g., referenced record doesn't exist)
  ForeignKeyConstraint(field: String, references: String)

  /// NOT NULL constraint (e.g., required field missing)
  NotNullConstraint(field: String)

  /// CHECK constraint (e.g., value out of allowed range)
  CheckConstraint(name: String)

  /// Unknown constraint type
  UnknownConstraint(name: String)
}

/// Validation error for changeset integration
pub type ValidationError {
  ValidationError(field: String, message: String, kind: ValidationKind)
}

/// Types of validation errors
pub type ValidationKind {
  Required
  Format
  Length
  Range
  Custom(name: String)
}

// ============================================================================
// ERROR CONVERSION
// ============================================================================

/// Convert an AdapterError to a RepoError with context
pub fn from_adapter_error(err: AdapterError, context: String) -> RepoError {
  case err {
    error.NotFound -> NotFound("", context)
    error.TooManyRows(expected, got) -> TooManyRows(expected, got)

    error.UniqueViolation(constraint, detail) ->
      ConstraintError(UniqueConstraint(constraint), detail)

    error.ForeignKeyViolation(constraint, detail) ->
      ConstraintError(ForeignKeyConstraint(constraint, ""), detail)

    error.NotNullViolation(column) ->
      ConstraintError(NotNullConstraint(column), "NULL value not allowed")

    error.CheckViolation(constraint, detail) ->
      ConstraintError(CheckConstraint(constraint), detail)

    error.ConstraintViolation(constraint, detail) ->
      ConstraintError(UnknownConstraint(constraint), detail)

    error.ConnectionFailed(reason) -> ConnectionError(reason)
    error.ConnectionTimeout -> ConnectionError("Connection timed out")
    error.PoolExhausted -> ConnectionError("Connection pool exhausted")
    error.ConnectionLost(reason) ->
      ConnectionError("Connection lost: " <> reason)

    error.QueryFailed(message, code) -> {
      let code_str = case code {
        Some(c) -> " [" <> c <> "]"
        None -> ""
      }
      QueryError(message <> code_str)
    }

    error.DecodeFailed(row, column, expected, got) ->
      QueryError(
        "Decode failed at row "
        <> int_to_string(row)
        <> ", column "
        <> column
        <> ": expected "
        <> expected
        <> ", got "
        <> got,
      )

    error.Timeout -> Timeout

    error.StaleData(expected, actual) ->
      QueryError(
        "Stale data: expected version " <> expected <> ", found " <> actual,
      )

    error.DataIntegrityError(message) ->
      QueryError("Data integrity: " <> message)

    error.NotSupported(operation) -> NotSupported(operation)

    error.AdapterSpecific(code, message) ->
      QueryError("Adapter error [" <> code <> "]: " <> message)
  }
}

/// Convert an AdapterError with schema context
fn from_adapter_error_with_schema(
  err: AdapterError,
  schema_name: String,
) -> RepoError {
  case err {
    error.NotFound -> NotFound(schema_name, "query")
    _ -> from_adapter_error(err, schema_name)
  }
}

// ============================================================================
// QUERY OPERATIONS
// ============================================================================

/// Fetch all records matching a query.
///
/// ## Example
/// ```gleam
/// query.from(user_schema)
/// |> query.where(query.eq_bool("active", True))
/// |> repo.all(adapter, conn, _, fn(row) { decode_user(row) })
/// ```
pub fn all(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
  decoder: fn(row) -> Result(a, String),
) -> Result(List(a), RepoError) {
  case adapter.query(adapter, connection, compiled) {
    Ok(rows) -> {
      let results = list.map(rows, decoder)
      case collect_results(results, []) {
        Ok(decoded) -> Ok(decoded)
        Error(msg) -> Error(QueryError("Decode error: " <> msg))
      }
    }
    Error(err) -> Error(from_adapter_error(err, "all"))
  }
}

/// Fetch a single record (None if not found).
///
/// Returns Ok(None) if no rows match, Ok(Some(record)) if exactly one matches,
/// or Error if more than one row matches.
///
/// ## Example
/// ```gleam
/// query.from(user_schema)
/// |> query.where(query.eq_string("email", email))
/// |> repo.one(adapter, conn, _, user_decoder)
/// ```
pub fn one(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
  decoder: fn(row) -> Result(a, String),
) -> Result(Option(a), RepoError) {
  case adapter.query_optional(adapter, connection, compiled) {
    Ok(None) -> Ok(None)
    Ok(Some(row)) ->
      case decoder(row) {
        Ok(decoded) -> Ok(Some(decoded))
        Error(msg) -> Error(QueryError("Decode error: " <> msg))
      }
    Error(err) -> Error(from_adapter_error(err, "one"))
  }
}

/// Fetch a single record (Error if not found).
///
/// Unlike `one`, this returns an error if no record is found.
///
/// ## Example
/// ```gleam
/// query.from(user_schema)
/// |> query.where(query.eq_int("id", user_id))
/// |> repo.get(adapter, conn, _, user_decoder)
/// ```
pub fn get(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
  decoder: fn(row) -> Result(a, String),
) -> Result(a, RepoError) {
  case adapter.query_one(adapter, connection, compiled) {
    Ok(row) ->
      case decoder(row) {
        Ok(decoded) -> Ok(decoded)
        Error(msg) -> Error(QueryError("Decode error: " <> msg))
      }
    Error(error.NotFound) -> Error(NotFound("", "get"))
    Error(error.TooManyRows(expected, got)) -> Error(TooManyRows(expected, got))
    Error(err) -> Error(from_adapter_error(err, "get"))
  }
}

/// Fetch a record by its primary key.
///
/// ## Example
/// ```gleam
/// repo.get_by_id(adapter, conn, user_schema, 123, user_decoder)
/// ```
pub fn get_by_id(
  adapter: Adapter(conn, row),
  connection: conn,
  schema: Schema,
  id_value: QueryParam,
  decoder: fn(row) -> Result(a, String),
) -> Result(a, RepoError) {
  let pk_columns = schema.get_primary_key(schema)
  case pk_columns {
    [] -> Error(QueryError("Schema has no primary key defined"))
    [pk_column] -> {
      // Build a simple SELECT ... WHERE pk = $1 query
      let table = schema.get_qualified_name(schema)
      let sql =
        "SELECT * FROM " <> table <> " WHERE " <> pk_column <> " = $1 LIMIT 1"
      let compiled =
        adapter.CompiledQuery(sql: sql, params: [id_value], expected_columns: 0)
      case adapter.query_one(adapter, connection, compiled) {
        Ok(row) ->
          case decoder(row) {
            Ok(decoded) -> Ok(decoded)
            Error(msg) -> Error(QueryError("Decode error: " <> msg))
          }
        Error(error.NotFound) ->
          Error(NotFound(schema.get_source(schema), "id lookup"))
        Error(err) ->
          Error(from_adapter_error_with_schema(err, schema.get_source(schema)))
      }
    }
    _ -> Error(NotSupported("Composite primary keys in get_by_id"))
  }
}

/// Check if any records match the query.
///
/// ## Example
/// ```gleam
/// query.from(user_schema)
/// |> query.where(query.eq_string("email", email))
/// |> repo.exists(adapter, conn, _)
/// ```
pub fn exists(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Bool, RepoError) {
  // Modify query to use EXISTS subquery pattern
  // For now, use a simpler approach: check if any rows are returned with LIMIT 1
  let exists_query =
    adapter.CompiledQuery(
      sql: "SELECT 1 FROM (" <> compiled.sql <> ") AS _exists_check LIMIT 1",
      params: compiled.params,
      expected_columns: 1,
    )

  case adapter.query(adapter, connection, exists_query) {
    Ok([]) -> Ok(False)
    Ok(_) -> Ok(True)
    Error(err) -> Error(from_adapter_error(err, "exists"))
  }
}

/// Count records matching the query.
///
/// ## Example
/// ```gleam
/// query.from(user_schema)
/// |> query.where(query.eq_bool("active", True))
/// |> repo.count(adapter, conn, _, int_decoder)
/// ```
pub fn count(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
  int_decoder: fn(row) -> Result(Int, String),
) -> Result(Int, RepoError) {
  // Wrap the query to count results
  let count_query =
    adapter.CompiledQuery(
      sql: "SELECT COUNT(*) FROM (" <> compiled.sql <> ") AS _count_subquery",
      params: compiled.params,
      expected_columns: 1,
    )

  case adapter.query_one(adapter, connection, count_query) {
    Ok(row) ->
      case int_decoder(row) {
        Ok(count) -> Ok(count)
        Error(msg) -> Error(QueryError("Count decode error: " <> msg))
      }
    Error(err) -> Error(from_adapter_error(err, "count"))
  }
}

// ============================================================================
// MUTATION OPERATIONS
// ============================================================================

/// Insert a new record and return the inserted record.
///
/// ## Example
/// ```gleam
/// let sql = "INSERT INTO users (email, name) VALUES ($1, $2) RETURNING *"
/// let params = [adapter.param_string(email), adapter.param_string(name)]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.insert(adapter, conn, compiled, user_decoder)
/// ```
pub fn insert(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
  decoder: fn(row) -> Result(a, String),
) -> Result(a, RepoError) {
  case adapter.insert_returning(adapter, connection, compiled) {
    Ok(Some(row)) ->
      case decoder(row) {
        Ok(decoded) -> Ok(decoded)
        Error(msg) -> Error(QueryError("Decode error: " <> msg))
      }
    Ok(None) -> Error(QueryError("Insert did not return a row"))
    Error(err) -> Error(from_adapter_error(err, "insert"))
  }
}

/// Insert a new record without returning data.
/// Returns the number of affected rows (usually 1).
///
/// ## Example
/// ```gleam
/// let sql = "INSERT INTO users (email, name) VALUES ($1, $2)"
/// let params = [adapter.param_string(email), adapter.param_string(name)]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.insert_no_return(adapter, conn, compiled)
/// ```
pub fn insert_no_return(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Int, RepoError) {
  case adapter.mutate(adapter, connection, compiled) {
    Ok(count) -> Ok(count)
    Error(err) -> Error(from_adapter_error(err, "insert"))
  }
}

/// Insert multiple records at once.
/// Returns the total number of inserted rows.
///
/// ## Example
/// ```gleam
/// let sql = "INSERT INTO users (email, name) VALUES ($1, $2), ($3, $4)"
/// let params = [
///   adapter.param_string("a@example.com"), adapter.param_string("Alice"),
///   adapter.param_string("b@example.com"), adapter.param_string("Bob"),
/// ]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.insert_all(adapter, conn, compiled)
/// ```
pub fn insert_all(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Int, RepoError) {
  case adapter.mutate(adapter, connection, compiled) {
    Ok(count) -> Ok(count)
    Error(err) -> Error(from_adapter_error(err, "insert_all"))
  }
}

/// Update a record and return the updated record.
///
/// ## Example
/// ```gleam
/// let sql = "UPDATE users SET name = $1 WHERE id = $2 RETURNING *"
/// let params = [adapter.param_string(new_name), adapter.param_int(user_id)]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.update(adapter, conn, compiled, user_decoder)
/// ```
pub fn update(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
  decoder: fn(row) -> Result(a, String),
) -> Result(a, RepoError) {
  case adapter.insert_returning(adapter, connection, compiled) {
    Ok(Some(row)) ->
      case decoder(row) {
        Ok(decoded) -> Ok(decoded)
        Error(msg) -> Error(QueryError("Decode error: " <> msg))
      }
    Ok(None) -> Error(NotFound("", "update"))
    Error(err) -> Error(from_adapter_error(err, "update"))
  }
}

/// Update all records matching a query.
/// Returns the number of updated rows.
///
/// ## Example
/// ```gleam
/// let sql = "UPDATE users SET active = $1 WHERE last_login < $2"
/// let params = [adapter.param_bool(False), adapter.param_string(cutoff_date)]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.update_all(adapter, conn, compiled)
/// ```
pub fn update_all(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Int, RepoError) {
  case adapter.mutate(adapter, connection, compiled) {
    Ok(count) -> Ok(count)
    Error(err) -> Error(from_adapter_error(err, "update_all"))
  }
}

/// Delete a record.
/// Returns the number of deleted rows (0 or 1).
///
/// ## Example
/// ```gleam
/// let sql = "DELETE FROM users WHERE id = $1"
/// let params = [adapter.param_int(user_id)]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.delete(adapter, conn, compiled)
/// ```
pub fn delete(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Int, RepoError) {
  case adapter.mutate(adapter, connection, compiled) {
    Ok(count) -> Ok(count)
    Error(err) -> Error(from_adapter_error(err, "delete"))
  }
}

/// Delete all records matching a query.
/// Returns the number of deleted rows.
///
/// ## Example
/// ```gleam
/// let sql = "DELETE FROM sessions WHERE expires_at < $1"
/// let params = [adapter.param_string(now)]
/// let compiled = adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0)
/// repo.delete_all(adapter, conn, compiled)
/// ```
pub fn delete_all(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Int, RepoError) {
  case adapter.mutate(adapter, connection, compiled) {
    Ok(count) -> Ok(count)
    Error(err) -> Error(from_adapter_error(err, "delete_all"))
  }
}

// ============================================================================
// TRANSACTION SUPPORT
// ============================================================================

/// Execute a function within a transaction.
///
/// If the function returns Ok, the transaction is committed.
/// If the function returns Error, the transaction is rolled back.
///
/// ## Example
/// ```gleam
/// repo.transaction(adapter, conn, fn(tx_conn) {
///   use from_account <- result.try(
///     repo.get_by_id(adapter, tx_conn, account_schema, from_id, account_decoder)
///     |> result.map_error(fn(_) { "Account not found" })
///   )
///   use _ <- result.try(
///     repo.update_all(adapter, tx_conn, debit_query)
///     |> result.map_error(fn(_) { "Failed to debit" })
///   )
///   Ok(Nil)
/// })
/// ```
pub fn transaction(
  adapter: Adapter(conn, row),
  connection: conn,
  operation: fn(conn) -> Result(a, e),
) -> Result(a, RepoTransactionError(e)) {
  case adapter.transaction(adapter, connection, operation) {
    Ok(result) -> Ok(result)
    Error(error.UserError(e)) -> Error(UserAborted(e))
    Error(error.AdapterTransactionError(adapter_err)) ->
      Error(AdapterError(adapter_err))
    Error(error.BeginFailed(reason)) -> Error(TransactionFailed(reason))
    Error(error.CommitFailed(reason)) -> Error(CommitFailed(reason))
    Error(error.RolledBack) -> Error(RolledBack)
    Error(error.TransactionRollback(reason)) ->
      Error(TransactionFailed("Rolled back: " <> reason))
    Error(error.TransactionConnectionLost) ->
      Error(TransactionFailed("Connection lost"))
    Error(error.NestedTransactionError) ->
      Error(TransactionFailed("Nested transactions not supported"))
    Error(error.TransactionTimeout) -> Error(TransactionTimedOut)
    Error(error.SerializationFailure) -> Error(SerializationConflict)
  }
}

/// Transaction error type for the repo API
pub type RepoTransactionError(e) {
  /// User's operation returned an error, transaction was rolled back
  UserAborted(e)

  /// Adapter/database error during transaction, transaction was rolled back
  AdapterError(error.AdapterError)

  /// Transaction failed to start or commit
  TransactionFailed(reason: String)

  /// Commit failed (transaction may have been rolled back)
  CommitFailed(reason: String)

  /// Transaction was explicitly rolled back
  RolledBack

  /// Transaction timed out
  TransactionTimedOut

  /// Serialization failure - concurrent transaction conflict (retry may succeed)
  SerializationConflict
}

// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

/// Create a simple SELECT * FROM table query
pub fn select_all_from(table: String) -> CompiledQuery {
  adapter.CompiledQuery(
    sql: "SELECT * FROM " <> table,
    params: [],
    expected_columns: 0,
  )
}

/// Create a SELECT * FROM table WHERE pk = $1 query
pub fn select_by_id(table: String, pk_column: String) -> CompiledQuery {
  adapter.CompiledQuery(
    sql: "SELECT * FROM " <> table <> " WHERE " <> pk_column <> " = $1",
    params: [],
    expected_columns: 0,
  )
}

/// Check if an adapter supports transactions
pub fn supports_transactions(adapter: Adapter(conn, row)) -> Bool {
  adapter.supports_transactions(adapter)
}

/// Check if an adapter supports RETURNING clauses
pub fn supports_returning(adapter: Adapter(conn, row)) -> Bool {
  adapter.supports_returning(adapter)
}

/// Get adapter capabilities
pub fn capabilities(adapter: Adapter(conn, row)) -> AdapterCapabilities {
  adapter.capabilities(adapter)
}

// ============================================================================
// ERROR FORMATTING
// ============================================================================

/// Format a RepoError for display
pub fn format_error(err: RepoError) -> String {
  case err {
    NotFound(schema, query) ->
      case schema {
        "" -> "Record not found: " <> query
        _ -> "Record not found in " <> schema <> ": " <> query
      }

    TooManyRows(expected, got) ->
      "Too many rows: expected "
      <> int_to_string(expected)
      <> ", got "
      <> int_to_string(got)

    ConstraintError(kind, detail) -> format_constraint_error(kind, detail)

    ConnectionError(reason) -> "Connection error: " <> reason

    QueryError(message) -> "Query error: " <> message

    ValidationErrors(errors) ->
      "Validation errors: "
      <> string.join(list.map(errors, format_validation_error), "; ")

    NotSupported(operation) -> "Operation not supported: " <> operation

    Timeout -> "Query timed out"
  }
}

fn format_constraint_error(kind: ConstraintKind, detail: String) -> String {
  let kind_str = case kind {
    UniqueConstraint(field) -> "Unique constraint violated on " <> field
    ForeignKeyConstraint(field, refs) ->
      "Foreign key constraint violated on " <> field <> " -> " <> refs
    NotNullConstraint(field) -> "NOT NULL constraint violated on " <> field
    CheckConstraint(name) -> "Check constraint violated: " <> name
    UnknownConstraint(name) -> "Constraint violated: " <> name
  }
  case detail {
    "" -> kind_str
    _ -> kind_str <> " (" <> detail <> ")"
  }
}

fn format_validation_error(err: ValidationError) -> String {
  err.field <> ": " <> err.message
}

/// Check if an error is a not-found error
pub fn is_not_found(err: RepoError) -> Bool {
  case err {
    NotFound(_, _) -> True
    _ -> False
  }
}

/// Check if an error is a constraint violation
pub fn is_constraint_error(err: RepoError) -> Bool {
  case err {
    ConstraintError(_, _) -> True
    _ -> False
  }
}

/// Check if an error is a unique constraint violation
pub fn is_unique_violation(err: RepoError) -> Bool {
  case err {
    ConstraintError(UniqueConstraint(_), _) -> True
    _ -> False
  }
}

/// Check if an error is a connection error
pub fn is_connection_error(err: RepoError) -> Bool {
  case err {
    ConnectionError(_) -> True
    _ -> False
  }
}

/// Check if an error is recoverable (can be retried)
pub fn is_recoverable(err: RepoError) -> Bool {
  case err {
    ConnectionError(_) -> True
    Timeout -> True
    _ -> False
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Collect a list of Results into a Result of list
fn collect_results(
  results: List(Result(a, String)),
  acc: List(a),
) -> Result(List(a), String) {
  case results {
    [] -> Ok(list.reverse(acc))
    [Ok(value), ..rest] -> collect_results(rest, [value, ..acc])
    [Error(msg), ..] -> Error(msg)
  }
}

fn int_to_string(n: Int) -> String {
  case n < 0 {
    True -> "-" <> int_to_string(-n)
    False ->
      case n {
        0 -> "0"
        _ -> do_int_to_string(n, "")
      }
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
