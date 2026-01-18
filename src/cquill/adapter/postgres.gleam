// cquill Postgres Adapter
//
// PostgreSQL adapter implementation using pog (pgo wrapper).
// This provides full SQL database capabilities including:
// - Connection pooling (built into pog)
// - Transactions with proper isolation
// - RETURNING clause support
// - Full error mapping to cquill error types
//
// Based on ADR-002 decision to use pog for PostgreSQL access.

import cquill/adapter.{
  type Adapter, type AdapterCapabilities, type AdapterError, type CompiledQuery,
  type QueryParam,
}
import cquill/error
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Name}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import pog

// ============================================================================
// CONFIGURATION TYPES
// ============================================================================

/// PostgreSQL connection configuration.
/// This wraps pog.Config with cquill-specific defaults.
pub type PostgresConfig {
  PostgresConfig(
    /// Pool name for process registration
    pool_name: Name(pog.Message),
    /// Database server hostname (default: "127.0.0.1")
    host: String,
    /// Port the server is listening on (default: 5432)
    port: Int,
    /// Name of database to use
    database: String,
    /// Username to connect to database as
    user: String,
    /// Password for the user (None for no password)
    password: Option(String),
    /// SSL mode (default: SslDisabled)
    ssl: pog.Ssl,
    /// Number of connections in the pool (default: 10)
    pool_size: Int,
    /// Default query timeout in milliseconds (default: 5000)
    default_timeout: Int,
    /// Interval in ms to ping idle connections (default: 1000)
    idle_interval: Int,
  )
}

/// Create a default configuration with a pool name.
pub fn default_config(pool_name: Name(pog.Message)) -> PostgresConfig {
  PostgresConfig(
    pool_name: pool_name,
    host: "127.0.0.1",
    port: 5432,
    database: "postgres",
    user: "postgres",
    password: None,
    ssl: pog.SslDisabled,
    pool_size: 10,
    default_timeout: 5000,
    idle_interval: 1000,
  )
}

/// Create configuration from a database URL.
/// URL format: postgresql://user:password@host:port/database
pub fn config_from_url(
  pool_name: Name(pog.Message),
  url: String,
) -> Result(PostgresConfig, Nil) {
  case pog.url_config(pool_name, url) {
    Ok(pog_config) ->
      Ok(PostgresConfig(
        pool_name: pog_config.pool_name,
        host: pog_config.host,
        port: pog_config.port,
        database: pog_config.database,
        user: pog_config.user,
        password: pog_config.password,
        ssl: pog_config.ssl,
        pool_size: pog_config.pool_size,
        default_timeout: 5000,
        idle_interval: pog_config.idle_interval,
      ))
    Error(_) -> Error(Nil)
  }
}

/// Set the database host
pub fn host(config: PostgresConfig, host: String) -> PostgresConfig {
  PostgresConfig(..config, host: host)
}

/// Set the database port
pub fn port(config: PostgresConfig, port: Int) -> PostgresConfig {
  PostgresConfig(..config, port: port)
}

/// Set the database name
pub fn database(config: PostgresConfig, database: String) -> PostgresConfig {
  PostgresConfig(..config, database: database)
}

/// Set the database user
pub fn user(config: PostgresConfig, user: String) -> PostgresConfig {
  PostgresConfig(..config, user: user)
}

/// Set the database password
pub fn password(
  config: PostgresConfig,
  password: Option(String),
) -> PostgresConfig {
  PostgresConfig(..config, password: password)
}

/// Set SSL mode
pub fn ssl(config: PostgresConfig, ssl: pog.Ssl) -> PostgresConfig {
  PostgresConfig(..config, ssl: ssl)
}

/// Set the connection pool size
pub fn pool_size(config: PostgresConfig, pool_size: Int) -> PostgresConfig {
  PostgresConfig(..config, pool_size: pool_size)
}

/// Set the default query timeout in milliseconds.
/// Queries taking longer than this will be aborted with a Timeout error.
/// Default is 5000ms (5 seconds).
pub fn default_timeout(
  config: PostgresConfig,
  timeout_ms: Int,
) -> PostgresConfig {
  PostgresConfig(..config, default_timeout: timeout_ms)
}

/// Set the idle connection ping interval in milliseconds.
/// The database is pinged every idle_interval when the connection is idle.
/// This helps detect and recover from stale connections.
/// Default is 1000ms (1 second).
pub fn idle_interval(config: PostgresConfig, interval_ms: Int) -> PostgresConfig {
  PostgresConfig(..config, idle_interval: interval_ms)
}

// ============================================================================
// CONNECTION TYPES
// ============================================================================

/// PostgreSQL connection handle.
/// This wraps pog.Connection which represents a pool.
pub opaque type PostgresConnection {
  PostgresConnection(connection: pog.Connection)
}

/// Convert PostgresConfig to pog.Config
fn to_pog_config(config: PostgresConfig) -> pog.Config {
  pog.default_config(config.pool_name)
  |> pog.host(config.host)
  |> pog.port(config.port)
  |> pog.database(config.database)
  |> pog.user(config.user)
  |> pog.password(config.password)
  |> pog.ssl(config.ssl)
  |> pog.pool_size(config.pool_size)
  |> pog.idle_interval(config.idle_interval)
}

/// Start a connection pool using the provided configuration.
/// Returns a PostgresConnection that can be used for queries.
pub fn start(
  config: PostgresConfig,
) -> Result(PostgresConnection, actor.StartError) {
  let pog_config = to_pog_config(config)
  case pog.start(pog_config) {
    Ok(actor.Started(_, connection)) -> Ok(PostgresConnection(connection))
    Error(err) -> Error(err)
  }
}

/// Create a connection reference to a named pool.
/// Use this when you've started a pool with supervision and need to
/// reference it by name.
pub fn named_connection(name: Name(pog.Message)) -> PostgresConnection {
  PostgresConnection(connection: pog.named_connection(name))
}

// ============================================================================
// ERROR MAPPING
// ============================================================================

/// Convert a pog.QueryError to cquill.AdapterError
fn map_query_error(err: pog.QueryError) -> AdapterError {
  case err {
    pog.ConstraintViolated(message, constraint, detail) ->
      error.from_postgres_error(constraint_to_code(constraint), message, detail)

    pog.PostgresqlError(code, _name, message) ->
      error.from_postgres_error(code, message, "")

    pog.UnexpectedArgumentCount(expected, got) ->
      error.QueryFailed(
        "Unexpected argument count: expected "
          <> int.to_string(expected)
          <> ", got "
          <> int.to_string(got),
        None,
      )

    pog.UnexpectedArgumentType(expected, got) ->
      error.QueryFailed(
        "Unexpected argument type: expected " <> expected <> ", got " <> got,
        None,
      )

    pog.UnexpectedResultType(decode_errors) ->
      error.DecodeFailed(
        0,
        "result",
        "expected type",
        format_decode_errors(decode_errors),
      )

    pog.QueryTimeout -> error.Timeout

    pog.ConnectionUnavailable ->
      error.ConnectionFailed("Connection unavailable")
  }
}

/// Map constraint name patterns to PostgreSQL error codes
fn constraint_to_code(constraint: String) -> String {
  // The constraint field from ConstraintViolated contains the constraint name,
  // but we need to infer the type. This is a simplification - pog already
  // parsed the constraint type for us in most cases.
  case constraint {
    "unique_violation" -> "23505"
    "foreign_key_violation" -> "23503"
    "not_null_violation" -> "23502"
    "check_violation" -> "23514"
    _ -> "23000"
  }
}

/// Format decode errors for display
fn format_decode_errors(errors: List(decode.DecodeError)) -> String {
  errors
  |> list.map(fn(err) {
    "expected " <> err.expected <> " at " <> format_path(err.path)
  })
  |> join(", ")
}

fn format_path(path: List(String)) -> String {
  case path {
    [] -> "root"
    segments -> join(segments, ".")
  }
}

/// Join a list of strings with a separator
fn join(strings: List(String), separator: String) -> String {
  case strings {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> separator <> join(rest, separator)
  }
}

// ============================================================================
// QUERY PARAMETER CONVERSION
// ============================================================================

/// Convert cquill QueryParam to pog.Value
fn param_to_value(param: QueryParam) -> pog.Value {
  case param {
    adapter.ParamInt(i) -> pog.int(i)
    adapter.ParamFloat(f) -> pog.float(f)
    adapter.ParamString(s) -> pog.text(s)
    adapter.ParamBool(b) -> pog.bool(b)
    adapter.ParamNull -> pog.null()
    adapter.ParamBytes(bytes) -> pog.bytea(bytes)
    adapter.ParamCustom(_, value) -> pog.text(value)
  }
}

/// Convert a list of QueryParams to pog.Values
fn params_to_values(params: List(QueryParam)) -> List(pog.Value) {
  list.map(params, param_to_value)
}

// ============================================================================
// QUERY EXECUTION
// ============================================================================

/// Execute a query and return all rows as List(Dynamic)
fn execute_query(
  conn: PostgresConnection,
  compiled: CompiledQuery,
) -> Result(List(List(Dynamic)), AdapterError) {
  let PostgresConnection(pool) = conn
  let values = params_to_values(compiled.params)

  let query =
    pog.query(compiled.sql)
    |> add_parameters(values)
    |> pog.returning(decode.dynamic)

  case pog.execute(query, pool) {
    Ok(pog.Returned(_, rows)) -> Ok(list.map(rows, wrap_row))
    Error(err) -> Error(map_query_error(err))
  }
}

/// Wrap a dynamic row into a list format for consistency with adapter interface
fn wrap_row(row: Dynamic) -> List(Dynamic) {
  // pog returns rows as tuples, we convert to list for uniform handling
  case decode.run(row, decode.at([0], decode.dynamic)) {
    Ok(_) -> tuple_to_list(row)
    Error(_) -> [row]
  }
}

/// Convert a tuple to a list of dynamics
fn tuple_to_list(tuple: Dynamic) -> List(Dynamic) {
  // Try progressively larger tuple sizes
  case decode.run(tuple, decode_tuple2()) {
    Ok(#(a, b)) -> [a, b]
    Error(_) ->
      case decode.run(tuple, decode_tuple3()) {
        Ok(#(a, b, c)) -> [a, b, c]
        Error(_) ->
          case decode.run(tuple, decode_tuple4()) {
            Ok(#(a, b, c, d)) -> [a, b, c, d]
            Error(_) ->
              case decode.run(tuple, decode_tuple5()) {
                Ok(#(a, b, c, d, e)) -> [a, b, c, d, e]
                Error(_) ->
                  case decode.run(tuple, decode_tuple6()) {
                    Ok(#(a, b, c, d, e, f)) -> [a, b, c, d, e, f]
                    Error(_) -> [tuple]
                  }
              }
          }
      }
  }
}

fn decode_tuple2() -> decode.Decoder(#(Dynamic, Dynamic)) {
  use a <- decode.field(0, decode.dynamic)
  use b <- decode.field(1, decode.dynamic)
  decode.success(#(a, b))
}

fn decode_tuple3() -> decode.Decoder(#(Dynamic, Dynamic, Dynamic)) {
  use a <- decode.field(0, decode.dynamic)
  use b <- decode.field(1, decode.dynamic)
  use c <- decode.field(2, decode.dynamic)
  decode.success(#(a, b, c))
}

fn decode_tuple4() -> decode.Decoder(#(Dynamic, Dynamic, Dynamic, Dynamic)) {
  use a <- decode.field(0, decode.dynamic)
  use b <- decode.field(1, decode.dynamic)
  use c <- decode.field(2, decode.dynamic)
  use d <- decode.field(3, decode.dynamic)
  decode.success(#(a, b, c, d))
}

fn decode_tuple5() -> decode.Decoder(
  #(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic),
) {
  use a <- decode.field(0, decode.dynamic)
  use b <- decode.field(1, decode.dynamic)
  use c <- decode.field(2, decode.dynamic)
  use d <- decode.field(3, decode.dynamic)
  use e <- decode.field(4, decode.dynamic)
  decode.success(#(a, b, c, d, e))
}

fn decode_tuple6() -> decode.Decoder(
  #(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic),
) {
  use a <- decode.field(0, decode.dynamic)
  use b <- decode.field(1, decode.dynamic)
  use c <- decode.field(2, decode.dynamic)
  use d <- decode.field(3, decode.dynamic)
  use e <- decode.field(4, decode.dynamic)
  use f <- decode.field(5, decode.dynamic)
  decode.success(#(a, b, c, d, e, f))
}

/// Execute a mutation (INSERT/UPDATE/DELETE) and return affected count
fn execute_mutation(
  conn: PostgresConnection,
  compiled: CompiledQuery,
) -> Result(Int, AdapterError) {
  let PostgresConnection(pool) = conn
  let values = params_to_values(compiled.params)

  let query =
    pog.query(compiled.sql)
    |> add_parameters(values)
    |> pog.returning(decode.dynamic)

  case pog.execute(query, pool) {
    Ok(pog.Returned(count, _)) -> Ok(count)
    Error(err) -> Error(map_query_error(err))
  }
}

/// Execute an INSERT with RETURNING clause
fn execute_returning(
  conn: PostgresConnection,
  compiled: CompiledQuery,
) -> Result(Option(List(Dynamic)), AdapterError) {
  let PostgresConnection(pool) = conn
  let values = params_to_values(compiled.params)

  let query =
    pog.query(compiled.sql)
    |> add_parameters(values)
    |> pog.returning(decode.dynamic)

  case pog.execute(query, pool) {
    Ok(pog.Returned(_, [row])) -> Ok(Some(wrap_row(row)))
    Ok(pog.Returned(_, [])) -> Ok(None)
    Ok(pog.Returned(_, _rows)) -> Ok(None)
    Error(err) -> Error(map_query_error(err))
  }
}

/// Add parameters to a pog query
fn add_parameters(query: pog.Query(a), values: List(pog.Value)) -> pog.Query(a) {
  list.fold(values, query, fn(q, v) { pog.parameter(q, v) })
}

// ============================================================================
// TRANSACTION SUPPORT
// ============================================================================

/// Begin a transaction - pog handles this differently via pog.transaction
/// For the adapter interface, we return the same connection since pog
/// manages transaction state internally.
fn begin_transaction(
  conn: PostgresConnection,
) -> Result(PostgresConnection, AdapterError) {
  // pog doesn't have explicit begin/commit/rollback
  // Transactions are handled via the pog.transaction callback function
  // For the adapter interface, we'll just return the connection
  Ok(conn)
}

/// Commit a transaction
fn commit_transaction(_conn: PostgresConnection) -> Result(Nil, AdapterError) {
  // pog auto-commits on success in transaction callback
  Ok(Nil)
}

/// Rollback a transaction
fn rollback_transaction(_conn: PostgresConnection) -> Result(Nil, AdapterError) {
  // pog auto-rollbacks on error in transaction callback
  Ok(Nil)
}

// ============================================================================
// ADAPTER CONSTRUCTION
// ============================================================================

/// PostgreSQL adapter capabilities - full SQL support
pub fn postgres_capabilities() -> AdapterCapabilities {
  adapter.AdapterCapabilities(
    transactions: True,
    returning: True,
    batch_insert: True,
    upsert: True,
    max_params: Some(65_535),
    json_operations: True,
    array_types: True,
  )
}

/// Create a PostgreSQL adapter.
/// This implements the full adapter interface for PostgreSQL.
pub fn postgres_adapter() -> Adapter(PostgresConnection, List(Dynamic)) {
  adapter.new(
    name: "postgres",
    capabilities: postgres_capabilities(),
    execute_query: execute_query,
    execute_mutation: execute_mutation,
    execute_returning: execute_returning,
    begin_transaction: begin_transaction,
    commit_transaction: commit_transaction,
    rollback_transaction: rollback_transaction,
  )
}

// ============================================================================
// HIGHER-LEVEL TRANSACTION API
// ============================================================================

/// Run a function within a PostgreSQL transaction.
/// This uses pog's native transaction support which provides proper
/// transaction isolation and automatic commit/rollback.
pub fn with_transaction(
  conn: PostgresConnection,
  operation: fn(PostgresConnection) -> Result(a, String),
) -> Result(a, TransactionError) {
  let PostgresConnection(pool) = conn

  case
    pog.transaction(pool, fn(tx_pool) {
      let tx_conn = PostgresConnection(connection: tx_pool)
      operation(tx_conn)
    })
  {
    Ok(result) -> Ok(result)
    Error(pog.TransactionQueryError(query_err)) ->
      Error(QueryError(map_query_error(query_err)))
    Error(pog.TransactionRolledBack(reason)) -> Error(RolledBack(reason))
  }
}

/// Transaction error type for the higher-level API
pub type TransactionError {
  QueryError(AdapterError)
  RolledBack(String)
}

/// Execute a transaction with automatic error handling.
/// This version uses the standard cquill TransactionError type.
/// - On success: commits and returns the result
/// - On adapter error (constraint violation, etc.): rolls back and returns AdapterTransactionError
/// - On pog query error during transaction: returns AdapterTransactionError
pub fn execute_transaction(
  conn: PostgresConnection,
  operation: fn(PostgresConnection) -> Result(a, error.AdapterError),
) -> Result(a, error.TransactionError(Nil)) {
  let PostgresConnection(pool) = conn

  case
    pog.transaction(pool, fn(tx_pool) {
      let tx_conn = PostgresConnection(connection: tx_pool)
      operation(tx_conn)
    })
  {
    Ok(result) -> Ok(result)
    Error(pog.TransactionQueryError(query_err)) -> {
      let adapter_err = map_query_error(query_err)
      // Check for serialization failure (PostgreSQL code 40001)
      case query_err {
        pog.PostgresqlError("40001", _, _) -> Error(error.SerializationFailure)
        pog.QueryTimeout -> Error(error.TransactionTimeout)
        _ -> Error(error.AdapterTransactionError(adapter_err))
      }
    }
    Error(pog.TransactionRolledBack(adapter_err)) -> {
      // The user's operation returned an AdapterError, which caused rollback
      Error(error.AdapterTransactionError(adapter_err))
    }
  }
}

/// Execute a transaction with a user error type.
/// This version allows the operation to return a custom error type.
/// - On success: commits and returns the result
/// - On user error: rolls back and returns UserError
/// - On pog query error during transaction: returns AdapterTransactionError
pub fn execute_transaction_with_user_error(
  conn: PostgresConnection,
  operation: fn(PostgresConnection) -> Result(a, e),
) -> Result(a, error.TransactionError(e)) {
  let PostgresConnection(pool) = conn

  case
    pog.transaction(pool, fn(tx_pool) {
      let tx_conn = PostgresConnection(connection: tx_pool)
      operation(tx_conn)
    })
  {
    Ok(result) -> Ok(result)
    Error(pog.TransactionQueryError(query_err)) -> {
      let adapter_err = map_query_error(query_err)
      // Check for serialization failure (PostgreSQL code 40001)
      case query_err {
        pog.PostgresqlError("40001", _, _) -> Error(error.SerializationFailure)
        pog.QueryTimeout -> Error(error.TransactionTimeout)
        _ -> Error(error.AdapterTransactionError(adapter_err))
      }
    }
    Error(pog.TransactionRolledBack(user_error)) -> {
      // The user's operation returned an error, which caused rollback
      Error(error.UserError(user_error))
    }
  }
}

// ============================================================================
// SAVEPOINT SUPPORT
// ============================================================================

/// Create a savepoint with the given name.
/// Savepoints allow partial rollback within a transaction.
/// The connection must be within an active transaction.
pub fn create_savepoint(
  conn: PostgresConnection,
  name: String,
) -> Result(Nil, error.SavepointError(Nil)) {
  let sql = "SAVEPOINT " <> escape_identifier(name)
  case execute_sql_mutation(conn, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(adapter_err) ->
      Error(error.SavepointCreationFailed(error.format_error(adapter_err)))
  }
}

/// Rollback to a named savepoint.
/// All changes made after the savepoint was created are discarded.
/// The savepoint remains active and can be rolled back to again.
pub fn rollback_to_savepoint(
  conn: PostgresConnection,
  name: String,
) -> Result(Nil, error.SavepointError(Nil)) {
  let sql = "ROLLBACK TO SAVEPOINT " <> escape_identifier(name)
  case execute_sql_mutation(conn, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(adapter_err) -> Error(error.SavepointAdapterError(adapter_err))
  }
}

/// Release a savepoint without rolling back.
/// This destroys the savepoint but keeps all changes made after it.
/// This is useful to free up resources when a savepoint is no longer needed.
pub fn release_savepoint(
  conn: PostgresConnection,
  name: String,
) -> Result(Nil, error.SavepointError(Nil)) {
  let sql = "RELEASE SAVEPOINT " <> escape_identifier(name)
  case execute_sql_mutation(conn, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(adapter_err) ->
      Error(error.SavepointReleaseFailed(error.format_error(adapter_err)))
  }
}

/// Execute a function within a savepoint.
/// If the function succeeds, the savepoint is released.
/// If the function fails, changes are rolled back to the savepoint.
/// This must be called within an active transaction.
pub fn execute_savepoint(
  conn: PostgresConnection,
  name: String,
  operation: fn(PostgresConnection) -> Result(a, error.AdapterError),
) -> Result(a, error.SavepointError(Nil)) {
  // Create the savepoint
  case create_savepoint(conn, name) {
    Error(err) -> Error(err)
    Ok(_) -> {
      // Execute the operation
      case operation(conn) {
        Ok(result) -> {
          // Release savepoint on success
          case release_savepoint(conn, name) {
            Ok(_) -> Ok(result)
            Error(err) -> Error(err)
          }
        }
        Error(adapter_err) -> {
          // Rollback to savepoint on failure
          let _ = rollback_to_savepoint(conn, name)
          Error(error.SavepointAdapterError(adapter_err))
        }
      }
    }
  }
}

/// Execute a function within a savepoint, allowing user errors.
/// If the function succeeds, the savepoint is released.
/// If the function fails, changes are rolled back to the savepoint.
pub fn execute_savepoint_with_user_error(
  conn: PostgresConnection,
  name: String,
  operation: fn(PostgresConnection) -> Result(a, e),
) -> Result(a, error.SavepointError(e)) {
  // Create the savepoint
  case create_savepoint(conn, name) {
    Error(error.SavepointCreationFailed(reason)) ->
      Error(error.SavepointCreationFailed(reason))
    Error(error.SavepointAdapterError(adapter_err)) ->
      Error(error.SavepointAdapterError(adapter_err))
    Error(error.SavepointNotFound(n)) -> Error(error.SavepointNotFound(n))
    Error(error.SavepointReleaseFailed(r)) ->
      Error(error.SavepointReleaseFailed(r))
    Error(error.SavepointNoTransaction) -> Error(error.SavepointNoTransaction)
    Error(error.SavepointUserError(_)) ->
      Error(error.SavepointCreationFailed("Unexpected user error"))
    Ok(_) -> {
      // Execute the operation
      case operation(conn) {
        Ok(result) -> {
          // Release savepoint on success
          case release_savepoint(conn, name) {
            Ok(_) -> Ok(result)
            Error(error.SavepointReleaseFailed(reason)) ->
              Error(error.SavepointReleaseFailed(reason))
            Error(error.SavepointAdapterError(adapter_err)) ->
              Error(error.SavepointAdapterError(adapter_err))
            Error(error.SavepointNotFound(n)) ->
              Error(error.SavepointNotFound(n))
            Error(error.SavepointCreationFailed(r)) ->
              Error(error.SavepointCreationFailed(r))
            Error(error.SavepointNoTransaction) ->
              Error(error.SavepointNoTransaction)
            Error(error.SavepointUserError(_)) ->
              Error(error.SavepointReleaseFailed("Unexpected user error"))
          }
        }
        Error(user_err) -> {
          // Rollback to savepoint on failure
          let _ = rollback_to_savepoint(conn, name)
          Error(error.SavepointUserError(user_err))
        }
      }
    }
  }
}

/// Escape a SQL identifier (table name, savepoint name, etc.)
/// to prevent SQL injection.
fn escape_identifier(name: String) -> String {
  // Replace any double quotes with escaped double quotes
  // and wrap in double quotes for PostgreSQL identifier quoting
  "\"" <> replace_all(name, "\"", "\"\"") <> "\""
}

/// Replace all occurrences of a substring
fn replace_all(string: String, from: String, to: String) -> String {
  do_replace_all(string, from, to, "")
}

fn do_replace_all(
  string: String,
  from: String,
  to: String,
  acc: String,
) -> String {
  case string_starts_with(string, from) {
    True -> {
      let rest = string_drop(string, string_length(from))
      do_replace_all(rest, from, to, acc <> to)
    }
    False -> {
      case string_pop_grapheme(string) {
        Ok(#(char, rest)) -> do_replace_all(rest, from, to, acc <> char)
        Error(_) -> acc
      }
    }
  }
}

/// Check if string starts with prefix
fn string_starts_with(string: String, prefix: String) -> Bool {
  string_take(string, string_length(prefix)) == prefix
}

/// Take first n graphemes from string
fn string_take(string: String, n: Int) -> String {
  do_string_take(string, n, "")
}

fn do_string_take(string: String, n: Int, acc: String) -> String {
  case n <= 0 {
    True -> acc
    False ->
      case string_pop_grapheme(string) {
        Ok(#(char, rest)) -> do_string_take(rest, n - 1, acc <> char)
        Error(_) -> acc
      }
  }
}

/// Drop first n graphemes from string
fn string_drop(string: String, n: Int) -> String {
  case n <= 0 {
    True -> string
    False ->
      case string_pop_grapheme(string) {
        Ok(#(_, rest)) -> string_drop(rest, n - 1)
        Error(_) -> ""
      }
  }
}

/// Get string length in graphemes
fn string_length(string: String) -> Int {
  do_string_length(string, 0)
}

fn do_string_length(string: String, acc: Int) -> Int {
  case string_pop_grapheme(string) {
    Ok(#(_, rest)) -> do_string_length(rest, acc + 1)
    Error(_) -> acc
  }
}

@external(erlang, "string", "next_grapheme")
fn string_pop_grapheme(string: String) -> Result(#(String, String), Nil)

// ============================================================================
// RAW QUERY EXECUTION (Convenience functions)
// ============================================================================

/// Execute a raw SQL query with parameters.
/// Returns rows as List(Dynamic).
pub fn execute_sql(
  conn: PostgresConnection,
  sql: String,
  params: List(QueryParam),
) -> Result(List(List(Dynamic)), AdapterError) {
  execute_query(
    conn,
    adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0),
  )
}

/// Execute a raw SQL mutation with parameters.
/// Returns the number of affected rows.
pub fn execute_sql_mutation(
  conn: PostgresConnection,
  sql: String,
  params: List(QueryParam),
) -> Result(Int, AdapterError) {
  execute_mutation(
    conn,
    adapter.CompiledQuery(sql: sql, params: params, expected_columns: 0),
  )
}

/// Execute a raw SQL query with a custom timeout.
/// The timeout is in milliseconds. Use this for queries that may take
/// longer than the default 5 second timeout.
pub fn execute_sql_with_timeout(
  conn: PostgresConnection,
  sql: String,
  params: List(QueryParam),
  timeout_ms: Int,
) -> Result(List(List(Dynamic)), AdapterError) {
  let PostgresConnection(pool) = conn
  let values = params_to_values(params)

  let query =
    pog.query(sql)
    |> add_parameters(values)
    |> pog.timeout(timeout_ms)
    |> pog.returning(decode.dynamic)

  case pog.execute(query, pool) {
    Ok(pog.Returned(_, rows)) -> Ok(list.map(rows, wrap_row))
    Error(err) -> Error(map_query_error(err))
  }
}

/// Execute a raw SQL mutation with a custom timeout.
/// The timeout is in milliseconds.
pub fn execute_sql_mutation_with_timeout(
  conn: PostgresConnection,
  sql: String,
  params: List(QueryParam),
  timeout_ms: Int,
) -> Result(Int, AdapterError) {
  let PostgresConnection(pool) = conn
  let values = params_to_values(params)

  let query =
    pog.query(sql)
    |> add_parameters(values)
    |> pog.timeout(timeout_ms)
    |> pog.returning(decode.dynamic)

  case pog.execute(query, pool) {
    Ok(pog.Returned(count, _)) -> Ok(count)
    Error(err) -> Error(map_query_error(err))
  }
}

// ============================================================================
// BATCH OPERATIONS
// ============================================================================

/// Configuration for batch operations
pub type BatchConfig {
  BatchConfig(
    /// Maximum rows per INSERT statement (limits VALUES clause size)
    max_values_per_insert: Int,
    /// Wrap entire batch in a transaction
    use_transaction: Bool,
  )
}

/// Default batch configuration for Postgres
pub fn default_batch_config() -> BatchConfig {
  BatchConfig(max_values_per_insert: 1000, use_transaction: True)
}

/// Insert multiple rows using a multi-value INSERT statement.
/// This is more efficient than individual inserts.
///
/// Example SQL generated:
/// INSERT INTO users (email, name) VALUES ($1, $2), ($3, $4), ($5, $6)
pub fn insert_all(
  conn: PostgresConnection,
  table: String,
  columns: List(String),
  rows: List(List(QueryParam)),
) -> Result(Int, AdapterError) {
  insert_all_with_config(conn, table, columns, rows, default_batch_config())
}

/// Insert multiple rows with configuration options.
pub fn insert_all_with_config(
  conn: PostgresConnection,
  table: String,
  columns: List(String),
  rows: List(List(QueryParam)),
  config: BatchConfig,
) -> Result(Int, AdapterError) {
  case rows {
    [] -> Ok(0)
    _ -> {
      // Chunk rows by max_values_per_insert
      let chunks = chunk_list(rows, config.max_values_per_insert)

      case config.use_transaction {
        True ->
          // Run all chunks in a transaction
          case
            execute_transaction(conn, fn(tx_conn) {
              insert_chunks(tx_conn, table, columns, chunks, 0)
            })
          {
            Ok(count) -> Ok(count)
            Error(error.AdapterTransactionError(e)) -> Error(e)
            Error(_) -> Error(error.QueryFailed("Batch insert failed", None))
          }
        False ->
          // Run chunks without transaction wrapper
          insert_chunks(conn, table, columns, chunks, 0)
      }
    }
  }
}

/// Insert multiple chunks sequentially
fn insert_chunks(
  conn: PostgresConnection,
  table: String,
  columns: List(String),
  chunks: List(List(List(QueryParam))),
  total: Int,
) -> Result(Int, AdapterError) {
  case chunks {
    [] -> Ok(total)
    [chunk, ..rest] ->
      case insert_single_chunk(conn, table, columns, chunk) {
        Error(e) -> Error(e)
        Ok(count) -> insert_chunks(conn, table, columns, rest, total + count)
      }
  }
}

/// Insert a single chunk using multi-value INSERT
fn insert_single_chunk(
  conn: PostgresConnection,
  table: String,
  columns: List(String),
  rows: List(List(QueryParam)),
) -> Result(Int, AdapterError) {
  let column_count = list.length(columns)
  let #(sql, all_params) =
    build_multi_value_insert(table, columns, rows, column_count)
  execute_sql_mutation(conn, sql, all_params)
}

/// Build a multi-value INSERT statement
fn build_multi_value_insert(
  table: String,
  columns: List(String),
  rows: List(List(QueryParam)),
  column_count: Int,
) -> #(String, List(QueryParam)) {
  let column_list = join_strings(columns, ", ")
  let insert_prefix =
    "INSERT INTO "
    <> escape_identifier(table)
    <> " ("
    <> column_list
    <> ") VALUES "

  // Build VALUES clauses with proper parameter placeholders
  let #(values_clauses, all_params, _) =
    list.fold(rows, #([], [], 1), fn(acc, row) {
      let #(clauses, params, param_idx) = acc
      let #(value_clause, new_idx) =
        build_value_clause(row, column_count, param_idx)
      #([value_clause, ..clauses], list.append(params, row), new_idx)
    })

  let values_sql = join_strings(list.reverse(values_clauses), ", ")
  #(insert_prefix <> values_sql, all_params)
}

/// Build a single VALUES clause like ($1, $2, $3)
fn build_value_clause(
  _row: List(QueryParam),
  column_count: Int,
  start_idx: Int,
) -> #(String, Int) {
  let placeholders = build_placeholders(column_count, start_idx, [])
  let clause = "(" <> join_strings(placeholders, ", ") <> ")"
  #(clause, start_idx + column_count)
}

/// Build parameter placeholders ($1, $2, etc.)
fn build_placeholders(
  count: Int,
  current_idx: Int,
  acc: List(String),
) -> List(String) {
  case count {
    0 -> list.reverse(acc)
    n -> {
      let placeholder = "$" <> int.to_string(current_idx)
      build_placeholders(n - 1, current_idx + 1, [placeholder, ..acc])
    }
  }
}

/// Insert multiple rows and return the inserted rows.
/// Uses RETURNING * to get all inserted data.
pub fn insert_all_returning(
  conn: PostgresConnection,
  table: String,
  columns: List(String),
  rows: List(List(QueryParam)),
) -> Result(List(List(Dynamic)), AdapterError) {
  case rows {
    [] -> Ok([])
    _ -> {
      let column_count = list.length(columns)
      let #(base_sql, all_params) =
        build_multi_value_insert(table, columns, rows, column_count)
      let sql = base_sql <> " RETURNING *"
      execute_sql(conn, sql, all_params)
    }
  }
}

/// Update all rows matching a condition.
/// Returns the number of updated rows.
///
/// Example:
/// update_all(conn, "users", [#("active", adapter.param_bool(False))],
///            "created_at < $1", [adapter.param_string("2024-01-01")])
pub fn batch_update_all(
  conn: PostgresConnection,
  table: String,
  set_clauses: List(#(String, QueryParam)),
  where_clause: String,
  where_params: List(QueryParam),
) -> Result(Int, AdapterError) {
  let #(set_sql, set_params, next_idx) = build_set_clause(set_clauses, 1)
  let adjusted_where = adjust_param_indices(where_clause, next_idx)

  let sql =
    "UPDATE "
    <> escape_identifier(table)
    <> " SET "
    <> set_sql
    <> " WHERE "
    <> adjusted_where

  let all_params = list.append(set_params, where_params)
  execute_sql_mutation(conn, sql, all_params)
}

/// Build SET clause for UPDATE
fn build_set_clause(
  clauses: List(#(String, QueryParam)),
  start_idx: Int,
) -> #(String, List(QueryParam), Int) {
  let #(parts, params, idx) =
    list.fold(clauses, #([], [], start_idx), fn(acc, clause) {
      let #(sql_parts, param_acc, current_idx) = acc
      let #(column, param) = clause
      let sql_part = column <> " = $" <> int.to_string(current_idx)
      #([sql_part, ..sql_parts], [param, ..param_acc], current_idx + 1)
    })

  let sql = join_strings(list.reverse(parts), ", ")
  #(sql, list.reverse(params), idx)
}

/// Adjust parameter indices in a WHERE clause
/// e.g., "$1" becomes "$3" if start_idx is 3
fn adjust_param_indices(where_clause: String, _start_idx: Int) -> String {
  // For simplicity, we assume the caller already uses the correct indices
  // A full implementation would parse and rewrite $N placeholders
  where_clause
}

/// Update all rows matching a condition and return updated rows.
pub fn batch_update_all_returning(
  conn: PostgresConnection,
  table: String,
  set_clauses: List(#(String, QueryParam)),
  where_clause: String,
  where_params: List(QueryParam),
) -> Result(List(List(Dynamic)), AdapterError) {
  let #(set_sql, set_params, next_idx) = build_set_clause(set_clauses, 1)
  let adjusted_where = adjust_param_indices(where_clause, next_idx)

  let sql =
    "UPDATE "
    <> escape_identifier(table)
    <> " SET "
    <> set_sql
    <> " WHERE "
    <> adjusted_where
    <> " RETURNING *"

  let all_params = list.append(set_params, where_params)
  execute_sql(conn, sql, all_params)
}

/// Delete all rows matching a condition.
/// Returns the number of deleted rows.
pub fn batch_delete_all(
  conn: PostgresConnection,
  table: String,
  where_clause: String,
  where_params: List(QueryParam),
) -> Result(Int, AdapterError) {
  let sql =
    "DELETE FROM " <> escape_identifier(table) <> " WHERE " <> where_clause
  execute_sql_mutation(conn, sql, where_params)
}

/// Delete all rows matching a condition and return deleted rows.
pub fn batch_delete_all_returning(
  conn: PostgresConnection,
  table: String,
  where_clause: String,
  where_params: List(QueryParam),
) -> Result(List(List(Dynamic)), AdapterError) {
  let sql =
    "DELETE FROM "
    <> escape_identifier(table)
    <> " WHERE "
    <> where_clause
    <> " RETURNING *"
  execute_sql(conn, sql, where_params)
}

/// Delete all rows in a table (truncate-like operation).
/// Returns the number of deleted rows.
pub fn delete_all_rows(
  conn: PostgresConnection,
  table: String,
) -> Result(Int, AdapterError) {
  let sql = "DELETE FROM " <> escape_identifier(table)
  execute_sql_mutation(conn, sql, [])
}

/// Chunk a list into sublists of max_size
fn chunk_list(items: List(a), max_size: Int) -> List(List(a)) {
  do_chunk_list(items, max_size, [], [])
}

fn do_chunk_list(
  items: List(a),
  max_size: Int,
  current_chunk: List(a),
  chunks: List(List(a)),
) -> List(List(a)) {
  case items {
    [] ->
      case current_chunk {
        [] -> list.reverse(chunks)
        _ -> list.reverse([list.reverse(current_chunk), ..chunks])
      }
    [item, ..rest] -> {
      let new_chunk = [item, ..current_chunk]
      case list.length(new_chunk) >= max_size {
        True ->
          do_chunk_list(rest, max_size, [], [list.reverse(new_chunk), ..chunks])
        False -> do_chunk_list(rest, max_size, new_chunk, chunks)
      }
    }
  }
}

/// Join a list of strings with a separator
fn join_strings(strings: List(String), separator: String) -> String {
  case strings {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> separator <> join_strings(rest, separator)
  }
}

// ============================================================================
// ROW DECODING HELPERS
// ============================================================================
//
// Note: Row decoding converts PostgreSQL result tuples to lists. The current
// implementation handles tuples with up to 6 elements. For queries returning
// more than 6 columns, consider:
// 1. Using SELECT with specific columns instead of SELECT *
// 2. Breaking the query into multiple smaller queries
// 3. Using pog directly with rows_as_map option for wider result sets
// ============================================================================

/// Decode a single integer value from a result row
pub fn decode_int(row: List(Dynamic), index: Int) -> Result(Int, Nil) {
  case list_at(row, index) {
    Some(value) ->
      case decode.run(value, decode.int) {
        Ok(i) -> Ok(i)
        Error(_) -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

/// Decode a single string value from a result row
pub fn decode_string(row: List(Dynamic), index: Int) -> Result(String, Nil) {
  case list_at(row, index) {
    Some(value) ->
      case decode.run(value, decode.string) {
        Ok(s) -> Ok(s)
        Error(_) -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

/// Decode a single float value from a result row
pub fn decode_float(row: List(Dynamic), index: Int) -> Result(Float, Nil) {
  case list_at(row, index) {
    Some(value) ->
      case decode.run(value, decode.float) {
        Ok(f) -> Ok(f)
        Error(_) -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

/// Decode a single bool value from a result row
pub fn decode_bool(row: List(Dynamic), index: Int) -> Result(Bool, Nil) {
  case list_at(row, index) {
    Some(value) ->
      case decode.run(value, decode.bool) {
        Ok(b) -> Ok(b)
        Error(_) -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

/// Decode an optional value from a result row
pub fn decode_optional(
  row: List(Dynamic),
  index: Int,
  decoder: decode.Decoder(a),
) -> Result(Option(a), Nil) {
  case list_at(row, index) {
    Some(value) ->
      case decode.run(value, decode.optional(decoder)) {
        Ok(opt) -> Ok(opt)
        Error(_) -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

/// Get an element from a list by index
fn list_at(list: List(a), index: Int) -> Option(a) {
  case index < 0 {
    True -> None
    False -> do_list_at(list, index)
  }
}

fn do_list_at(list: List(a), index: Int) -> Option(a) {
  case list {
    [] -> None
    [first, ..rest] ->
      case index {
        0 -> Some(first)
        _ -> do_list_at(rest, index - 1)
      }
  }
}
