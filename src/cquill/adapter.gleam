// cquill Adapter Protocol
//
// This module defines the minimal adapter interface that all storage backends
// must implement. The design follows these principles:
//
// 1. Minimal surface area - only operations ALL backends can support
// 2. Consistent error types across adapters
// 3. Optional capabilities declared explicitly via types
// 4. Clear contract for implementers
//
// Research incorporated from:
// - Ecto.Adapter behaviours (required vs optional callbacks)
// - Repository pattern (domain-focused abstraction)
// - Rust/Gleam Option/Result patterns for type-safe optionality

import cquill/error.{
  AdapterTransactionError, BeginFailed, CommitFailed, NotSupported, UserError,
}
import gleam/list
import gleam/option.{type Option}

// ============================================================================
// RE-EXPORT ERROR TYPES
// ============================================================================

// Re-export error types for convenience - users can import from either
// cquill/adapter or cquill/error
pub type AdapterError =
  error.AdapterError

pub type TransactionError(e) =
  error.TransactionError(e)

// Re-export error utilities as wrapper functions for backwards compatibility
pub fn is_not_found(err: AdapterError) -> Bool {
  error.is_not_found(err)
}

pub fn is_constraint_violation(err: AdapterError) -> Bool {
  error.is_constraint_violation(err)
}

pub fn is_unique_violation(err: AdapterError) -> Bool {
  error.is_unique_violation(err)
}

pub fn is_foreign_key_violation(err: AdapterError) -> Bool {
  error.is_foreign_key_violation(err)
}

pub fn is_connection_error(err: AdapterError) -> Bool {
  error.is_connection_error(err)
}

pub fn is_recoverable(err: AdapterError) -> Bool {
  error.is_recoverable(err)
}

pub fn is_query_error(err: AdapterError) -> Bool {
  error.is_query_error(err)
}

pub fn format_error(err: AdapterError) -> String {
  error.format_error(err)
}

// Re-export error mapping functions as wrappers
pub fn from_postgres_error(
  code: String,
  message: String,
  detail: String,
) -> AdapterError {
  error.from_postgres_error(code, message, detail)
}

pub fn from_mysql_error(code: Int, message: String) -> AdapterError {
  error.from_mysql_error(code, message)
}

pub fn from_sqlite_error(code: Int, message: String) -> AdapterError {
  error.from_sqlite_error(code, message)
}

// ============================================================================
// QUERY REPRESENTATION (Simplified for adapter interface)
// ============================================================================

/// Represents a compiled query ready for adapter execution.
/// The full Query AST is defined in cquill/query, but adapters
/// receive this simplified representation.
pub type CompiledQuery {
  CompiledQuery(
    /// The SQL or query string to execute
    sql: String,
    /// Bound parameters in order
    params: List(QueryParam),
    /// Expected number of columns in result
    expected_columns: Int,
  )
}

/// Query parameters with type information for proper encoding
pub type QueryParam {
  ParamInt(Int)
  ParamFloat(Float)
  ParamString(String)
  ParamBool(Bool)
  ParamNull
  ParamBytes(BitArray)
  /// For adapter-specific parameter types
  ParamCustom(type_name: String, value: String)
}

// ============================================================================
// ADAPTER CAPABILITIES
// ============================================================================

/// Declares what optional features an adapter supports.
/// This allows compile-time checking of feature availability.
pub type AdapterCapabilities {
  AdapterCapabilities(
    /// Does the adapter support multi-statement transactions?
    transactions: Bool,
    /// Does the adapter support RETURNING clauses (get inserted ID)?
    returning: Bool,
    /// Does the adapter support batch insert operations?
    batch_insert: Bool,
    /// Does the adapter support upsert (INSERT ON CONFLICT)?
    upsert: Bool,
    /// Maximum number of parameters in a single query (None = unlimited)
    max_params: Option(Int),
    /// Does the adapter support JSON operations?
    json_operations: Bool,
    /// Does the adapter support array types?
    array_types: Bool,
  )
}

/// Default capabilities - minimal feature set all adapters support
pub fn default_capabilities() -> AdapterCapabilities {
  AdapterCapabilities(
    transactions: False,
    returning: False,
    batch_insert: False,
    upsert: False,
    max_params: option.None,
    json_operations: False,
    array_types: False,
  )
}

/// Full SQL database capabilities (Postgres, MySQL, etc.)
pub fn sql_capabilities() -> AdapterCapabilities {
  AdapterCapabilities(
    transactions: True,
    returning: True,
    batch_insert: True,
    upsert: True,
    max_params: option.None,
    json_operations: True,
    array_types: True,
  )
}

// ============================================================================
// ADAPTER INTERFACE
// ============================================================================

/// The core adapter interface.
///
/// This uses a record of functions pattern rather than behaviours because:
/// 1. Gleam doesn't have Elixir-style behaviours
/// 2. Records are first-class and composable
/// 3. Easier to test with mock adapters
/// 4. Type-safe at compile time
///
/// Type parameters:
/// - `conn`: The connection/pool type specific to this adapter
/// - `row`: The raw row type returned by the adapter (usually List(Dynamic))
pub opaque type Adapter(conn, row) {
  Adapter(
    /// Adapter name for debugging/logging
    name: String,
    /// Declared capabilities
    capabilities: AdapterCapabilities,
    /// Execute a query and return all matching rows
    execute_query: fn(conn, CompiledQuery) -> Result(List(row), AdapterError),
    /// Execute a mutation (INSERT/UPDATE/DELETE) and return affected count
    execute_mutation: fn(conn, CompiledQuery) -> Result(Int, AdapterError),
    /// Execute an INSERT with RETURNING clause (if supported)
    execute_returning: fn(conn, CompiledQuery) ->
      Result(Option(row), AdapterError),
    /// Begin a transaction (if supported)
    begin_transaction: fn(conn) -> Result(conn, AdapterError),
    /// Commit a transaction
    commit_transaction: fn(conn) -> Result(Nil, AdapterError),
    /// Rollback a transaction
    rollback_transaction: fn(conn) -> Result(Nil, AdapterError),
  )
}

// ============================================================================
// ADAPTER CONSTRUCTORS
// ============================================================================

/// Create a new adapter with the given operations.
pub fn new(
  name name: String,
  capabilities capabilities: AdapterCapabilities,
  execute_query execute_query: fn(conn, CompiledQuery) ->
    Result(List(row), AdapterError),
  execute_mutation execute_mutation: fn(conn, CompiledQuery) ->
    Result(Int, AdapterError),
  execute_returning execute_returning: fn(conn, CompiledQuery) ->
    Result(Option(row), AdapterError),
  begin_transaction begin_transaction: fn(conn) -> Result(conn, AdapterError),
  commit_transaction commit_transaction: fn(conn) -> Result(Nil, AdapterError),
  rollback_transaction rollback_transaction: fn(conn) ->
    Result(Nil, AdapterError),
) -> Adapter(conn, row) {
  Adapter(
    name:,
    capabilities:,
    execute_query:,
    execute_mutation:,
    execute_returning:,
    begin_transaction:,
    commit_transaction:,
    rollback_transaction:,
  )
}

// ============================================================================
// ADAPTER ACCESSORS
// ============================================================================

/// Get the adapter's name
pub fn name(adapter: Adapter(conn, row)) -> String {
  let Adapter(name:, ..) = adapter
  name
}

/// Get the adapter's declared capabilities
pub fn capabilities(adapter: Adapter(conn, row)) -> AdapterCapabilities {
  let Adapter(capabilities:, ..) = adapter
  capabilities
}

/// Check if adapter supports transactions
pub fn supports_transactions(adapter: Adapter(conn, row)) -> Bool {
  capabilities(adapter).transactions
}

/// Check if adapter supports RETURNING clause
pub fn supports_returning(adapter: Adapter(conn, row)) -> Bool {
  capabilities(adapter).returning
}

/// Check if adapter supports batch inserts
pub fn supports_batch_insert(adapter: Adapter(conn, row)) -> Bool {
  capabilities(adapter).batch_insert
}

/// Check if adapter supports upsert operations
pub fn supports_upsert(adapter: Adapter(conn, row)) -> Bool {
  capabilities(adapter).upsert
}

/// Check if adapter supports JSON operations
pub fn supports_json(adapter: Adapter(conn, row)) -> Bool {
  capabilities(adapter).json_operations
}

/// Check if adapter supports array types
pub fn supports_arrays(adapter: Adapter(conn, row)) -> Bool {
  capabilities(adapter).array_types
}

// ============================================================================
// ADAPTER OPERATIONS
// ============================================================================

/// Execute a query and return all rows
pub fn query(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(List(row), AdapterError) {
  let Adapter(execute_query:, ..) = adapter
  execute_query(connection, compiled)
}

/// Execute a query and return exactly one row, or error
pub fn query_one(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(row, AdapterError) {
  let Adapter(execute_query:, ..) = adapter
  case execute_query(connection, compiled) {
    Ok([row]) -> Ok(row)
    Ok([]) -> Error(error.NotFound)
    Ok(rows) -> Error(error.TooManyRows(1, list.length(rows)))
    Error(e) -> Error(e)
  }
}

/// Execute a query and return an optional row (None if not found)
pub fn query_optional(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Option(row), AdapterError) {
  let Adapter(execute_query:, ..) = adapter
  case execute_query(connection, compiled) {
    Ok([row]) -> Ok(option.Some(row))
    Ok([]) -> Ok(option.None)
    Ok(rows) -> Error(error.TooManyRows(1, list.length(rows)))
    Error(e) -> Error(e)
  }
}

/// Execute a mutation and return affected row count
pub fn mutate(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Int, AdapterError) {
  let Adapter(execute_mutation:, ..) = adapter
  execute_mutation(connection, compiled)
}

/// Execute an INSERT with RETURNING (returns NotSupported if unavailable)
pub fn insert_returning(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(Option(row), AdapterError) {
  let Adapter(capabilities:, execute_returning:, ..) = adapter
  case capabilities.returning {
    True -> execute_returning(connection, compiled)
    False -> Error(NotSupported("RETURNING clause"))
  }
}

/// Execute a function within a transaction.
/// Returns UserError if the user's operation function fails.
/// Returns AdapterTransactionError if the adapter/database fails during the transaction.
pub fn transaction(
  adapter: Adapter(conn, row),
  connection: conn,
  operation: fn(conn) -> Result(a, e),
) -> Result(a, TransactionError(e)) {
  let Adapter(
    capabilities:,
    begin_transaction:,
    commit_transaction:,
    rollback_transaction:,
    ..,
  ) = adapter

  case capabilities.transactions {
    False ->
      // For non-transactional adapters, just run the operation directly
      // This matches Ecto's approach of not emulating unsupported features
      case operation(connection) {
        Ok(result) -> Ok(result)
        Error(e) -> Error(UserError(e))
      }

    True -> {
      // Start transaction
      case begin_transaction(connection) {
        Error(adapter_err) ->
          Error(BeginFailed(
            "Could not begin transaction: " <> error.format_error(adapter_err),
          ))
        Ok(tx_conn) -> {
          // Run the operation
          case operation(tx_conn) {
            Ok(result) -> {
              // Commit on success
              case commit_transaction(tx_conn) {
                Ok(_) -> Ok(result)
                Error(adapter_err) ->
                  Error(CommitFailed(
                    "Commit failed: " <> error.format_error(adapter_err),
                  ))
              }
            }
            Error(e) -> {
              // Best-effort rollback: the original user error takes precedence.
              // If rollback fails, we still return the user's error since that's
              // what caused the transaction to fail in the first place.
              let _ = rollback_transaction(tx_conn)
              Error(UserError(e))
            }
          }
        }
      }
    }
  }
}

/// Execute a function within a transaction with adapter error handling.
/// This version allows the operation to return AdapterError, which is wrapped
/// in AdapterTransactionError on failure instead of UserError.
pub fn transaction_with_adapter_errors(
  adapter: Adapter(conn, row),
  connection: conn,
  operation: fn(conn) -> Result(a, AdapterError),
) -> Result(a, TransactionError(Nil)) {
  let Adapter(
    capabilities:,
    begin_transaction:,
    commit_transaction:,
    rollback_transaction:,
    ..,
  ) = adapter

  case capabilities.transactions {
    False ->
      case operation(connection) {
        Ok(result) -> Ok(result)
        Error(adapter_err) -> Error(AdapterTransactionError(adapter_err))
      }

    True -> {
      case begin_transaction(connection) {
        Error(adapter_err) ->
          Error(BeginFailed(
            "Could not begin transaction: " <> error.format_error(adapter_err),
          ))
        Ok(tx_conn) -> {
          case operation(tx_conn) {
            Ok(result) -> {
              case commit_transaction(tx_conn) {
                Ok(_) -> Ok(result)
                Error(adapter_err) ->
                  Error(CommitFailed(
                    "Commit failed: " <> error.format_error(adapter_err),
                  ))
              }
            }
            Error(adapter_err) -> {
              // Best-effort rollback: the original adapter error takes precedence.
              // If rollback fails, we still return the operation's error since that's
              // what caused the transaction to fail in the first place.
              let _ = rollback_transaction(tx_conn)
              Error(AdapterTransactionError(adapter_err))
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// QUERY HELPERS
// ============================================================================

/// Create a simple SELECT query
pub fn select_query(sql: String, params: List(QueryParam)) -> CompiledQuery {
  CompiledQuery(sql:, params:, expected_columns: 0)
}

/// Create a mutation query (INSERT/UPDATE/DELETE)
pub fn mutation_query(sql: String, params: List(QueryParam)) -> CompiledQuery {
  CompiledQuery(sql:, params:, expected_columns: 0)
}

/// Create a query with expected column count
pub fn query_with_columns(
  sql: String,
  params: List(QueryParam),
  expected_columns: Int,
) -> CompiledQuery {
  CompiledQuery(sql:, params:, expected_columns:)
}

// ============================================================================
// QUERY PARAM HELPERS
// ============================================================================

/// Create an integer parameter
pub fn param_int(value: Int) -> QueryParam {
  ParamInt(value)
}

/// Create a float parameter
pub fn param_float(value: Float) -> QueryParam {
  ParamFloat(value)
}

/// Create a string parameter
pub fn param_string(value: String) -> QueryParam {
  ParamString(value)
}

/// Create a boolean parameter
pub fn param_bool(value: Bool) -> QueryParam {
  ParamBool(value)
}

/// Create a null parameter
pub fn param_null() -> QueryParam {
  ParamNull
}

/// Create a bytes parameter
pub fn param_bytes(value: BitArray) -> QueryParam {
  ParamBytes(value)
}

/// Create a custom type parameter
pub fn param_custom(type_name: String, value: String) -> QueryParam {
  ParamCustom(type_name, value)
}
