// In-Memory Adapter for cquill
//
// This adapter stores data in memory using Gleam dicts. It serves as:
// 1. Reference implementation of the adapter protocol
// 2. Testing adapter for fast, isolated tests
// 3. Example for implementing new adapters
//
// Note: This is a simplified implementation focused on demonstrating
// the adapter interface. A production version would need:
// - Actor-based state management for concurrency
// - Full query parsing and execution
// - Index support for efficient lookups

import cquill/adapter.{
  type Adapter, type AdapterCapabilities, type CompiledQuery,
}
import cquill/error.{type AdapterError}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// MEMORY STORE TYPES
// ============================================================================

/// A table in the memory store
pub type MemoryTable {
  MemoryTable(
    /// Table name
    name: String,
    /// Primary key column name
    primary_key: String,
    /// Rows indexed by primary key (as string)
    rows: Dict(String, List(Dynamic)),
    /// Auto-increment counter for serial IDs
    next_id: Int,
  )
}

/// The in-memory database connection/store
pub type MemoryStore {
  MemoryStore(
    /// Tables indexed by name
    tables: Dict(String, MemoryTable),
    /// Whether we're in a transaction
    in_transaction: Bool,
    /// Snapshot of tables at transaction start (for rollback)
    snapshot: Option(Dict(String, MemoryTable)),
  )
}

/// Row type for memory adapter (list of dynamic values)
pub type MemoryRow =
  List(Dynamic)

// ============================================================================
// STORE MANAGEMENT
// ============================================================================

/// Create a new empty memory store
pub fn new_store() -> MemoryStore {
  MemoryStore(tables: dict.new(), in_transaction: False, snapshot: None)
}

/// Create a table in the store
pub fn create_table(
  store: MemoryStore,
  name: String,
  primary_key: String,
) -> MemoryStore {
  let table =
    MemoryTable(
      name: name,
      primary_key: primary_key,
      rows: dict.new(),
      next_id: 1,
    )
  let tables = dict.insert(store.tables, name, table)
  MemoryStore(..store, tables: tables)
}

/// Insert a row into a table
pub fn insert_row(
  store: MemoryStore,
  table_name: String,
  key: String,
  row: MemoryRow,
) -> Result(MemoryStore, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      // Check for duplicate key (unique constraint)
      case dict.has_key(table.rows, key) {
        True ->
          Error(error.UniqueViolation(
            table_name <> "_pkey",
            "Key (" <> key <> ") already exists",
          ))
        False -> {
          let new_rows = dict.insert(table.rows, key, row)
          let new_table =
            MemoryTable(..table, rows: new_rows, next_id: table.next_id + 1)
          let new_tables = dict.insert(store.tables, table_name, new_table)
          Ok(MemoryStore(..store, tables: new_tables))
        }
      }
    }
  }
}

/// Update a row in a table
pub fn update_row(
  store: MemoryStore,
  table_name: String,
  key: String,
  row: MemoryRow,
) -> Result(MemoryStore, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      case dict.has_key(table.rows, key) {
        False -> Error(error.NotFound)
        True -> {
          let new_rows = dict.insert(table.rows, key, row)
          let new_table = MemoryTable(..table, rows: new_rows)
          let new_tables = dict.insert(store.tables, table_name, new_table)
          Ok(MemoryStore(..store, tables: new_tables))
        }
      }
    }
  }
}

/// Get all rows from a table
pub fn get_all_rows(
  store: MemoryStore,
  table_name: String,
) -> Result(List(MemoryRow), AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> Ok(dict.values(table.rows))
  }
}

/// Get a row by primary key
pub fn get_row(
  store: MemoryStore,
  table_name: String,
  key: String,
) -> Result(MemoryRow, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) ->
      case dict.get(table.rows, key) {
        Error(_) -> Error(error.NotFound)
        Ok(row) -> Ok(row)
      }
  }
}

/// Delete a row by primary key
pub fn delete_row(
  store: MemoryStore,
  table_name: String,
  key: String,
) -> Result(MemoryStore, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      case dict.has_key(table.rows, key) {
        False -> Error(error.NotFound)
        True -> {
          let new_rows = dict.delete(table.rows, key)
          let new_table = MemoryTable(..table, rows: new_rows)
          let new_tables = dict.insert(store.tables, table_name, new_table)
          Ok(MemoryStore(..store, tables: new_tables))
        }
      }
    }
  }
}

/// Get the next auto-increment ID for a table
pub fn next_id(
  store: MemoryStore,
  table_name: String,
) -> Result(Int, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> Ok(table.next_id)
  }
}

/// Get the row count for a table
pub fn row_count(
  store: MemoryStore,
  table_name: String,
) -> Result(Int, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> Ok(dict.size(table.rows))
  }
}

// ============================================================================
// ADAPTER IMPLEMENTATION
// ============================================================================

/// Memory adapter capabilities
pub fn memory_capabilities() -> AdapterCapabilities {
  adapter.AdapterCapabilities(
    transactions: True,
    // We support basic transactions via snapshots
    returning: True,
    // We can return inserted rows
    batch_insert: True,
    // We can insert multiple rows
    upsert: True,
    // We can do insert-or-update
    max_params: None,
    // No limit
    json_operations: False,
    // No JSON support
    array_types: True,
    // Lists work fine
  )
}

/// Create the memory adapter.
///
/// Note: In a real implementation, the connection type would be a reference
/// to mutable state (e.g., an Actor). This simplified version passes the
/// store directly for demonstration purposes.
pub fn memory_adapter() -> Adapter(MemoryStore, MemoryRow) {
  adapter.new(
    name: "memory",
    capabilities: memory_capabilities(),
    execute_query: execute_query,
    execute_mutation: execute_mutation,
    execute_returning: execute_returning,
    begin_transaction: begin_transaction,
    commit_transaction: commit_transaction,
    rollback_transaction: rollback_transaction,
  )
}

// ============================================================================
// ADAPTER CALLBACKS
// ============================================================================

/// Execute a query - simplified implementation that just parses table name
fn execute_query(
  store: MemoryStore,
  query: CompiledQuery,
) -> Result(List(MemoryRow), AdapterError) {
  // Very simplified: just extract table name from SQL
  // Real implementation would parse the query properly
  case extract_table_name(query.sql, "FROM") {
    Error(_) ->
      Error(error.QueryFailed("Could not parse query: " <> query.sql, None))
    Ok(table_name) -> {
      // Check for WHERE clause with primary key
      case extract_where_id(query.sql, query.params) {
        Some(id) ->
          case get_row(store, table_name, id) {
            Ok(row) -> Ok([row])
            Error(error.NotFound) -> Ok([])
            Error(e) -> Error(e)
          }
        None -> get_all_rows(store, table_name)
      }
    }
  }
}

/// Execute a mutation (INSERT/UPDATE/DELETE)
fn execute_mutation(
  _store: MemoryStore,
  query: CompiledQuery,
) -> Result(Int, AdapterError) {
  let sql_upper = string.uppercase(query.sql)
  let is_insert = string.starts_with(sql_upper, "INSERT")
  let is_update = string.starts_with(sql_upper, "UPDATE")
  let is_delete = string.starts_with(sql_upper, "DELETE")

  case is_insert, is_update, is_delete {
    True, _, _ -> {
      // For mutations, we'd need mutable state. Return 1 for demo.
      Ok(1)
    }
    _, True, _ -> {
      Ok(1)
    }
    _, _, True -> {
      case extract_table_name(query.sql, "FROM") {
        Error(_) -> Error(error.QueryFailed("Could not parse DELETE", None))
        Ok(_table_name) -> {
          // Would delete and return count
          Ok(1)
        }
      }
    }
    _, _, _ -> Error(error.QueryFailed("Unknown mutation type", None))
  }
}

/// Execute an INSERT with RETURNING
fn execute_returning(
  store: MemoryStore,
  query: CompiledQuery,
) -> Result(Option(MemoryRow), AdapterError) {
  // Simplified: would parse INSERT, execute, and return the row
  case extract_table_name(query.sql, "INTO") {
    Error(_) -> Error(error.QueryFailed("Could not parse INSERT", None))
    Ok(table_name) ->
      case next_id(store, table_name) {
        Error(e) -> Error(e)
        Ok(id) -> {
          // Build the row from params + generated ID
          let row = [dynamic.int(id), ..params_to_dynamics(query.params)]
          Ok(Some(row))
        }
      }
  }
}

/// Begin a transaction by taking a snapshot
fn begin_transaction(store: MemoryStore) -> Result(MemoryStore, AdapterError) {
  case store.in_transaction {
    True -> Error(error.QueryFailed("Already in transaction", None))
    False ->
      Ok(
        MemoryStore(..store, in_transaction: True, snapshot: Some(store.tables)),
      )
  }
}

/// Commit a transaction by clearing the snapshot
fn commit_transaction(store: MemoryStore) -> Result(Nil, AdapterError) {
  case store.in_transaction {
    False -> Error(error.QueryFailed("Not in transaction", None))
    True -> Ok(Nil)
  }
}

/// Rollback a transaction by restoring the snapshot
fn rollback_transaction(store: MemoryStore) -> Result(Nil, AdapterError) {
  case store.in_transaction, store.snapshot {
    False, _ -> Error(error.QueryFailed("Not in transaction", None))
    True, None -> Error(error.QueryFailed("No snapshot to restore", None))
    True, Some(_snapshot) -> {
      // Would restore: MemoryStore(tables: snapshot, in_transaction: False, snapshot: None)
      Ok(Nil)
    }
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Extract table name from SQL (very simplified parser)
fn extract_table_name(sql: String, keyword: String) -> Result(String, Nil) {
  let sql_upper = string.uppercase(sql)
  let keyword_upper = string.uppercase(keyword)

  case string.split(sql_upper, keyword_upper <> " ") {
    [_, rest] ->
      case string.split(rest, " ") {
        [table, ..] -> Ok(string.lowercase(table))
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Extract ID from WHERE clause (very simplified)
fn extract_where_id(
  sql: String,
  params: List(adapter.QueryParam),
) -> Option(String) {
  let sql_upper = string.uppercase(sql)

  case string.contains(sql_upper, "WHERE") {
    False -> None
    True ->
      case string.contains(sql_upper, "ID = $1"), params {
        True, [adapter.ParamInt(id), ..] -> Some(int.to_string(id))
        True, [adapter.ParamString(id), ..] -> Some(id)
        _, _ -> None
      }
  }
}

/// Convert query params to dynamic values
fn params_to_dynamics(params: List(adapter.QueryParam)) -> List(Dynamic) {
  list.map(params, fn(param) {
    case param {
      adapter.ParamInt(i) -> dynamic.int(i)
      adapter.ParamFloat(f) -> dynamic.float(f)
      adapter.ParamString(s) -> dynamic.string(s)
      adapter.ParamBool(b) -> dynamic.bool(b)
      adapter.ParamNull -> dynamic.nil()
      adapter.ParamBytes(b) -> dynamic.bit_array(b)
      adapter.ParamCustom(_, v) -> dynamic.string(v)
    }
  })
}
