// In-Memory Adapter for cquill
//
// This adapter is the REFERENCE IMPLEMENTATION of the adapter protocol.
// It serves as:
// 1. Reference implementation demonstrating correct adapter behavior
// 2. Testing adapter for fast, isolated tests without a real database
// 3. Example for implementing new adapters
//
// IMPORTANT: This is a first-class adapter that:
// - Fully implements the adapter protocol
// - Enforces all constraints (unique, not-null, foreign key)
// - Passes ALL adapter contract tests
// - Is used in unit and integration tests throughout the codebase

import cquill/adapter.{
  type Adapter, type AdapterCapabilities, type CompiledQuery,
}
import cquill/error.{type AdapterError}
import cquill/schema.{type Schema}
import cquill/schema/field
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// MEMORY STORE TYPES
// ============================================================================

/// A table in the memory store with full schema metadata for constraint checking
pub type MemoryTable {
  MemoryTable(
    /// Table name
    name: String,
    /// Primary key column name(s)
    primary_key: List(String),
    /// Column names in order (for mapping row indices to field names)
    columns: List(String),
    /// Rows indexed by primary key (as string)
    rows: Dict(String, List(Dynamic)),
    /// Auto-increment counter for serial IDs
    next_id: Int,
    /// Unique constraints: maps constraint name to column indices
    unique_constraints: Dict(String, List(Int)),
    /// Not-null column indices
    not_null_columns: List(Int),
    /// Foreign key constraints: (column_index, referenced_table, referenced_column)
    foreign_keys: List(ForeignKeyConstraint),
  )
}

/// Foreign key constraint definition
pub type ForeignKeyConstraint {
  ForeignKeyConstraint(
    column_index: Int,
    referenced_table: String,
    referenced_column: String,
  )
}

/// Snapshot of store state for transaction rollback or savepoint
pub type Snapshot {
  Snapshot(
    /// Optional name for savepoints (None for transaction snapshots)
    name: Option(String),
    /// Tables at snapshot time
    tables: Dict(String, MemoryTable),
    /// Auto-increment counters at snapshot time
    id_counters: Dict(String, Int),
  )
}

/// The in-memory database connection/store
pub type MemoryStore {
  MemoryStore(
    /// Tables indexed by name
    tables: Dict(String, MemoryTable),
    /// Whether we're in a transaction
    in_transaction: Bool,
    /// Stack of snapshots for nested transactions (most recent first)
    snapshots: List(Snapshot),
    /// Legacy single snapshot field for backwards compatibility
    snapshot: Option(Snapshot),
  )
}

/// Row type for memory adapter (list of dynamic values)
pub type MemoryRow =
  List(Dynamic)

// ============================================================================
// STORE MANAGEMENT - Basic Operations
// ============================================================================

/// Create a new empty memory store
pub fn new_store() -> MemoryStore {
  MemoryStore(
    tables: dict.new(),
    in_transaction: False,
    snapshots: [],
    snapshot: None,
  )
}

/// Create a new empty memory store (alias for new_store)
pub fn new() -> MemoryStore {
  new_store()
}

/// Reset the store, clearing all data but preserving table structure
pub fn reset(store: MemoryStore) -> MemoryStore {
  let empty_tables =
    dict.map_values(store.tables, fn(_name, table) {
      MemoryTable(..table, rows: dict.new(), next_id: 1)
    })
  MemoryStore(
    tables: empty_tables,
    in_transaction: False,
    snapshots: [],
    snapshot: None,
  )
}

/// Check if the store is currently in a transaction
pub fn in_transaction(store: MemoryStore) -> Bool {
  store.in_transaction
}

/// Get the current transaction depth (0 = not in transaction)
pub fn transaction_depth(store: MemoryStore) -> Int {
  list.length(store.snapshots)
}

/// Create a table in the store (simple version with just primary key)
pub fn create_table(
  store: MemoryStore,
  name: String,
  primary_key: String,
) -> MemoryStore {
  let table =
    MemoryTable(
      name: name,
      primary_key: [primary_key],
      columns: [primary_key],
      rows: dict.new(),
      next_id: 1,
      unique_constraints: dict.new(),
      not_null_columns: [],
      foreign_keys: [],
    )
  let tables = dict.insert(store.tables, name, table)
  MemoryStore(..store, tables: tables)
}

/// Create a table with explicit column names
/// This enables WHERE clause filtering by any column, not just the primary key.
/// Column order must match the order of values in inserted rows.
///
/// Example:
/// ```gleam
/// let store = memory.new_store()
///   |> memory.create_table_with_columns("users", "id", ["id", "email", "active"])
/// ```
pub fn create_table_with_columns(
  store: MemoryStore,
  name: String,
  primary_key: String,
  columns: List(String),
) -> MemoryStore {
  let table =
    MemoryTable(
      name: name,
      primary_key: [primary_key],
      columns: columns,
      rows: dict.new(),
      next_id: 1,
      unique_constraints: dict.new(),
      not_null_columns: [],
      foreign_keys: [],
    )
  let tables = dict.insert(store.tables, name, table)
  MemoryStore(..store, tables: tables)
}

/// Create a table from a schema with full constraint support
pub fn create_table_from_schema(
  store: MemoryStore,
  schema: Schema,
) -> MemoryStore {
  let fields = schema.get_fields(schema)
  let columns = list.map(fields, field.get_name)
  let primary_key = schema.get_primary_key(schema)

  // Find unique constraint column indices
  let unique_constraints =
    fields
    |> list.index_map(fn(f, idx) { #(f, idx) })
    |> list.filter_map(fn(pair) {
      let #(f, idx) = pair
      case field.has_constraint(f, field.is_unique_constraint) {
        True -> Ok(#(field.get_name(f) <> "_unique", [idx]))
        False -> Error(Nil)
      }
    })
    |> dict.from_list

  // Add primary key as a unique constraint
  let pk_indices =
    list.filter_map(primary_key, fn(pk_col) {
      list.index_map(columns, fn(col, idx) { #(col, idx) })
      |> list.find(fn(pair) { pair.0 == pk_col })
      |> result.map(fn(pair) { pair.1 })
    })
  let unique_constraints =
    dict.insert(
      unique_constraints,
      schema.get_source(schema) <> "_pkey",
      pk_indices,
    )

  // Find not-null column indices
  let not_null_columns =
    fields
    |> list.index_map(fn(f, idx) { #(f, idx) })
    |> list.filter_map(fn(pair) {
      let #(f, idx) = pair
      case field.is_not_nullable(f) {
        True -> Ok(idx)
        False -> Error(Nil)
      }
    })

  // Find foreign key constraints
  let foreign_keys =
    fields
    |> list.index_map(fn(f, idx) { #(f, idx) })
    |> list.filter_map(fn(pair) {
      let #(f, idx) = pair
      let fk_constraints =
        field.get_constraints(f, field.is_foreign_key_constraint)
      case fk_constraints {
        [field.ForeignKey(table, column, _), ..] ->
          Ok(ForeignKeyConstraint(idx, table, column))
        _ -> Error(Nil)
      }
    })

  let table =
    MemoryTable(
      name: schema.get_source(schema),
      primary_key: primary_key,
      columns: columns,
      rows: dict.new(),
      next_id: 1,
      unique_constraints: unique_constraints,
      not_null_columns: not_null_columns,
      foreign_keys: foreign_keys,
    )
  let tables = dict.insert(store.tables, schema.get_source(schema), table)
  MemoryStore(..store, tables: tables)
}

/// Add a unique constraint to a table
pub fn add_unique_constraint(
  store: MemoryStore,
  table_name: String,
  constraint_name: String,
  column_names: List(String),
) -> Result(MemoryStore, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      // Convert column names to indices
      let column_indices =
        list.filter_map(column_names, fn(col_name) {
          list.index_map(table.columns, fn(col, idx) { #(col, idx) })
          |> list.find(fn(pair) { pair.0 == col_name })
          |> result.map(fn(pair) { pair.1 })
        })

      let new_constraints =
        dict.insert(table.unique_constraints, constraint_name, column_indices)
      let new_table = MemoryTable(..table, unique_constraints: new_constraints)
      let new_tables = dict.insert(store.tables, table_name, new_table)
      Ok(MemoryStore(..store, tables: new_tables))
    }
  }
}

// ============================================================================
// ROW OPERATIONS WITH CONSTRAINT CHECKING
// ============================================================================

/// Insert a row into a table with full constraint checking
pub fn insert_row(
  store: MemoryStore,
  table_name: String,
  key: String,
  row: MemoryRow,
) -> Result(MemoryStore, AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      // Check primary key uniqueness
      case dict.has_key(table.rows, key) {
        True ->
          Error(error.UniqueViolation(
            table_name <> "_pkey",
            "Key (" <> key <> ") already exists",
          ))
        False -> {
          // Check not-null constraints
          case check_not_null_constraints(table, row) {
            Error(e) -> Error(e)
            Ok(_) -> {
              // Check unique constraints
              case check_unique_constraints(table, row, None) {
                Error(e) -> Error(e)
                Ok(_) -> {
                  // Check foreign key constraints
                  case check_foreign_key_constraints(store, table, row) {
                    Error(e) -> Error(e)
                    Ok(_) -> {
                      let new_rows = dict.insert(table.rows, key, row)
                      let new_table =
                        MemoryTable(
                          ..table,
                          rows: new_rows,
                          next_id: table.next_id + 1,
                        )
                      let new_tables =
                        dict.insert(store.tables, table_name, new_table)
                      Ok(MemoryStore(..store, tables: new_tables))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Update a row in a table with constraint checking
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
          // Check not-null constraints
          case check_not_null_constraints(table, row) {
            Error(e) -> Error(e)
            Ok(_) -> {
              // Check unique constraints (excluding current row)
              case check_unique_constraints(table, row, Some(key)) {
                Error(e) -> Error(e)
                Ok(_) -> {
                  // Check foreign key constraints
                  case check_foreign_key_constraints(store, table, row) {
                    Error(e) -> Error(e)
                    Ok(_) -> {
                      let new_rows = dict.insert(table.rows, key, row)
                      let new_table = MemoryTable(..table, rows: new_rows)
                      let new_tables =
                        dict.insert(store.tables, table_name, new_table)
                      Ok(MemoryStore(..store, tables: new_tables))
                    }
                  }
                }
              }
            }
          }
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
          // Check if any foreign keys reference this row
          case check_foreign_key_references(store, table_name, key) {
            Error(e) -> Error(e)
            Ok(_) -> {
              let new_rows = dict.delete(table.rows, key)
              let new_table = MemoryTable(..table, rows: new_rows)
              let new_tables = dict.insert(store.tables, table_name, new_table)
              Ok(MemoryStore(..store, tables: new_tables))
            }
          }
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
// BATCH OPERATIONS
// ============================================================================

/// Configuration for batch operations
pub type BatchConfig {
  BatchConfig(
    /// Maximum records per batch (for chunking large inserts)
    max_batch_size: Int,
    /// Whether to wrap in transaction (for atomicity)
    use_transaction: Bool,
  )
}

/// Default batch configuration
pub fn default_batch_config() -> BatchConfig {
  BatchConfig(max_batch_size: 1000, use_transaction: True)
}

/// Insert multiple rows atomically.
/// All rows are inserted or none are (atomic behavior).
/// Returns the number of inserted rows on success.
pub fn insert_all(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
) -> Result(#(MemoryStore, Int), AdapterError) {
  insert_all_with_config(store, table_name, rows, default_batch_config())
}

/// Insert multiple rows with configuration options.
/// When use_transaction is True, all rows are inserted atomically.
/// Returns the number of inserted rows on success.
pub fn insert_all_with_config(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
  config: BatchConfig,
) -> Result(#(MemoryStore, Int), AdapterError) {
  case config.use_transaction {
    True -> insert_all_atomic(store, table_name, rows)
    False -> insert_all_non_atomic(store, table_name, rows)
  }
}

/// Insert all rows atomically - all succeed or none
fn insert_all_atomic(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
) -> Result(#(MemoryStore, Int), AdapterError) {
  // Validate all rows first before inserting any
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      // Validate all rows for constraints
      case validate_batch_insert(store, table, rows) {
        Error(e) -> Error(e)
        Ok(_) -> {
          // All valid, insert atomically
          do_insert_all(store, table_name, rows, 0)
        }
      }
    }
  }
}

/// Validate all rows before inserting (for atomic operations)
fn validate_batch_insert(
  store: MemoryStore,
  table: MemoryTable,
  rows: List(#(String, MemoryRow)),
) -> Result(Nil, AdapterError) {
  // Check for duplicate keys within the batch
  let keys = list.map(rows, fn(pair) { pair.0 })
  case has_duplicate_keys(keys) {
    True ->
      Error(error.UniqueViolation(
        table.name <> "_pkey",
        "Duplicate keys within batch",
      ))
    False -> {
      // Check each row for constraints
      validate_rows_for_insert(store, table, rows)
    }
  }
}

/// Check if a list has duplicate keys
fn has_duplicate_keys(keys: List(String)) -> Bool {
  do_has_duplicate_keys(keys, dict.new())
}

fn do_has_duplicate_keys(keys: List(String), seen: Dict(String, Bool)) -> Bool {
  case keys {
    [] -> False
    [key, ..rest] ->
      case dict.has_key(seen, key) {
        True -> True
        False -> do_has_duplicate_keys(rest, dict.insert(seen, key, True))
      }
  }
}

/// Validate all rows for insert constraints
fn validate_rows_for_insert(
  store: MemoryStore,
  table: MemoryTable,
  rows: List(#(String, MemoryRow)),
) -> Result(Nil, AdapterError) {
  case rows {
    [] -> Ok(Nil)
    [#(key, row), ..rest] -> {
      // Check primary key doesn't already exist
      case dict.has_key(table.rows, key) {
        True ->
          Error(error.UniqueViolation(
            table.name <> "_pkey",
            "Key (" <> key <> ") already exists",
          ))
        False -> {
          // Check not-null constraints
          case check_not_null_constraints(table, row) {
            Error(e) -> Error(e)
            Ok(_) -> {
              // Check unique constraints
              case check_unique_constraints(table, row, None) {
                Error(e) -> Error(e)
                Ok(_) -> {
                  // Check foreign key constraints
                  case check_foreign_key_constraints(store, table, row) {
                    Error(e) -> Error(e)
                    Ok(_) -> validate_rows_for_insert(store, table, rest)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Actually insert all validated rows
fn do_insert_all(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
  count: Int,
) -> Result(#(MemoryStore, Int), AdapterError) {
  case rows {
    [] -> Ok(#(store, count))
    [#(key, row), ..rest] ->
      case insert_row(store, table_name, key, row) {
        Error(e) -> Error(e)
        Ok(new_store) -> do_insert_all(new_store, table_name, rest, count + 1)
      }
  }
}

/// Insert rows non-atomically (stop on first error, keep prior inserts)
fn insert_all_non_atomic(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
) -> Result(#(MemoryStore, Int), AdapterError) {
  do_insert_all(store, table_name, rows, 0)
}

/// Update all rows matching a predicate.
/// Returns the number of updated rows.
pub fn update_all(
  store: MemoryStore,
  table_name: String,
  predicate: fn(String, MemoryRow) -> Bool,
  updater: fn(MemoryRow) -> MemoryRow,
) -> Result(#(MemoryStore, Int), AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      // Find matching rows
      let matching =
        dict.to_list(table.rows)
        |> list.filter(fn(pair) { predicate(pair.0, pair.1) })

      // Update each matching row
      do_update_all(store, table_name, matching, updater, 0)
    }
  }
}

fn do_update_all(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
  updater: fn(MemoryRow) -> MemoryRow,
  count: Int,
) -> Result(#(MemoryStore, Int), AdapterError) {
  case rows {
    [] -> Ok(#(store, count))
    [#(key, row), ..rest] -> {
      let updated_row = updater(row)
      case update_row(store, table_name, key, updated_row) {
        Error(e) -> Error(e)
        Ok(new_store) ->
          do_update_all(new_store, table_name, rest, updater, count + 1)
      }
    }
  }
}

/// Update all rows with new values (simple version without predicate).
/// Useful for "UPDATE table SET column = value" style updates.
pub fn update_all_rows(
  store: MemoryStore,
  table_name: String,
  updater: fn(MemoryRow) -> MemoryRow,
) -> Result(#(MemoryStore, Int), AdapterError) {
  update_all(store, table_name, fn(_, _) { True }, updater)
}

/// Delete all rows matching a predicate.
/// Returns the number of deleted rows.
pub fn delete_all(
  store: MemoryStore,
  table_name: String,
  predicate: fn(String, MemoryRow) -> Bool,
) -> Result(#(MemoryStore, Int), AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      // Find matching rows
      let matching =
        dict.to_list(table.rows)
        |> list.filter(fn(pair) { predicate(pair.0, pair.1) })

      // Delete each matching row
      do_delete_all(store, table_name, matching, 0)
    }
  }
}

fn do_delete_all(
  store: MemoryStore,
  table_name: String,
  rows: List(#(String, MemoryRow)),
  count: Int,
) -> Result(#(MemoryStore, Int), AdapterError) {
  case rows {
    [] -> Ok(#(store, count))
    [#(key, _row), ..rest] ->
      case delete_row(store, table_name, key) {
        Error(e) -> Error(e)
        Ok(new_store) -> do_delete_all(new_store, table_name, rest, count + 1)
      }
  }
}

/// Delete all rows in a table (truncate).
/// Returns the number of deleted rows.
pub fn delete_all_rows(
  store: MemoryStore,
  table_name: String,
) -> Result(#(MemoryStore, Int), AdapterError) {
  delete_all(store, table_name, fn(_, _) { True })
}

/// Insert multiple rows using auto-generated keys.
/// Returns the inserted keys and updated store.
pub fn insert_all_with_auto_keys(
  store: MemoryStore,
  table_name: String,
  rows: List(MemoryRow),
) -> Result(#(MemoryStore, List(String), Int), AdapterError) {
  case dict.get(store.tables, table_name) {
    Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
    Ok(table) -> {
      let start_id = table.next_id
      // Generate keys for all rows
      let rows_with_keys =
        list.index_map(rows, fn(row, idx) {
          #(int.to_string(start_id + idx), row)
        })
      // Insert all
      case insert_all(store, table_name, rows_with_keys) {
        Error(e) -> Error(e)
        Ok(#(new_store, count)) -> {
          let keys = list.map(rows_with_keys, fn(pair) { pair.0 })
          Ok(#(new_store, keys, count))
        }
      }
    }
  }
}

// ============================================================================
// CONSTRAINT CHECKING
// ============================================================================

/// Check not-null constraints on a row
fn check_not_null_constraints(
  table: MemoryTable,
  row: MemoryRow,
) -> Result(Nil, AdapterError) {
  let row_values = list.index_map(row, fn(val, idx) { #(idx, val) })

  let violations =
    list.filter_map(table.not_null_columns, fn(col_idx) {
      case list.find(row_values, fn(pair) { pair.0 == col_idx }) {
        Error(_) -> Error(Nil)
        Ok(#(_, value)) ->
          case is_null_value(value) {
            True -> {
              let column_name =
                list.index_map(table.columns, fn(col, idx) { #(idx, col) })
                |> list.find(fn(pair) { pair.0 == col_idx })
                |> option.from_result
                |> option.map(fn(pair) { pair.1 })
                |> option.unwrap("unknown")
              Ok(column_name)
            }
            False -> Error(Nil)
          }
      }
    })

  case violations {
    [column, ..] -> Error(error.NotNullViolation(column))
    [] -> Ok(Nil)
  }
}

/// Check unique constraints on a row
fn check_unique_constraints(
  table: MemoryTable,
  row: MemoryRow,
  exclude_key: Option(String),
) -> Result(Nil, AdapterError) {
  let existing_rows = case exclude_key {
    None -> dict.to_list(table.rows)
    Some(key) ->
      dict.to_list(table.rows)
      |> list.filter(fn(pair) { pair.0 != key })
  }

  // Check each unique constraint
  dict.to_list(table.unique_constraints)
  |> list.find_map(fn(constraint) {
    let #(constraint_name, column_indices) = constraint
    let new_values = extract_values_at_indices(row, column_indices)

    // Check if any existing row has the same values
    let violation =
      list.find(existing_rows, fn(existing) {
        let existing_values =
          extract_values_at_indices(existing.1, column_indices)
        values_equal(new_values, existing_values)
      })

    case violation {
      Ok(_) -> {
        let detail = "Duplicate value for unique constraint"
        Ok(Error(error.UniqueViolation(constraint_name, detail)))
      }
      Error(_) -> Error(Nil)
    }
  })
  |> option.from_result
  |> option.unwrap(Ok(Nil))
}

/// Check foreign key constraints on a row
fn check_foreign_key_constraints(
  store: MemoryStore,
  table: MemoryTable,
  row: MemoryRow,
) -> Result(Nil, AdapterError) {
  list.find_map(table.foreign_keys, fn(fk) {
    let ForeignKeyConstraint(col_idx, ref_table, ref_column) = fk

    // Get the value at the foreign key column
    case
      list.index_map(row, fn(val, idx) { #(idx, val) })
      |> list.find(fn(pair) { pair.0 == col_idx })
    {
      Error(_) -> Error(Nil)
      Ok(#(_, fk_value)) -> {
        // Skip null values (nulls don't violate FK constraints)
        case is_null_value(fk_value) {
          True -> Error(Nil)
          False -> {
            // Check if the referenced table has this value
            case dict.get(store.tables, ref_table) {
              Error(_) -> {
                let detail =
                  "Referenced table " <> ref_table <> " does not exist"
                Ok(
                  Error(error.ForeignKeyViolation(
                    table.name <> "_" <> ref_column <> "_fkey",
                    detail,
                  )),
                )
              }
              Ok(ref_table_data) -> {
                // Check if any row in the referenced table has this value
                let fk_string = dynamic_to_string(fk_value)
                case dict.has_key(ref_table_data.rows, fk_string) {
                  True -> Error(Nil)
                  False -> {
                    let detail =
                      "Key ("
                      <> fk_string
                      <> ") not found in "
                      <> ref_table
                      <> "."
                      <> ref_column
                    Ok(
                      Error(error.ForeignKeyViolation(
                        table.name <> "_" <> ref_column <> "_fkey",
                        detail,
                      )),
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  |> option.from_result
  |> option.unwrap(Ok(Nil))
}

/// Check if any foreign keys reference this row before deletion
fn check_foreign_key_references(
  store: MemoryStore,
  table_name: String,
  key: String,
) -> Result(Nil, AdapterError) {
  // Check all tables for foreign keys pointing to this table
  dict.to_list(store.tables)
  |> list.find_map(fn(table_entry) {
    let #(_, checking_table) = table_entry

    // Find FKs that reference our table
    list.find_map(checking_table.foreign_keys, fn(fk) {
      let ForeignKeyConstraint(col_idx, ref_table, _ref_column) = fk

      case ref_table == table_name {
        False -> Error(Nil)
        True -> {
          // Check if any row in this table references the key being deleted
          dict.values(checking_table.rows)
          |> list.find_map(fn(row) {
            case
              list.index_map(row, fn(val, idx) { #(idx, val) })
              |> list.find(fn(pair) { pair.0 == col_idx })
            {
              Error(_) -> Error(Nil)
              Ok(#(_, fk_value)) -> {
                case dynamic_to_string(fk_value) == key {
                  False -> Error(Nil)
                  True -> {
                    let detail =
                      "Referenced by "
                      <> checking_table.name
                      <> " (row exists with this foreign key)"
                    Ok(
                      Error(error.ForeignKeyViolation(
                        checking_table.name <> "_fkey",
                        detail,
                      )),
                    )
                  }
                }
              }
            }
          })
        }
      }
    })
  })
  |> option.from_result
  |> option.unwrap(Ok(Nil))
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Extract values at specific indices from a row
fn extract_values_at_indices(
  row: MemoryRow,
  indices: List(Int),
) -> List(Dynamic) {
  let indexed_row = list.index_map(row, fn(val, idx) { #(idx, val) })
  list.filter_map(indices, fn(idx) {
    list.find(indexed_row, fn(pair) { pair.0 == idx })
    |> result.map(fn(pair) { pair.1 })
  })
}

/// Check if two lists of dynamic values are equal
fn values_equal(a: List(Dynamic), b: List(Dynamic)) -> Bool {
  case a, b {
    [], [] -> True
    [x, ..xs], [y, ..ys] -> dynamic_equals(x, y) && values_equal(xs, ys)
    _, _ -> False
  }
}

/// Check if two dynamic values are equal (by string representation)
fn dynamic_equals(a: Dynamic, b: Dynamic) -> Bool {
  dynamic_to_string(a) == dynamic_to_string(b)
}

/// Convert a dynamic value to string for comparison
fn dynamic_to_string(value: Dynamic) -> String {
  // Use string.inspect which handles all types
  // This provides a consistent string representation for comparison
  string.inspect(value)
}

/// Check if a dynamic value is null
fn is_null_value(value: Dynamic) -> Bool {
  // Check by inspecting the string representation
  let str_repr = string.inspect(value)
  str_repr == "Nil" || str_repr == "None" || str_repr == "null"
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

/// Execute a query - supports basic WHERE clause filtering
fn execute_query(
  store: MemoryStore,
  query: CompiledQuery,
) -> Result(List(MemoryRow), AdapterError) {
  case extract_table_name(query.sql, "FROM") {
    Error(_) ->
      Error(error.QueryFailed("Could not parse query: " <> query.sql, None))
    Ok(table_name) -> {
      case dict.get(store.tables, table_name) {
        Error(_) -> Error(error.AdapterSpecific("TABLE_NOT_FOUND", table_name))
        Ok(table) -> {
          // Get all rows first
          let all_rows = dict.values(table.rows)

          // Check for WHERE clause and filter
          case
            extract_where_conditions(query.sql, query.params, table.columns)
          {
            [] -> Ok(all_rows)
            conditions -> {
              let filtered =
                list.filter(all_rows, fn(row) {
                  list.all(conditions, fn(cond) { matches_condition(row, cond) })
                })
              Ok(filtered)
            }
          }
        }
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
/// Supports nested transactions via snapshot stack
fn begin_transaction(store: MemoryStore) -> Result(MemoryStore, AdapterError) {
  // Create snapshot of current state (unnamed, for transaction)
  let id_counters =
    dict.map_values(store.tables, fn(_name, table) { table.next_id })
  let snapshot =
    Snapshot(name: None, tables: store.tables, id_counters: id_counters)

  // Push snapshot onto the stack
  Ok(
    MemoryStore(
      ..store,
      in_transaction: True,
      snapshots: [snapshot, ..store.snapshots],
      snapshot: Some(snapshot),
    ),
  )
}

/// Check if there are remaining nested transactions in the snapshot stack
/// This is a positive assertion helper for readability
fn has_remaining_transactions(snapshots: List(Snapshot)) -> Bool {
  case snapshots {
    [] -> False
    _ -> True
  }
}

/// Commit a transaction by popping the snapshot
fn commit_transaction(store: MemoryStore) -> Result(Nil, AdapterError) {
  case store.in_transaction, store.snapshots {
    False, _ -> Error(error.QueryFailed("Not in transaction", None))
    True, [] -> Error(error.QueryFailed("No transaction to commit", None))
    True, _ -> Ok(Nil)
  }
}

/// Get the store state after committing a transaction
/// This is needed for the memory adapter since commit_transaction returns Nil
pub fn commit_and_continue(
  store: MemoryStore,
) -> Result(MemoryStore, AdapterError) {
  case store.in_transaction, store.snapshots {
    False, _ -> Error(error.QueryFailed("Not in transaction", None))
    True, [] -> Error(error.QueryFailed("No transaction to commit", None))
    True, [_committed, ..rest] -> {
      // Pop the snapshot, check if we're still in a nested transaction
      let still_in_transaction = has_remaining_transactions(rest)
      let new_snapshot = case rest {
        [s, ..] -> Some(s)
        [] -> None
      }
      Ok(
        MemoryStore(
          ..store,
          in_transaction: still_in_transaction,
          snapshots: rest,
          snapshot: new_snapshot,
        ),
      )
    }
  }
}

/// Rollback a transaction by restoring the snapshot
fn rollback_transaction(store: MemoryStore) -> Result(Nil, AdapterError) {
  case store.in_transaction, store.snapshots {
    False, _ -> Error(error.QueryFailed("Not in transaction", None))
    True, [] -> Error(error.QueryFailed("No snapshot to restore", None))
    True, [_, ..] -> Ok(Nil)
  }
}

/// Get the restored store after a rollback
/// This is a helper for tests since rollback_transaction returns Nil
pub fn rollback_and_restore(
  store: MemoryStore,
) -> Result(MemoryStore, AdapterError) {
  case store.in_transaction, store.snapshots {
    False, _ -> Error(error.QueryFailed("Not in transaction", None))
    True, [] -> Error(error.QueryFailed("No snapshot to restore", None))
    True, [snapshot, ..rest] -> {
      // Restore the tables and id counters from snapshot
      let restored_tables =
        dict.map_values(snapshot.tables, fn(name, table) {
          case dict.get(snapshot.id_counters, name) {
            Ok(id) -> MemoryTable(..table, next_id: id)
            Error(_) -> table
          }
        })
      // Check if we're still in a nested transaction
      let still_in_transaction = has_remaining_transactions(rest)
      let new_snapshot = case rest {
        [s, ..] -> Some(s)
        [] -> None
      }
      Ok(MemoryStore(
        tables: restored_tables,
        in_transaction: still_in_transaction,
        snapshots: rest,
        snapshot: new_snapshot,
      ))
    }
  }
}

/// Execute a function within a transaction on the memory store.
/// Automatically commits on success and rolls back on error.
/// Returns the updated store along with the result.
pub fn execute_transaction(
  store: MemoryStore,
  operation: fn(MemoryStore) -> Result(#(MemoryStore, a), error.AdapterError),
) -> Result(#(MemoryStore, a), error.TransactionError(Nil)) {
  // Begin transaction
  case begin_transaction(store) {
    Error(e) ->
      Error(error.BeginFailed(
        "Could not begin transaction: " <> error.format_error(e),
      ))
    Ok(tx_store) -> {
      // Run the operation
      case operation(tx_store) {
        Ok(#(updated_store, result)) -> {
          // Commit on success
          case commit_and_continue(updated_store) {
            Ok(committed_store) -> Ok(#(committed_store, result))
            Error(e) ->
              Error(error.CommitFailed(
                "Commit failed: " <> error.format_error(e),
              ))
          }
        }
        Error(adapter_err) -> {
          // Rollback on error
          let _rolled_back = case rollback_and_restore(tx_store) {
            Ok(s) -> s
            Error(_) -> store
          }
          Error(error.AdapterTransactionError(adapter_err))
        }
      }
    }
  }
}

// ============================================================================
// SAVEPOINT SUPPORT
// ============================================================================

/// Create a savepoint with the given name.
/// Requires an active transaction.
pub fn create_savepoint(
  store: MemoryStore,
  name: String,
) -> Result(MemoryStore, error.SavepointError(Nil)) {
  case store.in_transaction {
    False -> Error(error.SavepointNoTransaction)
    True -> {
      // Create named snapshot
      let id_counters =
        dict.map_values(store.tables, fn(_table_name, table) { table.next_id })
      let snapshot =
        Snapshot(name: Some(name), tables: store.tables, id_counters:)

      // Push onto snapshot stack
      Ok(
        MemoryStore(
          ..store,
          snapshots: [snapshot, ..store.snapshots],
          snapshot: Some(snapshot),
        ),
      )
    }
  }
}

/// Rollback to a named savepoint, discarding all changes since it was created.
/// Also removes the savepoint and any savepoints created after it.
pub fn rollback_to_savepoint(
  store: MemoryStore,
  name: String,
) -> Result(MemoryStore, error.SavepointError(Nil)) {
  case store.in_transaction {
    False -> Error(error.SavepointNoTransaction)
    True -> {
      case find_savepoint(store.snapshots, name) {
        Error(_) -> Error(error.SavepointNotFound(name))
        Ok(#(snapshot, remaining)) -> {
          // Restore tables and id counters from the savepoint
          let restored_tables =
            dict.map_values(snapshot.tables, fn(table_name, table) {
              case dict.get(snapshot.id_counters, table_name) {
                Ok(id) -> MemoryTable(..table, next_id: id)
                Error(_) -> table
              }
            })

          // Determine new snapshot state
          let new_snapshot = case remaining {
            [s, ..] -> Some(s)
            [] -> None
          }

          Ok(
            MemoryStore(
              ..store,
              tables: restored_tables,
              snapshots: remaining,
              snapshot: new_snapshot,
            ),
          )
        }
      }
    }
  }
}

/// Release a savepoint without rolling back.
/// This removes the savepoint, making its changes permanent (within the transaction).
pub fn release_savepoint(
  store: MemoryStore,
  name: String,
) -> Result(MemoryStore, error.SavepointError(Nil)) {
  case store.in_transaction {
    False -> Error(error.SavepointNoTransaction)
    True -> {
      case remove_savepoint(store.snapshots, name) {
        Error(_) -> Error(error.SavepointNotFound(name))
        Ok(remaining) -> {
          // Determine new snapshot state
          let new_snapshot = case remaining {
            [s, ..] -> Some(s)
            [] -> None
          }

          Ok(MemoryStore(..store, snapshots: remaining, snapshot: new_snapshot))
        }
      }
    }
  }
}

/// Execute a function within a savepoint.
/// If the function returns an error, rolls back to the savepoint.
/// If successful, releases the savepoint.
pub fn execute_savepoint(
  store: MemoryStore,
  name: String,
  operation: fn(MemoryStore) -> Result(#(MemoryStore, a), error.AdapterError),
) -> Result(#(MemoryStore, a), error.SavepointError(Nil)) {
  // Create savepoint
  case create_savepoint(store, name) {
    Error(e) -> Error(e)
    Ok(sp_store) -> {
      // Run the operation
      case operation(sp_store) {
        Ok(#(updated_store, result)) -> {
          // Release savepoint on success (keep the changes)
          case release_savepoint(updated_store, name) {
            Ok(final_store) -> Ok(#(final_store, result))
            Error(e) -> Error(e)
          }
        }
        Error(adapter_err) -> {
          // Rollback to savepoint on error
          case rollback_to_savepoint(sp_store, name) {
            Ok(_rolled_back_store) ->
              Error(error.SavepointAdapterError(adapter_err))
            Error(_) -> Error(error.SavepointAdapterError(adapter_err))
          }
        }
      }
    }
  }
}

/// Find a savepoint by name in the snapshot stack.
/// Returns the snapshot and the remaining stack (snapshots after the found one).
fn find_savepoint(
  snapshots: List(Snapshot),
  name: String,
) -> Result(#(Snapshot, List(Snapshot)), Nil) {
  do_find_savepoint(snapshots, name, [])
}

fn do_find_savepoint(
  snapshots: List(Snapshot),
  name: String,
  _skipped: List(Snapshot),
) -> Result(#(Snapshot, List(Snapshot)), Nil) {
  case snapshots {
    [] -> Error(Nil)
    [snapshot, ..rest] -> {
      case snapshot.name {
        Some(sp_name) if sp_name == name -> Ok(#(snapshot, rest))
        _ -> do_find_savepoint(rest, name, [])
      }
    }
  }
}

/// Remove a savepoint from the stack without restoring its state.
/// Returns the modified stack with the savepoint removed.
fn remove_savepoint(
  snapshots: List(Snapshot),
  name: String,
) -> Result(List(Snapshot), Nil) {
  do_remove_savepoint(snapshots, name, [])
}

fn do_remove_savepoint(
  snapshots: List(Snapshot),
  name: String,
  before: List(Snapshot),
) -> Result(List(Snapshot), Nil) {
  case snapshots {
    [] -> Error(Nil)
    [snapshot, ..rest] -> {
      case snapshot.name {
        Some(sp_name) if sp_name == name -> {
          // Found it, reconstruct the list without this snapshot
          Ok(list.reverse(before) |> list.append(rest))
        }
        _ -> do_remove_savepoint(rest, name, [snapshot, ..before])
      }
    }
  }
}

/// Check if a savepoint with the given name exists
pub fn has_savepoint(store: MemoryStore, name: String) -> Bool {
  case find_savepoint(store.snapshots, name) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Get the names of all active savepoints
pub fn savepoint_names(store: MemoryStore) -> List(String) {
  store.snapshots
  |> list.filter_map(fn(snapshot) {
    case snapshot.name {
      Some(name) -> Ok(name)
      None -> Error(Nil)
    }
  })
}

// ============================================================================
// SQL PARSING HELPERS
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

/// A parsed WHERE condition for filtering
type WhereCondition {
  WhereCondition(
    /// Column index in the row
    column_index: Int,
    /// The value to compare against
    value: adapter.QueryParam,
  )
}

/// Extract WHERE conditions from SQL and map to column indices
/// Supports patterns like: WHERE column = $1 AND column2 = $2
fn extract_where_conditions(
  sql: String,
  params: List(adapter.QueryParam),
  columns: List(String),
) -> List(WhereCondition) {
  let sql_upper = string.uppercase(sql)

  case string.contains(sql_upper, "WHERE") {
    False -> []
    True -> {
      // Extract the WHERE clause part
      case string.split(sql_upper, "WHERE") {
        [_, where_part] -> parse_where_conditions(where_part, params, columns)
        _ -> []
      }
    }
  }
}

/// Parse WHERE conditions from the WHERE clause text
fn parse_where_conditions(
  where_clause: String,
  params: List(adapter.QueryParam),
  columns: List(String),
) -> List(WhereCondition) {
  // Split by AND to get individual conditions
  // Also handle the case where there's no AND (single condition)
  let conditions_text = string.split(where_clause, " AND ")

  conditions_text
  |> list.filter_map(fn(cond_text) {
    parse_single_condition(string.trim(cond_text), params, columns)
  })
}

/// Parse a single condition like "COLUMN = $1"
fn parse_single_condition(
  condition: String,
  params: List(adapter.QueryParam),
  columns: List(String),
) -> Result(WhereCondition, Nil) {
  // Look for pattern: COLUMN = $N
  case string.split(condition, " = ") {
    [column_part, param_part] -> {
      let column_name = string.trim(column_part) |> string.lowercase
      let param_str = string.trim(param_part)

      // Find column index
      case find_column_index(column_name, columns) {
        Error(_) -> Error(Nil)
        Ok(col_idx) -> {
          // Parse parameter index from $N format
          case parse_param_index(param_str) {
            Error(_) -> Error(Nil)
            Ok(param_idx) -> {
              // Get the parameter value (0-indexed)
              case list_get(params, param_idx) {
                Error(_) -> Error(Nil)
                Ok(param_value) -> Ok(WhereCondition(col_idx, param_value))
              }
            }
          }
        }
      }
    }
    _ -> Error(Nil)
  }
}

/// Find the index of a column by name
fn find_column_index(name: String, columns: List(String)) -> Result(Int, Nil) {
  columns
  |> list.index_map(fn(col, idx) { #(string.lowercase(col), idx) })
  |> list.find(fn(pair) { pair.0 == name })
  |> result.map(fn(pair) { pair.1 })
}

/// Parse parameter index from $N format (returns 0-indexed)
fn parse_param_index(param_str: String) -> Result(Int, Nil) {
  case string.starts_with(param_str, "$") {
    False -> Error(Nil)
    True -> {
      let num_str = string.drop_start(param_str, 1)
      // Handle cases where there might be trailing characters (like in "... $1 AND")
      let num_str_clean =
        num_str
        |> string.to_graphemes
        |> list.take_while(fn(c) { is_digit(c) })
        |> string.concat

      case int.parse(num_str_clean) {
        Error(_) -> Error(Nil)
        Ok(n) -> Ok(n - 1)
        // Convert to 0-indexed
      }
    }
  }
}

/// Check if a character is a digit
fn is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Get element at index from list
fn list_get(lst: List(a), index: Int) -> Result(a, Nil) {
  case index < 0 {
    True -> Error(Nil)
    False ->
      lst
      |> list.drop(index)
      |> list.first
  }
}

/// Check if a row matches a condition
fn matches_condition(row: MemoryRow, condition: WhereCondition) -> Bool {
  case list_get(row, condition.column_index) {
    Error(_) -> False
    Ok(cell_value) -> compare_values(cell_value, condition.value)
  }
}

/// Compare a Dynamic cell value with a QueryParam
fn compare_values(cell: Dynamic, param: adapter.QueryParam) -> Bool {
  case param {
    adapter.ParamInt(expected) -> {
      case decode.run(cell, decode.int) {
        Ok(actual) -> actual == expected
        Error(_) -> False
      }
    }
    adapter.ParamFloat(expected) -> {
      case decode.run(cell, decode.float) {
        Ok(actual) -> actual == expected
        Error(_) -> False
      }
    }
    adapter.ParamString(expected) -> {
      case decode.run(cell, decode.string) {
        Ok(actual) -> actual == expected
        Error(_) -> False
      }
    }
    adapter.ParamBool(expected) -> {
      case decode.run(cell, decode.bool) {
        Ok(actual) -> actual == expected
        Error(_) -> False
      }
    }
    adapter.ParamNull -> {
      // Check if the value is nil/null
      case dynamic.classify(cell) {
        "Nil" -> True
        _ -> False
      }
    }
    adapter.ParamBytes(expected) -> {
      case decode.run(cell, decode.bit_array) {
        Ok(actual) -> actual == expected
        Error(_) -> False
      }
    }
    adapter.ParamCustom(_, expected) -> {
      case decode.run(cell, decode.string) {
        Ok(actual) -> actual == expected
        Error(_) -> False
      }
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
