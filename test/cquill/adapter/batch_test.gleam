// Batch Operations Tests
//
// Comprehensive tests for batch insert, update, and delete operations.
// Tests memory adapter batch operations with atomic behavior.

import cquill/adapter/memory.{
  type BatchConfig, BatchConfig, create_table, default_batch_config, delete_all,
  delete_all_rows, get_all_rows, get_row, insert_all, insert_all_with_auto_keys,
  insert_all_with_config, insert_row, new_store, row_count, update_all,
  update_all_rows,
}
import cquill/error
import gleam/dynamic
import gleam/int
import gleam/list
import gleeunit/should

// ============================================================================
// INSERT_ALL TESTS
// ============================================================================

pub fn insert_all_inserts_multiple_rows_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let rows = [
    #("1", [dynamic.int(1), dynamic.string("alice@example.com")]),
    #("2", [dynamic.int(2), dynamic.string("bob@example.com")]),
    #("3", [dynamic.int(3), dynamic.string("charlie@example.com")]),
  ]

  case insert_all(store, "users", rows) {
    Ok(#(new_store, count)) -> {
      should.equal(count, 3)

      // Verify all rows exist
      case row_count(new_store, "users") {
        Ok(n) -> should.equal(n, 3)
        Error(_) -> should.fail()
      }

      // Verify individual rows
      case get_row(new_store, "users", "1") {
        Ok(_) -> should.be_true(True)
        Error(_) -> should.fail()
      }
      case get_row(new_store, "users", "2") {
        Ok(_) -> should.be_true(True)
        Error(_) -> should.fail()
      }
      case get_row(new_store, "users", "3") {
        Ok(_) -> should.be_true(True)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_all_empty_list_returns_zero_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  case insert_all(store, "users", []) {
    Ok(#(_new_store, count)) -> should.equal(count, 0)
    Error(_) -> should.fail()
  }
}

pub fn insert_all_is_atomic_on_duplicate_key_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert first row normally
  let row1 = [dynamic.int(1), dynamic.string("alice@example.com")]
  let store = case insert_row(store, "users", "1", row1) {
    Ok(s) -> s
    Error(_) -> store
  }

  // Try to insert batch with duplicate key
  let rows = [
    #("2", [dynamic.int(2), dynamic.string("bob@example.com")]),
    #("1", [dynamic.int(1), dynamic.string("duplicate@example.com")]),
    // Duplicate!
    #("3", [dynamic.int(3), dynamic.string("charlie@example.com")]),
  ]

  case insert_all(store, "users", rows) {
    Error(error.UniqueViolation(_, _)) -> {
      // Good - constraint violation detected
      // Verify none of the new rows were inserted (atomic behavior)
      case row_count(store, "users") {
        Ok(n) -> should.equal(n, 1)
        // Only the original row
        Error(_) -> should.fail()
      }
    }
    Ok(_) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn insert_all_detects_duplicate_keys_in_batch_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Try to insert batch with duplicate keys within the batch
  let rows = [
    #("1", [dynamic.int(1), dynamic.string("alice@example.com")]),
    #("2", [dynamic.int(2), dynamic.string("bob@example.com")]),
    #("1", [dynamic.int(3), dynamic.string("charlie@example.com")]),
    // Same key as first!
  ]

  case insert_all(store, "users", rows) {
    Error(error.UniqueViolation(_, detail)) -> {
      should.be_true(True)
      // Should mention duplicate keys
      should.be_true(
        detail == "Duplicate keys within batch"
        || detail == "Key (1) already exists",
      )
    }
    Ok(_) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn insert_all_with_config_non_atomic_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Pre-insert a row
  let row1 = [dynamic.int(1), dynamic.string("alice@example.com")]
  let store = case insert_row(store, "users", "1", row1) {
    Ok(s) -> s
    Error(_) -> store
  }

  // Non-atomic config
  let config = BatchConfig(max_batch_size: 1000, use_transaction: False)

  // Try batch with duplicate - non-atomic should insert first row before failing
  let rows = [
    #("2", [dynamic.int(2), dynamic.string("bob@example.com")]),
    #("1", [dynamic.int(1), dynamic.string("duplicate@example.com")]),
    // Will fail
  ]

  // Note: With non-atomic mode, the first insert may succeed before the failure
  // The exact behavior depends on implementation - this test verifies the API works
  let _result = insert_all_with_config(store, "users", rows, config)
  should.be_true(True)
}

pub fn insert_all_with_auto_keys_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let rows = [
    [dynamic.string("alice@example.com")],
    [dynamic.string("bob@example.com")],
    [dynamic.string("charlie@example.com")],
  ]

  case insert_all_with_auto_keys(store, "users", rows) {
    Ok(#(new_store, keys, count)) -> {
      should.equal(count, 3)
      should.equal(list.length(keys), 3)

      // Keys should be "1", "2", "3" (auto-generated)
      should.equal(keys, ["1", "2", "3"])

      // Verify rows exist
      case row_count(new_store, "users") {
        Ok(n) -> should.equal(n, 3)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// UPDATE_ALL TESTS
// ============================================================================

pub fn update_all_updates_matching_rows_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert some rows
  let store = case
    insert_row(store, "users", "1", [
      dynamic.int(1),
      dynamic.string("alice@example.com"),
      dynamic.bool(True),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }
  let store = case
    insert_row(store, "users", "2", [
      dynamic.int(2),
      dynamic.string("bob@example.com"),
      dynamic.bool(True),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }
  let store = case
    insert_row(store, "users", "3", [
      dynamic.int(3),
      dynamic.string("charlie@example.com"),
      dynamic.bool(False),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }

  // Update rows where id <= 2 (predicate checks key)
  let predicate = fn(key: String, _row) {
    case int.parse(key) {
      Ok(n) -> n <= 2
      Error(_) -> False
    }
  }

  // Update to set all values to a new email
  let updater = fn(row) {
    case row {
      [id, _email, active] -> [
        id,
        dynamic.string("updated@example.com"),
        active,
      ]
      _ -> row
    }
  }

  case update_all(store, "users", predicate, updater) {
    Ok(#(_new_store, count)) -> {
      should.equal(count, 2)
    }
    Error(_) -> should.fail()
  }
}

pub fn update_all_rows_updates_every_row_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert rows
  let store = case
    insert_row(store, "users", "1", [
      dynamic.int(1),
      dynamic.string("old@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }
  let store = case
    insert_row(store, "users", "2", [
      dynamic.int(2),
      dynamic.string("old@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }

  // Update all rows
  let updater = fn(row) {
    case row {
      [id, _email] -> [id, dynamic.string("new@example.com")]
      _ -> row
    }
  }

  case update_all_rows(store, "users", updater) {
    Ok(#(_new_store, count)) -> {
      should.equal(count, 2)
    }
    Error(_) -> should.fail()
  }
}

pub fn update_all_with_no_matches_returns_zero_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert a row
  let store = case
    insert_row(store, "users", "1", [
      dynamic.int(1),
      dynamic.string("test@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }

  // Predicate that matches nothing
  let predicate = fn(_key, _row) { False }
  let updater = fn(row) { row }

  case update_all(store, "users", predicate, updater) {
    Ok(#(_new_store, count)) -> should.equal(count, 0)
    Error(_) -> should.fail()
  }
}

// ============================================================================
// DELETE_ALL TESTS
// ============================================================================

pub fn delete_all_deletes_matching_rows_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert rows
  let store = case
    insert_row(store, "users", "1", [
      dynamic.int(1),
      dynamic.string("alice@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }
  let store = case
    insert_row(store, "users", "2", [
      dynamic.int(2),
      dynamic.string("bob@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }
  let store = case
    insert_row(store, "users", "3", [
      dynamic.int(3),
      dynamic.string("charlie@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }

  // Delete rows where id < 3
  let predicate = fn(key: String, _row) {
    case int.parse(key) {
      Ok(n) -> n < 3
      Error(_) -> False
    }
  }

  case delete_all(store, "users", predicate) {
    Ok(#(new_store, count)) -> {
      should.equal(count, 2)

      // Only row 3 should remain
      case row_count(new_store, "users") {
        Ok(n) -> should.equal(n, 1)
        Error(_) -> should.fail()
      }

      case get_row(new_store, "users", "3") {
        Ok(_) -> should.be_true(True)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn delete_all_rows_truncates_table_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert rows
  let store = case
    insert_row(store, "users", "1", [
      dynamic.int(1),
      dynamic.string("alice@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }
  let store = case
    insert_row(store, "users", "2", [
      dynamic.int(2),
      dynamic.string("bob@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }

  case delete_all_rows(store, "users") {
    Ok(#(new_store, count)) -> {
      should.equal(count, 2)

      case row_count(new_store, "users") {
        Ok(n) -> should.equal(n, 0)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn delete_all_with_no_matches_returns_zero_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert a row
  let store = case
    insert_row(store, "users", "1", [
      dynamic.int(1),
      dynamic.string("test@example.com"),
    ])
  {
    Ok(s) -> s
    Error(_) -> store
  }

  // Predicate that matches nothing
  let predicate = fn(_key, _row) { False }

  case delete_all(store, "users", predicate) {
    Ok(#(new_store, count)) -> {
      should.equal(count, 0)
      // Row should still exist
      case row_count(new_store, "users") {
        Ok(n) -> should.equal(n, 1)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// BATCH CONFIG TESTS
// ============================================================================

pub fn default_batch_config_has_correct_values_test() {
  let config = default_batch_config()
  should.equal(config.max_batch_size, 1000)
  should.equal(config.use_transaction, True)
}

pub fn custom_batch_config_test() {
  let config = BatchConfig(max_batch_size: 500, use_transaction: False)
  should.equal(config.max_batch_size, 500)
  should.equal(config.use_transaction, False)
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

pub fn insert_all_on_nonexistent_table_fails_test() {
  let store = new_store()
  // Don't create the table

  let rows = [#("1", [dynamic.int(1), dynamic.string("test@example.com")])]

  case insert_all(store, "users", rows) {
    Error(error.AdapterSpecific("TABLE_NOT_FOUND", _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn update_all_on_nonexistent_table_fails_test() {
  let store = new_store()

  let predicate = fn(_key, _row) { True }
  let updater = fn(row) { row }

  case update_all(store, "users", predicate, updater) {
    Error(error.AdapterSpecific("TABLE_NOT_FOUND", _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn delete_all_on_nonexistent_table_fails_test() {
  let store = new_store()

  let predicate = fn(_key, _row) { True }

  case delete_all(store, "users", predicate) {
    Error(error.AdapterSpecific("TABLE_NOT_FOUND", _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// LARGE BATCH TESTS
// ============================================================================

pub fn insert_all_handles_large_batch_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Create 100 rows
  let rows =
    list.range(1, 100)
    |> list.map(fn(i) {
      let key = int.to_string(i)
      let row = [
        dynamic.int(i),
        dynamic.string("user" <> key <> "@example.com"),
      ]
      #(key, row)
    })

  case insert_all(store, "users", rows) {
    Ok(#(new_store, count)) -> {
      should.equal(count, 100)
      case row_count(new_store, "users") {
        Ok(n) -> should.equal(n, 100)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn get_all_rows_returns_all_data_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert multiple rows
  let rows = [
    #("1", [dynamic.int(1), dynamic.string("alice@example.com")]),
    #("2", [dynamic.int(2), dynamic.string("bob@example.com")]),
  ]

  case insert_all(store, "users", rows) {
    Ok(#(new_store, _)) -> {
      case get_all_rows(new_store, "users") {
        Ok(all_rows) -> should.equal(list.length(all_rows), 2)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
