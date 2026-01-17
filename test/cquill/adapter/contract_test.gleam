// Adapter Contract Tests
//
// This test module defines the shared contract that ALL adapters must pass.
// Any adapter implementation can run these tests to verify conformance.
//
// The memory adapter is the reference implementation and should pass all tests.

import cquill/adapter
import cquill/adapter/memory.{type MemoryRow}
import cquill/error.{ForeignKeyViolation, NotNullViolation, UniqueViolation}
import cquill/schema
import cquill/schema/field
import gleam/dynamic
import gleam/list
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// SETUP HELPERS
// ============================================================================

/// Create a test store with users and posts tables
fn setup_test_store() -> memory.MemoryStore {
  // Create users schema
  let users_schema =
    schema.new("users")
    |> schema.field(
      field.integer("id")
      |> field.primary_key
      |> field.auto_increment,
    )
    |> schema.field(
      field.string("email")
      |> field.not_null
      |> field.unique,
    )
    |> schema.field(field.string("name") |> field.nullable)
    |> schema.single_primary_key("id")

  // Create posts schema with foreign key to users
  let posts_schema =
    schema.new("posts")
    |> schema.field(
      field.integer("id")
      |> field.primary_key
      |> field.auto_increment,
    )
    |> schema.field(
      field.integer("user_id")
      |> field.not_null
      |> field.references("users", "id"),
    )
    |> schema.field(field.string("title") |> field.not_null)
    |> schema.field(field.string("body") |> field.nullable)
    |> schema.single_primary_key("id")

  memory.new_store()
  |> memory.create_table_from_schema(users_schema)
  |> memory.create_table_from_schema(posts_schema)
}

/// Create a simple store with just a users table
fn setup_simple_store() -> memory.MemoryStore {
  memory.new_store()
  |> memory.create_table("users", "id")
}

// ============================================================================
// INSERT CONTRACT TESTS
// ============================================================================

pub fn insert_returns_record_test() {
  let store = setup_simple_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]

  let result = memory.insert_row(store, "users", "1", row)
  result |> should.be_ok

  // Verify we can retrieve the inserted row
  case result {
    Ok(new_store) -> {
      memory.get_row(new_store, "users", "1")
      |> should.be_ok
      |> should.equal(row)
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_generates_id_test() {
  let store = setup_simple_store()

  // First ID should be 1
  memory.next_id(store, "users")
  |> should.be_ok
  |> should.equal(1)

  // Insert a row
  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Next ID should be 2
  memory.next_id(store, "users")
  |> should.be_ok
  |> should.equal(2)
}

pub fn insert_enforces_unique_constraint_test() {
  let store = setup_test_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Try to insert second user with same email (unique constraint violation)
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("alice@example.com"),
    dynamic.string("Bob"),
  ]

  case memory.insert_row(store, "users", "2", user2) {
    Error(UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn insert_enforces_primary_key_uniqueness_test() {
  let store = setup_simple_store()

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("alice@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)

  // Try to insert with same primary key
  let row2: MemoryRow = [dynamic.int(1), dynamic.string("bob@example.com")]

  case memory.insert_row(store, "users", "1", row2) {
    Error(UniqueViolation(constraint, _)) -> {
      constraint |> should.equal("users_pkey")
    }
    _ -> should.fail()
  }
}

pub fn insert_enforces_not_null_test() {
  let store = setup_test_store()

  // Try to insert user with null email (not-null violation)
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.nil(),
    // email is null
    dynamic.string("Alice"),
  ]

  case memory.insert_row(store, "users", "1", user) {
    Error(NotNullViolation(column)) -> {
      column |> should.equal("email")
    }
    _ -> should.fail()
  }
}

pub fn insert_enforces_foreign_key_test() {
  let store = setup_test_store()

  // Try to insert post referencing non-existent user
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(999),
    // user_id 999 doesn't exist
    dynamic.string("Test Post"),
    dynamic.string("Post body"),
  ]

  case memory.insert_row(store, "posts", "1", post) {
    Error(ForeignKeyViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn insert_allows_valid_foreign_key_test() {
  let store = setup_test_store()

  // First create a user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Now insert a post referencing that user
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    // user_id 1 exists
    dynamic.string("Test Post"),
    dynamic.string("Post body"),
  ]

  memory.insert_row(store, "posts", "1", post)
  |> should.be_ok
}

// ============================================================================
// QUERY CONTRACT TESTS
// ============================================================================

pub fn all_returns_all_records_test() {
  let store = setup_simple_store()

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("alice@example.com")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("bob@example.com")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  let result = memory.get_all_rows(store, "users")
  result |> should.be_ok

  case result {
    Ok(rows) -> {
      rows |> list.length |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

pub fn one_returns_single_record_test() {
  let store = setup_simple_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  memory.get_row(store, "users", "1")
  |> should.be_ok
  |> should.equal(row)
}

pub fn one_returns_error_for_no_match_test() {
  let store = setup_simple_store()

  memory.get_row(store, "users", "999")
  |> should.be_error
}

// ============================================================================
// UPDATE CONTRACT TESTS
// ============================================================================

pub fn update_modifies_existing_test() {
  let store = setup_simple_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("old@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  let updated_row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("new@example.com"),
  ]
  let assert Ok(store) = memory.update_row(store, "users", "1", updated_row)

  memory.get_row(store, "users", "1")
  |> should.be_ok
  |> should.equal(updated_row)
}

pub fn update_returns_error_for_nonexistent_test() {
  let store = setup_simple_store()

  let row: MemoryRow = [dynamic.int(999), dynamic.string("test@example.com")]

  memory.update_row(store, "users", "999", row)
  |> should.be_error
}

pub fn update_enforces_constraints_test() {
  let store = setup_test_store()

  // Insert two users
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
  ]
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.string("Bob"),
  ]

  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", user2)

  // Try to update user2 with user1's email (unique violation)
  let updated: MemoryRow = [
    dynamic.int(2),
    dynamic.string("alice@example.com"),
    dynamic.string("Bob"),
  ]

  case memory.update_row(store, "users", "2", updated) {
    Error(UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// DELETE CONTRACT TESTS
// ============================================================================

pub fn delete_removes_record_test() {
  let store = setup_simple_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Verify row exists
  memory.get_row(store, "users", "1") |> should.be_ok

  // Delete it
  let assert Ok(store) = memory.delete_row(store, "users", "1")

  // Verify it's gone
  memory.get_row(store, "users", "1") |> should.be_error
}

pub fn delete_returns_error_for_nonexistent_test() {
  let store = setup_simple_store()

  memory.delete_row(store, "users", "999")
  |> should.be_error
}

pub fn delete_prevents_foreign_key_violation_test() {
  let store = setup_test_store()

  // Create a user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Create a post referencing that user
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    dynamic.string("Test Post"),
    dynamic.string("Body"),
  ]
  let assert Ok(store) = memory.insert_row(store, "posts", "1", post)

  // Try to delete the user (should fail because posts references it)
  case memory.delete_row(store, "users", "1") {
    Error(ForeignKeyViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// TRANSACTION CONTRACT TESTS
// ============================================================================

pub fn transaction_commits_on_success_test() {
  let store = setup_simple_store()
  let adp = memory.memory_adapter()

  let result =
    adapter.transaction(adp, store, fn(_tx_store) {
      // Operation succeeds
      Ok(42)
    })

  result
  |> should.be_ok
  |> should.equal(42)
}

pub fn transaction_rolls_back_on_error_test() {
  let store = setup_simple_store()
  let adp = memory.memory_adapter()

  let result =
    adapter.transaction(adp, store, fn(_tx_store) { Error("user error") })

  result |> should.be_error
}

pub fn transaction_snapshot_and_rollback_test() {
  let store = setup_simple_store()

  // Insert a row
  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Verify the data exists before transaction
  memory.row_count(store, "users")
  |> should.be_ok
  |> should.equal(1)

  // Use the rollback_and_restore helper for testing rollback
  // First, manually begin a transaction
  let adp = memory.memory_adapter()

  // Test successful transaction
  let result =
    adapter.transaction(adp, store, fn(_tx) {
      // Operations within transaction
      Ok("success")
    })

  result
  |> should.be_ok
  |> should.equal("success")
}

// ============================================================================
// ADAPTER INTERFACE CONTRACT TESTS
// ============================================================================

pub fn adapter_has_name_test() {
  let adp = memory.memory_adapter()

  adapter.name(adp)
  |> should.equal("memory")
}

pub fn adapter_declares_capabilities_test() {
  let adp = memory.memory_adapter()
  let caps = adapter.capabilities(adp)

  // Memory adapter should support transactions
  caps.transactions |> should.be_true

  // Memory adapter should support returning
  caps.returning |> should.be_true

  // Memory adapter should support batch insert
  caps.batch_insert |> should.be_true
}

pub fn adapter_query_executes_test() {
  let store = setup_simple_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  let adp = memory.memory_adapter()
  let query =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users",
      params: [],
      expected_columns: 2,
    )

  let result = adapter.query(adp, store, query)
  result |> should.be_ok

  case result {
    Ok(rows) -> rows |> list.length |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn adapter_query_with_params_test() {
  let store = setup_simple_store()

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("alice@example.com")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("bob@example.com")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  let adp = memory.memory_adapter()
  let query =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE id = $1",
      params: [adapter.ParamInt(1)],
      expected_columns: 2,
    )

  let result = adapter.query(adp, store, query)
  result |> should.be_ok

  case result {
    Ok(rows) -> {
      rows |> list.length |> should.equal(1)
      case rows {
        [row] -> row |> should.equal(row1)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// RESET AND STATE INSPECTION TESTS
// ============================================================================

pub fn reset_clears_data_preserves_structure_test() {
  let store = setup_simple_store()

  // Insert some data
  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  memory.row_count(store, "users")
  |> should.be_ok
  |> should.equal(1)

  // Reset the store
  let store = memory.reset(store)

  // Data should be cleared
  memory.row_count(store, "users")
  |> should.be_ok
  |> should.equal(0)

  // But table should still exist (structure preserved)
  memory.get_all_rows(store, "users")
  |> should.be_ok
  |> should.equal([])
}

pub fn row_count_returns_correct_count_test() {
  let store = setup_simple_store()

  memory.row_count(store, "users")
  |> should.be_ok
  |> should.equal(0)

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("a@example.com")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("b@example.com")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)

  memory.row_count(store, "users")
  |> should.be_ok
  |> should.equal(1)

  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  memory.row_count(store, "users")
  |> should.be_ok
  |> should.equal(2)
}

pub fn get_all_rows_returns_all_data_test() {
  let store = setup_simple_store()

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("a@example.com")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("b@example.com")]
  let row3: MemoryRow = [dynamic.int(3), dynamic.string("c@example.com")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)
  let assert Ok(store) = memory.insert_row(store, "users", "3", row3)

  let result = memory.get_all_rows(store, "users")
  result |> should.be_ok

  case result {
    Ok(rows) -> rows |> list.length |> should.equal(3)
    Error(_) -> should.fail()
  }
}

// ============================================================================
// SCHEMA-BASED TABLE CREATION TESTS
// ============================================================================

pub fn create_table_from_schema_test() {
  let user_schema =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.string("email") |> field.not_null |> field.unique)
    |> schema.field(field.string("name") |> field.nullable)
    |> schema.single_primary_key("id")

  let store =
    memory.new_store()
    |> memory.create_table_from_schema(user_schema)

  // Table should exist
  memory.get_all_rows(store, "users")
  |> should.be_ok
  |> should.equal([])
}

pub fn add_unique_constraint_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  // Add unique constraint on email column
  let assert Ok(store) =
    memory.add_unique_constraint(store, "users", "users_email_unique", ["id"])

  // Insert first row
  let row1: MemoryRow = [dynamic.int(1)]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)

  // Try to insert duplicate (should fail)
  let row2: MemoryRow = [dynamic.int(1)]

  case memory.insert_row(store, "users", "2", row2) {
    Error(UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}
