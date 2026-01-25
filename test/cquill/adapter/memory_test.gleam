import cquill/adapter
import cquill/adapter/memory.{type MemoryRow}
import cquill/error.{UserError}
import gleam/dynamic
import gleam/list
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// MEMORY STORE TESTS
// ============================================================================

pub fn new_store_test() {
  let store = memory.new_store()

  store.in_transaction |> should.be_false
  store.snapshot |> should.equal(None)
}

pub fn create_table_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  // Table should exist (tested via operations)
  memory.get_all_rows(store, "users")
  |> should.be_ok
  |> should.equal([])
}

pub fn insert_and_get_row_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("test@example.com"),
    dynamic.string("Alice"),
  ]

  let result = memory.insert_row(store, "users", "1", row)
  result |> should.be_ok

  let store_with_row = case result {
    Ok(s) -> s
    Error(_) -> store
  }

  memory.get_row(store_with_row, "users", "1")
  |> should.be_ok
  |> should.equal(row)
}

pub fn get_row_not_found_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  memory.get_row(store, "users", "999")
  |> should.be_error
}

pub fn get_all_rows_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("alice@example.com")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("bob@example.com")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  let rows = memory.get_all_rows(store, "users")
  rows |> should.be_ok

  case rows {
    Ok(r) ->
      r
      |> list.length
      |> should.equal(2)
    Error(_) -> should.fail()
  }
}

pub fn delete_row_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Verify row exists
  memory.get_row(store, "users", "1") |> should.be_ok

  // Delete it
  let assert Ok(store) = memory.delete_row(store, "users", "1")

  // Verify it's gone
  memory.get_row(store, "users", "1")
  |> should.be_error
}

pub fn delete_row_not_found_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  memory.delete_row(store, "users", "999")
  |> should.be_error
}

pub fn table_not_found_test() {
  let store = memory.new_store()

  memory.get_all_rows(store, "nonexistent")
  |> should.be_error

  memory.get_row(store, "nonexistent", "1")
  |> should.be_error
}

pub fn next_id_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  memory.next_id(store, "users")
  |> should.be_ok
  |> should.equal(1)

  let row: MemoryRow = [dynamic.int(1), dynamic.string("test@example.com")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  memory.next_id(store, "users")
  |> should.be_ok
  |> should.equal(2)
}

// ============================================================================
// ADAPTER PROTOCOL TESTS
// ============================================================================

pub fn memory_adapter_name_test() {
  let adapter = memory.memory_adapter()

  adapter.name(adapter)
  |> should.equal("memory")
}

pub fn memory_adapter_capabilities_test() {
  let adp = memory.memory_adapter()
  let caps = adapter.capabilities(adp)

  caps.transactions |> should.be_true
  caps.returning |> should.be_true
  caps.batch_insert |> should.be_true
}

pub fn supports_transactions_test() {
  let adp = memory.memory_adapter()

  adapter.supports_transactions(adp)
  |> should.be_true
}

pub fn supports_returning_test() {
  let adp = memory.memory_adapter()

  adapter.supports_returning(adp)
  |> should.be_true
}

// ============================================================================
// QUERY EXECUTION TESTS
// ============================================================================

pub fn query_all_rows_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("alice@example.com")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("bob@example.com")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  let adp = memory.memory_adapter()
  let query =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users",
      params: [],
      expected_columns: 2,
    )

  adapter.query(adp, store, query)
  |> should.be_ok
  |> list.length
  |> should.equal(2)
}

pub fn query_with_where_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

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
      rows
      |> list.length
      |> should.equal(1)
      case rows {
        [row] -> row |> should.equal(row1)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// TRANSACTION TESTS
// ============================================================================

pub fn begin_transaction_test() {
  let store = memory.new_store()
  let adp = memory.memory_adapter()

  // Get the begin function from adapter internals
  let result = adapter.transaction(adp, store, fn(_tx_store) { Ok("success") })

  result |> should.be_ok
}

pub fn transaction_commits_on_success_test() {
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  let adp = memory.memory_adapter()

  let result =
    adapter.transaction(adp, store, fn(_tx_store) {
      // In a real implementation, mutations here would be committed
      Ok(42)
    })

  result
  |> should.be_ok
  |> should.equal(42)
}

pub fn transaction_returns_user_error_test() {
  let store = memory.new_store()
  let adp = memory.memory_adapter()

  let result =
    adapter.transaction(adp, store, fn(_tx_store) { Error("user error") })

  case result {
    Error(UserError(msg)) -> msg |> should.equal("user error")
    _ -> should.fail()
  }
}

// ============================================================================
// WHERE CLAUSE FILTERING TESTS
// ============================================================================

pub fn query_where_filter_by_boolean_test() {
  // Create table with explicit columns for WHERE filtering
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("users", "id", ["id", "email", "active"])

  // Insert test data
  let row1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.bool(True),
  ]
  let row2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.bool(True),
  ]
  let row3: MemoryRow = [
    dynamic.int(3),
    dynamic.string("charlie@example.com"),
    dynamic.bool(False),
  ]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)
  let assert Ok(store) = memory.insert_row(store, "users", "3", row3)

  // Query for active users
  let adp = memory.memory_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE active = $1",
      params: [adapter.ParamBool(True)],
      expected_columns: 3,
    )

  let result = adapter.query(adp, store, compiled)
  case result {
    Ok(rows) -> {
      // Should get 2 active users (alice and bob)
      list.length(rows) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

pub fn query_where_filter_by_string_test() {
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("users", "id", ["id", "email", "name"])

  let row1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
  ]
  let row2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.string("Bob"),
  ]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  let adp = memory.memory_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE name = $1",
      params: [adapter.ParamString("Alice")],
      expected_columns: 3,
    )

  let result = adapter.query(adp, store, compiled)
  case result {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

pub fn query_where_filter_by_int_test() {
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("users", "id", ["id", "age"])

  let row1: MemoryRow = [dynamic.int(1), dynamic.int(25)]
  let row2: MemoryRow = [dynamic.int(2), dynamic.int(30)]
  let row3: MemoryRow = [dynamic.int(3), dynamic.int(25)]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)
  let assert Ok(store) = memory.insert_row(store, "users", "3", row3)

  let adp = memory.memory_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE age = $1",
      params: [adapter.ParamInt(25)],
      expected_columns: 2,
    )

  let result = adapter.query(adp, store, compiled)
  case result {
    Ok(rows) -> {
      list.length(rows) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

pub fn query_where_multiple_conditions_test() {
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("users", "id", ["id", "role", "active"])

  let row1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("admin"),
    dynamic.bool(True),
  ]
  let row2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("user"),
    dynamic.bool(True),
  ]
  let row3: MemoryRow = [
    dynamic.int(3),
    dynamic.string("admin"),
    dynamic.bool(False),
  ]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)
  let assert Ok(store) = memory.insert_row(store, "users", "3", row3)

  let adp = memory.memory_adapter()
  // Query for active admins
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE role = $1 AND active = $2",
      params: [adapter.ParamString("admin"), adapter.ParamBool(True)],
      expected_columns: 3,
    )

  let result = adapter.query(adp, store, compiled)
  case result {
    Ok(rows) -> {
      // Only row1 matches both conditions
      list.length(rows) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

pub fn query_where_no_match_test() {
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("users", "id", ["id", "name"])

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("Alice")]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)

  let adp = memory.memory_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE name = $1",
      params: [adapter.ParamString("Nobody")],
      expected_columns: 2,
    )

  let result = adapter.query(adp, store, compiled)
  case result {
    Ok(rows) -> {
      list.length(rows) |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

pub fn query_no_where_returns_all_test() {
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("users", "id", ["id", "name"])

  let row1: MemoryRow = [dynamic.int(1), dynamic.string("Alice")]
  let row2: MemoryRow = [dynamic.int(2), dynamic.string("Bob")]

  let assert Ok(store) = memory.insert_row(store, "users", "1", row1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", row2)

  let adp = memory.memory_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users",
      params: [],
      expected_columns: 2,
    )

  let result = adapter.query(adp, store, compiled)
  case result {
    Ok(rows) -> {
      list.length(rows) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

pub fn create_table_with_columns_test() {
  let store =
    memory.new_store()
    |> memory.create_table_with_columns("posts", "id", [
      "id", "title", "body", "published",
    ])

  // Insert a row
  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("Hello"),
    dynamic.string("World"),
    dynamic.bool(True),
  ]

  let result = memory.insert_row(store, "posts", "1", row)
  result |> should.be_ok

  let store_with_row = case result {
    Ok(s) -> s
    Error(_) -> store
  }

  memory.get_row(store_with_row, "posts", "1")
  |> should.be_ok
  |> should.equal(row)
}
