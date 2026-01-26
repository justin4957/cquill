// Memory Adapter Benchmarks
//
// Measures performance of the in-memory adapter operations.
// Includes insert, query, update, delete, and transaction operations.

import bench/bench
import cquill/adapter/memory
import cquill/schema
import cquill/schema/field
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/list
import gleam/string

// ============================================================================
// TEST FIXTURES
// ============================================================================

/// Create a user schema for benchmarking
fn user_schema() -> schema.Schema {
  schema.new("users")
  |> schema.field(field.integer("id") |> field.primary_key)
  |> schema.field(field.string("email") |> field.not_null)
  |> schema.field(field.string("name"))
  |> schema.field(field.boolean("active"))
  |> schema.field(field.integer("age"))
  |> schema.single_primary_key("id")
}

/// Create a store with the user table
fn create_store() -> memory.MemoryStore {
  memory.new()
  |> memory.create_table_from_schema(user_schema())
}

/// Create a store pre-populated with n users
fn create_populated_store(count: Int) -> memory.MemoryStore {
  let store = create_store()
  let rows =
    list.range(1, count)
    |> list.map(fn(i) {
      let row = [
        dynamic.int(i),
        dynamic.string("user" <> int.to_string(i) <> "@example.com"),
        dynamic.string("User " <> int.to_string(i)),
        dynamic.bool(True),
        dynamic.int(20 + { i % 50 }),
      ]
      #(int.to_string(i), row)
    })

  case memory.insert_all(store, "users", rows) {
    Ok(#(new_store, _)) -> new_store
    Error(_) -> store
  }
}

/// Create a single user row
fn create_user_row(id: Int) -> memory.MemoryRow {
  [
    dynamic.int(id),
    dynamic.string("user" <> int.to_string(id) <> "@example.com"),
    dynamic.string("User " <> int.to_string(id)),
    dynamic.bool(True),
    dynamic.int(25),
  ]
}

// ============================================================================
// INSERT BENCHMARKS
// ============================================================================

/// Benchmark single row insert
fn bench_single_insert() -> Nil {
  let store = create_store()
  let row = create_user_row(1)
  let _ = memory.insert_row(store, "users", "1", row)
  Nil
}

/// Benchmark batch insert of 10 rows
fn bench_batch_insert_10() -> Nil {
  let store = create_store()
  let rows =
    list.range(1, 10)
    |> list.map(fn(i) { #(int.to_string(i), create_user_row(i)) })
  let _ = memory.insert_all(store, "users", rows)
  Nil
}

/// Benchmark batch insert of 100 rows
fn bench_batch_insert_100() -> Nil {
  let store = create_store()
  let rows =
    list.range(1, 100)
    |> list.map(fn(i) { #(int.to_string(i), create_user_row(i)) })
  let _ = memory.insert_all(store, "users", rows)
  Nil
}

/// Benchmark batch insert of 1000 rows
fn bench_batch_insert_1000() -> Nil {
  let store = create_store()
  let rows =
    list.range(1, 1000)
    |> list.map(fn(i) { #(int.to_string(i), create_user_row(i)) })
  let _ = memory.insert_all(store, "users", rows)
  Nil
}

// ============================================================================
// QUERY BENCHMARKS
// ============================================================================

/// Benchmark get single row by key
fn bench_get_by_key() -> Nil {
  let store = create_populated_store(1000)
  let _ = memory.get_row(store, "users", "500")
  Nil
}

/// Benchmark get all rows from small table (100 rows)
fn bench_get_all_100() -> Nil {
  let store = create_populated_store(100)
  let _ = memory.get_all_rows(store, "users")
  Nil
}

/// Benchmark get all rows from medium table (1000 rows)
fn bench_get_all_1000() -> Nil {
  let store = create_populated_store(1000)
  let _ = memory.get_all_rows(store, "users")
  Nil
}

/// Benchmark get all rows from large table (10000 rows)
fn bench_get_all_10000() -> Nil {
  let store = create_populated_store(10_000)
  let _ = memory.get_all_rows(store, "users")
  Nil
}

/// Benchmark row count operation
fn bench_row_count() -> Nil {
  let store = create_populated_store(1000)
  let _ = memory.row_count(store, "users")
  Nil
}

// ============================================================================
// UPDATE BENCHMARKS
// ============================================================================

/// Benchmark single row update
fn bench_single_update() -> Nil {
  let store = create_populated_store(100)
  let updated_row = [
    dynamic.int(50),
    dynamic.string("updated@example.com"),
    dynamic.string("Updated User"),
    dynamic.bool(False),
    dynamic.int(30),
  ]
  let _ = memory.update_row(store, "users", "50", updated_row)
  Nil
}

/// Benchmark update all rows matching predicate
fn bench_update_all_matching() -> Nil {
  let store = create_populated_store(100)
  // Update all users to inactive
  let _ =
    memory.update_all(store, "users", fn(_key, _row) { True }, fn(row) {
      case row {
        [id, email, name, _, age] -> [
          id,
          email,
          name,
          dynamic.bool(False),
          age,
        ]
        _ -> row
      }
    })
  Nil
}

// ============================================================================
// DELETE BENCHMARKS
// ============================================================================

/// Benchmark single row delete
fn bench_single_delete() -> Nil {
  let store = create_populated_store(100)
  let _ = memory.delete_row(store, "users", "50")
  Nil
}

/// Benchmark delete all rows matching predicate
fn bench_delete_all_matching() -> Nil {
  let store = create_populated_store(100)
  // Delete half the users (even IDs)
  let _ =
    memory.delete_all(store, "users", fn(key, _row) {
      case int.parse(key) {
        Ok(id) -> id % 2 == 0
        Error(_) -> False
      }
    })
  Nil
}

/// Benchmark truncate table
fn bench_truncate_table() -> Nil {
  let store = create_populated_store(1000)
  let _ = memory.delete_all_rows(store, "users")
  Nil
}

// ============================================================================
// TRANSACTION BENCHMARKS
// ============================================================================

/// Benchmark transaction with commit
fn bench_transaction_commit() -> Nil {
  let store = create_populated_store(100)
  let _ =
    memory.execute_transaction(store, fn(tx_store) {
      let row = create_user_row(999)
      case memory.insert_row(tx_store, "users", "999", row) {
        Ok(new_store) -> Ok(#(new_store, Nil))
        Error(e) -> Error(e)
      }
    })
  Nil
}

/// Benchmark transaction with rollback
fn bench_transaction_rollback() -> Nil {
  let store = create_populated_store(100)
  // Create a transaction that will fail due to duplicate key
  let _ =
    memory.execute_transaction(store, fn(tx_store) {
      let row = create_user_row(50)
      // This will fail - key 50 already exists
      case memory.insert_row(tx_store, "users", "50", row) {
        Ok(new_store) -> Ok(#(new_store, Nil))
        Error(e) -> Error(e)
      }
    })
  Nil
}

/// Benchmark nested transactions (savepoints)
fn bench_nested_transaction() -> Nil {
  let store = create_populated_store(100)
  let _ =
    memory.execute_transaction(store, fn(tx_store) {
      case memory.create_savepoint(tx_store, "sp1") {
        Ok(sp_store) -> {
          let row = create_user_row(999)
          case memory.insert_row(sp_store, "users", "999", row) {
            Ok(new_store) ->
              case memory.release_savepoint(new_store, "sp1") {
                Ok(final_store) -> Ok(#(final_store, Nil))
                Error(_) -> Ok(#(sp_store, Nil))
              }
            Error(e) -> Error(e)
          }
        }
        Error(_) -> Ok(#(tx_store, Nil))
      }
    })
  Nil
}

// ============================================================================
// CONSTRAINT CHECKING BENCHMARKS
// ============================================================================

/// Benchmark unique constraint check on insert
fn bench_unique_constraint_check() -> Nil {
  let store = create_populated_store(100)
  // Try to insert duplicate key (will fail constraint)
  let row = create_user_row(50)
  let _ = memory.insert_row(store, "users", "50", row)
  Nil
}

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================

pub fn main() {
  io.println("")
  io.println("cquill Memory Adapter Benchmarks")
  io.println("=================================")
  io.println("")

  // Use quick config for expensive operations
  let quick = bench.quick_config()
  let default = bench.default_config()

  let insert_suite =
    bench.run_suite(
      "Insert Operations",
      [
        #("Single insert", bench_single_insert),
        #("Batch insert (10 rows)", bench_batch_insert_10),
        #("Batch insert (100 rows)", bench_batch_insert_100),
      ],
      default,
    )

  // Expensive batch insert with fewer iterations
  let batch_insert_suite =
    bench.run_suite(
      "Large Batch Insert",
      [
        #("Batch insert (1000 rows)", bench_batch_insert_1000),
      ],
      quick,
    )

  let query_suite =
    bench.run_suite(
      "Query Operations",
      [
        #("Get by key (1000 rows)", bench_get_by_key),
        #("Get all (100 rows)", bench_get_all_100),
        #("Get all (1000 rows)", bench_get_all_1000),
        #("Row count", bench_row_count),
      ],
      default,
    )

  // Large query with fewer iterations
  let large_query_suite =
    bench.run_suite(
      "Large Query Operations",
      [
        #("Get all (10000 rows)", bench_get_all_10000),
      ],
      quick,
    )

  let update_suite =
    bench.run_suite(
      "Update Operations",
      [
        #("Single update", bench_single_update),
        #("Update all matching", bench_update_all_matching),
      ],
      default,
    )

  let delete_suite =
    bench.run_suite(
      "Delete Operations",
      [
        #("Single delete", bench_single_delete),
        #("Delete matching", bench_delete_all_matching),
        #("Truncate (1000 rows)", bench_truncate_table),
      ],
      default,
    )

  let transaction_suite =
    bench.run_suite(
      "Transaction Operations",
      [
        #("Transaction + commit", bench_transaction_commit),
        #("Transaction + rollback", bench_transaction_rollback),
        #("Nested transaction (savepoint)", bench_nested_transaction),
      ],
      default,
    )

  let constraint_suite =
    bench.run_suite(
      "Constraint Checking",
      [
        #("Unique constraint check", bench_unique_constraint_check),
      ],
      default,
    )

  // Print summaries
  bench.print_suite_summary(insert_suite)
  bench.print_suite_summary(batch_insert_suite)
  bench.print_suite_summary(query_suite)
  bench.print_suite_summary(large_query_suite)
  bench.print_suite_summary(update_suite)
  bench.print_suite_summary(delete_suite)
  bench.print_suite_summary(transaction_suite)
  bench.print_suite_summary(constraint_suite)

  // Print markdown
  io.println("")
  io.println("Markdown Output:")
  io.println("-" |> string.repeat(60))
  io.println(bench.to_markdown(insert_suite))
  io.println(bench.to_markdown(query_suite))
  io.println(bench.to_markdown(update_suite))
  io.println(bench.to_markdown(delete_suite))
  io.println(bench.to_markdown(transaction_suite))
}
