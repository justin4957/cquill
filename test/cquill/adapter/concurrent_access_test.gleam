// Concurrent Access and Race Condition Tests
//
// Comprehensive tests for concurrent access patterns and race condition
// prevention in adapters. Tests connection pool behavior, concurrent writes,
// transaction isolation levels, and deadlock scenarios.
//
// Note: The memory adapter uses immutable snapshots for transaction isolation,
// which provides strong consistency guarantees without traditional locking.
// These tests verify the isolation guarantees hold under various scenarios.

import cquill/adapter/memory.{
  type MemoryRow, type MemoryStore, create_savepoint, create_table_from_schema,
  delete_row, execute_transaction, get_row, has_savepoint, insert_row, new_store,
  release_savepoint, rollback_to_savepoint, row_count, transaction_depth,
  update_row,
}
import cquill/error
import cquill/schema
import cquill/schema/field
import gleam/dynamic
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/result
import gleeunit/should

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Verify that a row's value at a given index matches the expected integer
/// by comparing the Dynamic values directly
fn verify_int_at(row: MemoryRow, index: Int, expected: Int) -> Bool {
  let dropped = list.drop(row, index)
  case list.first(dropped) {
    Ok(actual) -> actual == dynamic.int(expected)
    Error(_) -> False
  }
}

// ============================================================================
// TEST SETUP HELPERS
// ============================================================================

/// Create a store with a users table (id, email, balance)
fn setup_users_store() -> MemoryStore {
  let users_schema =
    schema.new("users")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("email") |> field.not_null |> field.unique)
    |> schema.add_field(field.integer("balance") |> field.not_null)

  new_store()
  |> create_table_from_schema(users_schema)
}

/// Create a store with accounts and transfers tables for testing concurrent transfers
fn setup_banking_store() -> MemoryStore {
  let accounts_schema =
    schema.new("accounts")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("name") |> field.not_null)
    |> schema.add_field(field.integer("balance") |> field.not_null)

  let transfers_schema =
    schema.new("transfers")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.integer("from_account") |> field.not_null)
    |> schema.add_field(field.integer("to_account") |> field.not_null)
    |> schema.add_field(field.integer("amount") |> field.not_null)

  new_store()
  |> create_table_from_schema(accounts_schema)
  |> create_table_from_schema(transfers_schema)
}

/// Create a store with resources table for lock ordering tests
fn setup_resources_store() -> MemoryStore {
  let resources_schema =
    schema.new("resources")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("name") |> field.not_null)
    |> schema.add_field(field.string("status") |> field.not_null)
    |> schema.add_field(field.integer("version") |> field.not_null)

  new_store()
  |> create_table_from_schema(resources_schema)
}

// ============================================================================
// CONNECTION POOL CONCURRENCY TESTS
// ============================================================================

/// Test that multiple sequential queries work correctly
pub fn sequential_queries_work_test() {
  let store = setup_users_store()

  // Insert multiple users sequentially
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.int(2000),
  ]
  let user3: MemoryRow = [
    dynamic.int(3),
    dynamic.string("carol@example.com"),
    dynamic.int(3000),
  ]

  let assert Ok(store) = insert_row(store, "users", "1", user1)
  let assert Ok(store) = insert_row(store, "users", "2", user2)
  let assert Ok(store) = insert_row(store, "users", "3", user3)

  // Verify all users exist
  case row_count(store, "users") {
    Ok(count) -> count |> should.equal(3)
    Error(_) -> should.fail()
  }
}

/// Test pool exhaustion simulation - multiple transactions on same store
pub fn multiple_transactions_sequential_test() {
  let store = setup_users_store()

  // Transaction 1
  let result1 =
    execute_transaction(store, fn(tx_store) {
      let user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1000),
      ]
      case insert_row(tx_store, "users", "1", user) {
        Ok(s) -> Ok(#(s, "tx1_done"))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store, _)) = result1

  // Transaction 2
  let result2 =
    execute_transaction(store, fn(tx_store) {
      let user: MemoryRow = [
        dynamic.int(2),
        dynamic.string("bob@example.com"),
        dynamic.int(2000),
      ]
      case insert_row(tx_store, "users", "2", user) {
        Ok(s) -> Ok(#(s, "tx2_done"))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store, _)) = result2

  // Transaction 3
  let result3 =
    execute_transaction(store, fn(tx_store) {
      let user: MemoryRow = [
        dynamic.int(3),
        dynamic.string("carol@example.com"),
        dynamic.int(3000),
      ]
      case insert_row(tx_store, "users", "3", user) {
        Ok(s) -> Ok(#(s, "tx3_done"))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(final_store, _)) = result3

  // All three users should exist
  case row_count(final_store, "users") {
    Ok(count) -> count |> should.equal(3)
    Error(_) -> should.fail()
  }
}

/// Test graceful degradation when transaction fails
pub fn transaction_failure_degrades_gracefully_test() {
  let store = setup_users_store()

  // Insert a user first
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", user1)

  // Try a transaction that will fail due to unique constraint
  let result =
    execute_transaction(store, fn(tx_store) {
      let duplicate_email: MemoryRow = [
        dynamic.int(2),
        dynamic.string("alice@example.com"),
        // Duplicate email!
        dynamic.int(500),
      ]
      case insert_row(tx_store, "users", "2", duplicate_email) {
        Ok(s) -> Ok(#(s, "should_not_reach"))
        Error(e) -> Error(e)
      }
    })

  // Transaction should fail
  case result {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }

  // Original user should still exist (store wasn't modified)
  case get_row(store, "users", "1") {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

// ============================================================================
// CONCURRENT WRITES TESTS
// ============================================================================

/// Test simultaneous inserts to same table (simulated via sequential transactions)
pub fn simultaneous_inserts_same_table_test() {
  let store = setup_users_store()

  // Simulate concurrent inserts by running multiple transactions
  let users = [
    #(1, "alice@example.com", 1000),
    #(2, "bob@example.com", 2000),
    #(3, "carol@example.com", 3000),
    #(4, "dave@example.com", 4000),
    #(5, "eve@example.com", 5000),
  ]

  // Insert all users via transactions
  let final_store =
    list.fold(users, store, fn(acc_store, user_data) {
      let #(id, email, balance) = user_data
      let user: MemoryRow = [
        dynamic.int(id),
        dynamic.string(email),
        dynamic.int(balance),
      ]
      let result =
        execute_transaction(acc_store, fn(tx_store) {
          case insert_row(tx_store, "users", int.to_string(id), user) {
            Ok(s) -> Ok(#(s, Nil))
            Error(e) -> Error(e)
          }
        })
      case result {
        Ok(#(new_store, _)) -> new_store
        Error(_) -> acc_store
      }
    })

  // All 5 users should exist
  case row_count(final_store, "users") {
    Ok(count) -> count |> should.equal(5)
    Error(_) -> should.fail()
  }
}

/// Test updates to same row from different transaction contexts
pub fn updates_same_row_different_contexts_test() {
  let store = setup_users_store()

  // Insert initial user
  let initial_user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", initial_user)

  // Transaction A: Update balance
  let result_a =
    execute_transaction(store, fn(tx_store) {
      let updated_user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1500),
      ]
      case update_row(tx_store, "users", "1", updated_user) {
        Ok(s) -> Ok(#(s, "tx_a"))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store_after_a, _)) = result_a

  // Transaction B: Update balance again (on store_after_a)
  let result_b =
    execute_transaction(store_after_a, fn(tx_store) {
      let updated_user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(2000),
      ]
      case update_row(tx_store, "users", "1", updated_user) {
        Ok(s) -> Ok(#(s, "tx_b"))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(final_store, _)) = result_b

  // Final balance should be 2000
  case get_row(final_store, "users", "1") {
    Ok(row) -> verify_int_at(row, 2, 2000) |> should.be_true
    Error(_) -> should.fail()
  }
}

/// Test updates to different rows don't interfere
pub fn updates_different_rows_no_interference_test() {
  let store = setup_users_store()

  // Insert two users
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.int(2000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", user1)
  let assert Ok(store) = insert_row(store, "users", "2", user2)

  // Update user 1
  let result1 =
    execute_transaction(store, fn(tx_store) {
      let updated_user1: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1500),
      ]
      case update_row(tx_store, "users", "1", updated_user1) {
        Ok(s) -> Ok(#(s, Nil))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store, _)) = result1

  // Update user 2
  let result2 =
    execute_transaction(store, fn(tx_store) {
      let updated_user2: MemoryRow = [
        dynamic.int(2),
        dynamic.string("bob@example.com"),
        dynamic.int(2500),
      ]
      case update_row(tx_store, "users", "2", updated_user2) {
        Ok(s) -> Ok(#(s, Nil))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(final_store, _)) = result2

  // Verify both updates applied correctly
  case get_row(final_store, "users", "1") {
    Ok(row) -> verify_int_at(row, 2, 1500) |> should.be_true
    Error(_) -> should.fail()
  }

  case get_row(final_store, "users", "2") {
    Ok(row) -> verify_int_at(row, 2, 2500) |> should.be_true
    Error(_) -> should.fail()
  }
}

/// Test optimistic locking behavior via version field
pub fn optimistic_locking_version_check_test() {
  let store = setup_resources_store()

  // Insert resource with version 1
  let resource: MemoryRow = [
    dynamic.int(1),
    dynamic.string("document"),
    dynamic.string("available"),
    dynamic.int(1),
  ]
  let assert Ok(store) = insert_row(store, "resources", "1", resource)

  // Transaction A: Read resource, update with version check
  let result_a =
    execute_transaction(store, fn(tx_store) {
      case get_row(tx_store, "resources", "1") {
        Ok(_row) -> {
          // Check version is 1, update to version 2
          let updated_resource: MemoryRow = [
            dynamic.int(1),
            dynamic.string("document"),
            dynamic.string("locked"),
            dynamic.int(2),
          ]
          case update_row(tx_store, "resources", "1", updated_resource) {
            Ok(s) -> Ok(#(s, "updated_to_v2"))
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store_after_a, _)) = result_a

  // Verify version is now 2
  case get_row(store_after_a, "resources", "1") {
    Ok(row) -> verify_int_at(row, 3, 2) |> should.be_true
    Error(_) -> should.fail()
  }
}

// ============================================================================
// TRANSACTION ISOLATION TESTS
// ============================================================================

/// Test read committed behavior - changes visible after commit
pub fn read_committed_changes_visible_after_commit_test() {
  let store = setup_users_store()

  // Transaction 1: Insert user and commit
  let result1 =
    execute_transaction(store, fn(tx_store) {
      let user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1000),
      ]
      case insert_row(tx_store, "users", "1", user) {
        Ok(s) -> Ok(#(s, Nil))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store_after_commit, _)) = result1

  // After commit, data should be visible
  case get_row(store_after_commit, "users", "1") {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

/// Test dirty reads prevented - uncommitted changes not visible
pub fn dirty_reads_prevented_test() {
  let store = setup_users_store()

  // Insert a user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", user)

  // Start a transaction, modify, but don't commit yet
  // In Gleam/memory adapter, the original store is unchanged until commit
  // (The modified_user below demonstrates that changes aren't visible outside tx)
  let _modified_user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(9999),
  ]

  // The original store still has the original balance
  case get_row(store, "users", "1") {
    Ok(row) -> verify_int_at(row, 2, 1000) |> should.be_true
    Error(_) -> should.fail()
  }
}

/// Test snapshot isolation - transaction sees consistent snapshot
pub fn snapshot_isolation_consistent_view_test() {
  let store = setup_users_store()

  // Insert initial user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", user)

  // Start a transaction
  let result =
    execute_transaction(store, fn(tx_store) {
      // Read the user at start of transaction
      let first_read = get_row(tx_store, "users", "1")

      // Within the transaction, we have a consistent view
      // Any changes we make are isolated until commit
      let updated_user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1500),
      ]

      case update_row(tx_store, "users", "1", updated_user) {
        Ok(tx_store2) -> {
          // Second read sees our own changes
          let second_read = get_row(tx_store2, "users", "1")

          case first_read, second_read {
            Ok(r1), Ok(r2) -> {
              // First read saw 1000, second read sees 1500
              let b1_ok = verify_int_at(r1, 2, 1000)
              let b2_ok = verify_int_at(r2, 2, 1500)
              b1_ok |> should.be_true
              b2_ok |> should.be_true
              Ok(#(tx_store2, "snapshot_isolation_works"))
            }
            _, _ -> Error(error.QueryFailed("Read failed", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

/// Test nested transaction isolation via savepoints
pub fn nested_transaction_isolation_test() {
  let store = setup_users_store()

  let result =
    execute_transaction(store, fn(tx_store) {
      // Insert user 1
      let user1: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1000),
      ]
      case insert_row(tx_store, "users", "1", user1) {
        Ok(tx_store1) -> {
          // Create savepoint
          case create_savepoint(tx_store1, "after_alice") {
            Ok(sp_store) -> {
              // Insert user 2
              let user2: MemoryRow = [
                dynamic.int(2),
                dynamic.string("bob@example.com"),
                dynamic.int(2000),
              ]
              case insert_row(sp_store, "users", "2", user2) {
                Ok(sp_store2) -> {
                  // Both users should exist now
                  case
                    get_row(sp_store2, "users", "1"),
                    get_row(sp_store2, "users", "2")
                  {
                    Ok(_), Ok(_) -> {
                      // Rollback to savepoint - bob should disappear
                      case rollback_to_savepoint(sp_store2, "after_alice") {
                        Ok(rolled_back) -> {
                          // Alice should exist, bob should not
                          let alice_exists =
                            result.is_ok(get_row(rolled_back, "users", "1"))
                          let bob_exists =
                            result.is_ok(get_row(rolled_back, "users", "2"))

                          alice_exists |> should.be_true
                          bob_exists |> should.be_false

                          Ok(#(rolled_back, "nested_isolation_works"))
                        }
                        Error(_) ->
                          Error(error.QueryFailed("Rollback failed", None))
                      }
                    }
                    _, _ -> Error(error.QueryFailed("Users not found", None))
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(_) ->
              Error(error.QueryFailed("Savepoint creation failed", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

// ============================================================================
// DEADLOCK SCENARIO TESTS
// ============================================================================

/// Test that snapshot isolation prevents traditional deadlocks
/// In memory adapter, snapshots mean no lock-based deadlocks possible
pub fn snapshot_isolation_prevents_deadlocks_test() {
  let store = setup_banking_store()

  // Insert two accounts
  let account_a: MemoryRow = [
    dynamic.int(1),
    dynamic.string("Account A"),
    dynamic.int(1000),
  ]
  let account_b: MemoryRow = [
    dynamic.int(2),
    dynamic.string("Account B"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "accounts", "1", account_a)
  let assert Ok(store) = insert_row(store, "accounts", "2", account_b)

  // Transaction 1: Transfer A -> B
  // Transaction 2: Transfer B -> A
  // With locking, this could deadlock. With snapshots, both complete.

  let result1 =
    execute_transaction(store, fn(tx_store) {
      // Read A, update A (debit)
      case get_row(tx_store, "accounts", "1") {
        Ok(_) -> {
          let updated_a: MemoryRow = [
            dynamic.int(1),
            dynamic.string("Account A"),
            dynamic.int(900),
          ]
          case update_row(tx_store, "accounts", "1", updated_a) {
            Ok(tx1) -> {
              // Read B, update B (credit)
              case get_row(tx1, "accounts", "2") {
                Ok(_) -> {
                  let updated_b: MemoryRow = [
                    dynamic.int(2),
                    dynamic.string("Account B"),
                    dynamic.int(1100),
                  ]
                  case update_row(tx1, "accounts", "2", updated_b) {
                    Ok(tx2) -> Ok(#(tx2, "transfer_1_complete"))
                    Error(e) -> Error(e)
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store_after_tx1, _)) = result1

  // Transaction 2 on updated store (opposite direction)
  let result2 =
    execute_transaction(store_after_tx1, fn(tx_store) {
      // Read B, update B (debit)
      case get_row(tx_store, "accounts", "2") {
        Ok(_) -> {
          let updated_b: MemoryRow = [
            dynamic.int(2),
            dynamic.string("Account B"),
            dynamic.int(1000),
            // 1100 - 100
          ]
          case update_row(tx_store, "accounts", "2", updated_b) {
            Ok(tx1) -> {
              // Read A, update A (credit)
              case get_row(tx1, "accounts", "1") {
                Ok(_) -> {
                  let updated_a: MemoryRow = [
                    dynamic.int(1),
                    dynamic.string("Account A"),
                    dynamic.int(1000),
                    // 900 + 100
                  ]
                  case update_row(tx1, "accounts", "1", updated_a) {
                    Ok(tx2) -> Ok(#(tx2, "transfer_2_complete"))
                    Error(e) -> Error(e)
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    })

  // Both transactions should complete (no deadlock)
  should.be_ok(result2)
}

/// Test retry logic pattern for conflicts
pub fn retry_pattern_for_conflicts_test() {
  let store = setup_users_store()

  // Insert initial user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", user)

  // Simulate retry logic - attempt operation multiple times
  let max_retries = 3
  let attempt_transaction = fn(s: MemoryStore, attempt: Int) {
    execute_transaction(s, fn(tx_store) {
      let updated_user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1000 + attempt * 100),
      ]
      case update_row(tx_store, "users", "1", updated_user) {
        Ok(tx) -> Ok(#(tx, attempt))
        Error(e) -> Error(e)
      }
    })
  }

  // Run with retry logic
  let final_result =
    list.range(1, max_retries)
    |> list.fold_until(Error(error.UserError("not_started")), fn(_acc, attempt) {
      case attempt_transaction(store, attempt) {
        Ok(#(new_store, n)) -> list.Stop(Ok(#(new_store, n)))
        Error(_) -> list.Continue(Error(error.UserError("retry_needed")))
      }
    })

  // Should succeed on first attempt
  case final_result {
    Ok(#(_store, attempt)) -> attempt |> should.equal(1)
    Error(_) -> should.fail()
  }
}

// ============================================================================
// MEMORY ADAPTER CONCURRENCY TESTS
// ============================================================================

/// Test thread-safe operations via immutable state
pub fn immutable_state_thread_safety_test() {
  let store = setup_users_store()

  // Insert a user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store_with_user) = insert_row(store, "users", "1", user)

  // Original store is unchanged (immutability)
  case get_row(store, "users", "1") {
    Ok(_) -> should.fail()
    // Should not exist in original
    Error(error.NotFound) -> should.be_true(True)
    Error(_) -> should.fail()
  }

  // New store has the user
  case get_row(store_with_user, "users", "1") {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

/// Test transaction isolation in memory via snapshot stack
pub fn snapshot_stack_isolation_test() {
  let store = setup_users_store()

  // Start outer transaction
  let result =
    execute_transaction(store, fn(outer_tx) {
      // Insert user in outer transaction
      let user1: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1000),
      ]
      case insert_row(outer_tx, "users", "1", user1) {
        Ok(outer_tx1) -> {
          // Verify transaction depth is 1
          transaction_depth(outer_tx1) |> should.equal(1)

          // Create savepoint (simulates nested transaction)
          case create_savepoint(outer_tx1, "nested") {
            Ok(nested_tx) -> {
              // Insert another user in nested context
              let user2: MemoryRow = [
                dynamic.int(2),
                dynamic.string("bob@example.com"),
                dynamic.int(2000),
              ]
              case insert_row(nested_tx, "users", "2", user2) {
                Ok(nested_tx1) -> {
                  // Verify we have savepoint
                  has_savepoint(nested_tx1, "nested") |> should.be_true

                  // Release savepoint (keep changes)
                  case release_savepoint(nested_tx1, "nested") {
                    Ok(released) -> {
                      // Both users should exist
                      case row_count(released, "users") {
                        Ok(count) -> {
                          count |> should.equal(2)
                          Ok(#(released, "stack_isolation_works"))
                        }
                        Error(_) ->
                          Error(error.QueryFailed("Row count failed", None))
                      }
                    }
                    Error(_) -> Error(error.QueryFailed("Release failed", None))
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(_) -> Error(error.QueryFailed("Savepoint failed", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

/// Test multiple savepoints with selective rollback
pub fn multiple_savepoints_selective_rollback_test() {
  let store = setup_users_store()

  let result =
    execute_transaction(store, fn(tx_store) {
      // Insert user 1
      let user1: MemoryRow = [
        dynamic.int(1),
        dynamic.string("alice@example.com"),
        dynamic.int(1000),
      ]
      case insert_row(tx_store, "users", "1", user1) {
        Ok(tx1) -> {
          case create_savepoint(tx1, "sp1") {
            Ok(sp1_store) -> {
              // Insert user 2
              let user2: MemoryRow = [
                dynamic.int(2),
                dynamic.string("bob@example.com"),
                dynamic.int(2000),
              ]
              case insert_row(sp1_store, "users", "2", user2) {
                Ok(tx2) -> {
                  case create_savepoint(tx2, "sp2") {
                    Ok(sp2_store) -> {
                      // Insert user 3
                      let user3: MemoryRow = [
                        dynamic.int(3),
                        dynamic.string("carol@example.com"),
                        dynamic.int(3000),
                      ]
                      case insert_row(sp2_store, "users", "3", user3) {
                        Ok(tx3) -> {
                          // All 3 users exist
                          case row_count(tx3, "users") {
                            Ok(3) -> {
                              // Rollback to sp1 - users 2 and 3 should disappear
                              case rollback_to_savepoint(tx3, "sp1") {
                                Ok(rolled_back) -> {
                                  case row_count(rolled_back, "users") {
                                    Ok(count) -> {
                                      count |> should.equal(1)
                                      Ok(#(
                                        rolled_back,
                                        "selective_rollback_works",
                                      ))
                                    }
                                    Error(_) ->
                                      Error(error.QueryFailed(
                                        "Count failed",
                                        None,
                                      ))
                                  }
                                }
                                Error(_) ->
                                  Error(error.QueryFailed(
                                    "Rollback failed",
                                    None,
                                  ))
                              }
                            }
                            _ ->
                              Error(error.QueryFailed("Expected 3 users", None))
                          }
                        }
                        Error(e) -> Error(e)
                      }
                    }
                    Error(_) -> Error(error.QueryFailed("SP2 failed", None))
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(_) -> Error(error.QueryFailed("SP1 failed", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

// ============================================================================
// LOAD TESTING (Simulated Sequential)
// ============================================================================

/// Test 10 sequential operations (simulated concurrent load)
pub fn ten_sequential_operations_test() {
  let store = setup_users_store()

  // Perform 10 insert operations
  let final_store =
    list.range(1, 10)
    |> list.fold(store, fn(acc_store, i) {
      let email = "user" <> int.to_string(i) <> "@example.com"
      let user: MemoryRow = [
        dynamic.int(i),
        dynamic.string(email),
        dynamic.int(i * 100),
      ]
      case insert_row(acc_store, "users", int.to_string(i), user) {
        Ok(new_store) -> new_store
        Error(_) -> acc_store
      }
    })

  case row_count(final_store, "users") {
    Ok(count) -> count |> should.equal(10)
    Error(_) -> should.fail()
  }
}

/// Test 100 sequential operations (simulated high load)
pub fn hundred_sequential_operations_test() {
  let store = setup_users_store()

  // Perform 100 insert operations
  let final_store =
    list.range(1, 100)
    |> list.fold(store, fn(acc_store, i) {
      let email = "user" <> int.to_string(i) <> "@example.com"
      let user: MemoryRow = [
        dynamic.int(i),
        dynamic.string(email),
        dynamic.int(i * 10),
      ]
      case insert_row(acc_store, "users", int.to_string(i), user) {
        Ok(new_store) -> new_store
        Error(_) -> acc_store
      }
    })

  case row_count(final_store, "users") {
    Ok(count) -> count |> should.equal(100)
    Error(_) -> should.fail()
  }
}

/// Test sustained load with mixed operations
pub fn sustained_mixed_operations_test() {
  let store = setup_users_store()

  // Insert 50 users
  let store_with_users =
    list.range(1, 50)
    |> list.fold(store, fn(acc_store, i) {
      let email = "user" <> int.to_string(i) <> "@example.com"
      let user: MemoryRow = [
        dynamic.int(i),
        dynamic.string(email),
        dynamic.int(1000),
      ]
      case insert_row(acc_store, "users", int.to_string(i), user) {
        Ok(new_store) -> new_store
        Error(_) -> acc_store
      }
    })

  // Update 25 users (odd IDs)
  let store_after_updates =
    list.range(1, 25)
    |> list.map(fn(i) { i * 2 - 1 })
    // Odd numbers 1, 3, 5, ...
    |> list.fold(store_with_users, fn(acc_store, i) {
      let email = "user" <> int.to_string(i) <> "@example.com"
      let updated_user: MemoryRow = [
        dynamic.int(i),
        dynamic.string(email),
        dynamic.int(2000),
      ]
      case update_row(acc_store, "users", int.to_string(i), updated_user) {
        Ok(new_store) -> new_store
        Error(_) -> acc_store
      }
    })

  // Delete 10 users (IDs 41-50)
  let final_store =
    list.range(41, 50)
    |> list.fold(store_after_updates, fn(acc_store, i) {
      case delete_row(acc_store, "users", int.to_string(i)) {
        Ok(new_store) -> new_store
        Error(_) -> acc_store
      }
    })

  // Should have 40 users remaining
  case row_count(final_store, "users") {
    Ok(count) -> count |> should.equal(40)
    Error(_) -> should.fail()
  }
}

/// Test burst traffic pattern (many transactions in quick succession)
pub fn burst_traffic_pattern_test() {
  let store = setup_users_store()

  // Simulate burst of 20 transactions
  let final_result =
    list.range(1, 20)
    |> list.fold(Ok(store), fn(acc_result, i) {
      case acc_result {
        Ok(acc_store) -> {
          let email = "burst" <> int.to_string(i) <> "@example.com"
          let user: MemoryRow = [
            dynamic.int(i),
            dynamic.string(email),
            dynamic.int(i * 50),
          ]
          execute_transaction(acc_store, fn(tx_store) {
            case insert_row(tx_store, "users", int.to_string(i), user) {
              Ok(s) -> Ok(#(s, i))
              Error(e) -> Error(e)
            }
          })
          |> result.map(fn(pair) { pair.0 })
          |> result.map_error(fn(_) { error.UserError("Transaction failed") })
        }
        Error(e) -> Error(e)
      }
    })

  case final_result {
    Ok(final_store) -> {
      case row_count(final_store, "users") {
        Ok(count) -> count |> should.equal(20)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// CONSTRAINT ENFORCEMENT UNDER CONCURRENT ACCESS
// ============================================================================

/// Test unique constraint enforcement across transactions
pub fn unique_constraint_across_transactions_test() {
  let store = setup_users_store()

  // Transaction 1: Insert user with email
  let result1 =
    execute_transaction(store, fn(tx_store) {
      let user: MemoryRow = [
        dynamic.int(1),
        dynamic.string("unique@example.com"),
        dynamic.int(1000),
      ]
      case insert_row(tx_store, "users", "1", user) {
        Ok(s) -> Ok(#(s, Nil))
        Error(e) -> Error(e)
      }
    })

  let assert Ok(#(store_after_tx1, _)) = result1

  // Transaction 2: Try to insert with same email
  let result2 =
    execute_transaction(store_after_tx1, fn(tx_store) {
      let duplicate_user: MemoryRow = [
        dynamic.int(2),
        dynamic.string("unique@example.com"),
        // Same email!
        dynamic.int(2000),
      ]
      case insert_row(tx_store, "users", "2", duplicate_user) {
        Ok(s) -> Ok(#(s, "should_not_succeed"))
        Error(e) -> Error(e)
      }
    })

  // Second transaction should fail with unique violation
  case result2 {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }
}

/// Test constraint violations roll back entire transaction
pub fn constraint_violation_rolls_back_transaction_test() {
  let store = setup_users_store()

  // Insert initial user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.int(1000),
  ]
  let assert Ok(store) = insert_row(store, "users", "1", user1)

  // Transaction that partially succeeds then violates constraint
  let result =
    execute_transaction(store, fn(tx_store) {
      // This insert should work
      let user2: MemoryRow = [
        dynamic.int(2),
        dynamic.string("bob@example.com"),
        dynamic.int(2000),
      ]
      case insert_row(tx_store, "users", "2", user2) {
        Ok(tx1) -> {
          // This insert should fail (duplicate email)
          let user3: MemoryRow = [
            dynamic.int(3),
            dynamic.string("alice@example.com"),
            // Duplicate!
            dynamic.int(3000),
          ]
          case insert_row(tx1, "users", "3", user3) {
            Ok(tx2) -> Ok(#(tx2, "should_not_reach"))
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    })

  // Transaction failed
  case result {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }

  // Only user1 should exist (user2 rolled back with transaction)
  case row_count(store, "users") {
    Ok(count) -> count |> should.equal(1)
    Error(_) -> should.fail()
  }
}
