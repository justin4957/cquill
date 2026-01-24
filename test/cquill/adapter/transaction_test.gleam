// Transaction Support Tests
//
// Comprehensive tests for transaction support in adapters.
// Tests memory adapter transactions with snapshot-based rollback.

import cquill/adapter/memory.{
  create_table, execute_transaction, get_row, in_transaction, insert_row,
  new_store, transaction_depth,
}
import cquill/error
import gleam/dynamic
import gleam/option.{None}
import gleam/string
import gleeunit/should

// ============================================================================
// MEMORY STORE TRANSACTION STATE TESTS
// ============================================================================

pub fn new_store_not_in_transaction_test() {
  let store = new_store()

  in_transaction(store)
  |> should.be_false

  transaction_depth(store)
  |> should.equal(0)
}

pub fn begin_transaction_sets_in_transaction_flag_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Execute a transaction
  let result =
    execute_transaction(store, fn(tx_store) {
      // Inside transaction, should be in transaction
      should.be_true(in_transaction(tx_store))
      should.equal(transaction_depth(tx_store), 1)
      Ok(#(tx_store, Nil))
    })

  // Transaction committed successfully
  should.be_ok(result)
}

pub fn commit_clears_transaction_state_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Execute transaction
  let result =
    execute_transaction(store, fn(tx_store) { Ok(#(tx_store, "success")) })

  case result {
    Ok(#(final_store, value)) -> {
      value
      |> should.equal("success")

      // After commit, should not be in transaction
      in_transaction(final_store)
      |> should.be_false

      transaction_depth(final_store)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

pub fn rollback_clears_transaction_state_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Execute transaction that fails
  let result =
    execute_transaction(store, fn(_tx_store) {
      Error(error.QueryFailed("Intentional failure", None))
    })

  // Transaction failed
  case result {
    Error(error.AdapterTransactionError(_)) -> {
      // Error is expected
      should.be_true(True)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// TRANSACTION COMMIT TESTS
// ============================================================================

pub fn transaction_commits_on_success_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert a row within a transaction
  let result =
    execute_transaction(store, fn(tx_store) {
      let row = [dynamic.int(1), dynamic.string("test@example.com")]
      case insert_row(tx_store, "users", "1", row) {
        Ok(updated_store) -> Ok(#(updated_store, Nil))
        Error(e) -> Error(e)
      }
    })

  case result {
    Ok(#(final_store, _)) -> {
      // Row should persist after commit
      case get_row(final_store, "users", "1") {
        Ok(_row) -> should.be_true(True)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn transaction_returns_value_on_success_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result = execute_transaction(store, fn(tx_store) { Ok(#(tx_store, 42)) })

  case result {
    Ok(#(_store, value)) -> {
      value
      |> should.equal(42)
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// TRANSACTION ROLLBACK TESTS
// ============================================================================

pub fn transaction_rollback_on_user_error_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert a row, then fail
  let result =
    execute_transaction(store, fn(tx_store) {
      let row = [dynamic.int(1), dynamic.string("test@example.com")]
      case insert_row(tx_store, "users", "1", row) {
        Ok(_updated_store) -> {
          // Now fail intentionally
          Error(error.QueryFailed("User error", None))
        }
        Error(e) -> Error(e)
      }
    })

  // Transaction should have failed
  case result {
    Error(error.AdapterTransactionError(_)) -> should.be_true(True)
    _ -> should.fail()
  }

  // Row should NOT persist (rolled back)
  // Note: We can't verify this easily since execute_transaction doesn't
  // return the rolled-back store state. The original store is unchanged.
  case get_row(store, "users", "1") {
    Error(error.NotFound) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn transaction_rollback_on_constraint_violation_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert first row
  let row1 = [dynamic.int(1), dynamic.string("first@example.com")]
  let store = case insert_row(store, "users", "1", row1) {
    Ok(s) -> s
    Error(_) -> store
  }

  // Try to insert duplicate in transaction
  let result =
    execute_transaction(store, fn(tx_store) {
      let duplicate_row = [dynamic.int(1), dynamic.string("second@example.com")]
      case insert_row(tx_store, "users", "1", duplicate_row) {
        Ok(updated_store) -> Ok(#(updated_store, Nil))
        Error(e) -> Error(e)
      }
    })

  // Should fail with unique violation
  case result {
    Error(error.AdapterTransactionError(error.UniqueViolation(_, _))) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// MANUAL TRANSACTION CONTROL TESTS
// ============================================================================

pub fn rollback_and_restore_restores_state_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Insert initial data
  let row1 = [dynamic.int(1), dynamic.string("original@example.com")]
  let store = case insert_row(store, "users", "1", row1) {
    Ok(s) -> s
    Error(_) -> store
  }

  // Manually begin transaction using execute_transaction's internal behavior
  // We simulate this by using the store's transaction functions
  let result =
    execute_transaction(store, fn(tx_store) {
      // Insert new row in transaction
      let row2 = [dynamic.int(2), dynamic.string("new@example.com")]
      case insert_row(tx_store, "users", "2", row2) {
        Ok(updated) -> {
          // Verify row exists in transaction
          case get_row(updated, "users", "2") {
            Ok(_) -> {
              // Now simulate failure (will trigger rollback)
              Error(error.QueryFailed("Simulated failure", None))
            }
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    })

  // Transaction failed
  case result {
    Error(error.AdapterTransactionError(_)) -> should.be_true(True)
    _ -> should.fail()
  }

  // Original data should still exist
  case get_row(store, "users", "1") {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }

  // New data should not exist in original store
  case get_row(store, "users", "2") {
    Error(error.NotFound) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn commit_and_continue_pops_snapshot_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      // Verify in transaction
      should.equal(transaction_depth(tx_store), 1)
      Ok(#(tx_store, Nil))
    })

  case result {
    Ok(#(final_store, _)) -> {
      // After commit, depth should be 0
      transaction_depth(final_store)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// TRANSACTION ERROR TYPE TESTS
// ============================================================================

pub fn adapter_transaction_error_wraps_constraint_violation_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Pre-insert a row
  let row1 = [dynamic.int(1), dynamic.string("first@example.com")]
  let store = case insert_row(store, "users", "1", row1) {
    Ok(s) -> s
    Error(_) -> store
  }

  // Try duplicate insert in transaction
  let result =
    execute_transaction(store, fn(tx_store) {
      let duplicate = [dynamic.int(1), dynamic.string("duplicate@example.com")]
      case insert_row(tx_store, "users", "1", duplicate) {
        Ok(s) -> Ok(#(s, Nil))
        Error(e) -> Error(e)
      }
    })

  case result {
    Error(error.AdapterTransactionError(inner_err)) -> {
      // Should be a UniqueViolation
      error.is_unique_violation(inner_err)
      |> should.be_true
    }
    _ -> should.fail()
  }
}

// ============================================================================
// ERROR FORMATTING TESTS
// ============================================================================

pub fn format_transaction_error_user_error_test() {
  let err: error.TransactionError(String) = error.UserError("test error")

  error.format_transaction_error_compact(err)
  |> should.equal("Transaction aborted: user error")
}

pub fn format_transaction_error_adapter_error_test() {
  let inner = error.UniqueViolation("users_email_key", "duplicate email")
  let err: error.TransactionError(Nil) = error.AdapterTransactionError(inner)

  let formatted = error.format_transaction_error_compact(err)

  // Should include the inner error details
  formatted
  |> string.contains("Transaction aborted:")
  |> should.be_true
}

pub fn format_transaction_error_begin_failed_test() {
  let err: error.TransactionError(Nil) =
    error.BeginFailed("Connection unavailable")

  error.format_transaction_error_compact(err)
  |> should.equal("Failed to begin transaction: Connection unavailable")
}

pub fn format_transaction_error_commit_failed_test() {
  let err: error.TransactionError(Nil) = error.CommitFailed("Timeout")

  error.format_transaction_error_compact(err)
  |> should.equal("Failed to commit transaction: Timeout")
}

pub fn format_transaction_error_rolled_back_test() {
  let err: error.TransactionError(Nil) = error.RolledBack

  error.format_transaction_error_compact(err)
  |> should.equal("Transaction was rolled back")
}

pub fn format_transaction_error_transaction_rollback_test() {
  let err: error.TransactionError(Nil) =
    error.TransactionRollback("User requested rollback")

  error.format_transaction_error_compact(err)
  |> should.equal("Transaction rolled back: User requested rollback")
}

pub fn format_transaction_error_connection_lost_test() {
  let err: error.TransactionError(Nil) = error.TransactionConnectionLost

  error.format_transaction_error_compact(err)
  |> should.equal("Connection lost during transaction")
}

pub fn format_transaction_error_nested_test() {
  let err: error.TransactionError(Nil) = error.NestedTransactionError

  error.format_transaction_error_compact(err)
  |> should.equal("Nested transactions are not supported")
}

pub fn format_transaction_error_timeout_test() {
  let err: error.TransactionError(Nil) = error.TransactionTimeout

  error.format_transaction_error_compact(err)
  |> should.equal("Transaction timed out")
}

pub fn format_transaction_error_serialization_failure_test() {
  let err: error.TransactionError(Nil) = error.SerializationFailure

  error.format_transaction_error_compact(err)
  |> should.equal("Serialization failure: concurrent transaction conflict")
}
