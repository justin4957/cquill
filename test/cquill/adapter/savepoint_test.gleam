// Savepoint Tests
//
// Comprehensive tests for savepoint support in adapters.
// Tests memory adapter savepoints with named snapshot-based rollback.

import cquill/adapter/memory.{
  create_savepoint, create_table, execute_savepoint, execute_transaction,
  get_row, has_savepoint, insert_row, new_store, release_savepoint,
  rollback_to_savepoint, savepoint_names,
}
import cquill/error
import gleam/dynamic
import gleam/option.{None}
import gleeunit/should

// ============================================================================
// SAVEPOINT CREATION TESTS
// ============================================================================

pub fn create_savepoint_in_transaction_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Execute a transaction with a savepoint
  let result =
    execute_transaction(store, fn(tx_store) {
      // Create a savepoint
      case create_savepoint(tx_store, "sp1") {
        Ok(sp_store) -> {
          // Verify savepoint exists
          should.be_true(has_savepoint(sp_store, "sp1"))
          Ok(#(sp_store, Nil))
        }
        Error(_) -> Error(error.QueryFailed("Failed to create savepoint", None))
      }
    })

  should.be_ok(result)
}

pub fn create_savepoint_outside_transaction_fails_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  // Try to create savepoint outside transaction
  let result = create_savepoint(store, "sp1")

  case result {
    Error(error.SavepointNoTransaction) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn create_multiple_savepoints_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      case create_savepoint(tx_store, "sp1") {
        Ok(sp_store1) ->
          case create_savepoint(sp_store1, "sp2") {
            Ok(sp_store2) -> {
              // Both savepoints should exist
              should.be_true(has_savepoint(sp_store2, "sp1"))
              should.be_true(has_savepoint(sp_store2, "sp2"))

              // Check savepoint names
              let names = savepoint_names(sp_store2)
              should.equal(names, ["sp2", "sp1"])

              Ok(#(sp_store2, Nil))
            }
            Error(_) ->
              Error(error.QueryFailed("Failed to create savepoint 2", None))
          }
        Error(_) ->
          Error(error.QueryFailed("Failed to create savepoint 1", None))
      }
    })

  should.be_ok(result)
}

// ============================================================================
// SAVEPOINT ROLLBACK TESTS
// ============================================================================

pub fn rollback_to_savepoint_restores_state_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      // Insert first row
      let row1 = [dynamic.int(1), dynamic.string("alice@example.com")]
      case insert_row(tx_store, "users", "1", row1) {
        Ok(store1) -> {
          // Create savepoint
          case create_savepoint(store1, "before_bob") {
            Ok(sp_store) -> {
              // Insert second row
              let row2 = [dynamic.int(2), dynamic.string("bob@example.com")]
              case insert_row(sp_store, "users", "2", row2) {
                Ok(store2) -> {
                  // Verify bob exists
                  case get_row(store2, "users", "2") {
                    Ok(_) -> {
                      // Rollback to savepoint
                      case rollback_to_savepoint(store2, "before_bob") {
                        Ok(rolled_back) -> {
                          // Bob should no longer exist
                          case get_row(rolled_back, "users", "2") {
                            Error(error.NotFound) -> {
                              // Alice should still exist
                              case get_row(rolled_back, "users", "1") {
                                Ok(_) -> Ok(#(rolled_back, Nil))
                                Error(_) ->
                                  Error(error.QueryFailed("Alice missing", None))
                              }
                            }
                            Ok(_) ->
                              Error(error.QueryFailed(
                                "Bob should be rolled back",
                                None,
                              ))
                            Error(_) ->
                              Error(error.QueryFailed("Unexpected error", None))
                          }
                        }
                        Error(_) ->
                          Error(error.QueryFailed(
                            "Failed to rollback savepoint",
                            None,
                          ))
                      }
                    }
                    Error(_) ->
                      Error(error.QueryFailed("Bob not inserted", None))
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(_) ->
              Error(error.QueryFailed("Failed to create savepoint", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

pub fn rollback_to_nonexistent_savepoint_fails_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      case rollback_to_savepoint(tx_store, "nonexistent") {
        Error(error.SavepointNotFound(name)) -> {
          should.equal(name, "nonexistent")
          // Return error to trigger transaction rollback
          Error(error.QueryFailed("Expected failure", None))
        }
        _ -> Error(error.QueryFailed("Should have failed", None))
      }
    })

  case result {
    Error(error.AdapterTransactionError(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// SAVEPOINT RELEASE TESTS
// ============================================================================

pub fn release_savepoint_removes_it_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      case create_savepoint(tx_store, "sp1") {
        Ok(sp_store) -> {
          should.be_true(has_savepoint(sp_store, "sp1"))
          case release_savepoint(sp_store, "sp1") {
            Ok(released_store) -> {
              should.be_false(has_savepoint(released_store, "sp1"))
              Ok(#(released_store, Nil))
            }
            Error(_) ->
              Error(error.QueryFailed("Failed to release savepoint", None))
          }
        }
        Error(_) -> Error(error.QueryFailed("Failed to create savepoint", None))
      }
    })

  should.be_ok(result)
}

pub fn release_nonexistent_savepoint_fails_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      case release_savepoint(tx_store, "nonexistent") {
        Error(error.SavepointNotFound(_)) -> {
          Error(error.QueryFailed("Expected failure", None))
        }
        _ -> Error(error.QueryFailed("Should have failed", None))
      }
    })

  case result {
    Error(error.AdapterTransactionError(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// EXECUTE SAVEPOINT TESTS
// ============================================================================

pub fn execute_savepoint_commits_on_success_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      // Execute operation in savepoint
      case
        execute_savepoint(tx_store, "sp1", fn(sp_store) {
          let row = [dynamic.int(1), dynamic.string("test@example.com")]
          case insert_row(sp_store, "users", "1", row) {
            Ok(updated) -> Ok(#(updated, "inserted"))
            Error(e) -> Error(e)
          }
        })
      {
        Ok(#(final_store, value)) -> {
          should.equal(value, "inserted")
          // Savepoint should be released
          should.be_false(has_savepoint(final_store, "sp1"))
          // Row should exist
          case get_row(final_store, "users", "1") {
            Ok(_) -> Ok(#(final_store, Nil))
            Error(_) -> Error(error.QueryFailed("Row not found", None))
          }
        }
        Error(_) -> Error(error.QueryFailed("Savepoint execution failed", None))
      }
    })

  should.be_ok(result)
}

pub fn execute_savepoint_rollback_on_error_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      // Insert initial data
      let row1 = [dynamic.int(1), dynamic.string("alice@example.com")]
      case insert_row(tx_store, "users", "1", row1) {
        Ok(store1) -> {
          // Execute operation in savepoint that fails
          case
            execute_savepoint(store1, "sp1", fn(sp_store) {
              let row2 = [dynamic.int(2), dynamic.string("bob@example.com")]
              case insert_row(sp_store, "users", "2", row2) {
                Ok(_updated) -> {
                  // Now fail intentionally
                  Error(error.QueryFailed("Intentional failure", None))
                }
                Error(e) -> Error(e)
              }
            })
          {
            Error(error.SavepointAdapterError(_)) -> {
              // Bob should not exist (rolled back)
              case get_row(store1, "users", "2") {
                Error(error.NotFound) -> {
                  // Alice should still exist
                  case get_row(store1, "users", "1") {
                    Ok(_) -> Ok(#(store1, Nil))
                    Error(_) -> Error(error.QueryFailed("Alice missing", None))
                  }
                }
                Ok(_) ->
                  Error(error.QueryFailed("Bob should be rolled back", None))
                Error(_) -> Error(error.QueryFailed("Unexpected error", None))
              }
            }
            Ok(_) ->
              Error(error.QueryFailed("Savepoint should have failed", None))
            Error(_) -> Error(error.QueryFailed("Unexpected error type", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

// ============================================================================
// NESTED SAVEPOINT TESTS
// ============================================================================

pub fn nested_savepoints_work_correctly_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      // Insert alice
      let row1 = [dynamic.int(1), dynamic.string("alice@example.com")]
      case insert_row(tx_store, "users", "1", row1) {
        Ok(store1) -> {
          // Create outer savepoint
          case create_savepoint(store1, "outer") {
            Ok(outer_store) -> {
              // Insert bob
              let row2 = [dynamic.int(2), dynamic.string("bob@example.com")]
              case insert_row(outer_store, "users", "2", row2) {
                Ok(store2) -> {
                  // Create inner savepoint
                  case create_savepoint(store2, "inner") {
                    Ok(inner_store) -> {
                      // Insert charlie
                      let row3 = [
                        dynamic.int(3),
                        dynamic.string("charlie@example.com"),
                      ]
                      case insert_row(inner_store, "users", "3", row3) {
                        Ok(store3) -> {
                          // Rollback to inner savepoint (removes charlie)
                          case rollback_to_savepoint(store3, "inner") {
                            Ok(rolled1) -> {
                              // Charlie should be gone
                              case get_row(rolled1, "users", "3") {
                                Error(error.NotFound) -> {
                                  // Bob should still exist
                                  case get_row(rolled1, "users", "2") {
                                    Ok(_) -> {
                                      // Rollback to outer savepoint (removes bob too)
                                      case
                                        rollback_to_savepoint(rolled1, "outer")
                                      {
                                        Ok(rolled2) -> {
                                          // Both bob and charlie should be gone
                                          case get_row(rolled2, "users", "2") {
                                            Error(error.NotFound) -> {
                                              // Alice should still exist
                                              case
                                                get_row(rolled2, "users", "1")
                                              {
                                                Ok(_) -> Ok(#(rolled2, Nil))
                                                Error(_) ->
                                                  Error(error.QueryFailed(
                                                    "Alice missing",
                                                    None,
                                                  ))
                                              }
                                            }
                                            _ ->
                                              Error(error.QueryFailed(
                                                "Bob should be gone",
                                                None,
                                              ))
                                          }
                                        }
                                        Error(_) ->
                                          Error(error.QueryFailed(
                                            "Outer rollback failed",
                                            None,
                                          ))
                                      }
                                    }
                                    Error(_) ->
                                      Error(error.QueryFailed(
                                        "Bob missing",
                                        None,
                                      ))
                                  }
                                }
                                _ ->
                                  Error(error.QueryFailed(
                                    "Charlie should be gone",
                                    None,
                                  ))
                              }
                            }
                            Error(_) ->
                              Error(error.QueryFailed(
                                "Inner rollback failed",
                                None,
                              ))
                          }
                        }
                        Error(e) -> Error(e)
                      }
                    }
                    Error(_) ->
                      Error(error.QueryFailed(
                        "Failed to create inner savepoint",
                        None,
                      ))
                  }
                }
                Error(e) -> Error(e)
              }
            }
            Error(_) ->
              Error(error.QueryFailed("Failed to create outer savepoint", None))
          }
        }
        Error(e) -> Error(e)
      }
    })

  should.be_ok(result)
}

// ============================================================================
// SAVEPOINT STATE QUERY TESTS
// ============================================================================

pub fn has_savepoint_returns_false_when_not_exists_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      should.be_false(has_savepoint(tx_store, "nonexistent"))
      Ok(#(tx_store, Nil))
    })

  should.be_ok(result)
}

pub fn savepoint_names_returns_empty_list_initially_test() {
  let store = new_store()
  let store = create_table(store, "users", "id")

  let result =
    execute_transaction(store, fn(tx_store) {
      let names = savepoint_names(tx_store)
      should.equal(names, [])
      Ok(#(tx_store, Nil))
    })

  should.be_ok(result)
}

// ============================================================================
// ERROR TYPE TESTS
// ============================================================================

pub fn format_savepoint_not_found_error_test() {
  let err: error.SavepointError(Nil) = error.SavepointNotFound("my_savepoint")

  error.format_savepoint_error_compact(err)
  |> should.equal("Savepoint not found: my_savepoint")
}

pub fn format_savepoint_creation_failed_error_test() {
  let err: error.SavepointError(Nil) =
    error.SavepointCreationFailed("Database error")

  error.format_savepoint_error_compact(err)
  |> should.equal("Failed to create savepoint: Database error")
}

pub fn format_savepoint_release_failed_error_test() {
  let err: error.SavepointError(Nil) =
    error.SavepointReleaseFailed("Release error")

  error.format_savepoint_error_compact(err)
  |> should.equal("Failed to release savepoint: Release error")
}

pub fn format_savepoint_no_transaction_error_test() {
  let err: error.SavepointError(Nil) = error.SavepointNoTransaction

  error.format_savepoint_error_compact(err)
  |> should.equal("Cannot use savepoint outside of a transaction")
}

pub fn format_savepoint_user_error_test() {
  let err: error.SavepointError(String) = error.SavepointUserError("user error")

  error.format_savepoint_error_compact(err)
  |> should.equal("Savepoint aborted: user error")
}
