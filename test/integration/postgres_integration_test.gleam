// PostgreSQL Integration Tests
//
// These tests require a running PostgreSQL database.
// They test real database operations including:
// - Connection pooling
// - CRUD operations
// - Transactions
// - Constraint violations
// - Error handling
//
// Configuration via environment variables:
//   DATABASE_URL=postgresql://user:pass@host:port/database
//   or individual variables:
//   POSTGRES_HOST (default: localhost)
//   POSTGRES_PORT (default: 5432)
//   POSTGRES_USER (default: cquill_test)
//   POSTGRES_PASSWORD (default: cquill_test)
//   POSTGRES_DATABASE (default: cquill_test)
//
// Run locally:
//   docker-compose up -d
//   gleam test test/integration
//
// Skip these tests if no database is available by setting:
//   SKIP_POSTGRES_TESTS=true

import cquill/adapter
import cquill/adapter/postgres
import cquill/error
import envoy
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// TEST CONFIGURATION
// ============================================================================

/// Check if PostgreSQL tests should be skipped
fn should_skip_tests() -> Bool {
  case envoy.get("SKIP_POSTGRES_TESTS") {
    Ok("true") -> True
    Ok("1") -> True
    _ -> False
  }
}

/// Get database configuration from environment with a unique pool name
fn get_test_config() -> Result(postgres.PostgresConfig, String) {
  case should_skip_tests() {
    True -> Error("PostgreSQL tests skipped (SKIP_POSTGRES_TESTS=true)")
    False -> {
      // Each call creates a unique pool name to avoid conflicts
      let name = process.new_name("cquill_integration_test_pool")
      // Try DATABASE_URL first
      case envoy.get("DATABASE_URL") {
        Ok(url) -> {
          case postgres.config_from_url(name, url) {
            // Use pool_size of 2 for integration tests
            Ok(config) -> Ok(postgres.pool_size(config, 2))
            Error(_) -> Error("Invalid DATABASE_URL format")
          }
        }
        Error(_) -> {
          // Fall back to individual environment variables
          let host = result.unwrap(envoy.get("POSTGRES_HOST"), "localhost")
          let port =
            envoy.get("POSTGRES_PORT")
            |> result.try(int.parse)
            |> result.unwrap(5432)
          let user = result.unwrap(envoy.get("POSTGRES_USER"), "cquill_test")
          let password =
            envoy.get("POSTGRES_PASSWORD")
            |> result.map(Some)
            |> result.unwrap(Some("cquill_test"))
          let database =
            result.unwrap(envoy.get("POSTGRES_DATABASE"), "cquill_test")

          Ok(
            postgres.default_config(name)
            |> postgres.host(host)
            |> postgres.port(port)
            |> postgres.user(user)
            |> postgres.password(password)
            |> postgres.database(database)
            // Use pool_size of 2 for integration tests
            |> postgres.pool_size(2),
          )
        }
      }
    }
  }
}

/// Start a test connection pool
fn start_test_pool() -> Result(postgres.PostgresConnection, String) {
  case get_test_config() {
    Error(reason) -> Error(reason)
    Ok(config) ->
      case postgres.start(config) {
        Ok(conn) -> Ok(conn)
        Error(_) -> Error("Failed to connect to PostgreSQL")
      }
  }
}

/// Clean up test data before each test
fn cleanup_test_data(conn: postgres.PostgresConnection) -> Nil {
  // Delete in reverse order of dependencies
  let _ = postgres.execute_sql_mutation(conn, "DELETE FROM post_tags", [])
  let _ = postgres.execute_sql_mutation(conn, "DELETE FROM comments", [])
  let _ = postgres.execute_sql_mutation(conn, "DELETE FROM posts", [])
  let _ = postgres.execute_sql_mutation(conn, "DELETE FROM users", [])
  let _ = postgres.execute_sql_mutation(conn, "DELETE FROM tags", [])
  let _ = postgres.execute_sql_mutation(conn, "DELETE FROM products", [])
  // Reset sequences
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "ALTER SEQUENCE users_id_seq RESTART WITH 1",
      [],
    )
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "ALTER SEQUENCE posts_id_seq RESTART WITH 1",
      [],
    )
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "ALTER SEQUENCE comments_id_seq RESTART WITH 1",
      [],
    )
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "ALTER SEQUENCE tags_id_seq RESTART WITH 1",
      [],
    )
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "ALTER SEQUENCE products_id_seq RESTART WITH 1",
      [],
    )
  Nil
}

// ============================================================================
// MAIN INTEGRATION TEST
//
// This single test function runs all PostgreSQL integration tests sequentially
// to avoid connection pool contention when gleeunit runs tests in parallel.
// ============================================================================

pub fn postgres_integration_test() {
  case start_test_pool() {
    Error(reason) -> {
      io.println("Skipping PostgreSQL tests: " <> reason)
      True |> should.be_true
    }
    Ok(conn) -> {
      io.println("Running PostgreSQL integration tests...")

      // Run all tests sequentially with the same connection pool
      io.println("  - test_simple_query")
      test_simple_query(conn)

      io.println("  - test_insert_user")
      test_insert_user(conn)

      io.println("  - test_insert_duplicate_email_fails")
      test_insert_duplicate_email_fails(conn)

      io.println("  - test_select_all_users")
      test_select_all_users(conn)

      io.println("  - test_select_with_where_clause")
      test_select_with_where_clause(conn)

      io.println("  - test_update_user")
      test_update_user(conn)

      io.println("  - test_delete_user")
      test_delete_user(conn)

      io.println("  - test_transaction_commit")
      test_transaction_commit(conn)

      io.println("  - test_transaction_rollback")
      test_transaction_rollback(conn)

      io.println("  - test_foreign_key_violation")
      test_foreign_key_violation(conn)

      io.println("  - test_batch_insert")
      test_batch_insert(conn)

      io.println("  - test_decode_integer_values")
      test_decode_integer_values(conn)

      io.println("  - test_decode_string_values")
      test_decode_string_values(conn)

      io.println("  - test_decode_bool_values")
      test_decode_bool_values(conn)

      io.println("  - test_adapter_query_interface")
      test_adapter_query_interface(conn)

      io.println("  - test_adapter_mutate_interface")
      test_adapter_mutate_interface(conn)

      io.println("All PostgreSQL integration tests passed!")
      // All tests passed
      True |> should.be_true
    }
  }
}

// ============================================================================
// INDIVIDUAL TEST FUNCTIONS (called sequentially from main test)
// ============================================================================

fn test_simple_query(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  case postgres.execute_sql(conn, "SELECT 1 as value", []) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
    }
    Error(err) -> {
      io.println("simple_query failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_insert_user(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  let sql =
    "INSERT INTO users (email, name, active) VALUES ($1, $2, $3) RETURNING id"
  let params = [
    adapter.ParamString("alice@example.com"),
    adapter.ParamString("Alice"),
    adapter.ParamBool(True),
  ]

  case postgres.execute_sql(conn, sql, params) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)

      // Verify the user was inserted
      case
        postgres.execute_sql(conn, "SELECT email FROM users WHERE email = $1", [
          adapter.ParamString("alice@example.com"),
        ])
      {
        Ok(result) -> list.length(result) |> should.equal(1)
        Error(err) -> {
          io.println("insert_user verify failed: " <> error.format_error(err))
          should.fail()
        }
      }
    }
    Error(err) -> {
      io.println("insert_user failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_insert_duplicate_email_fails(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Insert first user
  let sql = "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)"
  let params = [
    adapter.ParamString("duplicate@example.com"),
    adapter.ParamString("First"),
    adapter.ParamBool(True),
  ]
  let assert Ok(_) = postgres.execute_sql_mutation(conn, sql, params)

  // Try to insert duplicate
  let result = postgres.execute_sql_mutation(conn, sql, params)

  case result {
    Error(err) -> {
      // Should be a unique violation
      error.is_unique_violation(err) |> should.be_true
    }
    Ok(_) -> {
      io.println("Expected unique violation error but got success")
      should.fail()
    }
  }
  Nil
}

fn test_select_all_users(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Insert test data
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("user1@example.com"),
        adapter.ParamString("User 1"),
        adapter.ParamBool(True),
      ],
    )
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("user2@example.com"),
        adapter.ParamString("User 2"),
        adapter.ParamBool(False),
      ],
    )

  // Select all users
  case postgres.execute_sql(conn, "SELECT id, email, name FROM users", []) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(2)
    }
    Error(err) -> {
      io.println("select_all_users failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_select_with_where_clause(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Insert test data
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("active@example.com"),
        adapter.ParamString("Active User"),
        adapter.ParamBool(True),
      ],
    )
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("inactive@example.com"),
        adapter.ParamString("Inactive User"),
        adapter.ParamBool(False),
      ],
    )

  // Select only active users
  case
    postgres.execute_sql(conn, "SELECT email FROM users WHERE active = $1", [
      adapter.ParamBool(True),
    ])
  {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
    }
    Error(err) -> {
      io.println("select_with_where failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_update_user(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Insert a user
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("update@example.com"),
        adapter.ParamString("Original Name"),
        adapter.ParamBool(True),
      ],
    )

  // Update the user
  case
    postgres.execute_sql_mutation(
      conn,
      "UPDATE users SET name = $1 WHERE email = $2",
      [
        adapter.ParamString("Updated Name"),
        adapter.ParamString("update@example.com"),
      ],
    )
  {
    Ok(count) -> {
      count |> should.equal(1)
    }
    Error(err) -> {
      io.println("update_user failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_delete_user(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Insert a user
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("delete@example.com"),
        adapter.ParamString("Delete Me"),
        adapter.ParamBool(True),
      ],
    )

  // Delete the user
  case
    postgres.execute_sql_mutation(conn, "DELETE FROM users WHERE email = $1", [
      adapter.ParamString("delete@example.com"),
    ])
  {
    Ok(count) -> {
      count |> should.equal(1)

      // Verify deletion
      case
        postgres.execute_sql(conn, "SELECT id FROM users WHERE email = $1", [
          adapter.ParamString("delete@example.com"),
        ])
      {
        Ok(rows) -> list.length(rows) |> should.equal(0)
        Error(err) -> {
          io.println("delete_user verify failed: " <> error.format_error(err))
          should.fail()
        }
      }
    }
    Error(err) -> {
      io.println("delete_user failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_transaction_commit(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Execute a transaction that succeeds
  let result =
    postgres.execute_transaction(conn, fn(tx_conn) {
      let _ =
        postgres.execute_sql_mutation(
          tx_conn,
          "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
          [
            adapter.ParamString("tx_commit@example.com"),
            adapter.ParamString("TX User"),
            adapter.ParamBool(True),
          ],
        )
      Ok(Nil)
    })

  case result {
    Ok(_) -> {
      // Verify the user was committed
      case
        postgres.execute_sql(conn, "SELECT id FROM users WHERE email = $1", [
          adapter.ParamString("tx_commit@example.com"),
        ])
      {
        Ok(rows) -> list.length(rows) |> should.equal(1)
        Error(err) -> {
          io.println("tx_commit verify failed: " <> error.format_error(err))
          should.fail()
        }
      }
    }
    Error(_) -> {
      io.println("tx_commit failed")
      should.fail()
    }
  }
  Nil
}

fn test_transaction_rollback(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Execute a transaction that fails
  let result =
    postgres.execute_transaction(conn, fn(tx_conn) {
      // Insert a user
      let _ =
        postgres.execute_sql_mutation(
          tx_conn,
          "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
          [
            adapter.ParamString("tx_rollback@example.com"),
            adapter.ParamString("TX User"),
            adapter.ParamBool(True),
          ],
        )
      // Return an error to trigger rollback
      Error(error.QueryFailed("Intentional failure", None))
    })

  case result {
    Error(_) -> {
      // Verify the user was NOT committed (rolled back)
      case
        postgres.execute_sql(conn, "SELECT id FROM users WHERE email = $1", [
          adapter.ParamString("tx_rollback@example.com"),
        ])
      {
        Ok(rows) -> list.length(rows) |> should.equal(0)
        Error(err) -> {
          io.println("tx_rollback verify failed: " <> error.format_error(err))
          should.fail()
        }
      }
    }
    Ok(_) -> {
      io.println("Expected transaction to fail but it succeeded")
      should.fail()
    }
  }
  Nil
}

fn test_foreign_key_violation(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Try to insert a post with non-existent user_id
  let result =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO posts (user_id, title, body) VALUES ($1, $2, $3)",
      [
        adapter.ParamInt(9999),
        adapter.ParamString("Orphan Post"),
        adapter.ParamString("This should fail"),
      ],
    )

  case result {
    Error(err) -> {
      error.is_foreign_key_violation(err) |> should.be_true
    }
    Ok(_) -> {
      io.println("Expected foreign key violation but got success")
      should.fail()
    }
  }
  Nil
}

fn test_batch_insert(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  let rows = [
    [
      adapter.ParamString("batch1@example.com"),
      adapter.ParamString("Batch 1"),
      adapter.ParamBool(True),
    ],
    [
      adapter.ParamString("batch2@example.com"),
      adapter.ParamString("Batch 2"),
      adapter.ParamBool(True),
    ],
    [
      adapter.ParamString("batch3@example.com"),
      adapter.ParamString("Batch 3"),
      adapter.ParamBool(False),
    ],
  ]

  case postgres.insert_all(conn, "users", ["email", "name", "active"], rows) {
    Ok(count) -> {
      count |> should.equal(3)

      // Verify all were inserted
      case postgres.execute_sql(conn, "SELECT id FROM users", []) {
        Ok(result) -> list.length(result) |> should.equal(3)
        Error(err) -> {
          io.println("batch_insert verify failed: " <> error.format_error(err))
          should.fail()
        }
      }
    }
    Error(err) -> {
      io.println("batch_insert failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_decode_integer_values(conn: postgres.PostgresConnection) -> Nil {
  case postgres.execute_sql(conn, "SELECT 42 as value", []) {
    Ok(rows) -> {
      case rows {
        [row] -> {
          case postgres.decode_int(row, 0) {
            Ok(value) -> value |> should.equal(42)
            Error(_) -> {
              io.println("Failed to decode int from row")
              should.fail()
            }
          }
        }
        _ -> {
          io.println("Expected 1 row, got " <> int.to_string(list.length(rows)))
          should.fail()
        }
      }
    }
    Error(err) -> {
      io.println("decode_integer failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_decode_string_values(conn: postgres.PostgresConnection) -> Nil {
  case postgres.execute_sql(conn, "SELECT 'hello' as value", []) {
    Ok(rows) -> {
      case rows {
        [row] -> {
          case postgres.decode_string(row, 0) {
            Ok(value) -> value |> should.equal("hello")
            Error(_) -> {
              io.println("Failed to decode string from row")
              should.fail()
            }
          }
        }
        _ -> {
          io.println("Expected 1 row, got " <> int.to_string(list.length(rows)))
          should.fail()
        }
      }
    }
    Error(err) -> {
      io.println("decode_string failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_decode_bool_values(conn: postgres.PostgresConnection) -> Nil {
  case postgres.execute_sql(conn, "SELECT true as value", []) {
    Ok(rows) -> {
      case rows {
        [row] -> {
          case postgres.decode_bool(row, 0) {
            Ok(value) -> value |> should.be_true
            Error(_) -> {
              io.println("Failed to decode bool from row")
              should.fail()
            }
          }
        }
        _ -> {
          io.println("Expected 1 row, got " <> int.to_string(list.length(rows)))
          should.fail()
        }
      }
    }
    Error(err) -> {
      io.println("decode_bool failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_adapter_query_interface(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  // Insert test data using raw SQL
  let _ =
    postgres.execute_sql_mutation(
      conn,
      "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      [
        adapter.ParamString("adapter@example.com"),
        adapter.ParamString("Adapter Test"),
        adapter.ParamBool(True),
      ],
    )

  // Use adapter interface
  let pg_adapter = postgres.postgres_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT id, email FROM users WHERE email = $1",
      params: [adapter.ParamString("adapter@example.com")],
      expected_columns: 2,
    )

  case adapter.query(pg_adapter, conn, compiled) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
    }
    Error(err) -> {
      io.println("adapter_query failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}

fn test_adapter_mutate_interface(conn: postgres.PostgresConnection) -> Nil {
  cleanup_test_data(conn)

  let pg_adapter = postgres.postgres_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "INSERT INTO users (email, name, active) VALUES ($1, $2, $3)",
      params: [
        adapter.ParamString("mutate@example.com"),
        adapter.ParamString("Mutate Test"),
        adapter.ParamBool(True),
      ],
      expected_columns: 0,
    )

  case adapter.mutate(pg_adapter, conn, compiled) {
    Ok(count) -> {
      count |> should.equal(1)
    }
    Error(err) -> {
      io.println("adapter_mutate failed: " <> error.format_error(err))
      should.fail()
    }
  }
  Nil
}
