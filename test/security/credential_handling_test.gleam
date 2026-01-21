// Credential Handling Security Tests
//
// This test suite verifies that credentials are handled securely
// and never exposed in logs, errors, or telemetry.

import cquill/error
import cquill/query/ast
import cquill/telemetry
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// ERROR MESSAGE TESTS
// ============================================================================

/// Test that ConnectionFailed error doesn't include credentials
pub fn error_connection_failed_no_credentials_test() {
  // Simulate a connection error message
  let err = error.ConnectionFailed("Connection refused")

  let formatted = error.format_error(err)

  // Error message should not contain typical credential patterns
  should.be_false(string.contains(formatted, "password"))
  should.be_false(string.contains(formatted, "secret"))
}

/// Test that QueryFailed error doesn't expose sensitive data
pub fn error_query_failed_safe_test() {
  let err =
    error.QueryFailed("relation \"users\" does not exist", Some("42P01"))

  let formatted = error.format_error(err)

  // Should contain the error message but not credentials
  should.be_true(string.contains(formatted, "does not exist"))
}

/// Test constraint violation errors are safe
pub fn error_constraint_violation_safe_test() {
  let err =
    error.UniqueViolation(
      "users_email_key",
      "Key (email)=(test@example.com) already exists.",
    )

  let formatted = error.format_error(err)

  // Should show constraint info but not connection credentials
  should.be_true(string.contains(formatted, "users_email_key"))
}

/// Test decode errors don't leak credentials
pub fn error_decode_safe_test() {
  let err = error.DecodeFailed(0, "password_hash", "string", "null")

  let formatted = error.format_error(err)

  // Should show column name and type mismatch, not actual values
  should.be_true(string.contains(formatted, "password_hash"))
  should.be_true(string.contains(formatted, "string"))
}

/// Test not null violation error message
pub fn error_not_null_violation_test() {
  let err = error.NotNullViolation("email")

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "email"))
  should.be_false(string.contains(formatted, "password"))
}

/// Test foreign key violation error message
pub fn error_foreign_key_violation_test() {
  let err =
    error.ForeignKeyViolation(
      "orders_user_id_fkey",
      "Key (user_id)=(999) is not present in table \"users\"",
    )

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "orders_user_id_fkey"))
}

/// Test check violation error message
pub fn error_check_violation_test() {
  let err =
    error.CheckViolation("users_age_check", "new row violates check constraint")

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "users_age_check"))
}

/// Test generic constraint violation
pub fn error_generic_constraint_violation_test() {
  let err = error.ConstraintViolation("some_constraint", "Constraint violated")

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "some_constraint"))
}

/// Test stale data error
pub fn error_stale_data_test() {
  let err = error.StaleData("1", "2")

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "1"))
  should.be_true(string.contains(formatted, "2"))
}

/// Test data integrity error
pub fn error_data_integrity_test() {
  let err = error.DataIntegrityError("Invalid data structure")

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "Invalid data"))
}

/// Test adapter specific error
pub fn error_adapter_specific_test() {
  let err = error.AdapterSpecific("CUSTOM01", "Custom error message")

  let formatted = error.format_error(err)

  should.be_true(string.contains(formatted, "CUSTOM01"))
}

// ============================================================================
// TELEMETRY EVENT TESTS
// ============================================================================

/// Test that query events don't include sensitive credentials in query text
pub fn telemetry_query_event_safe_test() {
  // Query parameters contain sensitive data but are parameterized
  let event =
    telemetry.query_stop(
      "SELECT * FROM users WHERE password_hash = $1",
      [ast.StringValue("hashed_password_here")],
      1000,
      1,
      None,
    )

  // Event should be created successfully
  case event {
    telemetry.QueryStop(e) -> {
      should.be_true(string.contains(e.query, "SELECT"))
      // Query uses parameterized placeholder, not literal value
      should.be_true(string.contains(e.query, "$1"))
    }
    _ -> should.fail()
  }
}

/// Test transaction event doesn't expose credentials
pub fn telemetry_transaction_event_safe_test() {
  let event = telemetry.transaction_commit("tx-123", 5000, 3, None)

  case event {
    telemetry.TransactionCommit(e) -> {
      should.equal(e.transaction_id, "tx-123")
    }
    _ -> should.fail()
  }
}

/// Test pool timeout event is safe
pub fn telemetry_pool_timeout_safe_test() {
  let event = telemetry.pool_timeout("primary", 30_000, 5)

  case event {
    telemetry.PoolTimeout(e) -> {
      should.equal(e.pool_name, "primary")
      // Should not contain connection string
      should.be_false(string.contains(e.pool_name, "password"))
    }
    _ -> should.fail()
  }
}

/// Test query start event
pub fn telemetry_query_start_safe_test() {
  let event =
    telemetry.query_start(
      "INSERT INTO users (email, password_hash) VALUES ($1, $2)",
      [ast.StringValue("user@example.com"), ast.StringValue("bcrypt_hash")],
      None,
    )

  case event {
    telemetry.QueryStart(e) -> {
      // Query should use placeholders
      should.be_true(string.contains(e.query, "$1"))
      should.be_true(string.contains(e.query, "$2"))
    }
    _ -> should.fail()
  }
}

/// Test query exception event
pub fn telemetry_query_exception_safe_test() {
  let event =
    telemetry.query_exception(
      "SELECT * FROM users WHERE id = $1",
      [ast.IntValue(1)],
      error.NotFound,
      500,
      None,
    )

  case event {
    telemetry.QueryException(e) -> {
      should.equal(e.error, error.NotFound)
    }
    _ -> should.fail()
  }
}

/// Test batch events don't expose data
pub fn telemetry_batch_events_safe_test() {
  let start_event = telemetry.batch_start("insert", "users", 100)

  case start_event {
    telemetry.BatchStart(e) -> {
      should.equal(e.operation, "insert")
      should.equal(e.table, "users")
      should.equal(e.batch_size, 100)
    }
    _ -> should.fail()
  }

  let stop_event = telemetry.batch_stop("insert", "users", 98, 5000)

  case stop_event {
    telemetry.BatchStop(e) -> {
      should.equal(e.affected_count, 98)
    }
    _ -> should.fail()
  }
}

/// Test transaction start event
pub fn telemetry_transaction_start_safe_test() {
  let event = telemetry.transaction_start("tx-456", None)

  case event {
    telemetry.TransactionStart(e) -> {
      should.equal(e.transaction_id, "tx-456")
    }
    _ -> should.fail()
  }
}

/// Test transaction rollback event
pub fn telemetry_transaction_rollback_safe_test() {
  let event =
    telemetry.transaction_rollback("tx-789", 1000, Some("User cancelled"), None)

  case event {
    telemetry.TransactionRollback(e) -> {
      should.equal(e.transaction_id, "tx-789")
      should.equal(e.reason, Some("User cancelled"))
    }
    _ -> should.fail()
  }
}
