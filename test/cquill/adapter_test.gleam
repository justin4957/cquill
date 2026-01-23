import cquill/adapter
import cquill/error.{
  AdapterSpecific, ConnectionFailed, ConstraintViolation, NotFound, NotSupported,
  QueryFailed, StaleData, Timeout,
}
import gleam/option.{None}
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// ERROR TYPE TESTS
// ============================================================================

pub fn not_found_check_test() {
  adapter.is_not_found(NotFound)
  |> should.be_true

  adapter.is_not_found(Timeout)
  |> should.be_false
}

pub fn constraint_violation_check_test() {
  adapter.is_constraint_violation(ConstraintViolation(
    "unique_email",
    "Email already exists",
  ))
  |> should.be_true

  adapter.is_constraint_violation(NotFound)
  |> should.be_false
}

pub fn recoverable_errors_test() {
  // Timeout is recoverable
  adapter.is_recoverable(Timeout)
  |> should.be_true

  // Connection errors are recoverable
  adapter.is_recoverable(ConnectionFailed("Connection reset"))
  |> should.be_true

  // Stale data is recoverable (retry with fresh data)
  adapter.is_recoverable(StaleData("1", "2"))
  |> should.be_true

  // Not found is not recoverable
  adapter.is_recoverable(NotFound)
  |> should.be_false

  // Query errors are not recoverable
  adapter.is_recoverable(QueryFailed("Syntax error", None))
  |> should.be_false
}

pub fn format_error_test() {
  // Test compact format (for logging/simple display)
  adapter.format_error_compact(NotFound)
  |> should.equal("Record not found")

  adapter.format_error_compact(ConstraintViolation(
    "users_email_key",
    "duplicate key",
  ))
  |> should.equal("Constraint violation: users_email_key - duplicate key")

  adapter.format_error_compact(Timeout)
  |> should.equal("Operation timed out")

  adapter.format_error_compact(NotSupported("RETURNING"))
  |> should.equal("Operation not supported: RETURNING")

  adapter.format_error_compact(AdapterSpecific("PG001", "Something went wrong"))
  |> should.equal("Adapter error [PG001]: Something went wrong")
}

pub fn format_error_with_hints_test() {
  // Test detailed format with hints
  let message = adapter.format_error(NotFound)
  string.contains(message, "Record not found") |> should.be_true
  string.contains(message, "Hint:") |> should.be_true
}

// ============================================================================
// CAPABILITIES TESTS
// ============================================================================

pub fn default_capabilities_test() {
  let caps = adapter.default_capabilities()

  caps.transactions |> should.be_false
  caps.returning |> should.be_false
  caps.batch_insert |> should.be_false
  caps.upsert |> should.be_false
  caps.max_params |> should.equal(None)
}

pub fn sql_capabilities_test() {
  let caps = adapter.sql_capabilities()

  caps.transactions |> should.be_true
  caps.returning |> should.be_true
  caps.batch_insert |> should.be_true
  caps.upsert |> should.be_true
  caps.json_operations |> should.be_true
  caps.array_types |> should.be_true
}

// ============================================================================
// QUERY PARAM TESTS
// ============================================================================

pub fn query_param_types_test() {
  // Just verify the types exist and can be constructed
  let _int_param = adapter.ParamInt(42)
  let _float_param = adapter.ParamFloat(3.14)
  let _string_param = adapter.ParamString("hello")
  let _bool_param = adapter.ParamBool(True)
  let _null_param = adapter.ParamNull
  let _bytes_param = adapter.ParamBytes(<<1, 2, 3>>)
  let _custom_param = adapter.ParamCustom("uuid", "abc-123")

  // If we get here, types are correct
  True |> should.be_true
}

pub fn compiled_query_test() {
  let query =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE id = $1",
      params: [adapter.ParamInt(1)],
      expected_columns: 3,
    )

  query.sql |> should.equal("SELECT * FROM users WHERE id = $1")
  query.expected_columns |> should.equal(3)
}
