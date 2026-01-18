// Tests for cquill/error/format.gleam
//
// These tests verify that error formatting produces rich, actionable messages
// with proper context, hints, and visual structure.

import cquill/error.{
  type AdapterError, type SavepointError, type TransactionError, AdapterSpecific,
  AdapterTransactionError, BeginFailed, CheckViolation, CommitFailed,
  ConnectionFailed, ConnectionLost, ConnectionTimeout, ConstraintViolation,
  DataIntegrityError, DecodeFailed, ForeignKeyViolation, NestedTransactionError,
  NotFound, NotNullViolation, NotSupported, PoolExhausted, QueryFailed,
  RolledBack, SavepointAdapterError, SavepointCreationFailed,
  SavepointNoTransaction, SavepointNotFound, SavepointReleaseFailed,
  SavepointUserError, SerializationFailure, StaleData, Timeout, TooManyRows,
  TransactionConnectionLost, TransactionRollback, TransactionTimeout,
  UniqueViolation, UserError,
}
import cquill/error/format.{
  type ConnectionConfig, type ErrorContext, type SourceLocation,
  ConnectionConfig, ErrorContext, SourceLocation, empty_context,
  format_check_violation, format_connection_failed,
  format_connection_failed_simple, format_connection_lost,
  format_connection_timeout, format_decode_error, format_foreign_key_violation,
  format_foreign_key_violation_error, format_not_found,
  format_not_null_violation, format_pool_exhausted, format_query_failed,
  format_rich_error, format_rich_savepoint_error, format_rich_transaction_error,
  format_timeout, format_too_many_rows, format_unique_violation,
  format_unique_violation_error, format_value, with_operation, with_query,
  with_source_location, with_table,
}
import cquill/query/ast.{
  BoolValue, FloatValue, IntValue, ListValue, NullValue, ParamValue, StringValue,
}
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// VALUE FORMATTING TESTS
// ============================================================================

pub fn format_value_int_test() {
  format_value(IntValue(42))
  |> should.equal("42")
}

pub fn format_value_negative_int_test() {
  format_value(IntValue(-100))
  |> should.equal("-100")
}

pub fn format_value_float_test() {
  let result = format_value(FloatValue(3.14))
  result
  |> string.contains("3.14")
  |> should.be_true()
}

pub fn format_value_string_test() {
  // Short strings should be masked with asterisks
  let result = format_value(StringValue("test"))
  result
  |> string.contains("\"")
  |> should.be_true()
}

pub fn format_value_bool_true_test() {
  format_value(BoolValue(True))
  |> should.equal("true")
}

pub fn format_value_bool_false_test() {
  format_value(BoolValue(False))
  |> should.equal("false")
}

pub fn format_value_null_test() {
  format_value(NullValue)
  |> should.equal("NULL")
}

pub fn format_value_param_test() {
  format_value(ParamValue(1))
  |> should.equal("$1")
}

pub fn format_value_list_test() {
  format_value(ListValue([IntValue(1), IntValue(2), IntValue(3)]))
  |> should.equal("[1, 2, 3]")
}

// ============================================================================
// CONTEXT BUILDER TESTS
// ============================================================================

pub fn empty_context_test() {
  let ctx = empty_context()
  ctx.query
  |> should.equal(None)
  ctx.params
  |> should.equal(None)
  ctx.source_location
  |> should.equal(None)
  ctx.table
  |> should.equal(None)
  ctx.operation
  |> should.equal(None)
}

pub fn with_query_test() {
  let ctx =
    empty_context()
    |> with_query("SELECT * FROM users")
  ctx.query
  |> should.equal(Some("SELECT * FROM users"))
}

pub fn with_table_test() {
  let ctx =
    empty_context()
    |> with_table("users")
  ctx.table
  |> should.equal(Some("users"))
}

pub fn with_operation_test() {
  let ctx =
    empty_context()
    |> with_operation("INSERT")
  ctx.operation
  |> should.equal(Some("INSERT"))
}

pub fn with_source_location_test() {
  let ctx =
    empty_context()
    |> with_source_location("src/app.gleam", 42, "create_user")
  case ctx.source_location {
    Some(loc) -> {
      loc.file
      |> should.equal("src/app.gleam")
      loc.line
      |> should.equal(42)
      loc.function
      |> should.equal("create_user")
    }
    None -> should.fail()
  }
}

pub fn chained_context_builders_test() {
  let ctx =
    empty_context()
    |> with_table("users")
    |> with_operation("INSERT")
    |> with_query("INSERT INTO users (email) VALUES ($1)")
  ctx.table
  |> should.equal(Some("users"))
  ctx.operation
  |> should.equal(Some("INSERT"))
  ctx.query
  |> should.equal(Some("INSERT INTO users (email) VALUES ($1)"))
}

// ============================================================================
// UNIQUE VIOLATION TESTS
// ============================================================================

pub fn format_unique_violation_test() {
  let result =
    format_unique_violation(
      "users_email_key",
      "users",
      "email",
      StringValue("test@example.com"),
    )

  // Check structure
  result
  |> string.contains("Error: UniqueConstraintViolation")
  |> should.be_true()

  result
  |> string.contains("INSERT INTO users")
  |> should.be_true()

  result
  |> string.contains("users_email_key")
  |> should.be_true()

  result
  |> string.contains("Hint:")
  |> should.be_true()

  result
  |> string.contains("on_conflict_do_nothing()")
  |> should.be_true()
}

pub fn format_unique_violation_error_test() {
  let ctx =
    empty_context()
    |> with_table("users")
    |> with_operation("INSERT")

  let result =
    format_unique_violation_error(
      "users_email_key",
      "Key (email)=(test@example.com) already exists.",
      ctx,
    )

  result
  |> string.contains("Error: UniqueConstraintViolation")
  |> should.be_true()

  result
  |> string.contains("users_email_key")
  |> should.be_true()

  result
  |> string.contains("already exists")
  |> should.be_true()
}

// ============================================================================
// FOREIGN KEY VIOLATION TESTS
// ============================================================================

pub fn format_foreign_key_violation_test() {
  let result =
    format_foreign_key_violation(
      "posts_user_id_fkey",
      "posts",
      "user_id",
      "users",
      "id",
    )

  result
  |> string.contains("Error: ForeignKeyViolation")
  |> should.be_true()

  result
  |> string.contains("posts_user_id_fkey")
  |> should.be_true()

  result
  |> string.contains("posts.user_id")
  |> should.be_true()

  result
  |> string.contains("users.id")
  |> should.be_true()

  result
  |> string.contains("Hint:")
  |> should.be_true()
}

pub fn format_foreign_key_violation_error_test() {
  let ctx =
    empty_context()
    |> with_table("posts")

  let result =
    format_foreign_key_violation_error(
      "posts_user_id_fkey",
      "Key (user_id)=(999) is not present in table \"users\".",
      ctx,
    )

  result
  |> string.contains("Error: ForeignKeyViolation")
  |> should.be_true()

  result
  |> string.contains("posts_user_id_fkey")
  |> should.be_true()
}

// ============================================================================
// CHECK VIOLATION TESTS
// ============================================================================

pub fn format_check_violation_test() {
  let ctx =
    empty_context()
    |> with_table("products")

  let result =
    format_check_violation(
      "products_price_positive",
      "new row violates check constraint \"products_price_positive\"",
      ctx,
    )

  result
  |> string.contains("Error: CheckConstraintViolation")
  |> should.be_true()

  result
  |> string.contains("products_price_positive")
  |> should.be_true()

  result
  |> string.contains("Hint:")
  |> should.be_true()
}

// ============================================================================
// NOT NULL VIOLATION TESTS
// ============================================================================

pub fn format_not_null_violation_test() {
  let ctx =
    empty_context()
    |> with_table("users")

  let result = format_not_null_violation("email", ctx)

  result
  |> string.contains("Error: NotNullViolation")
  |> should.be_true()

  result
  |> string.contains("email")
  |> should.be_true()

  result
  |> string.contains("cannot be NULL")
  |> should.be_true()

  result
  |> string.contains("Hint:")
  |> should.be_true()
}

// ============================================================================
// DECODE ERROR TESTS
// ============================================================================

pub fn format_decode_error_test() {
  let result = format_decode_error(5, "created_at", "DateTime", "String")

  result
  |> string.contains("Error: DecodeError")
  |> should.be_true()

  result
  |> string.contains("row 5")
  |> should.be_true()

  result
  |> string.contains("created_at")
  |> should.be_true()

  result
  |> string.contains("Expected: DateTime")
  |> should.be_true()

  result
  |> string.contains("Found:    String")
  |> should.be_true()

  result
  |> string.contains("Hint:")
  |> should.be_true()
}

// ============================================================================
// QUERY FAILED TESTS
// ============================================================================

pub fn format_query_failed_without_code_test() {
  let ctx =
    empty_context()
    |> with_query("SELECT * FROM nonexistent")

  let result =
    format_query_failed("relation \"nonexistent\" does not exist", None, ctx)

  result
  |> string.contains("Error: QueryFailed")
  |> should.be_true()

  result
  |> string.contains("nonexistent")
  |> should.be_true()
}

pub fn format_query_failed_with_code_test() {
  let ctx = empty_context()

  let result =
    format_query_failed("relation \"users\" does not exist", Some("42P01"), ctx)

  result
  |> string.contains("Error: QueryFailed [42P01]")
  |> should.be_true()

  result
  |> string.contains("Hint:")
  |> should.be_true()

  result
  |> string.contains("table does not exist")
  |> should.be_true()
}

pub fn format_query_failed_syntax_error_test() {
  let ctx = empty_context()

  let result =
    format_query_failed("syntax error at or near \"SELEC\"", Some("42601"), ctx)

  result
  |> string.contains("42601")
  |> should.be_true()

  result
  |> string.contains("syntax error")
  |> should.be_true()
}

// ============================================================================
// NOT FOUND TESTS
// ============================================================================

pub fn format_not_found_test() {
  let ctx =
    empty_context()
    |> with_table("users")

  let result = format_not_found(ctx)

  result
  |> string.contains("Error: RecordNotFound")
  |> should.be_true()

  result
  |> string.contains("users")
  |> should.be_true()

  result
  |> string.contains("repo.one_or_none")
  |> should.be_true()
}

// ============================================================================
// TOO MANY ROWS TESTS
// ============================================================================

pub fn format_too_many_rows_test() {
  let ctx = empty_context()

  let result = format_too_many_rows(1, 5, ctx)

  result
  |> string.contains("Error: TooManyRows")
  |> should.be_true()

  result
  |> string.contains("Expected 1 row(s)")
  |> should.be_true()

  result
  |> string.contains("found 5")
  |> should.be_true()

  result
  |> string.contains("repo.all")
  |> should.be_true()
}

// ============================================================================
// TIMEOUT TESTS
// ============================================================================

pub fn format_timeout_test() {
  let ctx =
    empty_context()
    |> with_query("SELECT * FROM large_table")

  let result = format_timeout(ctx)

  result
  |> string.contains("Error: Timeout")
  |> should.be_true()

  result
  |> string.contains("timed out")
  |> should.be_true()

  result
  |> string.contains("indexes")
  |> should.be_true()
}

// ============================================================================
// CONNECTION ERROR TESTS
// ============================================================================

pub fn format_connection_failed_test() {
  let config =
    ConnectionConfig(
      host: "localhost",
      port: 5432,
      database: "myapp_dev",
      user: "postgres",
    )

  let result = format_connection_failed("Connection refused", config)

  result
  |> string.contains("Error: ConnectionFailed")
  |> should.be_true()

  result
  |> string.contains("localhost:5432")
  |> should.be_true()

  result
  |> string.contains("myapp_dev")
  |> should.be_true()

  result
  |> string.contains("postgres")
  |> should.be_true()

  result
  |> string.contains("Check:")
  |> should.be_true()

  result
  |> string.contains("Is the database server running?")
  |> should.be_true()
}

pub fn format_connection_failed_simple_test() {
  let result = format_connection_failed_simple("Connection refused")

  result
  |> string.contains("Error: ConnectionFailed")
  |> should.be_true()

  result
  |> string.contains("Connection refused")
  |> should.be_true()
}

pub fn format_connection_timeout_test() {
  let result = format_connection_timeout(None)

  result
  |> string.contains("Error: ConnectionTimeout")
  |> should.be_true()

  result
  |> string.contains("timed out")
  |> should.be_true()
}

pub fn format_connection_timeout_with_config_test() {
  let config =
    ConnectionConfig(
      host: "db.example.com",
      port: 5432,
      database: "prod",
      user: "app",
    )

  let result = format_connection_timeout(Some(config))

  result
  |> string.contains("db.example.com:5432")
  |> should.be_true()
}

pub fn format_pool_exhausted_test() {
  let result = format_pool_exhausted()

  result
  |> string.contains("Error: PoolExhausted")
  |> should.be_true()

  result
  |> string.contains("pool are in use")
  |> should.be_true()

  result
  |> string.contains("Increase pool size")
  |> should.be_true()
}

pub fn format_connection_lost_test() {
  let result = format_connection_lost("Server has gone away")

  result
  |> string.contains("Error: ConnectionLost")
  |> should.be_true()

  result
  |> string.contains("Server has gone away")
  |> should.be_true()

  result
  |> string.contains("transient")
  |> should.be_true()
}

// ============================================================================
// TRANSACTION ERROR TESTS
// ============================================================================

pub fn format_transaction_user_error_test() {
  let error: TransactionError(String) = UserError("validation failed")
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: TransactionAborted")
  |> should.be_true()

  result
  |> string.contains("user error")
  |> should.be_true()
}

pub fn format_transaction_begin_failed_test() {
  let error: TransactionError(String) = BeginFailed("connection error")
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: TransactionBeginFailed")
  |> should.be_true()
}

pub fn format_transaction_commit_failed_test() {
  let error: TransactionError(String) = CommitFailed("constraint violation")
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: TransactionCommitFailed")
  |> should.be_true()

  result
  |> string.contains("rolled back")
  |> should.be_true()
}

pub fn format_transaction_rolled_back_test() {
  let error: TransactionError(String) = RolledBack
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: TransactionRolledBack")
  |> should.be_true()
}

pub fn format_transaction_connection_lost_test() {
  let error: TransactionError(String) = TransactionConnectionLost
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: TransactionConnectionLost")
  |> should.be_true()

  result
  |> string.contains("rolled back")
  |> should.be_true()
}

pub fn format_nested_transaction_error_test() {
  let error: TransactionError(String) = NestedTransactionError
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: NestedTransactionError")
  |> should.be_true()

  result
  |> string.contains("savepoints")
  |> should.be_true()
}

pub fn format_transaction_timeout_test() {
  let error: TransactionError(String) = TransactionTimeout
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: TransactionTimeout")
  |> should.be_true()
}

pub fn format_serialization_failure_test() {
  let error: TransactionError(String) = SerializationFailure
  let ctx = empty_context()

  let result = format_rich_transaction_error(error, ctx)

  result
  |> string.contains("Error: SerializationFailure")
  |> should.be_true()

  result
  |> string.contains("retry")
  |> should.be_true()
}

// ============================================================================
// SAVEPOINT ERROR TESTS
// ============================================================================

pub fn format_savepoint_not_found_test() {
  let error: SavepointError(String) = SavepointNotFound("sp_1")
  let ctx = empty_context()

  let result = format_rich_savepoint_error(error, ctx)

  result
  |> string.contains("Error: SavepointNotFound")
  |> should.be_true()

  result
  |> string.contains("sp_1")
  |> should.be_true()
}

pub fn format_savepoint_creation_failed_test() {
  let error: SavepointError(String) = SavepointCreationFailed("syntax error")
  let ctx = empty_context()

  let result = format_rich_savepoint_error(error, ctx)

  result
  |> string.contains("Error: SavepointCreationFailed")
  |> should.be_true()
}

pub fn format_savepoint_no_transaction_test() {
  let error: SavepointError(String) = SavepointNoTransaction
  let ctx = empty_context()

  let result = format_rich_savepoint_error(error, ctx)

  result
  |> string.contains("Error: SavepointNoTransaction")
  |> should.be_true()

  result
  |> string.contains("repo.transaction")
  |> should.be_true()
}

pub fn format_savepoint_user_error_test() {
  let error: SavepointError(String) = SavepointUserError("failed")
  let ctx = empty_context()

  let result = format_rich_savepoint_error(error, ctx)

  result
  |> string.contains("Error: SavepointAborted")
  |> should.be_true()
}

// ============================================================================
// RICH ERROR FORMATTER TESTS
// ============================================================================

pub fn format_rich_error_not_found_test() {
  let ctx =
    empty_context()
    |> with_table("users")

  let result = format_rich_error(NotFound, ctx)

  result
  |> string.contains("Error: RecordNotFound")
  |> should.be_true()
}

pub fn format_rich_error_unique_violation_test() {
  let ctx =
    empty_context()
    |> with_table("users")

  let error =
    UniqueViolation(
      "users_email_key",
      "Key (email)=(test@test.com) already exists.",
    )

  let result = format_rich_error(error, ctx)

  result
  |> string.contains("Error: UniqueConstraintViolation")
  |> should.be_true()
}

pub fn format_rich_error_connection_failed_test() {
  let ctx = empty_context()

  let result = format_rich_error(ConnectionFailed("Connection refused"), ctx)

  result
  |> string.contains("Error: ConnectionFailed")
  |> should.be_true()
}

pub fn format_rich_error_timeout_test() {
  let ctx = empty_context()

  let result = format_rich_error(Timeout, ctx)

  result
  |> string.contains("Error: Timeout")
  |> should.be_true()
}

pub fn format_rich_error_pool_exhausted_test() {
  let ctx = empty_context()

  let result = format_rich_error(PoolExhausted, ctx)

  result
  |> string.contains("Error: PoolExhausted")
  |> should.be_true()
}

pub fn format_rich_error_stale_data_test() {
  let ctx =
    empty_context()
    |> with_table("users")

  let result = format_rich_error(StaleData("1", "2"), ctx)

  result
  |> string.contains("Error: StaleData")
  |> should.be_true()

  result
  |> string.contains("Expected version: 1")
  |> should.be_true()

  result
  |> string.contains("Actual version:   2")
  |> should.be_true()
}

pub fn format_rich_error_not_supported_test() {
  let ctx = empty_context()

  let result = format_rich_error(NotSupported("batch_upsert"), ctx)

  result
  |> string.contains("Error: NotSupported")
  |> should.be_true()

  result
  |> string.contains("batch_upsert")
  |> should.be_true()
}

pub fn format_rich_error_adapter_specific_test() {
  let ctx = empty_context()

  let result = format_rich_error(AdapterSpecific("E001", "Custom error"), ctx)

  result
  |> string.contains("Error: AdapterSpecific [E001]")
  |> should.be_true()

  result
  |> string.contains("Custom error")
  |> should.be_true()
}

// ============================================================================
// SOURCE LOCATION TESTS
// ============================================================================

pub fn format_error_with_source_location_test() {
  let ctx =
    empty_context()
    |> with_table("users")
    |> with_source_location("src/user_service.gleam", 42, "create_user")

  let result = format_not_found(ctx)

  result
  |> string.contains("src/user_service.gleam:42")
  |> should.be_true()

  result
  |> string.contains("create_user()")
  |> should.be_true()
}

// ============================================================================
// OPERATION CONTEXT TESTS
// ============================================================================

pub fn format_error_with_operation_context_test() {
  let ctx =
    empty_context()
    |> with_table("users")
    |> with_operation("INSERT")

  let result = format_not_null_violation("email", ctx)

  result
  |> string.contains("During: INSERT on users")
  |> should.be_true()
}
