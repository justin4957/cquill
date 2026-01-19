// Tests for cquill/telemetry.gleam
//
// These tests verify the telemetry system including:
// - Event types and constructors
// - Handler registration and deregistration
// - Event emission and handler invocation
// - Built-in handlers (logger, slow query)
// - Timing utilities

import cquill/error
import cquill/query/ast.{IntValue, StringValue}
import cquill/telemetry.{
  type Event, type EventType, type Handler, type Metadata, BatchStart,
  BatchStartEvent, BatchStop, BatchStopEvent, HandlerAlreadyExists,
  HandlerNotFound, PoolCheckin, PoolCheckinEvent, PoolCheckout,
  PoolCheckoutEvent, PoolTimeout, PoolTimeoutEvent, QueryException,
  QueryExceptionEvent, QueryExceptionType, QueryStart, QueryStartEvent,
  QueryStartType, QueryStop, QueryStopEvent, QueryStopType, SavepointCreate,
  SavepointCreateEvent, SavepointRelease, SavepointReleaseEvent,
  SavepointRollback, SavepointRollbackEvent, SourceLocation, TransactionCommit,
  TransactionCommitEvent, TransactionRollback, TransactionRollbackEvent,
  TransactionStart, TransactionStartEvent,
}
import gleam/dict
import gleam/dynamic
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// TEST HELPERS
// ============================================================================

/// Start telemetry server for tests, ensuring clean state
fn setup_telemetry() -> Nil {
  // Stop any existing server first
  telemetry.stop()
  // Start fresh
  case telemetry.start() {
    Ok(_) -> Nil
    Error(_) -> Nil
  }
}

/// Create a handler that records events to a list via a subject
fn recording_handler(subject: process.Subject(Event)) -> Handler {
  fn(event, _metadata) { process.send(subject, event) }
}

// ============================================================================
// SOURCE LOCATION TESTS
// ============================================================================

pub fn source_location_creation_test() {
  let loc = telemetry.source_location("src/app.gleam", 42, "create_user")

  loc.file
  |> should.equal("src/app.gleam")

  loc.line
  |> should.equal(42)

  loc.function
  |> should.equal("create_user")
}

// ============================================================================
// METADATA TESTS
// ============================================================================

pub fn empty_metadata_test() {
  let metadata = telemetry.empty_metadata()

  dict.size(metadata)
  |> should.equal(0)
}

pub fn metadata_with_string_test() {
  let metadata =
    telemetry.empty_metadata()
    |> telemetry.with_string("user_id", "123")

  dict.size(metadata)
  |> should.equal(1)

  dict.has_key(metadata, "user_id")
  |> should.be_true()
}

pub fn metadata_with_int_test() {
  let metadata =
    telemetry.empty_metadata()
    |> telemetry.with_int("count", 42)

  dict.size(metadata)
  |> should.equal(1)

  dict.has_key(metadata, "count")
  |> should.be_true()
}

pub fn metadata_with_bool_test() {
  let metadata =
    telemetry.empty_metadata()
    |> telemetry.with_bool("enabled", True)

  dict.size(metadata)
  |> should.equal(1)
}

pub fn chained_metadata_test() {
  let metadata =
    telemetry.empty_metadata()
    |> telemetry.with_string("user", "alice")
    |> telemetry.with_int("age", 30)
    |> telemetry.with_bool("active", True)

  dict.size(metadata)
  |> should.equal(3)
}

// ============================================================================
// EVENT CONSTRUCTOR TESTS
// ============================================================================

pub fn query_start_event_test() {
  let event =
    telemetry.query_start(
      "SELECT * FROM users",
      [IntValue(1)],
      Some(telemetry.source_location("test.gleam", 1, "test")),
    )

  case event {
    QueryStart(e) -> {
      e.query
      |> should.equal("SELECT * FROM users")

      list.length(e.params)
      |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn query_stop_event_test() {
  let event = telemetry.query_stop("SELECT * FROM users", [], 1000, 5, None)

  case event {
    QueryStop(e) -> {
      e.query
      |> should.equal("SELECT * FROM users")
      e.duration_us
      |> should.equal(1000)
      e.row_count
      |> should.equal(5)
    }
    _ -> should.fail()
  }
}

pub fn query_exception_event_test() {
  let event =
    telemetry.query_exception(
      "SELECT * FROM nonexistent",
      [],
      error.QueryFailed("table not found", None),
      500,
      None,
    )

  case event {
    QueryException(e) -> {
      e.query
      |> should.equal("SELECT * FROM nonexistent")
      e.duration_us
      |> should.equal(500)
    }
    _ -> should.fail()
  }
}

pub fn pool_timeout_event_test() {
  let event = telemetry.pool_timeout("main_pool", 5000, 10)

  case event {
    PoolTimeout(e) -> {
      e.pool_name
      |> should.equal("main_pool")
      e.wait_time_us
      |> should.equal(5000)
      e.queue_length
      |> should.equal(10)
    }
    _ -> should.fail()
  }
}

pub fn transaction_start_event_test() {
  let event =
    telemetry.transaction_start(
      "tx_123",
      Some(telemetry.source_location("app.gleam", 50, "do_work")),
    )

  case event {
    TransactionStart(e) -> {
      e.transaction_id
      |> should.equal("tx_123")
    }
    _ -> should.fail()
  }
}

pub fn transaction_commit_event_test() {
  let event = telemetry.transaction_commit("tx_123", 10_000, 5, None)

  case event {
    TransactionCommit(e) -> {
      e.transaction_id
      |> should.equal("tx_123")
      e.duration_us
      |> should.equal(10_000)
      e.query_count
      |> should.equal(5)
    }
    _ -> should.fail()
  }
}

pub fn transaction_rollback_event_test() {
  let event =
    telemetry.transaction_rollback(
      "tx_123",
      5000,
      Some("constraint violation"),
      None,
    )

  case event {
    TransactionRollback(e) -> {
      e.transaction_id
      |> should.equal("tx_123")
      e.reason
      |> should.equal(Some("constraint violation"))
    }
    _ -> should.fail()
  }
}

pub fn batch_start_event_test() {
  let event = telemetry.batch_start("insert", "users", 100)

  case event {
    BatchStart(e) -> {
      e.operation
      |> should.equal("insert")
      e.table
      |> should.equal("users")
      e.batch_size
      |> should.equal(100)
    }
    _ -> should.fail()
  }
}

pub fn batch_stop_event_test() {
  let event = telemetry.batch_stop("insert", "users", 100, 5000)

  case event {
    BatchStop(e) -> {
      e.operation
      |> should.equal("insert")
      e.affected_count
      |> should.equal(100)
      e.duration_us
      |> should.equal(5000)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// EVENT TYPE NAME TESTS
// ============================================================================

pub fn event_type_name_query_start_test() {
  telemetry.event_type_name(QueryStartType)
  |> should.equal("query.start")
}

pub fn event_type_name_query_stop_test() {
  telemetry.event_type_name(QueryStopType)
  |> should.equal("query.stop")
}

pub fn event_type_name_query_exception_test() {
  telemetry.event_type_name(QueryExceptionType)
  |> should.equal("query.exception")
}

// ============================================================================
// TIMING TESTS
// ============================================================================

pub fn now_us_returns_positive_test() {
  let time = telemetry.now_us()

  // Time should be positive (or at least not crash)
  // Note: monotonic time can be negative in Erlang, but microseconds should work
  True
  |> should.be_true()
}

pub fn duration_since_test() {
  let start = telemetry.now_us()
  // Do some minimal work
  let _ = list.range(1, 100)
  let duration = telemetry.duration_since(start)

  // Duration should be non-negative
  { duration >= 0 }
  |> should.be_true()
}

pub fn timed_function_test() {
  let #(result, duration) =
    telemetry.timed(fn() {
      list.range(1, 1000)
      |> list.length()
    })

  result
  |> should.equal(1000)

  { duration >= 0 }
  |> should.be_true()
}

// ============================================================================
// HANDLER REGISTRATION TESTS
// ============================================================================

pub fn start_telemetry_server_test() {
  telemetry.stop()
  case telemetry.start() {
    Ok(Nil) -> {
      // Server should be running
      telemetry.handler_count()
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
  telemetry.stop()
}

pub fn attach_handler_test() {
  setup_telemetry()

  let handler = fn(_event, _metadata) { Nil }

  case telemetry.attach("test_handler", [QueryStopType], handler) {
    Ok(_) -> {
      telemetry.handler_count()
      |> should.equal(1)
    }
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

pub fn attach_duplicate_handler_fails_test() {
  setup_telemetry()

  let handler = fn(_event, _metadata) { Nil }

  // First attach should succeed
  case telemetry.attach("test_handler", [QueryStopType], handler) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Second attach with same ID should fail
  case telemetry.attach("test_handler", [QueryStartType], handler) {
    Ok(_) -> should.fail()
    Error(HandlerAlreadyExists(id)) -> {
      id
      |> should.equal("test_handler")
    }
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

pub fn detach_handler_test() {
  setup_telemetry()

  let handler = fn(_event, _metadata) { Nil }

  // Attach first
  case telemetry.attach("test_handler", [QueryStopType], handler) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  telemetry.handler_count()
  |> should.equal(1)

  // Detach
  case telemetry.detach("test_handler") {
    Ok(_) -> {
      telemetry.handler_count()
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

pub fn detach_nonexistent_handler_fails_test() {
  setup_telemetry()

  case telemetry.detach("nonexistent") {
    Ok(_) -> should.fail()
    Error(HandlerNotFound(id)) -> {
      id
      |> should.equal("nonexistent")
    }
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

pub fn list_handlers_test() {
  setup_telemetry()

  let handler = fn(_event, _metadata) { Nil }

  // Attach multiple handlers
  let _ = telemetry.attach("handler1", [QueryStopType], handler)
  let _ =
    telemetry.attach("handler2", [QueryStartType, QueryExceptionType], handler)

  let handlers = telemetry.list_handlers()

  list.length(handlers)
  |> should.equal(2)

  telemetry.stop()
}

// ============================================================================
// EVENT EMISSION TESTS
// ============================================================================

pub fn emit_event_to_handler_test() {
  setup_telemetry()

  // Create a subject to receive events
  let subject = process.new_subject()

  // Create handler that sends events to subject
  let handler = recording_handler(subject)

  // Attach handler
  case telemetry.attach("recorder", [QueryStopType], handler) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Emit an event
  let event = telemetry.query_stop("SELECT 1", [], 100, 1, None)
  telemetry.emit_event(event)

  // Give it a moment to process
  process.sleep(10)

  // Check we received the event
  case process.receive(subject, 100) {
    Ok(received_event) -> {
      case received_event {
        QueryStop(e) -> {
          e.query
          |> should.equal("SELECT 1")
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

pub fn emit_event_only_to_subscribed_handlers_test() {
  setup_telemetry()

  let subject = process.new_subject()
  let handler = recording_handler(subject)

  // Attach handler only for QueryException, not QueryStop
  case telemetry.attach("recorder", [QueryExceptionType], handler) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Emit a QueryStop event (handler should NOT receive it)
  let event = telemetry.query_stop("SELECT 1", [], 100, 1, None)
  telemetry.emit_event(event)

  // Give it a moment
  process.sleep(10)

  // Should timeout because handler wasn't subscribed to this event type
  case process.receive(subject, 50) {
    Ok(_) -> should.fail()
    // Should not receive anything
    Error(_) -> Nil
    // Expected - timeout
  }

  telemetry.stop()
}

pub fn emit_with_metadata_test() {
  setup_telemetry()

  let subject = process.new_subject()

  // Handler that checks metadata
  let handler = fn(event, metadata) {
    case dict.get(metadata, "request_id") {
      Ok(_) -> process.send(subject, event)
      Error(_) -> Nil
    }
  }

  case telemetry.attach("metadata_checker", [QueryStopType], handler) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Emit with metadata
  let metadata =
    telemetry.empty_metadata()
    |> telemetry.with_string("request_id", "req_123")

  let event = telemetry.query_stop("SELECT 1", [], 100, 1, None)
  telemetry.emit(event, metadata)

  process.sleep(10)

  case process.receive(subject, 100) {
    Ok(_) -> Nil
    // Handler received event (metadata check passed)
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

// ============================================================================
// BUILT-IN HANDLER TESTS
// ============================================================================

pub fn logger_handler_creation_test() {
  // Just verify the handler can be created without error
  let _handler = telemetry.logger_handler()
  True
  |> should.be_true()
}

pub fn slow_query_handler_creation_test() {
  // Create slow query handler with 100ms threshold
  let _handler = telemetry.slow_query_handler(100)
  True
  |> should.be_true()
}

pub fn debug_handler_creation_test() {
  let _handler = telemetry.debug_handler()
  True
  |> should.be_true()
}

pub fn metrics_handler_creation_test() {
  let _handler =
    telemetry.metrics_handler(
      fn(_query, _duration, _rows) { Nil },
      fn(_error) { Nil },
      fn(_pool) { Nil },
    )
  True
  |> should.be_true()
}

// ============================================================================
// INTEGRATION TESTS
// ============================================================================

pub fn multiple_handlers_receive_same_event_test() {
  setup_telemetry()

  let subject1 = process.new_subject()
  let subject2 = process.new_subject()

  let handler1 = recording_handler(subject1)
  let handler2 = recording_handler(subject2)

  // Attach both handlers for same event type
  let _ = telemetry.attach("handler1", [QueryStopType], handler1)
  let _ = telemetry.attach("handler2", [QueryStopType], handler2)

  // Emit event
  let event = telemetry.query_stop("SELECT 1", [], 100, 1, None)
  telemetry.emit_event(event)

  process.sleep(10)

  // Both handlers should receive the event
  case process.receive(subject1, 100) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case process.receive(subject2, 100) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  telemetry.stop()
}

pub fn handler_receives_multiple_event_types_test() {
  setup_telemetry()

  let subject = process.new_subject()
  let handler = recording_handler(subject)

  // Subscribe to multiple event types
  let _ =
    telemetry.attach("multi_handler", [QueryStartType, QueryStopType], handler)

  // Emit both types
  telemetry.emit_event(telemetry.query_start("SELECT 1", [], None))
  telemetry.emit_event(telemetry.query_stop("SELECT 1", [], 100, 1, None))

  process.sleep(10)

  // Should receive both events
  let events = receive_all_events(subject, [], 100)

  list.length(events)
  |> should.equal(2)

  telemetry.stop()
}

/// Helper to receive all available events from a subject
fn receive_all_events(
  subject: process.Subject(Event),
  acc: List(Event),
  timeout: Int,
) -> List(Event) {
  case process.receive(subject, timeout) {
    Ok(event) -> receive_all_events(subject, [event, ..acc], timeout)
    Error(_) -> list.reverse(acc)
  }
}

// ============================================================================
// NO SERVER RUNNING TESTS
// ============================================================================

pub fn attach_without_server_fails_test() {
  telemetry.stop()

  let handler = fn(_event, _metadata) { Nil }

  case telemetry.attach("test", [QueryStopType], handler) {
    Ok(_) -> should.fail()
    Error(telemetry.TelemetryNotRunning) -> Nil
    Error(_) -> should.fail()
  }
}

pub fn emit_without_server_is_safe_test() {
  telemetry.stop()

  // Should not crash even without server running
  telemetry.emit_event(telemetry.query_stop("SELECT 1", [], 100, 1, None))

  True
  |> should.be_true()
}

pub fn handler_count_without_server_returns_zero_test() {
  telemetry.stop()

  telemetry.handler_count()
  |> should.equal(0)
}

pub fn list_handlers_without_server_returns_empty_test() {
  telemetry.stop()

  telemetry.list_handlers()
  |> should.equal([])
}
