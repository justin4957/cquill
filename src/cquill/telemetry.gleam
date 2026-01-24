// cquill Telemetry and Logging Hooks
//
// This module provides telemetry hooks for observability, enabling
// logging, metrics, and tracing integration.
//
// Features:
// - Event types for queries, transactions, pool operations
// - Handler registration and deregistration
// - Built-in handlers for logging and slow query detection
// - Zero overhead when no handlers are attached
//
// Usage:
//   telemetry.attach("logger", [QueryStop, QueryException], telemetry.logger_handler())
//   telemetry.attach("slow_query", [QueryStop], telemetry.slow_query_handler(100))

import cquill/error.{type AdapterError}
import cquill/query/ast.{type Value}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

// ============================================================================
// SOURCE LOCATION TYPE
// ============================================================================

/// Source location in user code where an operation originated
pub type SourceLocation {
  SourceLocation(file: String, line: Int, function: String)
}

// ============================================================================
// EVENT TYPES
// ============================================================================

/// All telemetry event types that can be emitted
pub type Event {
  // Query events
  QueryStart(QueryStartEvent)
  QueryStop(QueryStopEvent)
  QueryException(QueryExceptionEvent)

  // Pool events
  PoolCheckout(PoolCheckoutEvent)
  PoolCheckin(PoolCheckinEvent)
  PoolTimeout(PoolTimeoutEvent)

  // Transaction events
  TransactionStart(TransactionStartEvent)
  TransactionCommit(TransactionCommitEvent)
  TransactionRollback(TransactionRollbackEvent)

  // Savepoint events
  SavepointCreate(SavepointCreateEvent)
  SavepointRollback(SavepointRollbackEvent)
  SavepointRelease(SavepointReleaseEvent)

  // Batch events
  BatchStart(BatchStartEvent)
  BatchStop(BatchStopEvent)
}

/// Event type enumeration for handler registration
pub type EventType {
  QueryStartType
  QueryStopType
  QueryExceptionType
  PoolCheckoutType
  PoolCheckinType
  PoolTimeoutType
  TransactionStartType
  TransactionCommitType
  TransactionRollbackType
  SavepointCreateType
  SavepointRollbackType
  SavepointReleaseType
  BatchStartType
  BatchStopType
}

// ============================================================================
// QUERY EVENTS
// ============================================================================

/// Emitted when a query starts executing
pub type QueryStartEvent {
  QueryStartEvent(
    /// The SQL query being executed
    query: String,
    /// Query parameters
    params: List(Value),
    /// Source location in user code
    source: Option(SourceLocation),
    /// Monotonic start time in microseconds
    start_time_us: Int,
  )
}

/// Emitted when a query completes successfully
pub type QueryStopEvent {
  QueryStopEvent(
    /// The SQL query that was executed
    query: String,
    /// Query parameters
    params: List(Value),
    /// Duration in microseconds
    duration_us: Int,
    /// Number of rows returned/affected
    row_count: Int,
    /// Source location in user code
    source: Option(SourceLocation),
  )
}

/// Emitted when a query fails with an exception
pub type QueryExceptionEvent {
  QueryExceptionEvent(
    /// The SQL query that failed
    query: String,
    /// Query parameters
    params: List(Value),
    /// The error that occurred
    error: AdapterError,
    /// Duration in microseconds until failure
    duration_us: Int,
    /// Source location in user code
    source: Option(SourceLocation),
  )
}

// ============================================================================
// POOL EVENTS
// ============================================================================

/// Emitted when a connection is checked out from the pool
pub type PoolCheckoutEvent {
  PoolCheckoutEvent(
    /// Name/identifier of the pool
    pool_name: String,
    /// Time spent waiting for a connection (microseconds)
    wait_time_us: Int,
    /// Number of requests waiting in queue
    queue_length: Int,
  )
}

/// Emitted when a connection is returned to the pool
pub type PoolCheckinEvent {
  PoolCheckinEvent(
    /// Name/identifier of the pool
    pool_name: String,
    /// How long the connection was used (microseconds)
    usage_time_us: Int,
  )
}

/// Emitted when a pool checkout times out
pub type PoolTimeoutEvent {
  PoolTimeoutEvent(
    /// Name/identifier of the pool
    pool_name: String,
    /// How long the request waited before timing out (microseconds)
    wait_time_us: Int,
    /// Number of requests waiting in queue when timeout occurred
    queue_length: Int,
  )
}

// ============================================================================
// TRANSACTION EVENTS
// ============================================================================

/// Emitted when a transaction begins
pub type TransactionStartEvent {
  TransactionStartEvent(
    /// Unique transaction ID
    transaction_id: String,
    /// Source location in user code
    source: Option(SourceLocation),
    /// Monotonic start time in microseconds
    start_time_us: Int,
  )
}

/// Emitted when a transaction commits successfully
pub type TransactionCommitEvent {
  TransactionCommitEvent(
    /// Unique transaction ID
    transaction_id: String,
    /// Duration of the transaction (microseconds)
    duration_us: Int,
    /// Number of queries executed in the transaction
    query_count: Int,
    /// Source location in user code
    source: Option(SourceLocation),
  )
}

/// Emitted when a transaction is rolled back
pub type TransactionRollbackEvent {
  TransactionRollbackEvent(
    /// Unique transaction ID
    transaction_id: String,
    /// Duration of the transaction (microseconds)
    duration_us: Int,
    /// Reason for rollback (if known)
    reason: Option(String),
    /// Source location in user code
    source: Option(SourceLocation),
  )
}

// ============================================================================
// SAVEPOINT EVENTS
// ============================================================================

/// Emitted when a savepoint is created
pub type SavepointCreateEvent {
  SavepointCreateEvent(
    /// Transaction ID containing this savepoint
    transaction_id: String,
    /// Name of the savepoint
    savepoint_name: String,
    /// Monotonic time when created
    created_at_us: Int,
  )
}

/// Emitted when a savepoint is rolled back to
pub type SavepointRollbackEvent {
  SavepointRollbackEvent(
    /// Transaction ID containing this savepoint
    transaction_id: String,
    /// Name of the savepoint
    savepoint_name: String,
    /// Reason for rollback (if known)
    reason: Option(String),
  )
}

/// Emitted when a savepoint is released
pub type SavepointReleaseEvent {
  SavepointReleaseEvent(
    /// Transaction ID containing this savepoint
    transaction_id: String,
    /// Name of the savepoint
    savepoint_name: String,
  )
}

// ============================================================================
// BATCH EVENTS
// ============================================================================

/// Emitted when a batch operation starts
pub type BatchStartEvent {
  BatchStartEvent(
    /// Type of batch operation (insert, update, delete)
    operation: String,
    /// Table being operated on
    table: String,
    /// Number of records in the batch
    batch_size: Int,
    /// Monotonic start time in microseconds
    start_time_us: Int,
  )
}

/// Emitted when a batch operation completes
pub type BatchStopEvent {
  BatchStopEvent(
    /// Type of batch operation (insert, update, delete)
    operation: String,
    /// Table being operated on
    table: String,
    /// Number of records affected
    affected_count: Int,
    /// Duration in microseconds
    duration_us: Int,
  )
}

// ============================================================================
// METADATA TYPE
// ============================================================================

/// Metadata dictionary for additional context
pub type Metadata =
  Dict(String, Dynamic)

/// Create empty metadata
pub fn empty_metadata() -> Metadata {
  dict.new()
}

/// Add a string to metadata
pub fn with_string(metadata: Metadata, key: String, value: String) -> Metadata {
  dict.insert(metadata, key, to_dynamic(value))
}

/// Add an int to metadata
pub fn with_int(metadata: Metadata, key: String, value: Int) -> Metadata {
  dict.insert(metadata, key, to_dynamic(value))
}

/// Add a bool to metadata
pub fn with_bool(metadata: Metadata, key: String, value: Bool) -> Metadata {
  dict.insert(metadata, key, to_dynamic(value))
}

/// Convert a value to Dynamic
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

// ============================================================================
// HANDLER TYPE
// ============================================================================

/// A telemetry handler function that receives events
pub type Handler =
  fn(Event, Metadata) -> Nil

/// Error types for handler operations
pub type AttachError {
  /// Handler with this ID already exists
  HandlerAlreadyExists(handler_id: String)
  /// Telemetry server not running
  TelemetryNotRunning
}

pub type DetachError {
  /// Handler with this ID not found
  HandlerNotFound(handler_id: String)
  /// Telemetry server not running
  DetachTelemetryNotRunning
}

// ============================================================================
// TELEMETRY SERVER (Actor-based handler management)
// ============================================================================

/// Internal message type for the telemetry actor
type TelemetryMessage {
  Attach(
    handler_id: String,
    events: List(EventType),
    handler: Handler,
    reply_to: Subject(Result(Nil, AttachError)),
  )
  Detach(handler_id: String, reply_to: Subject(Result(Nil, DetachError)))
  Emit(event: Event, metadata: Metadata)
  ListHandlers(reply_to: Subject(List(#(String, List(EventType)))))
  GetHandlerCount(reply_to: Subject(Int))
}

/// Internal state for the telemetry actor
type TelemetryState {
  TelemetryState(handlers: Dict(String, #(List(EventType), Handler)))
}

/// Global telemetry server subject (process dictionary based)
/// In a real implementation this would use a registry or named process
@external(erlang, "cquill_telemetry_ffi", "get_server")
fn get_server() -> Result(Subject(TelemetryMessage), Nil)

@external(erlang, "cquill_telemetry_ffi", "set_server")
fn set_server(server: Subject(TelemetryMessage)) -> Nil

@external(erlang, "cquill_telemetry_ffi", "clear_server")
fn clear_server() -> Nil

/// Start the telemetry server
/// Returns Ok(Nil) on success, Error on failure
pub fn start() -> Result(Nil, actor.StartError) {
  let initial_state = TelemetryState(handlers: dict.new())

  actor.new(initial_state)
  |> actor.on_message(fn(state, msg) {
    case msg {
      Attach(handler_id, events, handler, reply_to) -> {
        case dict.has_key(state.handlers, handler_id) {
          True -> {
            process.send(reply_to, Error(HandlerAlreadyExists(handler_id)))
            actor.continue(state)
          }
          False -> {
            let new_handlers =
              dict.insert(state.handlers, handler_id, #(events, handler))
            process.send(reply_to, Ok(Nil))
            actor.continue(TelemetryState(handlers: new_handlers))
          }
        }
      }

      Detach(handler_id, reply_to) -> {
        case dict.has_key(state.handlers, handler_id) {
          True -> {
            let new_handlers = dict.delete(state.handlers, handler_id)
            process.send(reply_to, Ok(Nil))
            actor.continue(TelemetryState(handlers: new_handlers))
          }
          False -> {
            process.send(reply_to, Error(HandlerNotFound(handler_id)))
            actor.continue(state)
          }
        }
      }

      Emit(event, metadata) -> {
        let event_type = event_to_type(event)
        // Call all handlers registered for this event type
        let _ =
          dict.each(state.handlers, fn(_id, handler_info) {
            let #(registered_events, handler) = handler_info
            case list.contains(registered_events, event_type) {
              True -> handler(event, metadata)
              False -> Nil
            }
          })
        actor.continue(state)
      }

      ListHandlers(reply_to) -> {
        let handler_list =
          dict.to_list(state.handlers)
          |> list.map(fn(entry) {
            let #(id, #(events, _handler)) = entry
            #(id, events)
          })
        process.send(reply_to, handler_list)
        actor.continue(state)
      }

      GetHandlerCount(reply_to) -> {
        process.send(reply_to, dict.size(state.handlers))
        actor.continue(state)
      }
    }
  })
  |> actor.start()
  |> result.map(fn(started) {
    set_server(started.data)
    Nil
  })
}

/// Stop the telemetry server
pub fn stop() -> Nil {
  clear_server()
}

// ============================================================================
// PUBLIC API
// ============================================================================

/// Attach a telemetry handler for specific event types
pub fn attach(
  handler_id: String,
  events: List(EventType),
  handler: Handler,
) -> Result(Nil, AttachError) {
  case get_server() {
    Ok(server) -> {
      process.call(server, 5000, fn(reply_to) {
        Attach(handler_id, events, handler, reply_to)
      })
    }
    Error(_) -> Error(TelemetryNotRunning)
  }
}

/// Detach a handler by ID
pub fn detach(handler_id: String) -> Result(Nil, DetachError) {
  case get_server() {
    Ok(server) -> {
      process.call(server, 5000, fn(reply_to) { Detach(handler_id, reply_to) })
    }
    Error(_) -> Error(DetachTelemetryNotRunning)
  }
}

/// List all attached handlers
pub fn list_handlers() -> List(#(String, List(EventType))) {
  case get_server() {
    Ok(server) -> {
      process.call(server, 5000, fn(reply_to) { ListHandlers(reply_to) })
    }
    Error(_) -> []
  }
}

/// Get the number of attached handlers
pub fn handler_count() -> Int {
  case get_server() {
    Ok(server) -> {
      process.call(server, 5000, fn(reply_to) { GetHandlerCount(reply_to) })
    }
    Error(_) -> 0
  }
}

/// Emit an event to all registered handlers
pub fn emit(event: Event, metadata: Metadata) -> Nil {
  case get_server() {
    Ok(server) -> {
      process.send(server, Emit(event, metadata))
    }
    Error(_) -> Nil
  }
}

/// Emit an event with empty metadata
pub fn emit_event(event: Event) -> Nil {
  emit(event, empty_metadata())
}

// ============================================================================
// TIMING HELPERS
// ============================================================================

/// Get current monotonic time in microseconds
/// Uses Erlang's monotonic time converted to microseconds
@external(erlang, "cquill_telemetry_ffi", "now_us")
pub fn now_us() -> Int

/// Calculate duration in microseconds between start time and now
pub fn duration_since(start_time_us: Int) -> Int {
  now_us() - start_time_us
}

/// Time a function and return its result along with duration
pub fn timed(f: fn() -> a) -> #(a, Int) {
  let start = now_us()
  let result = f()
  let duration = duration_since(start)
  #(result, duration)
}

// ============================================================================
// SPAN HELPER
// ============================================================================

/// Execute a function with query start/stop telemetry
pub fn query_span(
  query: String,
  params: List(Value),
  source: Option(SourceLocation),
  metadata: Metadata,
  f: fn() -> Result(#(a, Int), AdapterError),
) -> Result(#(a, Int), AdapterError) {
  let start_time = now_us()

  // Emit start event
  emit(
    QueryStart(QueryStartEvent(
      query: query,
      params: params,
      source: source,
      start_time_us: start_time,
    )),
    metadata,
  )

  // Execute the function
  let result = f()
  let duration = duration_since(start_time)

  // Emit stop or exception event
  case result {
    Ok(#(value, row_count)) -> {
      emit(
        QueryStop(QueryStopEvent(
          query: query,
          params: params,
          duration_us: duration,
          row_count: row_count,
          source: source,
        )),
        metadata,
      )
      Ok(#(value, row_count))
    }
    Error(error) -> {
      emit(
        QueryException(QueryExceptionEvent(
          query: query,
          params: params,
          error: error,
          duration_us: duration,
          source: source,
        )),
        metadata,
      )
      Error(error)
    }
  }
}

/// Execute a function with transaction start/commit/rollback telemetry
pub fn transaction_span(
  transaction_id: String,
  source: Option(SourceLocation),
  metadata: Metadata,
  f: fn() -> Result(#(a, Int), String),
) -> Result(a, String) {
  let start_time = now_us()

  // Emit start event
  emit(
    TransactionStart(TransactionStartEvent(
      transaction_id: transaction_id,
      source: source,
      start_time_us: start_time,
    )),
    metadata,
  )

  // Execute the function
  let result = f()
  let duration = duration_since(start_time)

  // Emit commit or rollback event
  case result {
    Ok(#(value, query_count)) -> {
      emit(
        TransactionCommit(TransactionCommitEvent(
          transaction_id: transaction_id,
          duration_us: duration,
          query_count: query_count,
          source: source,
        )),
        metadata,
      )
      Ok(value)
    }
    Error(reason) -> {
      emit(
        TransactionRollback(TransactionRollbackEvent(
          transaction_id: transaction_id,
          duration_us: duration,
          reason: Some(reason),
          source: source,
        )),
        metadata,
      )
      Error(reason)
    }
  }
}

// ============================================================================
// BUILT-IN HANDLERS
// ============================================================================

/// Logger handler that prints query events to stdout/stderr
pub fn logger_handler() -> Handler {
  fn(event, _metadata) {
    case event {
      QueryStop(e) -> {
        let duration_ms = e.duration_us / 1000
        let location_str = format_source_location(e.source)
        io.println(
          string.concat([
            "[cquill] ",
            truncate_query(e.query, 100),
            " [",
            int.to_string(duration_ms),
            "ms, ",
            int.to_string(e.row_count),
            " rows]",
            location_str,
          ]),
        )
      }

      QueryException(e) -> {
        let duration_ms = e.duration_us / 1000
        let location_str = format_source_location(e.source)
        io.println_error(
          string.concat([
            "[cquill] QUERY ERROR: ",
            truncate_query(e.query, 100),
            " - ",
            format_adapter_error(e.error),
            " [",
            int.to_string(duration_ms),
            "ms]",
            location_str,
          ]),
        )
      }

      TransactionCommit(e) -> {
        let duration_ms = e.duration_us / 1000
        io.println(
          string.concat([
            "[cquill] TRANSACTION COMMIT: ",
            e.transaction_id,
            " [",
            int.to_string(duration_ms),
            "ms, ",
            int.to_string(e.query_count),
            " queries]",
          ]),
        )
      }

      TransactionRollback(e) -> {
        let duration_ms = e.duration_us / 1000
        let reason_str = case e.reason {
          Some(r) -> " - " <> r
          None -> ""
        }
        io.println_error(
          string.concat([
            "[cquill] TRANSACTION ROLLBACK: ",
            e.transaction_id,
            reason_str,
            " [",
            int.to_string(duration_ms),
            "ms]",
          ]),
        )
      }

      PoolTimeout(e) -> {
        let wait_ms = e.wait_time_us / 1000
        io.println_error(
          string.concat([
            "[cquill] POOL TIMEOUT: ",
            e.pool_name,
            " after ",
            int.to_string(wait_ms),
            "ms (queue: ",
            int.to_string(e.queue_length),
            ")",
          ]),
        )
      }

      _ -> Nil
    }
  }
}

/// Slow query handler that logs queries exceeding a threshold
pub fn slow_query_handler(threshold_ms: Int) -> Handler {
  let threshold_us = threshold_ms * 1000
  fn(event, _metadata) {
    case event {
      QueryStop(e) -> {
        case e.duration_us > threshold_us {
          True -> {
            let duration_ms = e.duration_us / 1000
            let location_str = format_source_location(e.source)
            io.println_error(
              string.concat([
                "[cquill] SLOW QUERY (",
                int.to_string(duration_ms),
                "ms): ",
                truncate_query(e.query, 200),
                location_str,
              ]),
            )
          }
          False -> Nil
        }
      }
      _ -> Nil
    }
  }
}

/// Debug handler that logs all events
pub fn debug_handler() -> Handler {
  fn(event, metadata) {
    let event_name = event_type_name(event_to_type(event))
    let metadata_str = case dict.size(metadata) > 0 {
      True -> " metadata=" <> string.inspect(dict.to_list(metadata))
      False -> ""
    }
    io.println(string.concat(["[cquill:debug] ", event_name, metadata_str]))
    // Print event details
    case event {
      QueryStart(e) -> io.println("  query: " <> truncate_query(e.query, 150))
      QueryStop(e) ->
        io.println(
          "  query: "
          <> truncate_query(e.query, 150)
          <> " duration="
          <> int.to_string(e.duration_us)
          <> "us rows="
          <> int.to_string(e.row_count),
        )
      QueryException(e) ->
        io.println(
          "  query: "
          <> truncate_query(e.query, 150)
          <> " error="
          <> format_adapter_error(e.error),
        )
      TransactionStart(e) ->
        io.println("  transaction_id: " <> e.transaction_id)
      TransactionCommit(e) ->
        io.println(
          "  transaction_id: "
          <> e.transaction_id
          <> " duration="
          <> int.to_string(e.duration_us)
          <> "us queries="
          <> int.to_string(e.query_count),
        )
      TransactionRollback(e) ->
        io.println(
          "  transaction_id: "
          <> e.transaction_id
          <> " reason="
          <> option.unwrap(e.reason, "unknown"),
        )
      PoolCheckout(e) ->
        io.println(
          "  pool: "
          <> e.pool_name
          <> " wait="
          <> int.to_string(e.wait_time_us)
          <> "us queue="
          <> int.to_string(e.queue_length),
        )
      PoolCheckin(e) ->
        io.println(
          "  pool: "
          <> e.pool_name
          <> " usage="
          <> int.to_string(e.usage_time_us)
          <> "us",
        )
      PoolTimeout(e) ->
        io.println(
          "  pool: "
          <> e.pool_name
          <> " wait="
          <> int.to_string(e.wait_time_us)
          <> "us queue="
          <> int.to_string(e.queue_length),
        )
      SavepointCreate(e) ->
        io.println(
          "  savepoint: " <> e.savepoint_name <> " in tx=" <> e.transaction_id,
        )
      SavepointRollback(e) ->
        io.println(
          "  savepoint: " <> e.savepoint_name <> " in tx=" <> e.transaction_id,
        )
      SavepointRelease(e) ->
        io.println(
          "  savepoint: " <> e.savepoint_name <> " in tx=" <> e.transaction_id,
        )
      BatchStart(e) ->
        io.println(
          "  batch: "
          <> e.operation
          <> " on "
          <> e.table
          <> " size="
          <> int.to_string(e.batch_size),
        )
      BatchStop(e) ->
        io.println(
          "  batch: "
          <> e.operation
          <> " on "
          <> e.table
          <> " affected="
          <> int.to_string(e.affected_count)
          <> " duration="
          <> int.to_string(e.duration_us)
          <> "us",
        )
    }
  }
}

/// Metrics handler builder - returns a handler that calls the provided callbacks
pub fn metrics_handler(
  on_query_complete: fn(String, Int, Int) -> Nil,
  on_query_error: fn(String) -> Nil,
  on_pool_timeout: fn(String) -> Nil,
) -> Handler {
  fn(event, _metadata) {
    case event {
      QueryStop(e) -> on_query_complete(e.query, e.duration_us, e.row_count)
      QueryException(_e) -> on_query_error("query_error")
      PoolTimeout(e) -> on_pool_timeout(e.pool_name)
      _ -> Nil
    }
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Convert an event to its type enum
fn event_to_type(event: Event) -> EventType {
  case event {
    QueryStart(_) -> QueryStartType
    QueryStop(_) -> QueryStopType
    QueryException(_) -> QueryExceptionType
    PoolCheckout(_) -> PoolCheckoutType
    PoolCheckin(_) -> PoolCheckinType
    PoolTimeout(_) -> PoolTimeoutType
    TransactionStart(_) -> TransactionStartType
    TransactionCommit(_) -> TransactionCommitType
    TransactionRollback(_) -> TransactionRollbackType
    SavepointCreate(_) -> SavepointCreateType
    SavepointRollback(_) -> SavepointRollbackType
    SavepointRelease(_) -> SavepointReleaseType
    BatchStart(_) -> BatchStartType
    BatchStop(_) -> BatchStopType
  }
}

/// Get a human-readable name for an event type
pub fn event_type_name(event_type: EventType) -> String {
  case event_type {
    QueryStartType -> "query.start"
    QueryStopType -> "query.stop"
    QueryExceptionType -> "query.exception"
    PoolCheckoutType -> "pool.checkout"
    PoolCheckinType -> "pool.checkin"
    PoolTimeoutType -> "pool.timeout"
    TransactionStartType -> "transaction.start"
    TransactionCommitType -> "transaction.commit"
    TransactionRollbackType -> "transaction.rollback"
    SavepointCreateType -> "savepoint.create"
    SavepointRollbackType -> "savepoint.rollback"
    SavepointReleaseType -> "savepoint.release"
    BatchStartType -> "batch.start"
    BatchStopType -> "batch.stop"
  }
}

/// Format source location for logging
fn format_source_location(source: Option(SourceLocation)) -> String {
  case source {
    Some(loc) ->
      " at "
      <> loc.file
      <> ":"
      <> int.to_string(loc.line)
      <> " in "
      <> loc.function
      <> "()"
    None -> ""
  }
}

/// Truncate a query for display
fn truncate_query(query: String, max_length: Int) -> String {
  let length = string.length(query)
  case length > max_length {
    True -> string.slice(query, 0, max_length) <> "..."
    False -> query
  }
}

/// Format an adapter error for logging (simplified)
fn format_adapter_error(err: AdapterError) -> String {
  case err {
    error.NotFound -> "NotFound"
    error.TooManyRows(expected, got) ->
      "TooManyRows(expected="
      <> int.to_string(expected)
      <> ", got="
      <> int.to_string(got)
      <> ")"
    error.ConnectionFailed(reason) -> "ConnectionFailed: " <> reason
    error.ConnectionTimeout -> "ConnectionTimeout"
    error.PoolExhausted -> "PoolExhausted"
    error.ConnectionLost(reason) -> "ConnectionLost: " <> reason
    error.QueryFailed(message, _code) -> "QueryFailed: " <> message
    error.DecodeFailed(row, column, expected, got) ->
      "DecodeFailed at row "
      <> int.to_string(row)
      <> ", column "
      <> column
      <> " (expected "
      <> expected
      <> ", got "
      <> got
      <> ")"
    error.Timeout -> "Timeout"
    error.UniqueViolation(constraint, _detail) ->
      "UniqueViolation: " <> constraint
    error.ForeignKeyViolation(constraint, _detail) ->
      "ForeignKeyViolation: " <> constraint
    error.CheckViolation(constraint, _detail) ->
      "CheckViolation: " <> constraint
    error.NotNullViolation(column) -> "NotNullViolation: " <> column
    error.ConstraintViolation(constraint, _detail) ->
      "ConstraintViolation: " <> constraint
    error.StaleData(expected, actual) ->
      "StaleData(expected=" <> expected <> ", actual=" <> actual <> ")"
    error.DataIntegrityError(message) -> "DataIntegrityError: " <> message
    error.NotSupported(operation) -> "NotSupported: " <> operation
    error.AdapterSpecific(code, message) ->
      "AdapterSpecific[" <> code <> "]: " <> message
  }
}

// ============================================================================
// CONVENIENCE CONSTRUCTORS
// ============================================================================

/// Create a source location
pub fn source_location(
  file: String,
  line: Int,
  function: String,
) -> SourceLocation {
  SourceLocation(file: file, line: line, function: function)
}

/// Create a query start event
pub fn query_start(
  query: String,
  params: List(Value),
  source: Option(SourceLocation),
) -> Event {
  QueryStart(QueryStartEvent(
    query: query,
    params: params,
    source: source,
    start_time_us: now_us(),
  ))
}

/// Create a query stop event
pub fn query_stop(
  query: String,
  params: List(Value),
  duration_us: Int,
  row_count: Int,
  source: Option(SourceLocation),
) -> Event {
  QueryStop(QueryStopEvent(
    query: query,
    params: params,
    duration_us: duration_us,
    row_count: row_count,
    source: source,
  ))
}

/// Create a query exception event
pub fn query_exception(
  query: String,
  params: List(Value),
  err: AdapterError,
  duration_us: Int,
  source: Option(SourceLocation),
) -> Event {
  QueryException(QueryExceptionEvent(
    query: query,
    params: params,
    error: err,
    duration_us: duration_us,
    source: source,
  ))
}

/// Create a pool timeout event
pub fn pool_timeout(
  pool_name: String,
  wait_time_us: Int,
  queue_length: Int,
) -> Event {
  PoolTimeout(PoolTimeoutEvent(
    pool_name: pool_name,
    wait_time_us: wait_time_us,
    queue_length: queue_length,
  ))
}

/// Create a transaction start event
pub fn transaction_start(
  transaction_id: String,
  source: Option(SourceLocation),
) -> Event {
  TransactionStart(TransactionStartEvent(
    transaction_id: transaction_id,
    source: source,
    start_time_us: now_us(),
  ))
}

/// Create a transaction commit event
pub fn transaction_commit(
  transaction_id: String,
  duration_us: Int,
  query_count: Int,
  source: Option(SourceLocation),
) -> Event {
  TransactionCommit(TransactionCommitEvent(
    transaction_id: transaction_id,
    duration_us: duration_us,
    query_count: query_count,
    source: source,
  ))
}

/// Create a transaction rollback event
pub fn transaction_rollback(
  transaction_id: String,
  duration_us: Int,
  reason: Option(String),
  source: Option(SourceLocation),
) -> Event {
  TransactionRollback(TransactionRollbackEvent(
    transaction_id: transaction_id,
    duration_us: duration_us,
    reason: reason,
    source: source,
  ))
}

/// Create a batch start event
pub fn batch_start(operation: String, table: String, batch_size: Int) -> Event {
  BatchStart(BatchStartEvent(
    operation: operation,
    table: table,
    batch_size: batch_size,
    start_time_us: now_us(),
  ))
}

/// Create a batch stop event
pub fn batch_stop(
  operation: String,
  table: String,
  affected_count: Int,
  duration_us: Int,
) -> Event {
  BatchStop(BatchStopEvent(
    operation: operation,
    table: table,
    affected_count: affected_count,
    duration_us: duration_us,
  ))
}
