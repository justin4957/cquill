// cquill Development Mode
//
// This module provides enhanced debugging and diagnostic information
// for development environments. It automatically logs queries with
// parameters, detects slow queries, and provides pool statistics.
//
// Activation:
//   - Environment variable: CQUILL_DEV=1
//   - Programmatic: dev.enable()
//
// Features:
//   - Automatic query logging with timing
//   - Slow query warnings with hints
//   - Pool statistics logging
//   - Sensitive data masking
//
// Usage:
//   import cquill/dev
//   dev.enable()
//   // ... run queries ...
//   dev.disable()

import cquill/error.{type AdapterError}
import cquill/query/ast.{type Value}
import cquill/telemetry
import gleam/bool
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

// ============================================================================
// CONFIGURATION TYPES
// ============================================================================

/// Development mode configuration
pub type DevConfig {
  DevConfig(
    /// Whether dev mode is enabled
    enabled: Bool,
    /// Log all queries
    log_queries: Bool,
    /// Include parameters in logs
    log_params: Bool,
    /// Threshold for slow query warnings (milliseconds)
    slow_query_threshold_ms: Int,
    /// Log pool statistics periodically
    log_pool_stats: Bool,
    /// Interval for pool stats logging (milliseconds)
    pool_stats_interval_ms: Int,
    /// Fields to mask in logs (passwords, tokens, etc.)
    mask_fields: List(String),
    /// Pattern to use for masking
    mask_pattern: String,
    /// Enable EXPLAIN query output (requires adapter support)
    explain_queries: Bool,
  )
}

/// Create a default configuration
pub fn default_config() -> DevConfig {
  DevConfig(
    enabled: True,
    log_queries: True,
    log_params: True,
    slow_query_threshold_ms: 100,
    log_pool_stats: False,
    pool_stats_interval_ms: 10_000,
    mask_fields: ["password", "token", "secret", "key", "credential", "api_key"],
    mask_pattern: "[REDACTED]",
    explain_queries: False,
  )
}

/// Create a minimal configuration (queries only, no params)
pub fn minimal_config() -> DevConfig {
  DevConfig(
    enabled: True,
    log_queries: True,
    log_params: False,
    slow_query_threshold_ms: 500,
    log_pool_stats: False,
    pool_stats_interval_ms: 30_000,
    mask_fields: [],
    mask_pattern: "[REDACTED]",
    explain_queries: False,
  )
}

/// Create a verbose configuration (all debugging enabled)
pub fn verbose_config() -> DevConfig {
  DevConfig(
    enabled: True,
    log_queries: True,
    log_params: True,
    slow_query_threshold_ms: 50,
    log_pool_stats: True,
    pool_stats_interval_ms: 5000,
    mask_fields: ["password", "token", "secret", "key", "credential", "api_key"],
    mask_pattern: "[REDACTED]",
    explain_queries: True,
  )
}

// ============================================================================
// DEV MODE STATE (Actor-based)
// ============================================================================

/// Internal message type for dev mode actor
type DevMessage {
  Enable(config: DevConfig, reply_to: Subject(Result(Nil, DevError)))
  Disable(reply_to: Subject(Result(Nil, DevError)))
  GetConfig(reply_to: Subject(Option(DevConfig)))
  IsEnabled(reply_to: Subject(Bool))
  UpdateConfig(config: DevConfig, reply_to: Subject(Result(Nil, DevError)))
  UpdatePoolStats(stats: PoolStats)
}

/// Internal state for dev mode actor
type DevState {
  DevState(config: Option(DevConfig), pool_stats: Option(PoolStats))
}

/// Pool statistics for monitoring
pub type PoolStats {
  PoolStats(
    /// Name of the pool
    pool_name: String,
    /// Total pool size
    size: Int,
    /// Currently in use
    in_use: Int,
    /// Available connections
    available: Int,
    /// Requests waiting for connection
    waiting: Int,
    /// Total checkouts since start
    total_checkouts: Int,
    /// Total timeouts since start
    total_timeouts: Int,
  )
}

/// Error types for dev mode operations
pub type DevError {
  /// Dev mode already enabled
  AlreadyEnabled
  /// Dev mode not enabled
  NotEnabled
  /// Telemetry server not running
  TelemetryRequired
  /// Dev mode server not running
  DevModeNotRunning
}

/// FFI for global dev mode server storage
@external(erlang, "cquill_dev_ffi", "get_dev_server")
fn get_dev_server() -> Result(Subject(DevMessage), Nil)

@external(erlang, "cquill_dev_ffi", "set_dev_server")
fn set_dev_server(server: Subject(DevMessage)) -> Nil

@external(erlang, "cquill_dev_ffi", "clear_dev_server")
fn clear_dev_server() -> Nil

@external(erlang, "cquill_dev_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

// ============================================================================
// DEV MODE SERVER
// ============================================================================

/// Start the dev mode server
fn start_server() -> Result(Subject(DevMessage), actor.StartError) {
  let initial_state = DevState(config: None, pool_stats: None)

  actor.new(initial_state)
  |> actor.on_message(fn(state, msg) {
    case msg {
      Enable(config, reply_to) -> {
        case state.config {
          Some(_) -> {
            process.send(reply_to, Error(AlreadyEnabled))
            actor.continue(state)
          }
          None -> {
            process.send(reply_to, Ok(Nil))
            actor.continue(DevState(..state, config: Some(config)))
          }
        }
      }

      Disable(reply_to) -> {
        case state.config {
          Some(_) -> {
            process.send(reply_to, Ok(Nil))
            actor.continue(DevState(config: None, pool_stats: None))
          }
          None -> {
            process.send(reply_to, Error(NotEnabled))
            actor.continue(state)
          }
        }
      }

      GetConfig(reply_to) -> {
        process.send(reply_to, state.config)
        actor.continue(state)
      }

      IsEnabled(reply_to) -> {
        process.send(reply_to, option.is_some(state.config))
        actor.continue(state)
      }

      UpdateConfig(config, reply_to) -> {
        case state.config {
          Some(_) -> {
            process.send(reply_to, Ok(Nil))
            actor.continue(DevState(..state, config: Some(config)))
          }
          None -> {
            process.send(reply_to, Error(NotEnabled))
            actor.continue(state)
          }
        }
      }

      UpdatePoolStats(stats) -> {
        actor.continue(DevState(..state, pool_stats: Some(stats)))
      }
    }
  })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

// ============================================================================
// PUBLIC API
// ============================================================================

/// Enable development mode with default configuration
pub fn enable() -> Result(Nil, DevError) {
  enable_with_config(default_config())
}

/// Enable development mode with custom configuration
pub fn enable_with_config(config: DevConfig) -> Result(Nil, DevError) {
  // Ensure telemetry is running
  case telemetry.start() {
    Ok(_) -> Nil
    Error(_) -> Nil
  }

  // Start or get dev server
  let server = case get_dev_server() {
    Ok(s) -> Ok(s)
    Error(_) -> {
      case start_server() {
        Ok(s) -> {
          set_dev_server(s)
          Ok(s)
        }
        Error(_) -> Error(DevModeNotRunning)
      }
    }
  }

  case server {
    Error(e) -> Error(e)
    Ok(s) -> {
      // Enable dev mode
      let result =
        process.call(s, 5000, fn(reply_to) { Enable(config, reply_to) })

      case result {
        Ok(_) -> {
          // Attach telemetry handlers
          attach_handlers(config)
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Disable development mode
pub fn disable() -> Result(Nil, DevError) {
  case get_dev_server() {
    Error(_) -> Error(DevModeNotRunning)
    Ok(server) -> {
      let result =
        process.call(server, 5000, fn(reply_to) { Disable(reply_to) })

      case result {
        Ok(_) -> {
          // Detach telemetry handlers
          detach_handlers()
          Ok(Nil)
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Check if development mode is enabled
pub fn is_enabled() -> Bool {
  case get_dev_server() {
    Error(_) -> False
    Ok(server) -> {
      process.call(server, 5000, fn(reply_to) { IsEnabled(reply_to) })
    }
  }
}

/// Get the current configuration (if enabled)
pub fn get_config() -> Option(DevConfig) {
  case get_dev_server() {
    Error(_) -> None
    Ok(server) -> {
      process.call(server, 5000, fn(reply_to) { GetConfig(reply_to) })
    }
  }
}

/// Update configuration while dev mode is running
pub fn configure(config: DevConfig) -> Result(Nil, DevError) {
  case get_dev_server() {
    Error(_) -> Error(DevModeNotRunning)
    Ok(server) -> {
      let result =
        process.call(server, 5000, fn(reply_to) {
          UpdateConfig(config, reply_to)
        })

      case result {
        Ok(_) -> {
          // Re-attach handlers with new config
          detach_handlers()
          attach_handlers(config)
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Enable dev mode from environment variable
/// Call this at application startup to auto-enable if CQUILL_DEV=1
pub fn enable_from_env() -> Result(Nil, DevError) {
  case get_env("CQUILL_DEV") {
    Ok(value) -> {
      case value {
        "1" | "true" | "yes" | "on" -> enable()
        _ -> Ok(Nil)
      }
    }
    Error(_) -> Ok(Nil)
  }
}

/// Stop the dev mode server completely
pub fn stop() -> Nil {
  // Fire-and-forget cleanup: stop() always succeeds from the caller's perspective.
  // Any errors during disable() are silently ignored since the intent is to clean up.
  let _ = disable()
  clear_dev_server()
}

// ============================================================================
// TELEMETRY HANDLER ATTACHMENT
// ============================================================================

/// Handler IDs used by dev mode
const query_handler_id = "cquill_dev_query"

const slow_query_handler_id = "cquill_dev_slow_query"

const pool_handler_id = "cquill_dev_pool"

const transaction_handler_id = "cquill_dev_transaction"

/// Attach telemetry handlers based on configuration.
/// Telemetry attachment is fire-and-forget: failures don't prevent dev mode from working.
/// This is intentional since telemetry is observability infrastructure, not core functionality.
fn attach_handlers(config: DevConfig) -> Result(Nil, DevError) {
  // Query logging handler
  case config.log_queries {
    True -> {
      let _ =
        telemetry.attach(
          query_handler_id,
          [
            telemetry.QueryStartType,
            telemetry.QueryStopType,
            telemetry.QueryExceptionType,
          ],
          dev_query_handler(config),
        )
      Nil
    }
    False -> Nil
  }

  // Slow query handler
  let _ =
    telemetry.attach(
      slow_query_handler_id,
      [telemetry.QueryStopType],
      dev_slow_query_handler(config.slow_query_threshold_ms),
    )

  // Pool events handler
  case config.log_pool_stats {
    True -> {
      let _ =
        telemetry.attach(
          pool_handler_id,
          [
            telemetry.PoolCheckoutType,
            telemetry.PoolCheckinType,
            telemetry.PoolTimeoutType,
          ],
          dev_pool_handler(),
        )
      Nil
    }
    False -> Nil
  }

  // Transaction handler
  let _ =
    telemetry.attach(
      transaction_handler_id,
      [
        telemetry.TransactionStartType,
        telemetry.TransactionCommitType,
        telemetry.TransactionRollbackType,
      ],
      dev_transaction_handler(),
    )

  Ok(Nil)
}

/// Detach all dev mode handlers.
/// Detachment is fire-and-forget cleanup: failures are silently ignored.
/// This ensures cleanup always completes, even if some handlers were never attached.
fn detach_handlers() -> Nil {
  let _ = telemetry.detach(query_handler_id)
  let _ = telemetry.detach(slow_query_handler_id)
  let _ = telemetry.detach(pool_handler_id)
  let _ = telemetry.detach(transaction_handler_id)
  Nil
}

// ============================================================================
// DEV MODE HANDLERS
// ============================================================================

/// Query logging handler with parameter masking
fn dev_query_handler(config: DevConfig) -> telemetry.Handler {
  fn(event, _metadata) {
    case event {
      telemetry.QueryStop(e) -> {
        let duration_ms = e.duration_us / 1000
        let params_str = case config.log_params {
          True ->
            format_params(e.params, config.mask_fields, config.mask_pattern)
          False -> ""
        }

        io.println(
          string.concat([
            "[cquill] ",
            e.query,
            " [",
            int.to_string(duration_ms),
            "ms, ",
            int.to_string(e.row_count),
            " rows]",
          ]),
        )

        case params_str {
          "" -> Nil
          _ -> io.println("         params: " <> params_str)
        }
      }

      telemetry.QueryException(e) -> {
        let duration_ms = e.duration_us / 1000
        let params_str = case config.log_params {
          True ->
            format_params(e.params, config.mask_fields, config.mask_pattern)
          False -> ""
        }

        io.println_error(
          string.concat([
            "[cquill] QUERY ERROR: ",
            e.query,
            " [",
            int.to_string(duration_ms),
            "ms]",
          ]),
        )

        case params_str {
          "" -> Nil
          _ -> io.println_error("         params: " <> params_str)
        }

        io.println_error("         error: " <> format_error(e.error))
      }

      _ -> Nil
    }
  }
}

/// Slow query handler with helpful hints
fn dev_slow_query_handler(threshold_ms: Int) -> telemetry.Handler {
  let threshold_us = threshold_ms * 1000
  fn(event, _metadata) {
    case event {
      telemetry.QueryStop(e) -> {
        case e.duration_us > threshold_us {
          True -> {
            let duration_ms = e.duration_us / 1000
            io.println_error(
              string.concat([
                "[cquill] \u{26A0}\u{FE0F} SLOW QUERY (",
                int.to_string(duration_ms),
                "ms):",
              ]),
            )
            io.println_error("         " <> e.query)
            io.println_error("         rows: " <> int.to_string(e.row_count))

            // Provide hints based on query analysis
            let hints = analyze_query_for_hints(e.query, e.row_count)
            case hints {
              [] -> Nil
              _ -> {
                io.println_error("")
                io.println_error("         Hints:")
                list.each(hints, fn(hint) {
                  io.println_error("           - " <> hint)
                })
              }
            }
          }
          False -> Nil
        }
      }
      _ -> Nil
    }
  }
}

/// Pool events handler
fn dev_pool_handler() -> telemetry.Handler {
  fn(event, _metadata) {
    case event {
      telemetry.PoolCheckout(e) -> {
        let wait_ms = e.wait_time_us / 1000
        case wait_ms > 10 {
          True ->
            io.println(
              string.concat([
                "[cquill] Pool checkout: ",
                e.pool_name,
                " (waited ",
                int.to_string(wait_ms),
                "ms, queue: ",
                int.to_string(e.queue_length),
                ")",
              ]),
            )
          False -> Nil
        }
      }

      telemetry.PoolTimeout(e) -> {
        let wait_ms = e.wait_time_us / 1000
        io.println_error(
          string.concat([
            "[cquill] \u{26A0}\u{FE0F} Pool TIMEOUT: ",
            e.pool_name,
            " after ",
            int.to_string(wait_ms),
            "ms (queue: ",
            int.to_string(e.queue_length),
            ")",
          ]),
        )
        io.println_error("         Hint: Consider increasing pool size")
      }

      _ -> Nil
    }
  }
}

/// Transaction events handler
fn dev_transaction_handler() -> telemetry.Handler {
  fn(event, _metadata) {
    case event {
      telemetry.TransactionStart(e) -> {
        io.println("[cquill] Transaction started: " <> e.transaction_id)
      }

      telemetry.TransactionCommit(e) -> {
        let duration_ms = e.duration_us / 1000
        io.println(
          string.concat([
            "[cquill] Transaction committed: ",
            e.transaction_id,
            " [",
            int.to_string(duration_ms),
            "ms, ",
            int.to_string(e.query_count),
            " queries]",
          ]),
        )
      }

      telemetry.TransactionRollback(e) -> {
        let duration_ms = e.duration_us / 1000
        let reason_str = case e.reason {
          Some(r) -> " - " <> r
          None -> ""
        }
        io.println_error(
          string.concat([
            "[cquill] Transaction rolled back: ",
            e.transaction_id,
            reason_str,
            " [",
            int.to_string(duration_ms),
            "ms]",
          ]),
        )
      }

      _ -> Nil
    }
  }
}

// ============================================================================
// POOL STATISTICS
// ============================================================================

/// Log current pool statistics
pub fn log_pool_stats(stats: PoolStats) -> Nil {
  io.println("[cquill] Pool stats:")
  io.println("         - Name: " <> stats.pool_name)
  io.println("         - Size: " <> int.to_string(stats.size))
  io.println("         - In use: " <> int.to_string(stats.in_use))
  io.println("         - Available: " <> int.to_string(stats.available))
  io.println("         - Waiting: " <> int.to_string(stats.waiting))
  io.println("         - Checkouts: " <> int.to_string(stats.total_checkouts))
  io.println("         - Timeouts: " <> int.to_string(stats.total_timeouts))
}

/// Update pool statistics in dev mode server
pub fn update_pool_stats(stats: PoolStats) -> Nil {
  case get_dev_server() {
    Error(_) -> Nil
    Ok(server) -> {
      process.send(server, UpdatePoolStats(stats))
    }
  }
}

/// Create pool stats from values
pub fn pool_stats(
  pool_name pool_name: String,
  size size: Int,
  in_use in_use: Int,
  available available: Int,
  waiting waiting: Int,
  total_checkouts total_checkouts: Int,
  total_timeouts total_timeouts: Int,
) -> PoolStats {
  PoolStats(
    pool_name: pool_name,
    size: size,
    in_use: in_use,
    available: available,
    waiting: waiting,
    total_checkouts: total_checkouts,
    total_timeouts: total_timeouts,
  )
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Format query parameters with sensitive field masking
fn format_params(
  params: List(Value),
  mask_fields: List(String),
  mask_pattern: String,
) -> String {
  let formatted =
    params
    |> list.index_map(fn(param, _index) {
      // Check if this parameter position should be masked
      // In practice, we can't know the field name from position alone,
      // so we mask based on value patterns (e.g., long strings that look like tokens)
      let value_str = format_value(param)

      // Mask if the value looks like a secret
      case should_mask_value(value_str, mask_fields) {
        True -> mask_pattern
        False -> value_str
      }
    })
    |> string.join(", ")

  "[" <> formatted <> "]"
}

/// Format a single value for display
fn format_value(value: Value) -> String {
  case value {
    ast.IntValue(i) -> int.to_string(i)
    ast.FloatValue(f) -> string.inspect(f)
    ast.StringValue(s) -> "\"" <> escape_string(s) <> "\""
    ast.BoolValue(b) -> bool.to_string(b)
    ast.NullValue -> "NULL"
    ast.ParamValue(p) -> "$" <> int.to_string(p)
    ast.ListValue(values) -> {
      let formatted =
        values
        |> list.map(format_value)
        |> string.join(", ")
      "[" <> formatted <> "]"
    }
  }
}

/// Check if a value should be masked based on heuristics
fn should_mask_value(value: String, _mask_fields: List(String)) -> Bool {
  // Mask if the value looks like a token, password, or secret
  let is_long_string = string.length(value) > 32
  let looks_like_hash =
    string.contains(value, "$2") || string.contains(value, "sha")
  let looks_like_token =
    string.contains(value, "bearer")
    || string.contains(value, "token")
    || string.contains(value, "api_key")

  is_long_string && { looks_like_hash || looks_like_token }
}

/// Escape special characters in a string for display
fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

/// Analyze a query and provide optimization hints
fn analyze_query_for_hints(query: String, row_count: Int) -> List(String) {
  let query_lower = string.lowercase(query)
  let hints = []

  // Check for LIKE with leading wildcard
  let hints = case string.contains(query_lower, "like '%") {
    True ->
      list.append(hints, [
        "LIKE with leading wildcard '%...' cannot use indexes. Consider full-text search.",
      ])
    False -> hints
  }

  // Check for SELECT *
  let hints = case string.contains(query_lower, "select *") {
    True ->
      list.append(hints, [
        "SELECT * retrieves all columns. Consider selecting only needed columns.",
      ])
    False -> hints
  }

  // Check for large result sets
  let hints = case row_count > 1000 {
    True ->
      list.append(hints, [
        "Large result set ("
        <> int.to_string(row_count)
        <> " rows). Consider pagination with LIMIT/OFFSET.",
      ])
    False -> hints
  }

  // Check for ORDER BY without index hint
  let has_order_by = string.contains(query_lower, "order by")
  let has_limit = string.contains(query_lower, "limit")
  let hints = case has_order_by && !has_limit && row_count > 100 {
    True ->
      list.append(hints, [
        "ORDER BY without LIMIT on large results is expensive. Consider adding an index.",
      ])
    False -> hints
  }

  // Check for JOIN without WHERE
  let has_join =
    string.contains(query_lower, " join ")
    || string.contains(query_lower, "inner join")
    || string.contains(query_lower, "left join")
  let has_where = string.contains(query_lower, "where")
  let hints = case has_join && !has_where {
    True ->
      list.append(hints, [
        "JOIN without WHERE clause may return unexpected large results.",
      ])
    False -> hints
  }

  // Check for functions in WHERE clause
  let hints = case
    string.contains(query_lower, "where lower(")
    || string.contains(query_lower, "where upper(")
    || string.contains(query_lower, "where date(")
  {
    True ->
      list.append(hints, [
        "Functions in WHERE clause prevent index usage. Consider functional indexes.",
      ])
    False -> hints
  }

  hints
}

/// Format an adapter error for display
fn format_error(err: AdapterError) -> String {
  case err {
    error.NotFound -> "Record not found"
    error.TooManyRows(expected, got) ->
      "Expected "
      <> int.to_string(expected)
      <> " row(s), got "
      <> int.to_string(got)
    error.ConnectionFailed(reason) -> "Connection failed: " <> reason
    error.ConnectionTimeout -> "Connection timeout"
    error.PoolExhausted -> "Connection pool exhausted"
    error.ConnectionLost(reason) -> "Connection lost: " <> reason
    error.QueryFailed(message, code) ->
      "Query failed [" <> option.unwrap(code, "?") <> "]: " <> message
    error.DecodeFailed(row, column, expected, got) ->
      "Decode failed at row "
      <> int.to_string(row)
      <> ", column "
      <> column
      <> " (expected "
      <> expected
      <> ", got "
      <> got
      <> ")"
    error.Timeout -> "Query timeout"
    error.UniqueViolation(constraint, detail) ->
      "Unique violation: " <> constraint <> " - " <> detail
    error.ForeignKeyViolation(constraint, detail) ->
      "Foreign key violation: " <> constraint <> " - " <> detail
    error.CheckViolation(constraint, detail) ->
      "Check violation: " <> constraint <> " - " <> detail
    error.NotNullViolation(column) -> "NOT NULL violation: " <> column
    error.ConstraintViolation(constraint, detail) ->
      "Constraint violation: " <> constraint <> " - " <> detail
    error.StaleData(expected, actual) ->
      "Stale data (expected version " <> expected <> ", got " <> actual <> ")"
    error.DataIntegrityError(message) -> "Data integrity error: " <> message
    error.NotSupported(operation) -> "Not supported: " <> operation
    error.AdapterSpecific(code, message) ->
      "Adapter error [" <> code <> "]: " <> message
  }
}
