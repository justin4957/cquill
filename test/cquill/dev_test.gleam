// cquill Development Mode Tests

import cquill/dev
import cquill/error
import cquill/query/ast
import cquill/telemetry
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// CONFIGURATION TESTS
// ============================================================================

pub fn default_config_has_expected_values_test() {
  let config = dev.default_config()

  config.enabled |> should.be_true()
  config.log_queries |> should.be_true()
  config.log_params |> should.be_true()
  config.slow_query_threshold_ms |> should.equal(100)
  config.log_pool_stats |> should.be_false()
  config.pool_stats_interval_ms |> should.equal(10_000)
  config.mask_pattern |> should.equal("[REDACTED]")
  config.explain_queries |> should.be_false()
}

pub fn minimal_config_has_expected_values_test() {
  let config = dev.minimal_config()

  config.enabled |> should.be_true()
  config.log_queries |> should.be_true()
  config.log_params |> should.be_false()
  config.slow_query_threshold_ms |> should.equal(500)
  config.log_pool_stats |> should.be_false()
}

pub fn verbose_config_has_expected_values_test() {
  let config = dev.verbose_config()

  config.enabled |> should.be_true()
  config.log_queries |> should.be_true()
  config.log_params |> should.be_true()
  config.slow_query_threshold_ms |> should.equal(50)
  config.log_pool_stats |> should.be_true()
  config.pool_stats_interval_ms |> should.equal(5000)
  config.explain_queries |> should.be_true()
}

// ============================================================================
// ENABLE/DISABLE TESTS
// ============================================================================

pub fn enable_starts_dev_mode_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode
  let result = dev.enable()
  result |> should.be_ok()

  // Verify enabled
  dev.is_enabled() |> should.be_true()

  // Verify config is set
  let config = dev.get_config()
  config |> should.be_some()

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn disable_stops_dev_mode_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable then disable
  let _ = dev.enable()
  dev.is_enabled() |> should.be_true()

  let result = dev.disable()
  result |> should.be_ok()

  // Verify disabled
  dev.is_enabled() |> should.be_false()

  // Config should be None
  dev.get_config() |> should.be_none()

  // Cleanup
  dev.stop()
  telemetry.stop()
}

pub fn double_enable_returns_error_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // First enable should succeed
  dev.enable() |> should.be_ok()

  // Second enable should fail
  let result = dev.enable()
  result |> should.be_error()

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn disable_without_enable_returns_error_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Start server but don't enable
  let _ = dev.enable()
  let _ = dev.disable()

  // Second disable should fail
  let result = dev.disable()
  result |> should.be_error()

  // Cleanup
  dev.stop()
  telemetry.stop()
}

pub fn is_enabled_returns_false_when_not_started_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Should be false when not started
  dev.is_enabled() |> should.be_false()
}

// ============================================================================
// ENABLE WITH CONFIG TESTS
// ============================================================================

pub fn enable_with_custom_config_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  let config =
    dev.DevConfig(
      enabled: True,
      log_queries: True,
      log_params: False,
      slow_query_threshold_ms: 200,
      log_pool_stats: True,
      pool_stats_interval_ms: 15_000,
      mask_fields: ["password"],
      mask_pattern: "***",
      explain_queries: True,
    )

  // Enable with custom config
  dev.enable_with_config(config) |> should.be_ok()

  // Verify config matches
  let retrieved = dev.get_config()
  retrieved |> should.be_some()

  case retrieved {
    Some(c) -> {
      c.slow_query_threshold_ms |> should.equal(200)
      c.log_params |> should.be_false()
      c.log_pool_stats |> should.be_true()
      c.mask_pattern |> should.equal("***")
    }
    None -> should.fail()
  }

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

// ============================================================================
// CONFIGURE TESTS
// ============================================================================

pub fn configure_updates_running_dev_mode_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable with default config
  dev.enable() |> should.be_ok()

  // Verify default threshold
  case dev.get_config() {
    Some(c) -> c.slow_query_threshold_ms |> should.equal(100)
    None -> should.fail()
  }

  // Update config
  let new_config =
    dev.DevConfig(
      enabled: True,
      log_queries: True,
      log_params: True,
      slow_query_threshold_ms: 500,
      log_pool_stats: False,
      pool_stats_interval_ms: 10_000,
      mask_fields: [],
      mask_pattern: "[REDACTED]",
      explain_queries: False,
    )

  dev.configure(new_config) |> should.be_ok()

  // Verify updated
  case dev.get_config() {
    Some(c) -> c.slow_query_threshold_ms |> should.equal(500)
    None -> should.fail()
  }

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn configure_fails_when_not_enabled_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  let config = dev.default_config()
  let result = dev.configure(config)

  result |> should.be_error()
}

// ============================================================================
// POOL STATS TESTS
// ============================================================================

pub fn pool_stats_creation_test() {
  let stats =
    dev.pool_stats(
      pool_name: "test_pool",
      size: 10,
      in_use: 3,
      available: 7,
      waiting: 0,
      total_checkouts: 100,
      total_timeouts: 2,
    )

  stats.pool_name |> should.equal("test_pool")
  stats.size |> should.equal(10)
  stats.in_use |> should.equal(3)
  stats.available |> should.equal(7)
  stats.waiting |> should.equal(0)
  stats.total_checkouts |> should.equal(100)
  stats.total_timeouts |> should.equal(2)
}

pub fn update_pool_stats_does_not_crash_when_not_running_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  let stats =
    dev.pool_stats(
      pool_name: "test",
      size: 5,
      in_use: 1,
      available: 4,
      waiting: 0,
      total_checkouts: 10,
      total_timeouts: 0,
    )

  // Should not crash even when dev mode not running
  dev.update_pool_stats(stats)
}

// ============================================================================
// STOP TESTS
// ============================================================================

pub fn stop_clears_server_test() {
  // Ensure clean state then enable
  dev.stop()
  telemetry.stop()
  dev.enable() |> should.be_ok()

  // Verify enabled
  dev.is_enabled() |> should.be_true()

  // Stop completely
  dev.stop()
  telemetry.stop()

  // Should be disabled
  dev.is_enabled() |> should.be_false()
}

// ============================================================================
// TELEMETRY INTEGRATION TESTS
// ============================================================================

pub fn enable_attaches_telemetry_handlers_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Start telemetry separately to check handlers
  let _ = telemetry.start()

  // Check initial handler count
  let initial_count = telemetry.handler_count()

  // Enable dev mode
  dev.enable() |> should.be_ok()

  // Should have more handlers now
  let new_count = telemetry.handler_count()
  { new_count > initial_count } |> should.be_true()

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn disable_detaches_telemetry_handlers_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode
  dev.enable() |> should.be_ok()

  // Disable and check handlers are removed
  dev.disable() |> should.be_ok()

  // Handler count should be low (only telemetry internals)
  let count = telemetry.handler_count()
  { count == 0 } |> should.be_true()

  // Cleanup
  dev.stop()
  telemetry.stop()
}

// ============================================================================
// QUERY HINT ANALYSIS TESTS
// ============================================================================

// These tests verify the hint generation for slow queries
// The hints are generated internally but we can test the behavior
// by checking that dev mode enables without error and handles events

pub fn dev_mode_handles_query_events_without_crash_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode
  dev.enable() |> should.be_ok()

  // Emit a query event - should not crash
  telemetry.emit(
    telemetry.QueryStop(telemetry.QueryStopEvent(
      query: "SELECT * FROM users WHERE name LIKE '%test%'",
      params: [ast.StringValue("test")],
      duration_us: 150_000,
      row_count: 1500,
      source: None,
    )),
    telemetry.empty_metadata(),
  )

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn dev_mode_handles_slow_query_without_crash_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode with low threshold
  let config =
    dev.DevConfig(
      enabled: True,
      log_queries: True,
      log_params: True,
      slow_query_threshold_ms: 10,
      log_pool_stats: False,
      pool_stats_interval_ms: 10_000,
      mask_fields: [],
      mask_pattern: "[REDACTED]",
      explain_queries: False,
    )
  dev.enable_with_config(config) |> should.be_ok()

  // Emit a slow query event - should trigger hints
  telemetry.emit(
    telemetry.QueryStop(telemetry.QueryStopEvent(
      query: "SELECT * FROM posts ORDER BY created_at DESC",
      params: [],
      duration_us: 500_000,
      row_count: 10_000,
      source: None,
    )),
    telemetry.empty_metadata(),
  )

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn dev_mode_handles_pool_timeout_without_crash_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode with pool stats
  let config =
    dev.DevConfig(
      enabled: True,
      log_queries: True,
      log_params: True,
      slow_query_threshold_ms: 100,
      log_pool_stats: True,
      pool_stats_interval_ms: 10_000,
      mask_fields: [],
      mask_pattern: "[REDACTED]",
      explain_queries: False,
    )
  dev.enable_with_config(config) |> should.be_ok()

  // Emit a pool timeout event
  telemetry.emit(
    telemetry.PoolTimeout(telemetry.PoolTimeoutEvent(
      pool_name: "test_pool",
      wait_time_us: 5_000_000,
      queue_length: 10,
    )),
    telemetry.empty_metadata(),
  )

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn dev_mode_handles_transaction_events_without_crash_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode
  dev.enable() |> should.be_ok()

  // Emit transaction events
  telemetry.emit(
    telemetry.TransactionStart(telemetry.TransactionStartEvent(
      transaction_id: "tx_123",
      source: None,
      start_time_us: telemetry.now_us(),
    )),
    telemetry.empty_metadata(),
  )

  telemetry.emit(
    telemetry.TransactionCommit(telemetry.TransactionCommitEvent(
      transaction_id: "tx_123",
      duration_us: 50_000,
      query_count: 5,
      source: None,
    )),
    telemetry.empty_metadata(),
  )

  telemetry.emit(
    telemetry.TransactionRollback(telemetry.TransactionRollbackEvent(
      transaction_id: "tx_456",
      duration_us: 25_000,
      reason: Some("test rollback"),
      source: None,
    )),
    telemetry.empty_metadata(),
  )

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

pub fn dev_mode_handles_query_exception_without_crash_test() {
  // Ensure clean state
  dev.stop()
  telemetry.stop()

  // Enable dev mode
  dev.enable() |> should.be_ok()

  // Emit a query exception event
  telemetry.emit(
    telemetry.QueryException(telemetry.QueryExceptionEvent(
      query: "SELECT * FROM nonexistent_table",
      params: [],
      error: error.QueryFailed("relation does not exist", Some("42P01")),
      duration_us: 1000,
      source: None,
    )),
    telemetry.empty_metadata(),
  )

  // Cleanup
  dev.disable() |> should.be_ok()
  dev.stop()
  telemetry.stop()
}

// ============================================================================
// MASK FIELDS TESTS
// ============================================================================

pub fn mask_fields_included_in_config_test() {
  let config = dev.default_config()

  // Default config should include common sensitive fields
  { config.mask_fields != [] } |> should.be_true()
}
