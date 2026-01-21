// cquill Benchmark Utilities
//
// A simple benchmarking framework for measuring cquill performance.
// Used to establish baselines and detect regressions.

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// TYPES
// ============================================================================

/// Configuration for a benchmark run
pub type BenchConfig {
  BenchConfig(
    /// Number of warmup iterations (not counted)
    warmup_iterations: Int,
    /// Number of measured iterations
    iterations: Int,
    /// Whether to print individual iteration times
    verbose: Bool,
  )
}

/// Result from a benchmark run
pub type BenchResult {
  BenchResult(
    /// Name of the benchmark
    name: String,
    /// Total time in milliseconds
    total_ms: Int,
    /// Number of iterations
    iterations: Int,
    /// Average time per iteration in microseconds
    avg_us: Float,
    /// Minimum time in microseconds (if captured)
    min_us: Option(Float),
    /// Maximum time in microseconds (if captured)
    max_us: Option(Float),
  )
}

/// A collection of benchmark results for a suite
pub type BenchSuite {
  BenchSuite(name: String, results: List(BenchResult))
}

// ============================================================================
// CONFIGURATION
// ============================================================================

/// Default benchmark configuration
pub fn default_config() -> BenchConfig {
  BenchConfig(warmup_iterations: 10, iterations: 1000, verbose: False)
}

/// Quick benchmark configuration (fewer iterations)
pub fn quick_config() -> BenchConfig {
  BenchConfig(warmup_iterations: 5, iterations: 100, verbose: False)
}

/// Thorough benchmark configuration (more iterations)
pub fn thorough_config() -> BenchConfig {
  BenchConfig(warmup_iterations: 100, iterations: 10_000, verbose: False)
}

/// Create a custom configuration
pub fn config(
  warmup warmup_iterations: Int,
  iterations iterations: Int,
  verbose verbose: Bool,
) -> BenchConfig {
  BenchConfig(warmup_iterations:, iterations:, verbose:)
}

// ============================================================================
// BENCHMARK EXECUTION
// ============================================================================

/// Run a benchmark with default configuration
pub fn benchmark(name: String, f: fn() -> a) -> BenchResult {
  benchmark_with_config(name, default_config(), f)
}

/// Run a benchmark with custom configuration
pub fn benchmark_with_config(
  name: String,
  config: BenchConfig,
  f: fn() -> a,
) -> BenchResult {
  // Warmup phase
  run_iterations(config.warmup_iterations, f)

  // Measurement phase
  let start = system_time_microseconds()
  run_iterations(config.iterations, f)
  let end = system_time_microseconds()

  let total_us = end - start
  let total_ms = total_us / 1000
  let avg_us = int.to_float(total_us) /. int.to_float(config.iterations)

  BenchResult(
    name:,
    total_ms:,
    iterations: config.iterations,
    avg_us:,
    min_us: None,
    max_us: None,
  )
}

/// Run a benchmark capturing min/max times (slower but more detailed)
pub fn benchmark_detailed(
  name: String,
  config: BenchConfig,
  f: fn() -> a,
) -> BenchResult {
  // Warmup phase
  run_iterations(config.warmup_iterations, f)

  // Measurement phase with individual timing
  let times = measure_each_iteration(config.iterations, f, [])
  let total_us = list.fold(times, 0, int.add)
  let total_ms = total_us / 1000
  let avg_us = int.to_float(total_us) /. int.to_float(config.iterations)

  let min_us =
    times
    |> list.reduce(int.min)
    |> result.map(int.to_float)
    |> option.from_result

  let max_us =
    times
    |> list.reduce(int.max)
    |> result.map(int.to_float)
    |> option.from_result

  BenchResult(
    name:,
    total_ms:,
    iterations: config.iterations,
    avg_us:,
    min_us:,
    max_us:,
  )
}

/// Run a function that needs setup/teardown for each iteration
pub fn benchmark_with_setup(
  name: String,
  config: BenchConfig,
  setup: fn() -> s,
  f: fn(s) -> a,
  teardown: fn(s, a) -> Nil,
) -> BenchResult {
  // Warmup phase
  run_iterations_with_setup(config.warmup_iterations, setup, f, teardown)

  // Measurement phase
  let start = system_time_microseconds()
  run_iterations_with_setup(config.iterations, setup, f, teardown)
  let end = system_time_microseconds()

  let total_us = end - start
  let total_ms = total_us / 1000
  let avg_us = int.to_float(total_us) /. int.to_float(config.iterations)

  BenchResult(
    name:,
    total_ms:,
    iterations: config.iterations,
    avg_us:,
    min_us: None,
    max_us: None,
  )
}

// ============================================================================
// SUITE EXECUTION
// ============================================================================

/// Create a new benchmark suite
pub fn new_suite(name: String) -> BenchSuite {
  BenchSuite(name:, results: [])
}

/// Add a benchmark result to a suite
pub fn add_result(suite: BenchSuite, result: BenchResult) -> BenchSuite {
  BenchSuite(..suite, results: list.append(suite.results, [result]))
}

/// Run all benchmarks in a suite
pub fn run_suite(
  suite_name: String,
  benchmarks: List(#(String, fn() -> a)),
  config: BenchConfig,
) -> BenchSuite {
  io.println("")
  io.println("=" |> string.repeat(60))
  io.println("Benchmark Suite: " <> suite_name)
  io.println("=" |> string.repeat(60))
  io.println(
    "Config: "
    <> int.to_string(config.iterations)
    <> " iterations, "
    <> int.to_string(config.warmup_iterations)
    <> " warmup",
  )
  io.println("-" |> string.repeat(60))

  let results =
    list.map(benchmarks, fn(bench) {
      let #(name, f) = bench
      let result = benchmark_with_config(name, config, f)
      print_result(result)
      result
    })

  io.println("-" |> string.repeat(60))
  io.println("")

  BenchSuite(name: suite_name, results:)
}

// ============================================================================
// OUTPUT FORMATTING
// ============================================================================

/// Print a single benchmark result
pub fn print_result(result: BenchResult) -> Nil {
  let avg_str = format_duration_us(result.avg_us)
  let total_str = int.to_string(result.total_ms) <> "ms"
  let iter_str = int.to_string(result.iterations) <> " iters"

  let extra = case result.min_us, result.max_us {
    Some(min), Some(max) ->
      " (min: "
      <> format_duration_us(min)
      <> ", max: "
      <> format_duration_us(max)
      <> ")"
    _, _ -> ""
  }

  io.println(
    pad_right(result.name, 35)
    <> pad_left(avg_str, 12)
    <> "/op  "
    <> pad_left(total_str, 8)
    <> " total  "
    <> pad_left(iter_str, 12)
    <> extra,
  )
}

/// Print summary of a benchmark suite
pub fn print_suite_summary(suite: BenchSuite) -> Nil {
  io.println("")
  io.println("=" |> string.repeat(60))
  io.println("Summary: " <> suite.name)
  io.println("=" |> string.repeat(60))

  list.each(suite.results, fn(result) {
    io.println(
      "  "
      <> pad_right(result.name, 30)
      <> ": "
      <> format_duration_us(result.avg_us)
      <> "/op",
    )
  })

  io.println("")
}

/// Format a benchmark suite as markdown table
pub fn to_markdown(suite: BenchSuite) -> String {
  let header = "| Benchmark | Avg/Op | Total | Iterations |\n"
  let separator = "|-----------|--------|-------|------------|\n"

  let rows =
    list.map(suite.results, fn(result) {
      "| "
      <> result.name
      <> " | "
      <> format_duration_us(result.avg_us)
      <> " | "
      <> int.to_string(result.total_ms)
      <> "ms | "
      <> int.to_string(result.iterations)
      <> " |\n"
    })
    |> string.concat

  "### " <> suite.name <> "\n\n" <> header <> separator <> rows
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Get current system time in microseconds using erlang:monotonic_time/1
@external(erlang, "bench_ffi", "monotonic_time_us")
fn system_time_microseconds() -> Int

/// Run a function n times
fn run_iterations(n: Int, f: fn() -> a) -> Nil {
  case n > 0 {
    False -> Nil
    True -> {
      let _ = f()
      run_iterations(n - 1, f)
    }
  }
}

/// Measure each iteration individually
fn measure_each_iteration(n: Int, f: fn() -> a, times: List(Int)) -> List(Int) {
  case n > 0 {
    False -> list.reverse(times)
    True -> {
      let start = system_time_microseconds()
      let _ = f()
      let end = system_time_microseconds()
      measure_each_iteration(n - 1, f, [end - start, ..times])
    }
  }
}

/// Run iterations with setup/teardown
fn run_iterations_with_setup(
  n: Int,
  setup: fn() -> s,
  f: fn(s) -> a,
  teardown: fn(s, a) -> Nil,
) -> Nil {
  case n > 0 {
    False -> Nil
    True -> {
      let state = setup()
      let result = f(state)
      teardown(state, result)
      run_iterations_with_setup(n - 1, setup, f, teardown)
    }
  }
}

/// Format microseconds as a human-readable duration
fn format_duration_us(us: Float) -> String {
  case us <. 1000.0 {
    True -> float_to_string_precision(us, 2) <> "us"
    False ->
      case us <. 1_000_000.0 {
        True -> float_to_string_precision(us /. 1000.0, 2) <> "ms"
        False -> float_to_string_precision(us /. 1_000_000.0, 2) <> "s"
      }
  }
}

/// Convert float to string with limited precision
fn float_to_string_precision(f: Float, precision: Int) -> String {
  let multiplier = power_of_10(precision)
  let rounded = int.to_float(float.round(f *. multiplier)) /. multiplier
  let str = float.to_string(rounded)

  // Ensure we have the decimal point and precision
  case string.contains(str, ".") {
    True -> str
    False -> str <> ".0"
  }
}

fn power_of_10(n: Int) -> Float {
  case n {
    0 -> 1.0
    1 -> 10.0
    2 -> 100.0
    3 -> 1000.0
    _ -> 10.0 *. power_of_10(n - 1)
  }
}

/// Pad a string on the right to a given length
fn pad_right(s: String, len: Int) -> String {
  let current_len = string.length(s)
  case current_len >= len {
    True -> s
    False -> s <> string.repeat(" ", len - current_len)
  }
}

/// Pad a string on the left to a given length
fn pad_left(s: String, len: Int) -> String {
  let current_len = string.length(s)
  case current_len >= len {
    True -> s
    False -> string.repeat(" ", len - current_len) <> s
  }
}

// ============================================================================
// COMPARISON UTILITIES
// ============================================================================

/// Compare two benchmark results and return percentage change
pub fn compare_results(baseline: BenchResult, current: BenchResult) -> Float {
  { current.avg_us -. baseline.avg_us } /. baseline.avg_us *. 100.0
}

/// Check if a benchmark result is within acceptable threshold of baseline
pub fn within_threshold(
  baseline: BenchResult,
  current: BenchResult,
  threshold_percent: Float,
) -> Bool {
  let change = compare_results(baseline, current)
  change <. threshold_percent
}
