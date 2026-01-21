# Performance Benchmarks

This document describes cquill's performance benchmarking framework and provides baseline measurements for key operations.

## Running Benchmarks

cquill includes a comprehensive benchmarking suite in `src/bench/`. Run individual benchmarks with:

```bash
# Query building benchmarks
gleam run -m bench/query_bench

# Schema operation benchmarks
gleam run -m bench/schema_bench

# Memory adapter benchmarks (slower - includes I/O operations)
gleam run -m bench/memory_adapter_bench
```

## Benchmark Framework

The benchmark utilities are in `src/bench/bench.gleam` and provide:

- **Configurable iterations**: Warmup and measurement phases
- **Multiple configurations**: `quick_config()`, `default_config()`, `thorough_config()`
- **Detailed measurements**: Average, min/max timing (optional)
- **Suite organization**: Group related benchmarks
- **Markdown output**: Generate documentation-ready tables

### Configuration Options

| Config | Warmup | Iterations | Use Case |
|--------|--------|------------|----------|
| `quick_config()` | 5 | 100 | Fast sanity checks |
| `default_config()` | 10 | 1,000 | Normal benchmarking |
| `thorough_config()` | 100 | 10,000 | Detailed analysis |

### Example Usage

```gleam
import bench/bench

pub fn main() {
  let config = bench.default_config()

  let suite = bench.run_suite("My Benchmarks", [
    #("Operation A", fn() { do_operation_a() }),
    #("Operation B", fn() { do_operation_b() }),
  ], config)

  bench.print_suite_summary(suite)
}
```

## Baseline Results

The following results were captured on an Apple Silicon Mac. Your results may vary based on hardware.

### Query Construction

| Benchmark | Avg/Op | Description |
|-----------|--------|-------------|
| Simple query (from schema) | ~0.4µs | Basic `from(schema)` |
| Query with single WHERE | ~0.1µs | Adding one condition |
| Query with multiple WHERE | ~0.3µs | Adding three conditions |
| Complex query (full features) | ~0.4µs | SELECT, WHERE, ORDER BY, LIMIT, DISTINCT |
| Query with JOINs | ~0.4µs | Multiple JOIN clauses |
| Query composition pipeline | ~0.2µs | Building query incrementally |
| Query from table name | ~0.05µs | `from_table("users")` |

**Key insight**: Query construction is extremely fast (sub-microsecond). The query DSL builds an AST without any string operations, making composition essentially free.

### Schema Operations

| Benchmark | Avg/Op | Description |
|-----------|--------|-------------|
| Simple schema (3 fields) | ~0.2µs | Basic schema creation |
| Large schema (20 fields) | ~1.0µs | Schema with many fields |
| Schema with constraints | ~0.3µs | Including FK, unique, index |
| Get field (by name) | ~0.3µs | Field lookup |
| Has field check | ~0.2µs | Existence check |
| Field count | ~0.2µs | Count fields |
| Select fields | ~0.4µs | Transform to subset |
| Schema validation | ~0.5µs | Full validation |
| Schema diff | ~0.3µs | Compare two schemas |

**Key insight**: Schema operations scale linearly with field count. Even large schemas (20+ fields) remain sub-microsecond for most operations.

### Memory Adapter Operations

| Benchmark | Avg/Op | Description |
|-----------|--------|-------------|
| Single insert | ~5-10µs | Insert one row |
| Batch insert (10 rows) | ~50µs | Insert with batching |
| Batch insert (100 rows) | ~500µs | Larger batch |
| Get by key | ~1µs | Point lookup |
| Get all (100 rows) | ~10µs | Full table scan |
| Get all (1000 rows) | ~100µs | Larger scan |
| Single update | ~5µs | Update one row |
| Single delete | ~5µs | Delete one row |
| Transaction + commit | ~10µs | Full transaction cycle |

**Key insight**: The memory adapter is designed for testing, not production performance. It provides predictable semantics for verifying business logic.

## Performance Guidelines

### Query Building

1. **Compose freely**: Query composition has negligible overhead
2. **Reuse schemas**: Schema creation is fast but still faster to reuse
3. **Use `from_table`**: When you don't need schema validation, `from_table` is faster

### Memory Adapter

1. **Use for testing**: The memory adapter prioritizes correctness over speed
2. **Batch operations**: Use `insert_all` for multiple rows
3. **Limit dataset size**: Keep test datasets small (< 10,000 rows)

### PostgreSQL Adapter

1. **Connection pooling**: Use appropriate pool sizes for your workload
2. **Batch inserts**: Use `insert_all` with appropriate batch sizes (100-1000)
3. **Prepared statements**: Queries are parameterized for security and performance

## Regression Testing

To detect performance regressions:

1. Run benchmarks before changes: `gleam run -m bench/query_bench > baseline.txt`
2. Make your changes
3. Run benchmarks after: `gleam run -m bench/query_bench > current.txt`
4. Compare results

The benchmark framework includes comparison utilities:

```gleam
import bench/bench

// Compare two results
let change = bench.compare_results(baseline, current)  // Returns % change

// Check if within threshold
let ok = bench.within_threshold(baseline, current, 10.0)  // 10% threshold
```

## Adding New Benchmarks

1. Create a new file in `src/bench/` or add to existing benchmark file
2. Define benchmark functions that return `Nil`
3. Add to a suite with descriptive names
4. Document expected performance characteristics

Example:

```gleam
fn bench_my_operation() -> Nil {
  let _ = my_module.expensive_operation()
  Nil
}

pub fn main() {
  let suite = bench.run_suite("My Module", [
    #("Expensive operation", bench_my_operation),
  ], bench.default_config())

  bench.print_suite_summary(suite)
}
```

## Hardware Considerations

Benchmark results vary significantly based on:

- **CPU**: Clock speed, cache size, architecture
- **Memory**: Speed, latency
- **OS**: Scheduler, background processes
- **BEAM VM**: Erlang/OTP version, schedulers

For consistent results:

1. Close unnecessary applications
2. Run multiple times and average
3. Use `thorough_config()` for final measurements
4. Document your hardware when sharing results
