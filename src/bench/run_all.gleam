// Run All Benchmarks
//
// A quick benchmark runner that executes a subset of benchmarks
// from all benchmark suites with minimal iterations.

import bench/query_bench
import bench/schema_bench
import gleam/io

pub fn main() {
  io.println("")
  io.println("=======================================")
  io.println("cquill Benchmark Suite")
  io.println("=======================================")
  io.println("")

  // Run query benchmarks
  query_bench.main()

  io.println("")
  io.println("---------------------------------------")
  io.println("")

  // Run schema benchmarks
  schema_bench.main()

  io.println("")
  io.println("=======================================")
  io.println("All benchmarks completed!")
  io.println("=======================================")
}
