// Query Building Benchmarks
//
// Measures performance of query construction and composition operations.
// These are pure operations - no I/O involved.

import bench/bench
import cquill/query
import cquill/schema
import cquill/schema/field
import gleam/io
import gleam/list

// ============================================================================
// TEST FIXTURES
// ============================================================================

/// Create a simple user schema for benchmarking
fn user_schema() -> schema.Schema {
  schema.new("users")
  |> schema.field(field.integer("id") |> field.primary_key)
  |> schema.field(field.string("email") |> field.not_null)
  |> schema.field(field.string("name"))
  |> schema.field(field.boolean("active"))
  |> schema.field(field.integer("age"))
  |> schema.single_primary_key("id")
}

/// Create a more complex schema with many fields
fn complex_schema() -> schema.Schema {
  schema.new("products")
  |> schema.field(field.integer("id") |> field.primary_key)
  |> schema.field(field.string("sku") |> field.not_null |> field.unique)
  |> schema.field(field.string("name") |> field.not_null)
  |> schema.field(field.string("description"))
  |> schema.field(field.decimal("price", 10, 2))
  |> schema.field(field.integer("quantity"))
  |> schema.field(field.integer("category_id"))
  |> schema.field(field.integer("brand_id"))
  |> schema.field(field.boolean("active"))
  |> schema.field(field.boolean("featured"))
  |> schema.field(field.datetime("created_at"))
  |> schema.field(field.datetime("updated_at"))
  |> schema.single_primary_key("id")
}

// ============================================================================
// QUERY CONSTRUCTION BENCHMARKS
// ============================================================================

/// Benchmark simple query construction
fn bench_simple_query() -> Nil {
  let user_schema = user_schema()
  let _ = query.from(user_schema)
  Nil
}

/// Benchmark query with single where clause
fn bench_query_with_where() -> Nil {
  let user_schema = user_schema()
  let _ =
    query.from(user_schema)
    |> query.where(query.eq("active", True))
  Nil
}

/// Benchmark query with multiple where clauses
fn bench_query_with_multiple_where() -> Nil {
  let user_schema = user_schema()
  let _ =
    query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.where(query.gt("age", 18))
    |> query.where(query.not_eq("email", ""))
  Nil
}

/// Benchmark complex query with many operations
fn bench_complex_query() -> Nil {
  let product_schema = complex_schema()
  let _ =
    query.from(product_schema)
    |> query.select(["id", "sku", "name", "price"])
    |> query.where(query.eq("active", True))
    |> query.where(query.gt("quantity", 0))
    |> query.where(query.is_in("category_id", [1, 2, 3, 4, 5]))
    |> query.order_by_desc("created_at")
    |> query.limit(50)
    |> query.offset(0)
    |> query.distinct()
  Nil
}

/// Benchmark query with joins
fn bench_query_with_joins() -> Nil {
  let product_schema = complex_schema()
  let _ =
    query.from(product_schema)
    |> query.join(
      "categories",
      on: query.eq("products.category_id", "categories.id"),
    )
    |> query.left_join("brands", on: query.eq("products.brand_id", "brands.id"))
    |> query.select([
      "products.id",
      "products.name",
      "categories.name",
      "brands.name",
    ])
    |> query.where(query.eq("products.active", True))
  Nil
}

/// Benchmark query composition (building up a query step by step)
fn bench_query_composition() -> Nil {
  let user_schema = user_schema()

  // Start with base query
  let base = query.from(user_schema)

  // Add filters conditionally
  let with_active = base |> query.where(query.eq("active", True))
  let with_age = with_active |> query.where(query.gte("age", 21))

  // Add sorting
  let sorted = with_age |> query.order_by_asc("name")

  // Add pagination
  let _ = sorted |> query.paginate(page: 1, per_page: 20)
  Nil
}

/// Benchmark OR conditions
fn bench_or_conditions() -> Nil {
  let user_schema = user_schema()
  let _ =
    query.from(user_schema)
    |> query.where(
      query.or([
        query.eq("email", "admin@example.com"),
        query.eq("email", "super@example.com"),
        query.and([
          query.eq("active", True),
          query.gt("age", 30),
        ]),
      ]),
    )
  Nil
}

/// Benchmark BETWEEN condition
fn bench_between_condition() -> Nil {
  let user_schema = user_schema()
  let _ =
    query.from(user_schema)
    |> query.where(query.between("age", 18, 65))
    |> query.where(query.eq("active", True))
  Nil
}

/// Benchmark LIKE conditions
fn bench_like_conditions() -> Nil {
  let user_schema = user_schema()
  let _ =
    query.from(user_schema)
    |> query.where(query.like("email", "%@example.com"))
    |> query.where(query.not_like("name", "Test%"))
  Nil
}

/// Benchmark query from table name (no schema)
fn bench_query_from_table() -> Nil {
  let _ =
    query.from_table("users")
    |> query.select(["id", "name", "email"])
    |> query.where(query.eq("active", True))
    |> query.limit(10)
  Nil
}

// ============================================================================
// QUERY INSPECTION BENCHMARKS
// ============================================================================

/// Benchmark query inspection operations
fn bench_query_inspection() -> Nil {
  let user_schema = user_schema()
  let q =
    query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.where(query.gt("age", 18))
    |> query.order_by_asc("name")
    |> query.limit(10)

  let _ = query.get_conditions(q)
  let _ = query.get_order_bys(q)
  let _ = query.get_select(q)
  let _ = query.get_limit(q)
  let _ = query.has_conditions(q)
  let _ = query.has_pagination(q)
  let _ = query.condition_count(q)
  Nil
}

/// Benchmark query debug string generation
fn bench_query_debug_string() -> Nil {
  let user_schema = user_schema()
  let q =
    query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.where(query.gt("age", 18))
    |> query.order_by_asc("name")
    |> query.limit(10)

  let _ = query.to_debug_string(q)
  Nil
}

// ============================================================================
// BATCH QUERY CONSTRUCTION
// ============================================================================

/// Benchmark creating many queries
fn bench_batch_query_construction() -> Nil {
  let user_schema = user_schema()
  let _ =
    list.range(1, 100)
    |> list.map(fn(i) {
      query.from(user_schema)
      |> query.where(query.eq("id", i))
    })
  Nil
}

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================

pub fn main() {
  io.println("")
  io.println("cquill Query Building Benchmarks")
  io.println("=================================")
  io.println("")

  let config = bench.default_config()

  let suite =
    bench.run_suite(
      "Query Construction",
      [
        #("Simple query (from schema)", bench_simple_query),
        #("Query with single WHERE", bench_query_with_where),
        #("Query with multiple WHERE", bench_query_with_multiple_where),
        #("Complex query (full features)", bench_complex_query),
        #("Query with JOINs", bench_query_with_joins),
        #("Query composition pipeline", bench_query_composition),
        #("OR conditions", bench_or_conditions),
        #("BETWEEN condition", bench_between_condition),
        #("LIKE conditions", bench_like_conditions),
        #("Query from table name", bench_query_from_table),
      ],
      config,
    )

  let inspection_suite =
    bench.run_suite(
      "Query Inspection",
      [
        #("Query inspection ops", bench_query_inspection),
        #("Debug string generation", bench_query_debug_string),
      ],
      config,
    )

  let batch_suite =
    bench.run_suite(
      "Batch Operations",
      [
        #("Create 100 queries", bench_batch_query_construction),
      ],
      config,
    )

  // Print summary
  bench.print_suite_summary(suite)
  bench.print_suite_summary(inspection_suite)
  bench.print_suite_summary(batch_suite)

  // Print markdown output for docs
  io.println("")
  io.println("Markdown Output:")
  io.println("-" |> string.repeat(60))
  io.println(bench.to_markdown(suite))
  io.println(bench.to_markdown(inspection_suite))
  io.println(bench.to_markdown(batch_suite))
}

// Helper for string repeat
import gleam/string
