// Schema Operation Benchmarks
//
// Measures performance of schema construction, field lookup,
// validation, and transformation operations.

import bench/bench
import cquill/schema
import cquill/schema/field
import gleam/io
import gleam/string

// ============================================================================
// SCHEMA CONSTRUCTION BENCHMARKS
// ============================================================================

/// Benchmark simple schema construction
fn bench_simple_schema() -> Nil {
  let _ =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name"))
    |> schema.single_primary_key("id")
  Nil
}

/// Benchmark schema with many fields
fn bench_large_schema() -> Nil {
  let _ =
    schema.new("large_table")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.string("field_1"))
    |> schema.field(field.string("field_2"))
    |> schema.field(field.string("field_3"))
    |> schema.field(field.string("field_4"))
    |> schema.field(field.string("field_5"))
    |> schema.field(field.integer("field_6"))
    |> schema.field(field.integer("field_7"))
    |> schema.field(field.integer("field_8"))
    |> schema.field(field.integer("field_9"))
    |> schema.field(field.integer("field_10"))
    |> schema.field(field.boolean("field_11"))
    |> schema.field(field.boolean("field_12"))
    |> schema.field(field.string("field_13"))
    |> schema.field(field.string("field_14"))
    |> schema.field(field.decimal("field_15", 10, 2))
    |> schema.field(field.datetime("field_16"))
    |> schema.field(field.datetime("field_17"))
    |> schema.field(field.date("field_18"))
    |> schema.field(field.time("field_19"))
    |> schema.field(field.uuid("field_20"))
    |> schema.single_primary_key("id")
  Nil
}

/// Benchmark schema with constraints
fn bench_schema_with_constraints() -> Nil {
  let _ =
    schema.new("products")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.string("sku") |> field.not_null |> field.unique)
    |> schema.field(field.string("name") |> field.not_null)
    |> schema.field(field.decimal("price", 10, 2) |> field.not_null)
    |> schema.field(
      field.integer("category_id")
      |> field.not_null
      |> field.references("categories", "id"),
    )
    |> schema.field(field.integer("quantity"))
    |> schema.field(field.boolean("active"))
    |> schema.single_primary_key("id")
    |> schema.unique_constraint("products_sku_unique", ["sku"])
    |> schema.index("products_category_idx", ["category_id"], False)
  Nil
}

/// Benchmark schema with composite primary key
fn bench_composite_primary_key() -> Nil {
  let _ =
    schema.new("order_items")
    |> schema.field(field.integer("order_id") |> field.primary_key)
    |> schema.field(field.integer("product_id") |> field.primary_key)
    |> schema.field(field.integer("quantity") |> field.not_null)
    |> schema.field(field.decimal("unit_price", 10, 2) |> field.not_null)
    |> schema.primary_key(["order_id", "product_id"])
  Nil
}

// ============================================================================
// FIELD LOOKUP BENCHMARKS
// ============================================================================

/// Create a test schema for lookup benchmarks
fn test_schema() -> schema.Schema {
  schema.new("test_table")
  |> schema.field(field.integer("id") |> field.primary_key)
  |> schema.field(field.string("email") |> field.not_null |> field.unique)
  |> schema.field(field.string("name"))
  |> schema.field(field.boolean("active"))
  |> schema.field(field.integer("age"))
  |> schema.field(field.string("bio"))
  |> schema.field(field.datetime("created_at"))
  |> schema.field(field.datetime("updated_at"))
  |> schema.field(field.integer("department_id"))
  |> schema.field(field.integer("manager_id"))
  |> schema.single_primary_key("id")
}

/// Benchmark get field by name (first field)
fn bench_get_field_first() -> Nil {
  let s = test_schema()
  let _ = schema.get_field(s, "id")
  Nil
}

/// Benchmark get field by name (middle field)
fn bench_get_field_middle() -> Nil {
  let s = test_schema()
  let _ = schema.get_field(s, "active")
  Nil
}

/// Benchmark get field by name (last field)
fn bench_get_field_last() -> Nil {
  let s = test_schema()
  let _ = schema.get_field(s, "manager_id")
  Nil
}

/// Benchmark has_field check
fn bench_has_field() -> Nil {
  let s = test_schema()
  let _ = schema.has_field(s, "email")
  let _ = schema.has_field(s, "nonexistent")
  Nil
}

/// Benchmark get all field names
fn bench_get_field_names() -> Nil {
  let s = test_schema()
  let _ = schema.field_names(s)
  Nil
}

/// Benchmark get all fields
fn bench_get_all_fields() -> Nil {
  let s = test_schema()
  let _ = schema.get_fields(s)
  Nil
}

/// Benchmark field count
fn bench_field_count() -> Nil {
  let s = test_schema()
  let _ = schema.field_count(s)
  Nil
}

// ============================================================================
// SCHEMA TRANSFORMATION BENCHMARKS
// ============================================================================

/// Benchmark select_fields transformation
fn bench_select_fields() -> Nil {
  let s = test_schema()
  let _ = schema.select_fields(s, ["id", "email", "name"])
  Nil
}

/// Benchmark exclude_fields transformation
fn bench_exclude_fields() -> Nil {
  let s = test_schema()
  let _ = schema.exclude_fields(s, ["created_at", "updated_at"])
  Nil
}

/// Benchmark insertable_fields transformation
fn bench_insertable_fields() -> Nil {
  let s =
    schema.new("users")
    |> schema.field(
      field.integer("id") |> field.primary_key |> field.auto_increment,
    )
    |> schema.field(field.string("email") |> field.not_null)
    |> schema.field(field.string("name"))
    |> schema.field(field.datetime("created_at") |> field.auto_increment)
    |> schema.single_primary_key("id")

  let _ = schema.insertable_fields(s)
  Nil
}

/// Benchmark rename_field transformation
fn bench_rename_field() -> Nil {
  let s = test_schema()
  let _ = schema.rename_field(s, "email", "email_address")
  Nil
}

// ============================================================================
// SCHEMA VALIDATION BENCHMARKS
// ============================================================================

/// Benchmark validation of valid schema
fn bench_validate_valid() -> Nil {
  let s = test_schema()
  let _ = schema.validate(s)
  Nil
}

/// Benchmark validation of invalid schema
fn bench_validate_invalid() -> Nil {
  let s =
    schema.new("invalid")
    |> schema.field(field.string("name"))
    |> schema.field(field.string("name"))
    // Duplicate field
    |> schema.primary_key(["nonexistent"])
  // Invalid PK

  let _ = schema.validate(s)
  Nil
}

/// Benchmark is_valid check
fn bench_is_valid() -> Nil {
  let s = test_schema()
  let _ = schema.is_valid(s)
  Nil
}

// ============================================================================
// SCHEMA COMPARISON BENCHMARKS
// ============================================================================

/// Benchmark schema diff
fn bench_schema_diff() -> Nil {
  let old_schema =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name"))
    |> schema.single_primary_key("id")

  let new_schema =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name"))
    // Changed type
    |> schema.field(field.boolean("active"))
    // Added field
    |> schema.single_primary_key("id")

  let _ = schema.diff(old_schema, new_schema)
  Nil
}

// ============================================================================
// FIELD FILTERING BENCHMARKS
// ============================================================================

/// Benchmark getting primary key fields
fn bench_primary_key_fields() -> Nil {
  let s = test_schema()
  let _ = schema.primary_key_fields(s)
  Nil
}

/// Benchmark getting non-primary key fields
fn bench_non_primary_key_fields() -> Nil {
  let s = test_schema()
  let _ = schema.non_primary_key_fields(s)
  Nil
}

/// Benchmark getting nullable fields
fn bench_nullable_fields() -> Nil {
  let s = test_schema()
  let _ = schema.nullable_fields(s)
  Nil
}

/// Benchmark getting required fields
fn bench_required_fields() -> Nil {
  let s = test_schema()
  let _ = schema.required_fields(s)
  Nil
}

/// Benchmark getting foreign key fields
fn bench_foreign_key_fields() -> Nil {
  let s =
    schema.new("orders")
    |> schema.field(field.integer("id") |> field.primary_key)
    |> schema.field(field.integer("user_id") |> field.references("users", "id"))
    |> schema.field(
      field.integer("product_id") |> field.references("products", "id"),
    )
    |> schema.field(field.integer("quantity"))
    |> schema.single_primary_key("id")

  let _ = schema.foreign_key_fields(s)
  Nil
}

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================

pub fn main() {
  io.println("")
  io.println("cquill Schema Operation Benchmarks")
  io.println("===================================")
  io.println("")

  let config = bench.default_config()

  let construction_suite =
    bench.run_suite(
      "Schema Construction",
      [
        #("Simple schema (3 fields)", bench_simple_schema),
        #("Large schema (20 fields)", bench_large_schema),
        #("Schema with constraints", bench_schema_with_constraints),
        #("Composite primary key", bench_composite_primary_key),
      ],
      config,
    )

  let lookup_suite =
    bench.run_suite(
      "Field Lookup",
      [
        #("Get field (first)", bench_get_field_first),
        #("Get field (middle)", bench_get_field_middle),
        #("Get field (last)", bench_get_field_last),
        #("Has field check", bench_has_field),
        #("Get all field names", bench_get_field_names),
        #("Get all fields", bench_get_all_fields),
        #("Field count", bench_field_count),
      ],
      config,
    )

  let transform_suite =
    bench.run_suite(
      "Schema Transformations",
      [
        #("Select fields", bench_select_fields),
        #("Exclude fields", bench_exclude_fields),
        #("Insertable fields", bench_insertable_fields),
        #("Rename field", bench_rename_field),
      ],
      config,
    )

  let validation_suite =
    bench.run_suite(
      "Schema Validation",
      [
        #("Validate (valid schema)", bench_validate_valid),
        #("Validate (invalid schema)", bench_validate_invalid),
        #("Is valid check", bench_is_valid),
      ],
      config,
    )

  let comparison_suite =
    bench.run_suite(
      "Schema Comparison",
      [
        #("Schema diff", bench_schema_diff),
      ],
      config,
    )

  let filtering_suite =
    bench.run_suite(
      "Field Filtering",
      [
        #("Primary key fields", bench_primary_key_fields),
        #("Non-primary key fields", bench_non_primary_key_fields),
        #("Nullable fields", bench_nullable_fields),
        #("Required fields", bench_required_fields),
        #("Foreign key fields", bench_foreign_key_fields),
      ],
      config,
    )

  // Print summaries
  bench.print_suite_summary(construction_suite)
  bench.print_suite_summary(lookup_suite)
  bench.print_suite_summary(transform_suite)
  bench.print_suite_summary(validation_suite)
  bench.print_suite_summary(comparison_suite)
  bench.print_suite_summary(filtering_suite)

  // Print markdown
  io.println("")
  io.println("Markdown Output:")
  io.println("-" |> string.repeat(60))
  io.println(bench.to_markdown(construction_suite))
  io.println(bench.to_markdown(lookup_suite))
  io.println(bench.to_markdown(transform_suite))
  io.println(bench.to_markdown(validation_suite))
  io.println(bench.to_markdown(filtering_suite))
}
