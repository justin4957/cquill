// Tests to verify README code examples compile and work correctly
// These mirror the Quick Start examples in the README

import cquill/adapter
import cquill/adapter/memory
import cquill/changeset
import cquill/query
import cquill/schema
import cquill/schema/field
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// README: Defining Schemas
// ============================================================================

pub fn readme_schema_example_test() {
  // Define your schema - this describes the table structure
  let user_schema =
    schema.new("users")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("email") |> field.not_null)
    |> schema.add_field(field.string("name") |> field.nullable)
    |> schema.add_field(field.boolean("active") |> field.not_null)
    |> schema.add_field(field.integer("age") |> field.nullable)

  // Verify schema was created correctly
  schema.get_source(user_schema) |> should.equal("users")
  schema.has_primary_key(user_schema) |> should.be_true
  schema.field_count(user_schema) |> should.equal(5)
}

// ============================================================================
// README: Building Queries
// ============================================================================

pub fn readme_query_example_test() {
  let user_schema =
    schema.new("users")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("email") |> field.not_null)
    |> schema.add_field(field.boolean("active") |> field.not_null)

  // Build queries using composable pipelines
  let active_users =
    query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.order_by_desc("created_at")
    |> query.limit(10)

  // Queries are just data - inspect them for debugging
  let debug_str = query.to_debug_string(active_users)

  // Verify query was built correctly
  debug_str |> string.contains("users") |> should.be_true
  debug_str |> string.contains("limit: 10") |> should.be_true
}

// ============================================================================
// README: Executing Queries (Memory Adapter)
// ============================================================================

pub fn readme_memory_adapter_example_test() {
  // Create an in-memory store with a table
  let store =
    memory.new_store()
    |> memory.create_table("users", "id")

  // Insert data
  let row = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
    dynamic.bool(True),
    dynamic.int(30),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Query using the adapter
  let adp = memory.memory_adapter()
  let compiled =
    adapter.CompiledQuery(
      sql: "SELECT * FROM users WHERE active = $1",
      params: [adapter.ParamBool(True)],
      expected_columns: 5,
    )

  case adapter.query(adp, store, compiled) {
    Ok(rows) -> list.length(rows) |> should.equal(1)
    Error(_) -> should.fail()
  }
}

// ============================================================================
// README: Validating Data with Changesets
// ============================================================================

pub fn readme_changeset_example_test() {
  let data =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))
    |> dict.insert("name", dynamic.string("John Doe"))
    |> dict.insert("age", dynamic.int(30))

  let result =
    changeset.new(data)
    |> changeset.validate_required(["email", "name"])
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")
    |> changeset.validate_length("name", min: 2, max: 100)
    |> changeset.validate_number_range("age", min: Some(0), max: Some(150))
    |> changeset.apply()

  case result {
    Ok(_valid_data) -> should.be_true(True)
    Error(_errors) -> should.fail()
  }
}

pub fn readme_changeset_with_errors_test() {
  let data =
    dict.new()
    |> dict.insert("email", dynamic.string("invalid-email"))
    |> dict.insert("name", dynamic.string("J"))

  let result =
    changeset.new(data)
    |> changeset.validate_required(["email", "name"])
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")
    |> changeset.validate_length("name", min: 2, max: 100)
    |> changeset.apply()

  case result {
    Ok(_) -> should.fail()
    Error(errors) -> {
      dict.has_key(errors, "email") |> should.be_true
      dict.has_key(errors, "name") |> should.be_true
    }
  }
}

// Helper import
import gleam/string
