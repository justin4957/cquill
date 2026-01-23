// Property-Based Tests for Schema
//
// This test suite verifies schema-related properties across
// randomly generated schemas and field configurations.

import cquill/query
import cquill/query/ast.{Eq, IntValue, StringValue}
import cquill/schema
import cquill/schema/field
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import property/generators.{
  type Seed, random_element, random_int, random_list, seed,
}

// ============================================================================
// TEST CONFIGURATION
// ============================================================================

const test_iterations = 500

/// Run a property test
fn run_property(
  name: String,
  iterations: Int,
  property: fn(Seed) -> #(Bool, Seed),
) -> Nil {
  let result = do_run_property(seed(123), iterations, 0, property)
  case result {
    Ok(_) -> Nil
    Error(#(iteration, _seed)) ->
      panic as {
        "Property '"
        <> name
        <> "' failed at iteration "
        <> int.to_string(iteration)
      }
  }
}

fn do_run_property(
  s: Seed,
  remaining: Int,
  current: Int,
  property: fn(Seed) -> #(Bool, Seed),
) -> Result(Nil, #(Int, Seed)) {
  case remaining <= 0 {
    True -> Ok(Nil)
    False -> {
      let #(passed, s2) = property(s)
      case passed {
        True -> do_run_property(s2, remaining - 1, current + 1, property)
        False -> Error(#(current, s))
      }
    }
  }
}

// ============================================================================
// FIELD NAME GENERATORS
// ============================================================================

const field_names = [
  "id", "name", "email", "age", "active", "created_at", "updated_at", "status",
  "user_id", "price", "quantity", "description", "title", "score", "count",
]

const table_names = [
  "users", "posts", "comments", "orders", "products", "sessions", "events",
  "categories", "tags", "items", "accounts", "profiles", "logs", "messages",
]

fn random_field_name(s: Seed) -> #(String, Seed) {
  random_element(s, field_names)
}

fn random_table_name(s: Seed) -> #(String, Seed) {
  random_element(s, table_names)
}

// ============================================================================
// SCHEMA PROPERTIES
// ============================================================================

/// Property: Schema source matches what was set
pub fn property_schema_source_matches_test() {
  run_property("schema_source_matches", test_iterations, fn(s) {
    let #(table, s1) = random_table_name(s)

    let sch = schema.new(table)

    let source = schema.get_source(sch)
    #(source == table, s1)
  })
}

/// Property: Adding fields preserves field order
pub fn property_fields_preserve_order_test() {
  run_property("fields_preserve_order", test_iterations, fn(s) {
    let #(count, s1) = random_int(s, 5)
    let #(names, s2) = random_list(s1, count + 1, random_field_name)

    // Create schema and add fields
    let sch =
      list.fold(names, schema.new("test"), fn(acc, name) {
        schema.add_field(acc, field.string(name))
      })

    let field_names =
      schema.get_fields(sch)
      |> list.map(field.get_name)

    // Field names should match in order
    #(field_names == names, s2)
  })
}

/// Property: Primary key is retrievable after setting
pub fn property_primary_key_retrievable_test() {
  run_property("primary_key_retrievable", test_iterations, fn(s) {
    let #(pk_field, s1) = random_field_name(s)

    let sch =
      schema.new("test")
      |> schema.single_primary_key(pk_field)
      |> schema.add_field(field.integer(pk_field) |> field.primary_key)

    let pk = schema.get_primary_key(sch)
    #(pk == [pk_field], s1)
  })
}

/// Property: Schema with schema qualifier includes it in qualified name
pub fn property_qualified_name_includes_schema_test() {
  run_property("qualified_name_includes_schema", test_iterations, fn(s) {
    let #(table, s1) = random_table_name(s)
    let #(schema_name, s2) =
      random_element(s1, ["public", "app", "data", "test"])

    let sch =
      schema.new(table)
      |> schema.in_schema(schema_name)

    let qualified = schema.get_qualified_name(sch)
    let expected = schema_name <> "." <> table
    #(qualified == expected, s2)
  })
}

/// Property: Field metadata is preserved
pub fn property_field_metadata_preserved_test() {
  run_property("field_metadata_preserved", test_iterations, fn(s) {
    let #(name, s1) = random_field_name(s)
    let #(is_nullable, s2) = generators.random_bool(s1)
    let #(is_unique, s3) = generators.random_bool(s2)

    let f = field.string(name)
    let f = case is_nullable {
      True -> field.nullable(f)
      False -> field.not_null(f)
    }
    let f = case is_unique {
      True -> field.unique(f)
      False -> f
    }

    let sch = schema.new("test") |> schema.add_field(f)
    let fields = schema.get_fields(sch)

    case fields {
      [retrieved] -> {
        let name_matches = field.get_name(retrieved) == name
        let nullable_matches = field.is_nullable(retrieved) == is_nullable
        #(name_matches && nullable_matches, s3)
      }
      _ -> #(False, s3)
    }
  })
}

// ============================================================================
// QUERY FROM SCHEMA PROPERTIES
// ============================================================================

/// Property: Query from schema uses correct source
pub fn property_query_from_schema_source_test() {
  run_property("query_from_schema_source", test_iterations, fn(s) {
    let #(table, s1) = random_table_name(s)

    let sch =
      schema.new(table)
      |> schema.add_field(field.integer("id") |> field.primary_key)

    let q = query.from(sch)

    let source = query.get_source(q)
    let is_correct = case source {
      ast.TableSource(t, None) -> t == table
      ast.TableSource(_, Some(_)) -> True
      _ -> False
    }
    #(is_correct, s1)
  })
}

/// Property: Query from qualified schema includes schema name
pub fn property_query_from_qualified_schema_test() {
  run_property("query_from_qualified_schema", test_iterations, fn(s) {
    let #(table, s1) = random_table_name(s)
    let #(schema_name, s2) = random_element(s1, ["public", "app", "data"])

    let sch =
      schema.new(table)
      |> schema.in_schema(schema_name)
      |> schema.add_field(field.integer("id") |> field.primary_key)

    let q = query.from(sch)

    let source = query.get_source(q)
    let is_correct = case source {
      ast.TableSource(t, Some(s_name)) -> t == table && s_name == schema_name
      _ -> False
    }
    #(is_correct, s2)
  })
}

// ============================================================================
// FIELD TYPE PROPERTIES
// ============================================================================

/// Property: Integer fields can have integer conditions
pub fn property_integer_field_integer_condition_test() {
  run_property("integer_field_integer_condition", test_iterations, fn(s) {
    let #(name, s1) = random_field_name(s)
    let #(value, s2) = random_int(s1, 1000)

    let sch =
      schema.new("test")
      |> schema.add_field(field.integer(name))

    let q =
      query.from(sch)
      |> query.where(Eq(name, IntValue(value)))

    let conditions = query.get_conditions(q)
    #(list.length(conditions) == 1, s2)
  })
}

/// Property: String fields can have string conditions
pub fn property_string_field_string_condition_test() {
  run_property("string_field_string_condition", test_iterations, fn(s) {
    let #(name, s1) = random_field_name(s)
    let #(value, s2) = random_element(s1, ["test", "hello", "world", ""])

    let sch =
      schema.new("test")
      |> schema.add_field(field.string(name))

    let q =
      query.from(sch)
      |> query.where(Eq(name, StringValue(value)))

    let conditions = query.get_conditions(q)
    #(list.length(conditions) == 1, s2)
  })
}

// ============================================================================
// EDGE CASE UNIT TESTS
// ============================================================================

/// Test: Schema with no fields
pub fn edge_case_schema_no_fields_test() {
  let sch = schema.new("empty_table")

  let fields = schema.get_fields(sch)
  list.length(fields) |> should.equal(0)
}

/// Test: Schema with many fields
pub fn edge_case_schema_many_fields_test() {
  let names = [
    "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12",
    "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20",
  ]

  let sch =
    list.fold(names, schema.new("big_table"), fn(acc, name) {
      schema.add_field(acc, field.string(name))
    })

  let fields = schema.get_fields(sch)
  list.length(fields) |> should.equal(20)
}

/// Test: Schema with composite primary key
pub fn edge_case_composite_primary_key_test() {
  let sch =
    schema.new("join_table")
    |> schema.primary_key(["user_id", "role_id"])
    |> schema.add_field(field.integer("user_id") |> field.primary_key)
    |> schema.add_field(field.integer("role_id") |> field.primary_key)

  let pk = schema.get_primary_key(sch)
  list.length(pk) |> should.equal(2)
}

/// Test: Schema field type variety
pub fn edge_case_field_type_variety_test() {
  let sch =
    schema.new("typed_table")
    |> schema.add_field(field.integer("int_field"))
    |> schema.add_field(field.string("string_field"))
    |> schema.add_field(field.boolean("bool_field"))
    |> schema.add_field(field.float("float_field"))

  let fields = schema.get_fields(sch)
  list.length(fields) |> should.equal(4)

  // Verify types are preserved
  let field_types =
    list.map(fields, fn(f) {
      case field.get_type(f) {
        field.Integer -> "int"
        field.String -> "string"
        field.Boolean -> "bool"
        field.Float -> "float"
        _ -> "other"
      }
    })

  field_types |> should.equal(["int", "string", "bool", "float"])
}

/// Test: Field constraints combination
pub fn edge_case_field_constraints_combination_test() {
  let f =
    field.string("email")
    |> field.not_null
    |> field.unique

  let sch = schema.new("users") |> schema.add_field(f)

  case schema.get_fields(sch) {
    [email_field] -> {
      field.is_nullable(email_field) |> should.be_false
      field.has_constraint(email_field, field.is_unique_constraint)
      |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test: Query with conditions from schema fields
pub fn edge_case_query_from_schema_with_conditions_test() {
  let sch =
    schema.new("users")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("name") |> field.not_null)
    |> schema.add_field(field.boolean("active"))

  let q =
    query.from(sch)
    |> query.where(Eq("active", ast.BoolValue(True)))
    |> query.where(ast.IsNotNull("name"))
    |> query.order_by_asc("name")
    |> query.limit(10)

  query.get_conditions(q) |> list.length |> should.equal(2)
  query.get_order_bys(q) |> list.length |> should.equal(1)
  query.get_limit(q) |> should.equal(Some(10))
}
