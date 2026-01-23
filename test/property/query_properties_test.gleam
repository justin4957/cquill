// Property-Based Tests for Query Builder
//
// This test suite uses property-based testing to verify query builder
// invariants across thousands of randomly generated queries.
//
// Properties tested:
// - Query composition is consistent
// - Idempotency of certain operations
// - Any generated query produces valid AST
// - Value handling across types and edge cases

import cquill/query
import cquill/query/ast.{
  type Condition, And, Eq, Gt, IntValue, IsNotNull, IsNull, Or, SelectAll,
  SelectFields, TableSource,
}
import cquill/query/builder
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import property/generators.{
  type Seed, complex_config, minimal_config, random_condition, random_query,
  random_simple_condition, seed, simple_config,
}

// ============================================================================
// TEST CONFIGURATION
// ============================================================================

/// Number of iterations for each property test
const test_iterations = 1000

/// Run a property test with the given number of iterations
fn run_property(
  name: String,
  iterations: Int,
  property: fn(Seed) -> #(Bool, Seed),
) -> Nil {
  let result = do_run_property(seed(42), iterations, 0, property)
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
// QUERY COMPOSITION PROPERTIES
// ============================================================================

/// Property: Multiple where clauses accumulate correctly
pub fn property_where_clauses_accumulate_test() {
  run_property("where_clauses_accumulate", test_iterations, fn(s) {
    let #(cond1, s1) = random_simple_condition(s)
    let #(cond2, s2) = random_simple_condition(s1)

    let query1 =
      query.from_table("users")
      |> query.where(cond1)
      |> query.where(cond2)

    let conditions = query.get_conditions(query1)
    let has_both = list.length(conditions) == 2
    #(has_both, s2)
  })
}

/// Property: Adding conditions preserves existing conditions
pub fn property_conditions_preserved_test() {
  run_property("conditions_preserved", test_iterations, fn(s) {
    let #(cond1, s1) = random_simple_condition(s)
    let #(cond2, s2) = random_simple_condition(s1)
    let #(cond3, s3) = random_simple_condition(s2)

    let query1 =
      query.from_table("users")
      |> query.where(cond1)
      |> query.where(cond2)

    let query2 = query.where(query1, cond3)

    let conditions = query.get_conditions(query2)
    // All three conditions should be present
    let has_all = list.length(conditions) == 3
    #(has_all, s3)
  })
}

/// Property: Empty where conditions produce valid query
pub fn property_empty_where_valid_test() {
  run_property("empty_where_valid", test_iterations, fn(s) {
    let #(table, s1) = generators.random_table_name(s)

    let q = query.from_table(table)

    // Query should have no conditions
    let conditions = query.get_conditions(q)
    let is_empty = list.is_empty(conditions)
    #(is_empty, s1)
  })
}

/// Property: Order of independent operations doesn't affect final structure
/// (where, limit, offset, distinct are independent)
pub fn property_independent_operations_order_test() {
  run_property("independent_operations_order", test_iterations, fn(s) {
    let #(cond, s1) = random_simple_condition(s)
    let #(limit_val, s2) = generators.random_int(s1, 100)
    let #(offset_val, s3) = generators.random_int(s2, 50)

    // Order 1: where -> limit -> offset -> distinct
    let query1 =
      query.from_table("users")
      |> query.where(cond)
      |> query.limit(limit_val)
      |> query.offset(offset_val)
      |> query.distinct

    // Order 2: distinct -> offset -> limit -> where
    let query2 =
      query.from_table("users")
      |> query.distinct
      |> query.offset(offset_val)
      |> query.limit(limit_val)
      |> query.where(cond)

    // Both should have same structural properties
    let same_limit = query.get_limit(query1) == query.get_limit(query2)
    let same_offset = query.get_offset(query1) == query.get_offset(query2)
    let same_distinct = query.is_distinct(query1) == query.is_distinct(query2)
    let same_condition_count =
      query.condition_count(query1) == query.condition_count(query2)

    #(same_limit && same_offset && same_distinct && same_condition_count, s3)
  })
}

/// Property: Composed modifiers produce same result as sequential application
pub fn property_modifier_composition_test() {
  run_property("modifier_composition", test_iterations, fn(s) {
    let #(cond1, s1) = random_simple_condition(s)
    let #(cond2, s2) = random_simple_condition(s1)
    let #(limit_val, s3) = generators.random_int(s2, 100)

    // Using individual modifiers
    let query1 =
      query.from_table("users")
      |> builder.filter(cond1)
      |> builder.filter(cond2)
      |> builder.with_limit(limit_val)

    // Using composed modifiers
    let combined =
      builder.compose([
        builder.filter(cond1),
        builder.filter(cond2),
        builder.with_limit(limit_val),
      ])
    let query2 = query.from_table("users") |> combined

    // Results should be equivalent
    let same_limit = query.get_limit(query1) == query.get_limit(query2)
    let same_conditions =
      query.condition_count(query1) == query.condition_count(query2)

    #(same_limit && same_conditions, s3)
  })
}

// ============================================================================
// IDEMPOTENCY PROPERTIES
// ============================================================================

/// Property: Applying same limit twice uses last value
pub fn property_limit_last_value_test() {
  run_property("limit_last_value", test_iterations, fn(s) {
    let #(limit1, s1) = generators.random_int(s, 100)
    let #(limit2, s2) = generators.random_int(s1, 100)

    let q =
      query.from_table("users")
      |> query.limit(limit1)
      |> query.limit(limit2)

    // Last limit should win
    let final_limit = query.get_limit(q)
    let is_last = final_limit == Some(limit2)
    #(is_last, s2)
  })
}

/// Property: Applying same offset twice uses last value
pub fn property_offset_last_value_test() {
  run_property("offset_last_value", test_iterations, fn(s) {
    let #(offset1, s1) = generators.random_int(s, 100)
    let #(offset2, s2) = generators.random_int(s1, 100)

    let q =
      query.from_table("users")
      |> query.offset(offset1)
      |> query.offset(offset2)

    // Last offset should win
    let final_offset = query.get_offset(q)
    let is_last = final_offset == Some(offset2)
    #(is_last, s2)
  })
}

/// Property: Clearing conditions then adding new ones works correctly
pub fn property_clear_then_add_test() {
  run_property("clear_then_add", test_iterations, fn(s) {
    let #(cond1, s1) = random_simple_condition(s)
    let #(cond2, s2) = random_simple_condition(s1)
    let #(cond3, s3) = random_simple_condition(s2)

    let q =
      query.from_table("users")
      |> query.where(cond1)
      |> query.where(cond2)
      |> query.where_clear
      |> query.where(cond3)

    // Should only have one condition after clear
    let conditions = query.get_conditions(q)
    let has_one = list.length(conditions) == 1
    #(has_one, s3)
  })
}

/// Property: Distinct is idempotent
pub fn property_distinct_idempotent_test() {
  run_property("distinct_idempotent", test_iterations, fn(s) {
    let q1 =
      query.from_table("users")
      |> query.distinct

    let q2 =
      query.from_table("users")
      |> query.distinct
      |> query.distinct
      |> query.distinct

    // Both should be distinct
    let both_distinct = query.is_distinct(q1) && query.is_distinct(q2)
    #(both_distinct, s)
  })
}

// ============================================================================
// AST VALIDITY PROPERTIES
// ============================================================================

/// Property: Any generated query has a valid source
pub fn property_query_has_valid_source_test() {
  run_property("query_has_valid_source", test_iterations, fn(s) {
    let #(q, s1) = random_query(s, simple_config())

    let source = query.get_source(q)
    let is_valid = case source {
      TableSource(table, _) -> table != ""
      ast.SubquerySource(_) -> True
    }
    #(is_valid, s1)
  })
}

/// Property: Any generated query has a valid select clause
pub fn property_query_has_valid_select_test() {
  run_property("query_has_valid_select", test_iterations, fn(s) {
    let #(q, s1) = random_query(s, simple_config())

    let select = query.get_select(q)
    let is_valid = case select {
      SelectAll -> True
      SelectFields(_fields) -> True
      // Fields can be empty list (valid)
      ast.SelectExpr(_) -> True
    }
    #(is_valid, s1)
  })
}

/// Property: Generated conditions are structurally valid
pub fn property_conditions_structurally_valid_test() {
  run_property("conditions_structurally_valid", test_iterations, fn(s) {
    let #(cond, s1) = random_condition(s, 3)

    // Check that condition can be inspected without panic
    let is_valid = is_condition_valid(cond)
    #(is_valid, s1)
  })
}

fn is_condition_valid(cond: Condition) -> Bool {
  case cond {
    Eq(field, _) -> field != ""
    ast.NotEq(field, _) -> field != ""
    Gt(field, _) -> field != ""
    ast.Gte(field, _) -> field != ""
    ast.Lt(field, _) -> field != ""
    ast.Lte(field, _) -> field != ""
    ast.In(field, _) -> field != ""
    ast.NotIn(field, _) -> field != ""
    ast.Like(field, _) -> field != ""
    ast.NotLike(field, _) -> field != ""
    ast.ILike(field, _) -> field != ""
    ast.NotILike(field, _) -> field != ""
    IsNull(field) -> field != ""
    IsNotNull(field) -> field != ""
    ast.Between(field, _, _) -> field != ""
    And(conditions) -> list.all(conditions, is_condition_valid)
    Or(conditions) -> list.all(conditions, is_condition_valid)
    ast.Not(inner) -> is_condition_valid(inner)
    ast.Raw(_, _) -> True
  }
}

/// Property: Complex queries don't cause stack overflow
pub fn property_complex_queries_dont_overflow_test() {
  run_property("complex_queries_dont_overflow", 100, fn(s) {
    // Generate more complex queries
    let #(q, s1) = random_query(s, complex_config())

    // Query should be inspectable
    let _conditions = query.get_conditions(q)
    let _order_bys = query.get_order_bys(q)
    let _joins = query.get_joins(q)

    #(True, s1)
  })
}

/// Property: Query debug string doesn't panic
pub fn property_debug_string_safe_test() {
  run_property("debug_string_safe", test_iterations, fn(s) {
    let #(q, s1) = random_query(s, simple_config())

    // Should not panic
    let _debug = query.to_debug_string(q)
    #(True, s1)
  })
}

// ============================================================================
// VALUE HANDLING PROPERTIES
// ============================================================================

/// Property: Any value type can be used in conditions
pub fn property_any_value_in_condition_test() {
  run_property("any_value_in_condition", test_iterations, fn(s) {
    let #(val, s1) = generators.random_value(s)
    let #(field, s2) = generators.random_field_name(s1)

    // All value types should work in conditions
    let cond = Eq(field, val)
    let q = query.from_table("users") |> query.where(cond)

    let conditions = query.get_conditions(q)
    let has_condition = list.length(conditions) == 1
    #(has_condition, s2)
  })
}

/// Property: Unicode strings are handled correctly
pub fn property_unicode_strings_handled_test() {
  run_property("unicode_strings_handled", test_iterations, fn(s) {
    let #(val, s1) = generators.random_unicode_string_value(s)
    let #(field, s2) = generators.random_field_name(s1)

    let cond = Eq(field, val)
    let q = query.from_table("users") |> query.where(cond)

    // Query should be constructable with unicode
    let conditions = query.get_conditions(q)
    let has_condition = list.length(conditions) == 1

    // Debug string should not panic
    let _debug = query.to_debug_string(q)

    #(has_condition, s2)
  })
}

/// Property: Boundary numeric values are valid
pub fn property_boundary_numerics_valid_test() {
  run_property("boundary_numerics_valid", test_iterations, fn(s) {
    let #(val, s1) = generators.random_boundary_int_value(s)
    let #(field, s2) = generators.random_field_name(s1)

    let cond = Eq(field, val)
    let q = query.from_table("users") |> query.where(cond)

    let conditions = query.get_conditions(q)
    let has_condition = list.length(conditions) == 1
    #(has_condition, s2)
  })
}

/// Property: Edge case strings don't break queries
pub fn property_edge_case_strings_safe_test() {
  run_property("edge_case_strings_safe", test_iterations, fn(s) {
    let #(val, s1) = generators.random_edge_case_string_value(s)
    let #(field, s2) = generators.random_field_name(s1)

    let cond = Eq(field, val)
    let q = query.from_table("users") |> query.where(cond)

    // Query should be constructable
    let conditions = query.get_conditions(q)
    let has_condition = list.length(conditions) == 1

    // Debug string should handle edge cases
    let _debug = query.to_debug_string(q)

    #(has_condition, s2)
  })
}

/// Property: Null handling is consistent
pub fn property_null_handling_consistent_test() {
  run_property("null_handling_consistent", test_iterations, fn(s) {
    let #(field, s1) = generators.random_field_name(s)

    // Test IS NULL and IS NOT NULL
    let q1 = query.from_table("users") |> query.where(IsNull(field))
    let q2 = query.from_table("users") |> query.where(IsNotNull(field))

    let conds1 = query.get_conditions(q1)
    let conds2 = query.get_conditions(q2)

    let both_valid = list.length(conds1) == 1 && list.length(conds2) == 1
    #(both_valid, s1)
  })
}

// ============================================================================
// QUERY BUILDER SPECIFIC PROPERTIES
// ============================================================================

/// Property: Builder identity modifier doesn't change query
pub fn property_identity_modifier_test() {
  run_property("identity_modifier", test_iterations, fn(s) {
    let #(q, s1) = random_query(s, simple_config())

    let q_modified = builder.identity()(q)

    // Should be identical
    let same_conditions =
      query.get_conditions(q) == query.get_conditions(q_modified)
    let same_limit = query.get_limit(q) == query.get_limit(q_modified)
    let same_offset = query.get_offset(q) == query.get_offset(q_modified)

    #(same_conditions && same_limit && same_offset, s1)
  })
}

/// Property: Conditional modifier only applies when condition is true
pub fn property_when_modifier_test() {
  run_property("when_modifier", test_iterations, fn(s) {
    let #(cond, s1) = random_simple_condition(s)
    let #(apply, s2) = generators.random_bool(s1)

    let base_query = query.from_table("users")
    let modifier = builder.filter(cond)

    let q = base_query |> builder.when(apply, modifier)

    let condition_count = query.condition_count(q)
    let expected_count = case apply {
      True -> 1
      False -> 0
    }

    #(condition_count == expected_count, s2)
  })
}

/// Property: clone_without_conditions removes all conditions
pub fn property_clone_without_conditions_test() {
  run_property("clone_without_conditions", test_iterations, fn(s) {
    let #(q, s1) = random_query(s, simple_config())

    let cloned = builder.clone_without_conditions(q)

    // Should have no conditions
    let no_conditions = list.is_empty(query.get_conditions(cloned))

    // But should preserve other properties
    let same_limit = query.get_limit(q) == query.get_limit(cloned)
    let same_offset = query.get_offset(q) == query.get_offset(cloned)

    #(no_conditions && same_limit && same_offset, s1)
  })
}

/// Property: clone_without_pagination removes limit and offset
pub fn property_clone_without_pagination_test() {
  run_property("clone_without_pagination", test_iterations, fn(s) {
    let #(q, s1) = random_query(s, simple_config())

    let cloned = builder.clone_without_pagination(q)

    // Should have no pagination
    let no_limit = query.get_limit(cloned) == None
    let no_offset = query.get_offset(cloned) == None

    // But should preserve conditions
    let same_condition_count =
      query.condition_count(q) == query.condition_count(cloned)

    #(no_limit && no_offset && same_condition_count, s1)
  })
}

// ============================================================================
// STRESS TESTS
// ============================================================================

/// Property: Can generate and inspect 1000 simple queries without issues
pub fn stress_test_1000_simple_queries_test() {
  run_property("stress_1000_simple", 1000, fn(s) {
    let #(q, s1) = random_query(s, minimal_config())

    // Basic inspections
    let _source = query.get_source(q)
    let _select = query.get_select(q)
    let _conditions = query.get_conditions(q)
    let _debug = query.to_debug_string(q)

    #(True, s1)
  })
}

/// Property: Can generate and inspect 500 complex queries without issues
pub fn stress_test_500_complex_queries_test() {
  run_property("stress_500_complex", 500, fn(s) {
    let #(q, s1) = random_query(s, complex_config())

    // Full inspection
    let _source = query.get_source(q)
    let _select = query.get_select(q)
    let _conditions = query.get_conditions(q)
    let _order_bys = query.get_order_bys(q)
    let _joins = query.get_joins(q)
    let _limit = query.get_limit(q)
    let _offset = query.get_offset(q)
    let _distinct = query.is_distinct(q)
    let _debug = query.to_debug_string(q)

    #(True, s1)
  })
}

// ============================================================================
// EDGE CASE UNIT TESTS (Discovered through property testing)
// ============================================================================

/// Test: Empty AND condition list
pub fn edge_case_empty_and_test() {
  let q =
    query.from_table("users")
    |> query.where(And([]))

  let conditions = query.get_conditions(q)
  list.length(conditions) |> should.equal(1)
}

/// Test: Empty OR condition list
pub fn edge_case_empty_or_test() {
  let q =
    query.from_table("users")
    |> query.where(Or([]))

  let conditions = query.get_conditions(q)
  list.length(conditions) |> should.equal(1)
}

/// Test: Deeply nested conditions
pub fn edge_case_deep_nesting_test() {
  // Create deeply nested condition
  let deep =
    And([
      Or([
        And([Eq("a", IntValue(1)), Eq("b", IntValue(2))]),
        Eq("c", IntValue(3)),
      ]),
      ast.Not(Or([Eq("d", IntValue(4)), Eq("e", IntValue(5))])),
    ])

  let q = query.from_table("users") |> query.where(deep)

  // Should not panic and should have the condition
  let conditions = query.get_conditions(q)
  list.length(conditions) |> should.equal(1)

  // Debug string should work
  let debug = query.to_debug_string(q)
  debug |> should.not_equal("")
}

/// Test: Query with all features combined
pub fn edge_case_full_featured_query_test() {
  let q =
    query.from_table("users")
    |> query.select(["id", "name", "email"])
    |> query.distinct
    |> query.where(Eq("active", ast.BoolValue(True)))
    |> query.where(Gt("age", IntValue(18)))
    |> query.order_by_desc("created_at")
    |> query.order_by_asc("name")
    |> query.group_by("status")
    |> query.having(Gt("count", IntValue(5)))
    |> query.limit(10)
    |> query.offset(20)
    |> query.join("orders", on: Eq("users.id", ast.ParamValue(1)))

  // All properties should be accessible
  query.get_conditions(q) |> list.length |> should.equal(2)
  query.get_order_bys(q) |> list.length |> should.equal(2)
  query.get_joins(q) |> list.length |> should.equal(1)
  query.get_limit(q) |> should.equal(Some(10))
  query.get_offset(q) |> should.equal(Some(20))
  query.is_distinct(q) |> should.be_true

  // Debug string should capture everything
  let debug = query.to_debug_string(q)
  debug |> should.not_equal("")
}

/// Test: Pagination calculation
pub fn edge_case_paginate_test() {
  let q =
    query.from_table("users")
    |> query.paginate(page: 3, per_page: 10)

  query.get_limit(q) |> should.equal(Some(10))
  query.get_offset(q) |> should.equal(Some(20))
  // (3-1) * 10
}

/// Test: First page pagination
pub fn edge_case_paginate_first_page_test() {
  let q =
    query.from_table("users")
    |> query.paginate(page: 1, per_page: 25)

  query.get_limit(q) |> should.equal(Some(25))
  query.get_offset(q) |> should.equal(Some(0))
}
