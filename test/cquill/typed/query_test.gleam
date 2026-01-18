// Phantom-Typed Query Tests
//
// Tests for the type-safe query builder that uses phantom types to
// ensure compile-time validation of queries.

import cquill/query/ast
import cquill/typed/query.{
  condition_to_ast, to_ast, typed_and, typed_between, typed_column_eq,
  typed_contains, typed_distinct, typed_ends_with, typed_eq, typed_eq_columns,
  typed_from, typed_get_limit, typed_get_offset, typed_group_by, typed_gt,
  typed_gt_columns, typed_gte, typed_has_conditions, typed_has_order_by,
  typed_has_pagination, typed_having, typed_ilike, typed_in, typed_is_distinct,
  typed_is_not_null, typed_is_null, typed_join, typed_left_join, typed_like,
  typed_limit, typed_lt, typed_lt_columns, typed_lte, typed_not, typed_not_eq,
  typed_not_eq_columns, typed_not_ilike, typed_not_in, typed_not_like,
  typed_offset, typed_or, typed_or_where, typed_order_by_asc,
  typed_order_by_clear, typed_order_by_desc, typed_paginate, typed_select,
  typed_select_all, typed_starts_with, typed_where, typed_where_clear,
}
import cquill/typed/table.{
  type Column, type Join2, type Table, column, in_join2_left, in_join2_right,
  table,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleeunit/should

// ============================================================================
// MOCK TABLE TYPES (simulating generated code)
// ============================================================================

/// Phantom type representing the "users" table
pub type UserTable

/// Phantom type representing the "posts" table
pub type PostTable

// ============================================================================
// MOCK TABLE AND COLUMN FACTORY FUNCTIONS
// ============================================================================

fn users() -> Table(UserTable) {
  table("users")
}

fn posts() -> Table(PostTable) {
  table("posts")
}

// User columns
fn user_id() -> Column(UserTable, Int) {
  column("id")
}

fn user_email() -> Column(UserTable, String) {
  column("email")
}

fn user_name() -> Column(UserTable, Option(String)) {
  column("name")
}

fn user_active() -> Column(UserTable, Bool) {
  column("active")
}

fn user_age() -> Column(UserTable, Int) {
  column("age")
}

// Post columns
fn post_id() -> Column(PostTable, Int) {
  column("id")
}

fn post_user_id() -> Column(PostTable, Int) {
  column("user_id")
}

fn post_title() -> Column(PostTable, String) {
  column("title")
}

// ============================================================================
// QUERY CONSTRUCTION TESTS
// ============================================================================

pub fn typed_from_creates_query_test() {
  let query = typed_from(users())
  let ast_query = to_ast(query)

  case ast_query.source {
    ast.TableSource(table, _) -> table |> should.equal("users")
    _ -> should.fail()
  }
}

pub fn typed_from_uses_select_all_by_default_test() {
  let query = typed_from(users())
  let ast_query = to_ast(query)

  ast_query.select
  |> should.equal(ast.SelectAll)
}

// ============================================================================
// SELECTION TESTS
// ============================================================================

pub fn typed_select_sets_fields_test() {
  // Note: typed_select requires columns of the same value type in a list
  // For mixed types, we use multiple calls or select by names
  let query =
    typed_from(users())
    |> typed_select([user_email()])
  let ast_query = to_ast(query)

  case ast_query.select {
    ast.SelectFields(fields) -> fields |> should.equal(["email"])
    _ -> should.fail()
  }
}

pub fn typed_select_all_resets_to_all_test() {
  let query =
    typed_from(users())
    |> typed_select([user_id()])
    |> typed_select_all
  let ast_query = to_ast(query)

  ast_query.select
  |> should.equal(ast.SelectAll)
}

pub fn typed_distinct_sets_flag_test() {
  let query =
    typed_from(users())
    |> typed_distinct
  let ast_query = to_ast(query)

  ast_query.distinct
  |> should.be_true
}

// ============================================================================
// WHERE CONDITION TESTS
// ============================================================================

pub fn typed_where_adds_condition_test() {
  let query =
    typed_from(users())
    |> typed_where(typed_eq(user_email(), "test@example.com"))

  typed_has_conditions(query)
  |> should.be_true
}

pub fn typed_eq_creates_equality_condition_test() {
  let condition = typed_eq(user_email(), "test@example.com")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Eq(field, value) -> {
      field |> should.equal("email")
      value |> should.equal(ast.StringValue("test@example.com"))
    }
    _ -> should.fail()
  }
}

pub fn typed_not_eq_creates_not_equal_condition_test() {
  let condition = typed_not_eq(user_id(), 5)
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.NotEq(field, value) -> {
      field |> should.equal("id")
      value |> should.equal(ast.IntValue(5))
    }
    _ -> should.fail()
  }
}

pub fn typed_gt_creates_greater_than_condition_test() {
  let condition = typed_gt(user_age(), 18)
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Gt(field, value) -> {
      field |> should.equal("age")
      value |> should.equal(ast.IntValue(18))
    }
    _ -> should.fail()
  }
}

pub fn typed_gte_creates_greater_than_or_equal_condition_test() {
  let condition = typed_gte(user_age(), 21)
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Gte(field, value) -> {
      field |> should.equal("age")
      value |> should.equal(ast.IntValue(21))
    }
    _ -> should.fail()
  }
}

pub fn typed_lt_creates_less_than_condition_test() {
  let condition = typed_lt(user_age(), 65)
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Lt(field, value) -> {
      field |> should.equal("age")
      value |> should.equal(ast.IntValue(65))
    }
    _ -> should.fail()
  }
}

pub fn typed_lte_creates_less_than_or_equal_condition_test() {
  let condition = typed_lte(user_age(), 65)
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Lte(field, value) -> {
      field |> should.equal("age")
      value |> should.equal(ast.IntValue(65))
    }
    _ -> should.fail()
  }
}

pub fn typed_in_creates_in_condition_test() {
  let condition = typed_in(user_id(), [1, 2, 3])
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.In(field, values) -> {
      field |> should.equal("id")
      values
      |> should.equal([ast.IntValue(1), ast.IntValue(2), ast.IntValue(3)])
    }
    _ -> should.fail()
  }
}

pub fn typed_not_in_creates_not_in_condition_test() {
  let condition = typed_not_in(user_id(), [4, 5])
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.NotIn(field, values) -> {
      field |> should.equal("id")
      values |> should.equal([ast.IntValue(4), ast.IntValue(5)])
    }
    _ -> should.fail()
  }
}

pub fn typed_like_creates_like_condition_test() {
  let condition = typed_like(user_email(), "%@example.com")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Like(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("%@example.com")
    }
    _ -> should.fail()
  }
}

pub fn typed_not_like_creates_not_like_condition_test() {
  let condition = typed_not_like(user_email(), "%spam%")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.NotLike(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("%spam%")
    }
    _ -> should.fail()
  }
}

pub fn typed_is_null_creates_is_null_condition_test() {
  let condition = typed_is_null(user_name())
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.IsNull(field) -> field |> should.equal("name")
    _ -> should.fail()
  }
}

pub fn typed_is_not_null_creates_is_not_null_condition_test() {
  let condition = typed_is_not_null(user_name())
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.IsNotNull(field) -> field |> should.equal("name")
    _ -> should.fail()
  }
}

pub fn typed_between_creates_between_condition_test() {
  let condition = typed_between(user_age(), 18, 65)
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Between(field, low, high) -> {
      field |> should.equal("age")
      low |> should.equal(ast.IntValue(18))
      high |> should.equal(ast.IntValue(65))
    }
    _ -> should.fail()
  }
}

// ============================================================================
// LOGICAL CONDITION TESTS
// ============================================================================

pub fn typed_and_combines_conditions_test() {
  let condition =
    typed_and([typed_eq(user_active(), True), typed_gt(user_age(), 18)])
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.And(conditions) -> {
      list.length(conditions)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn typed_or_combines_conditions_test() {
  let condition =
    typed_or([
      typed_eq(user_email(), "admin@test.com"),
      typed_eq(user_active(), True),
    ])
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Or(conditions) -> {
      list.length(conditions)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn typed_not_negates_condition_test() {
  let condition = typed_not(typed_eq(user_active(), False))
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Not(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn typed_or_where_adds_or_condition_test() {
  let query =
    typed_from(users())
    |> typed_where(typed_eq(user_active(), True))
    |> typed_or_where(typed_eq(user_email(), "admin@test.com"))
  let ast_query = to_ast(query)

  list.length(ast_query.wheres)
  |> should.equal(1)
  // The combined condition should be an Or
  case ast_query.wheres {
    [ast.Where(ast.Or(_))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn typed_where_clear_removes_all_conditions_test() {
  let query =
    typed_from(users())
    |> typed_where(typed_eq(user_active(), True))
    |> typed_where_clear

  typed_has_conditions(query)
  |> should.be_false
}

// ============================================================================
// ORDER BY TESTS
// ============================================================================

pub fn typed_order_by_asc_adds_ascending_order_test() {
  let query =
    typed_from(users())
    |> typed_order_by_asc(user_email())

  typed_has_order_by(query)
  |> should.be_true

  let ast_query = to_ast(query)
  case ast_query.order_bys {
    [ast.OrderBy(field, direction, _)] -> {
      field |> should.equal("email")
      direction |> should.equal(ast.Asc)
    }
    _ -> should.fail()
  }
}

pub fn typed_order_by_desc_adds_descending_order_test() {
  let query =
    typed_from(users())
    |> typed_order_by_desc(user_id())

  let ast_query = to_ast(query)
  case ast_query.order_bys {
    [ast.OrderBy(field, direction, _)] -> {
      field |> should.equal("id")
      direction |> should.equal(ast.Desc)
    }
    _ -> should.fail()
  }
}

pub fn typed_order_by_clear_removes_all_orders_test() {
  let query =
    typed_from(users())
    |> typed_order_by_asc(user_email())
    |> typed_order_by_clear

  typed_has_order_by(query)
  |> should.be_false
}

// ============================================================================
// PAGINATION TESTS
// ============================================================================

pub fn typed_limit_sets_limit_test() {
  let query =
    typed_from(users())
    |> typed_limit(10)

  typed_get_limit(query)
  |> should.equal(Some(10))
}

pub fn typed_offset_sets_offset_test() {
  let query =
    typed_from(users())
    |> typed_offset(20)

  typed_get_offset(query)
  |> should.equal(Some(20))
}

pub fn typed_paginate_sets_limit_and_offset_test() {
  let query =
    typed_from(users())
    |> typed_paginate(page: 3, per_page: 10)

  typed_get_limit(query)
  |> should.equal(Some(10))
  typed_get_offset(query)
  |> should.equal(Some(20))
  // (page 3 - 1) * 10 = 20
}

pub fn typed_has_pagination_detects_limit_test() {
  let query =
    typed_from(users())
    |> typed_limit(10)

  typed_has_pagination(query)
  |> should.be_true
}

pub fn typed_has_pagination_detects_offset_test() {
  let query =
    typed_from(users())
    |> typed_offset(5)

  typed_has_pagination(query)
  |> should.be_true
}

pub fn typed_has_pagination_false_when_none_test() {
  let query = typed_from(users())

  typed_has_pagination(query)
  |> should.be_false
}

// ============================================================================
// INSPECTION TESTS
// ============================================================================

pub fn typed_is_distinct_detects_distinct_test() {
  let query =
    typed_from(users())
    |> typed_distinct

  typed_is_distinct(query)
  |> should.be_true
}

pub fn typed_is_distinct_false_by_default_test() {
  let query = typed_from(users())

  typed_is_distinct(query)
  |> should.be_false
}

// ============================================================================
// JOIN TESTS
// ============================================================================

pub fn typed_join_creates_inner_join_test() {
  let join_condition: query.TypedCondition(Join2(UserTable, PostTable)) =
    typed_column_eq(in_join2_left(user_id()), in_join2_right(post_user_id()))

  let query =
    typed_from(users())
    |> typed_join(posts(), on: join_condition)

  let ast_query = to_ast(query)
  case ast_query.joins {
    [ast.Join(join_type, table_name, _, _)] -> {
      join_type |> should.equal(ast.InnerJoin)
      table_name |> should.equal("posts")
    }
    _ -> should.fail()
  }
}

pub fn typed_left_join_creates_left_join_test() {
  let join_condition: query.TypedCondition(Join2(UserTable, PostTable)) =
    typed_column_eq(in_join2_left(user_id()), in_join2_right(post_user_id()))

  let query =
    typed_from(users())
    |> typed_left_join(posts(), on: join_condition)

  let ast_query = to_ast(query)
  case ast_query.joins {
    [ast.Join(join_type, _, _, _)] -> {
      join_type |> should.equal(ast.LeftJoin)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// GROUP BY / HAVING TESTS
// ============================================================================

pub fn typed_group_by_adds_field_test() {
  let query =
    typed_from(users())
    |> typed_group_by(user_active())

  let ast_query = to_ast(query)
  ast_query.group_bys
  |> should.equal(["active"])
}

pub fn typed_having_adds_condition_test() {
  let query =
    typed_from(users())
    |> typed_group_by(user_active())
    |> typed_having(typed_gt(user_age(), 18))

  let ast_query = to_ast(query)
  list.length(ast_query.havings)
  |> should.equal(1)
}

// ============================================================================
// COMPLEX QUERY TESTS
// ============================================================================

pub fn complex_query_builds_correctly_test() {
  // Note: typed_select requires columns of the same value type
  // For selecting columns with different types, use select_all or string names
  let query =
    typed_from(users())
    |> typed_select([user_email()])
    |> typed_where(typed_eq(user_active(), True))
    |> typed_where(typed_gt(user_age(), 18))
    |> typed_order_by_asc(user_email())
    |> typed_limit(10)
    |> typed_offset(0)
    |> typed_distinct

  let ast_query = to_ast(query)

  // Check select
  case ast_query.select {
    ast.SelectFields(fields) -> fields |> should.equal(["email"])
    _ -> should.fail()
  }

  // Check wheres
  list.length(ast_query.wheres)
  |> should.equal(2)

  // Check order_bys
  list.length(ast_query.order_bys)
  |> should.equal(1)

  // Check pagination
  ast_query.limit
  |> should.equal(Some(10))
  ast_query.offset
  |> should.equal(Some(0))

  // Check distinct
  ast_query.distinct
  |> should.be_true
}

// ============================================================================
// TYPE SAFETY VERIFICATION TESTS
// ============================================================================

// These tests verify that the type system correctly constrains column usage.
// They compile successfully because the types are used correctly.

pub fn query_uses_only_its_table_columns_test() {
  // This test verifies that we can only use columns from the users table
  // in a query on the users table. If we tried to use post_title here,
  // it would not compile.
  let query =
    typed_from(users())
    |> typed_where(typed_eq(user_email(), "test@example.com"))
    |> typed_where(typed_gt(user_age(), 18))

  typed_has_conditions(query)
  |> should.be_true
}

pub fn joined_query_uses_columns_from_both_tables_test() {
  // After a join, we can use columns from both tables (when coerced)
  let join_condition: query.TypedCondition(Join2(UserTable, PostTable)) =
    typed_column_eq(in_join2_left(user_id()), in_join2_right(post_user_id()))

  let query =
    typed_from(users())
    |> typed_join(posts(), on: join_condition)

  // The query now has type TypedQuery(Join2(UserTable, PostTable))
  let ast_query = to_ast(query)
  list.length(ast_query.joins)
  |> should.equal(1)
}

// ============================================================================
// STRING CONDITION BUILDER TESTS (Issue #25)
// ============================================================================

pub fn typed_ilike_creates_case_insensitive_like_condition_test() {
  let condition = typed_ilike(user_email(), "%@EXAMPLE.COM")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.ILike(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("%@EXAMPLE.COM")
    }
    _ -> should.fail()
  }
}

pub fn typed_not_ilike_creates_not_ilike_condition_test() {
  let condition = typed_not_ilike(user_email(), "%spam%")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.NotILike(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("%spam%")
    }
    _ -> should.fail()
  }
}

pub fn typed_starts_with_creates_like_prefix_condition_test() {
  let condition = typed_starts_with(user_email(), "admin")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Like(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("admin%")
    }
    _ -> should.fail()
  }
}

pub fn typed_ends_with_creates_like_suffix_condition_test() {
  let condition = typed_ends_with(user_email(), "@example.com")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Like(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("%@example.com")
    }
    _ -> should.fail()
  }
}

pub fn typed_contains_creates_like_substring_condition_test() {
  let condition = typed_contains(user_email(), "admin")
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Like(field, pattern) -> {
      field |> should.equal("email")
      pattern |> should.equal("%admin%")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// CROSS-TABLE COLUMN COMPARISON TESTS (Issue #25)
// ============================================================================

pub fn typed_eq_columns_creates_cross_table_equality_test() {
  // This creates a Join2 scoped condition for comparing columns from different tables
  let condition: query.TypedCondition(Join2(PostTable, UserTable)) =
    typed_eq_columns(post_user_id(), user_id())
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Raw(sql, params) -> {
      sql |> should.equal("user_id = id")
      list.length(params) |> should.equal(0)
    }
    _ -> should.fail()
  }
}

pub fn typed_not_eq_columns_creates_cross_table_inequality_test() {
  let condition: query.TypedCondition(Join2(PostTable, UserTable)) =
    typed_not_eq_columns(post_user_id(), user_id())
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Raw(sql, params) -> {
      sql |> should.equal("user_id != id")
      list.length(params) |> should.equal(0)
    }
    _ -> should.fail()
  }
}

pub fn typed_gt_columns_creates_cross_table_greater_than_test() {
  let condition: query.TypedCondition(Join2(PostTable, UserTable)) =
    typed_gt_columns(post_user_id(), user_id())
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Raw(sql, params) -> {
      sql |> should.equal("user_id > id")
      list.length(params) |> should.equal(0)
    }
    _ -> should.fail()
  }
}

pub fn typed_lt_columns_creates_cross_table_less_than_test() {
  let condition: query.TypedCondition(Join2(PostTable, UserTable)) =
    typed_lt_columns(post_user_id(), user_id())
  let ast_cond = condition_to_ast(condition)

  case ast_cond {
    ast.Raw(sql, params) -> {
      sql |> should.equal("user_id < id")
      list.length(params) |> should.equal(0)
    }
    _ -> should.fail()
  }
}

pub fn string_conditions_work_in_query_pipeline_test() {
  // Test that string conditions can be used in full query pipelines
  let query =
    typed_from(users())
    |> typed_where(typed_starts_with(user_email(), "admin"))
    |> typed_where(typed_ends_with(user_email(), "@example.com"))
    |> typed_where(typed_contains(user_email(), "test"))
    |> typed_where(typed_ilike(user_email(), "%SUPPORT%"))

  typed_has_conditions(query)
  |> should.be_true

  let ast_query = to_ast(query)
  list.length(ast_query.wheres)
  |> should.equal(4)
}

pub fn cross_table_column_comparison_in_join_condition_test() {
  // Test using typed_eq_columns in an actual join
  let join_condition: query.TypedCondition(Join2(UserTable, PostTable)) =
    typed_eq_columns(user_id(), post_user_id())

  let query =
    typed_from(users())
    |> typed_join(posts(), on: join_condition)

  let ast_query = to_ast(query)
  list.length(ast_query.joins)
  |> should.equal(1)

  case ast_query.joins {
    [ast.Join(join_type, table_name, _, _)] -> {
      join_type |> should.equal(ast.InnerJoin)
      table_name |> should.equal("posts")
    }
    _ -> should.fail()
  }
}
