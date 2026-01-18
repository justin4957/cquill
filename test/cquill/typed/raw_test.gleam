// Raw SQL Escape Hatch Tests
//
// Tests for the raw SQL module that provides escape hatches
// when the query builder can't express something.

import cquill/query/ast
import cquill/typed/raw.{
  add, age_from_now, and_raw, avg, case_when, cast, cast_column, coalesce_column,
  coalesce_int_column, coalesce_raw, column_expr, column_raw_between,
  column_raw_eq, column_raw_gt, column_raw_gte, column_raw_lt, column_raw_lte,
  concat, concat_columns_with_separator, count_all, count_column, count_distinct,
  count_over, count_over_with_alias, current_date, current_time,
  current_timestamp, date_trunc, divide, exists, extract, false_literal,
  float_literal, in_subquery, int_literal, interval, length, lower, max, min,
  modulo, multiply, not_exists, not_in_subquery, not_raw, now, null_literal,
  nullif_column, or_raw, random, raw, raw_params, raw_sql, raw_to_condition,
  raw_to_order_by, raw_with_params, row_number_over, string_literal, subtract,
  sum, trim, true_literal, upper,
}
import cquill/typed/table.{type Column, column}
import gleam/option.{type Option, None, Some}
import gleeunit/should

// ============================================================================
// MOCK TABLE TYPES
// ============================================================================

/// Phantom type for the users table
pub type UserTable

/// Phantom type for the orders table
pub type OrderTable

// Mock column factories
fn user_id() -> Column(UserTable, Int) {
  column("id")
}

fn user_email() -> Column(UserTable, String) {
  column("email")
}

fn user_name() -> Column(UserTable, String) {
  column("name")
}

fn user_nickname() -> Column(UserTable, Option(String)) {
  column("nickname")
}

fn user_age() -> Column(UserTable, Option(Int)) {
  column("age")
}

fn user_status() -> Column(UserTable, String) {
  column("status")
}

fn user_created_at() -> Column(UserTable, String) {
  column("created_at")
}

fn order_amount() -> Column(OrderTable, Int) {
  column("amount")
}

// ============================================================================
// RAW EXPRESSION TESTS
// ============================================================================

pub fn raw_creates_expression_test() {
  let expr = raw("NOW()")

  raw_sql(expr)
  |> should.equal("NOW()")

  raw_params(expr)
  |> should.equal([])
}

pub fn raw_with_params_creates_parameterized_expression_test() {
  let expr =
    raw_with_params("email = $1", [ast.StringValue("test@example.com")])

  raw_sql(expr)
  |> should.equal("email = $1")

  raw_params(expr)
  |> should.equal([ast.StringValue("test@example.com")])
}

pub fn raw_with_multiple_params_test() {
  let expr =
    raw_with_params("created_at > $1 AND status = $2", [
      ast.StringValue("2024-01-01"),
      ast.StringValue("active"),
    ])

  raw_sql(expr)
  |> should.equal("created_at > $1 AND status = $2")

  raw_params(expr)
  |> should.equal([
    ast.StringValue("2024-01-01"),
    ast.StringValue("active"),
  ])
}

pub fn raw_to_condition_converts_to_ast_test() {
  let expr = raw_with_params("id = $1", [ast.IntValue(42)])
  let condition = raw_to_condition(expr)

  case condition {
    ast.Raw(sql, params) -> {
      sql |> should.equal("id = $1")
      params |> should.equal([ast.IntValue(42)])
    }
    _ -> should.fail()
  }
}

pub fn raw_to_order_by_creates_order_clause_test() {
  let expr = raw("RANDOM()")
  let order = raw_to_order_by(expr, ast.Asc)

  order.field |> should.equal("RANDOM()")
  order.direction |> should.equal(ast.Asc)
}

// ============================================================================
// FRAGMENT HELPERS TESTS
// ============================================================================

pub fn now_creates_now_expression_test() {
  let expr = now()
  raw_sql(expr) |> should.equal("NOW()")
}

pub fn current_timestamp_test() {
  let expr = current_timestamp()
  raw_sql(expr) |> should.equal("CURRENT_TIMESTAMP")
}

pub fn current_date_test() {
  let expr = current_date()
  raw_sql(expr) |> should.equal("CURRENT_DATE")
}

pub fn current_time_test() {
  let expr = current_time()
  raw_sql(expr) |> should.equal("CURRENT_TIME")
}

pub fn interval_creates_interval_expression_test() {
  let expr = interval(7, "days")
  raw_sql(expr) |> should.equal("INTERVAL '7 days'")
}

pub fn interval_with_hour_test() {
  let expr = interval(1, "hour")
  raw_sql(expr) |> should.equal("INTERVAL '1 hour'")
}

pub fn interval_with_minutes_test() {
  let expr = interval(30, "minutes")
  raw_sql(expr) |> should.equal("INTERVAL '30 minutes'")
}

pub fn coalesce_column_creates_coalesce_test() {
  let expr = coalesce_column(user_nickname(), "Anonymous")
  raw_sql(expr) |> should.equal("COALESCE(nickname, 'Anonymous')")
}

pub fn coalesce_int_column_test() {
  let expr = coalesce_int_column(user_age(), 0)
  raw_sql(expr) |> should.equal("COALESCE(age, 0)")
}

pub fn coalesce_raw_test() {
  let expr = coalesce_raw([raw("nickname"), raw("'Guest'")])
  raw_sql(expr) |> should.equal("COALESCE(nickname, 'Guest')")
}

pub fn nullif_column_test() {
  let expr = nullif_column(user_status(), "deleted")
  raw_sql(expr) |> should.equal("NULLIF(status, 'deleted')")
}

pub fn cast_expression_test() {
  let expr = cast(raw("'123'"), "INTEGER")
  raw_sql(expr) |> should.equal("CAST('123' AS INTEGER)")
}

pub fn cast_column_test() {
  let expr = cast_column(user_created_at(), "DATE")
  raw_sql(expr) |> should.equal("CAST(created_at AS DATE)")
}

pub fn random_test() {
  let expr = random()
  raw_sql(expr) |> should.equal("RANDOM()")
}

// ============================================================================
// AGGREGATE FUNCTION TESTS
// ============================================================================

pub fn count_all_test() {
  let expr = count_all()
  raw_sql(expr) |> should.equal("COUNT(*)")
}

pub fn count_column_test() {
  let expr = count_column(user_id())
  raw_sql(expr) |> should.equal("COUNT(id)")
}

pub fn count_distinct_test() {
  let expr = count_distinct(user_email())
  raw_sql(expr) |> should.equal("COUNT(DISTINCT email)")
}

pub fn sum_test() {
  let expr = sum(order_amount())
  raw_sql(expr) |> should.equal("SUM(amount)")
}

pub fn avg_test() {
  let expr = avg(order_amount())
  raw_sql(expr) |> should.equal("AVG(amount)")
}

pub fn min_test() {
  let expr = min(order_amount())
  raw_sql(expr) |> should.equal("MIN(amount)")
}

pub fn max_test() {
  let expr = max(order_amount())
  raw_sql(expr) |> should.equal("MAX(amount)")
}

pub fn count_over_test() {
  let expr = count_over()
  raw_sql(expr) |> should.equal("COUNT(*) OVER ()")
}

pub fn count_over_with_alias_test() {
  let expr = count_over_with_alias("total_count")
  raw_sql(expr) |> should.equal("COUNT(*) OVER () AS total_count")
}

pub fn row_number_over_test() {
  let expr = row_number_over(user_id(), ast.Desc)
  raw_sql(expr) |> should.equal("ROW_NUMBER() OVER (ORDER BY id DESC)")
}

// ============================================================================
// STRING FUNCTION TESTS
// ============================================================================

pub fn lower_test() {
  let expr = lower(user_email())
  raw_sql(expr) |> should.equal("LOWER(email)")
}

pub fn upper_test() {
  let expr = upper(user_name())
  raw_sql(expr) |> should.equal("UPPER(name)")
}

pub fn trim_test() {
  let expr = trim(user_name())
  raw_sql(expr) |> should.equal("TRIM(name)")
}

pub fn length_test() {
  let expr = length(user_name())
  raw_sql(expr) |> should.equal("LENGTH(name)")
}

pub fn concat_test() {
  let expr = concat([raw("first_name"), raw("' '"), raw("last_name")])
  raw_sql(expr) |> should.equal("CONCAT(first_name, ' ', last_name)")
}

pub fn concat_columns_with_separator_test() {
  let expr = concat_columns_with_separator([user_name(), user_email()], " - ")
  raw_sql(expr) |> should.equal("name || ' - ' || email")
}

// ============================================================================
// DATE/TIME FUNCTION TESTS
// ============================================================================

pub fn extract_test() {
  let expr = extract("year", user_created_at())
  raw_sql(expr) |> should.equal("EXTRACT(year FROM created_at)")
}

pub fn extract_month_test() {
  let expr = extract("month", user_created_at())
  raw_sql(expr) |> should.equal("EXTRACT(month FROM created_at)")
}

pub fn date_trunc_test() {
  let expr = date_trunc("month", user_created_at())
  raw_sql(expr) |> should.equal("DATE_TRUNC('month', created_at)")
}

pub fn age_from_now_test() {
  let expr = age_from_now(user_created_at())
  raw_sql(expr) |> should.equal("AGE(created_at)")
}

// ============================================================================
// CASE EXPRESSION TESTS
// ============================================================================

pub fn case_when_test() {
  let expr =
    case_when(
      [
        #(raw("status = 'active'"), raw("'Active'")),
        #(raw("status = 'pending'"), raw("'Pending'")),
      ],
      raw("'Unknown'"),
    )
  raw_sql(expr)
  |> should.equal(
    "CASE WHEN status = 'active' THEN 'Active' WHEN status = 'pending' THEN 'Pending' ELSE 'Unknown' END",
  )
}

// ============================================================================
// SUBQUERY TESTS
// ============================================================================

pub fn exists_test() {
  let expr = exists(raw("SELECT 1 FROM orders WHERE orders.user_id = users.id"))
  raw_sql(expr)
  |> should.equal(
    "EXISTS (SELECT 1 FROM orders WHERE orders.user_id = users.id)",
  )
}

pub fn not_exists_test() {
  let expr = not_exists(raw("SELECT 1 FROM bans WHERE bans.user_id = users.id"))
  raw_sql(expr)
  |> should.equal(
    "NOT EXISTS (SELECT 1 FROM bans WHERE bans.user_id = users.id)",
  )
}

pub fn in_subquery_test() {
  let expr = in_subquery(user_id(), raw("SELECT id FROM active_users"))
  raw_sql(expr) |> should.equal("id IN (SELECT id FROM active_users)")
}

pub fn not_in_subquery_test() {
  let expr = not_in_subquery(user_id(), raw("SELECT id FROM banned_users"))
  raw_sql(expr) |> should.equal("id NOT IN (SELECT id FROM banned_users)")
}

// ============================================================================
// COMPARISON HELPER TESTS
// ============================================================================

pub fn column_raw_eq_test() {
  let expr = column_raw_eq(user_created_at(), now())
  raw_sql(expr) |> should.equal("created_at = NOW()")
}

pub fn column_raw_gt_test() {
  let expr = column_raw_gt(user_created_at(), raw("NOW() - INTERVAL '7 days'"))
  raw_sql(expr) |> should.equal("created_at > NOW() - INTERVAL '7 days'")
}

pub fn column_raw_gte_test() {
  let expr = column_raw_gte(user_created_at(), raw("'2024-01-01'"))
  raw_sql(expr) |> should.equal("created_at >= '2024-01-01'")
}

pub fn column_raw_lt_test() {
  let expr = column_raw_lt(user_created_at(), now())
  raw_sql(expr) |> should.equal("created_at < NOW()")
}

pub fn column_raw_lte_test() {
  let expr = column_raw_lte(user_created_at(), now())
  raw_sql(expr) |> should.equal("created_at <= NOW()")
}

pub fn column_raw_between_test() {
  let expr =
    column_raw_between(
      user_created_at(),
      raw("'2024-01-01'"),
      raw("'2024-12-31'"),
    )
  raw_sql(expr)
  |> should.equal("created_at BETWEEN '2024-01-01' AND '2024-12-31'")
}

// ============================================================================
// ARITHMETIC HELPER TESTS
// ============================================================================

pub fn add_test() {
  let expr = add(column_expr(order_amount()), int_literal(100))
  raw_sql(expr) |> should.equal("(amount + 100)")
}

pub fn subtract_test() {
  let expr = subtract(column_expr(order_amount()), int_literal(10))
  raw_sql(expr) |> should.equal("(amount - 10)")
}

pub fn multiply_test() {
  let expr = multiply(column_expr(order_amount()), int_literal(2))
  raw_sql(expr) |> should.equal("(amount * 2)")
}

pub fn divide_test() {
  let expr = divide(column_expr(order_amount()), int_literal(2))
  raw_sql(expr) |> should.equal("(amount / 2)")
}

pub fn modulo_test() {
  let expr = modulo(column_expr(order_amount()), int_literal(10))
  raw_sql(expr) |> should.equal("(amount % 10)")
}

// ============================================================================
// LITERAL TESTS
// ============================================================================

pub fn int_literal_test() {
  let expr = int_literal(42)
  raw_sql(expr) |> should.equal("42")
}

pub fn int_literal_negative_test() {
  let expr = int_literal(-5)
  raw_sql(expr) |> should.equal("-5")
}

pub fn string_literal_test() {
  let expr = string_literal("hello")
  raw_sql(expr) |> should.equal("'hello'")
}

pub fn string_literal_with_quote_test() {
  let expr = string_literal("it's")
  raw_sql(expr) |> should.equal("'it''s'")
}

pub fn true_literal_test() {
  let expr = true_literal()
  raw_sql(expr) |> should.equal("TRUE")
}

pub fn false_literal_test() {
  let expr = false_literal()
  raw_sql(expr) |> should.equal("FALSE")
}

pub fn null_literal_test() {
  let expr = null_literal()
  raw_sql(expr) |> should.equal("NULL")
}

// ============================================================================
// LOGICAL OPERATOR TESTS
// ============================================================================

pub fn and_raw_test() {
  let expr = and_raw([raw("status = 'active'"), raw("age > 18")])
  raw_sql(expr) |> should.equal("(status = 'active' AND age > 18)")
}

pub fn or_raw_test() {
  let expr = or_raw([raw("status = 'active'"), raw("status = 'pending'")])
  raw_sql(expr) |> should.equal("(status = 'active' OR status = 'pending')")
}

pub fn not_raw_test() {
  let expr = not_raw(raw("deleted"))
  raw_sql(expr) |> should.equal("NOT (deleted)")
}

// ============================================================================
// COMPLEX EXPRESSION TESTS
// ============================================================================

pub fn complex_date_calculation_test() {
  // Example: created_at > NOW() - INTERVAL '7 days'
  let expr =
    column_raw_gt(user_created_at(), subtract(now(), interval(7, "days")))
  raw_sql(expr)
  |> should.equal("created_at > (NOW() - INTERVAL '7 days')")
}

pub fn nested_coalesce_test() {
  // COALESCE(nickname, name, 'Anonymous')
  let expr =
    coalesce_raw([raw("nickname"), raw("name"), string_literal("Anonymous")])
  raw_sql(expr) |> should.equal("COALESCE(nickname, name, 'Anonymous')")
}

pub fn combined_aggregate_test() {
  // Test combining multiple aggregates
  let count_expr = count_all()
  let sum_expr = sum(order_amount())

  raw_sql(count_expr) |> should.equal("COUNT(*)")
  raw_sql(sum_expr) |> should.equal("SUM(amount)")
}
