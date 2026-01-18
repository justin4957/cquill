// cquill Raw SQL Escape Hatch
//
// This module provides escape hatches for raw SQL when the query builder
// can't express something. Use with caution as raw SQL bypasses type checking.
//
// WARNING: Raw SQL bypasses compile-time type checking.
// Ensure your SQL is valid and properly parameterized to prevent SQL injection.
//
// ## Safe Usage (with parameters):
// ```gleam
// raw_with_params("email = $1", [StringValue(user_input)])
// ```
//
// ## Unsafe Usage (DO NOT DO THIS):
// ```gleam
// // SQL injection risk!
// raw("email = '" <> user_input <> "'")
// ```

import cquill/query/ast
import cquill/typed/table.{type Column, column_name}
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// RAW EXPRESSION TYPE
// ============================================================================

/// A raw SQL expression with optional parameters.
///
/// WARNING: Raw SQL bypasses compile-time type checking.
/// Ensure your SQL is valid and properly parameterized.
///
/// ## Example
/// ```gleam
/// raw("NOW()")
/// raw_with_params("created_at > $1", [StringValue("2024-01-01")])
/// ```
pub opaque type RawExpr {
  RawExpr(sql: String, params: List(ast.Value))
}

/// Create a raw SQL expression without parameters.
///
/// WARNING: This bypasses type safety. Use for database functions
/// and expressions that don't accept user input.
///
/// ## Example
/// ```gleam
/// raw("NOW()")
/// raw("RANDOM()")
/// ```
pub fn raw(sql: String) -> RawExpr {
  RawExpr(sql: sql, params: [])
}

/// Create a raw SQL expression with parameters.
/// Always prefer this over string concatenation to prevent SQL injection.
///
/// ## Example
/// ```gleam
/// raw_with_params("email = $1", [StringValue(user_input)])
/// raw_with_params("created_at > $1 AND status = $2", [
///   StringValue("2024-01-01"),
///   StringValue("active"),
/// ])
/// ```
pub fn raw_with_params(sql: String, params: List(ast.Value)) -> RawExpr {
  RawExpr(sql: sql, params: params)
}

/// Extract the SQL string from a RawExpr.
pub fn raw_sql(expr: RawExpr) -> String {
  let RawExpr(sql: sql, params: _) = expr
  sql
}

/// Extract the parameters from a RawExpr.
pub fn raw_params(expr: RawExpr) -> List(ast.Value) {
  let RawExpr(sql: _, params: params) = expr
  params
}

/// Convert a RawExpr to an AST Condition for use in WHERE clauses.
pub fn raw_to_condition(expr: RawExpr) -> ast.Condition {
  let RawExpr(sql: sql, params: params) = expr
  ast.Raw(sql: sql, params: params)
}

/// Convert a RawExpr to an AST SelectExpression for use in SELECT clauses.
pub fn raw_to_select_expr(
  expr: RawExpr,
  alias: Option(String),
) -> ast.SelectExpression {
  let RawExpr(sql: sql, params: _) = expr
  // For select expressions, we embed the SQL as a function expression
  // The compiler will handle rendering this appropriately
  ast.SelectExpression(
    expr: ast.FunctionExpr(name: sql, args: []),
    alias: alias,
  )
}

/// Convert a RawExpr to an AST OrderBy for use in ORDER BY clauses.
pub fn raw_to_order_by(expr: RawExpr, direction: ast.Direction) -> ast.OrderBy {
  let RawExpr(sql: sql, params: _) = expr
  ast.OrderBy(field: sql, direction: direction, nulls: ast.NullsDefault)
}

// ============================================================================
// FRAGMENT HELPERS - Common SQL patterns
// ============================================================================

/// NOW() - Current timestamp
///
/// ## Example
/// ```gleam
/// select_raw(query, [now()])
/// where_raw(query, raw_with_params("created_at > $1 - $2", [
///   now_as_value(),
///   interval(7, "days"),
/// ]))
/// ```
pub fn now() -> RawExpr {
  raw("NOW()")
}

/// CURRENT_TIMESTAMP - Current timestamp (alternative to NOW())
pub fn current_timestamp() -> RawExpr {
  raw("CURRENT_TIMESTAMP")
}

/// CURRENT_DATE - Current date without time
pub fn current_date() -> RawExpr {
  raw("CURRENT_DATE")
}

/// CURRENT_TIME - Current time without date
pub fn current_time() -> RawExpr {
  raw("CURRENT_TIME")
}

/// Generate an INTERVAL expression.
///
/// ## Example
/// ```gleam
/// interval(7, "days")   // INTERVAL '7 days'
/// interval(1, "hour")   // INTERVAL '1 hour'
/// interval(30, "minutes")  // INTERVAL '30 minutes'
/// ```
pub fn interval(amount: Int, unit: String) -> RawExpr {
  raw("INTERVAL '" <> int_to_string(amount) <> " " <> unit <> "'")
}

/// COALESCE - Return first non-null value.
/// Uses column name for type safety reference.
///
/// ## Example
/// ```gleam
/// coalesce_column(user_nickname, "Anonymous")
/// // COALESCE(nickname, 'Anonymous')
/// ```
pub fn coalesce_column(
  column: Column(t, Option(String)),
  default: String,
) -> RawExpr {
  raw(
    "COALESCE("
    <> column_name(column)
    <> ", '"
    <> escape_string(default)
    <> "')",
  )
}

/// COALESCE for integer columns.
pub fn coalesce_int_column(
  column: Column(t, Option(Int)),
  default: Int,
) -> RawExpr {
  raw(
    "COALESCE(" <> column_name(column) <> ", " <> int_to_string(default) <> ")",
  )
}

/// COALESCE with raw expressions.
///
/// ## Example
/// ```gleam
/// coalesce_raw([raw("nickname"), raw("'Anonymous'")])
/// ```
pub fn coalesce_raw(expressions: List(RawExpr)) -> RawExpr {
  let sqls = list.map(expressions, raw_sql)
  raw("COALESCE(" <> join_with_comma(sqls) <> ")")
}

/// NULLIF - Return NULL if two expressions are equal.
///
/// ## Example
/// ```gleam
/// nullif_column(status, "deleted")
/// // NULLIF(status, 'deleted')
/// ```
pub fn nullif_column(column: Column(t, String), value: String) -> RawExpr {
  raw("NULLIF(" <> column_name(column) <> ", '" <> escape_string(value) <> "')")
}

/// CAST - Cast an expression to a different type.
///
/// ## Example
/// ```gleam
/// cast(raw("'123'"), "INTEGER")  // CAST('123' AS INTEGER)
/// cast(raw("created_at"), "DATE")  // CAST(created_at AS DATE)
/// ```
pub fn cast(expr: RawExpr, to_type: String) -> RawExpr {
  raw("CAST(" <> raw_sql(expr) <> " AS " <> to_type <> ")")
}

/// CAST a column to a different type.
pub fn cast_column(column: Column(t, a), to_type: String) -> RawExpr {
  raw("CAST(" <> column_name(column) <> " AS " <> to_type <> ")")
}

/// RANDOM() - Generate a random value (useful for random ordering).
///
/// ## Example
/// ```gleam
/// order_by_raw(query, random(), Asc)  // Random order
/// ```
pub fn random() -> RawExpr {
  raw("RANDOM()")
}

/// COUNT(*) - Count all rows.
pub fn count_all() -> RawExpr {
  raw("COUNT(*)")
}

/// COUNT(column) - Count non-null values in a column.
pub fn count_column(column: Column(t, a)) -> RawExpr {
  raw("COUNT(" <> column_name(column) <> ")")
}

/// COUNT(DISTINCT column) - Count distinct non-null values.
pub fn count_distinct(column: Column(t, a)) -> RawExpr {
  raw("COUNT(DISTINCT " <> column_name(column) <> ")")
}

/// SUM(column) - Sum values in a column.
pub fn sum(column: Column(t, a)) -> RawExpr {
  raw("SUM(" <> column_name(column) <> ")")
}

/// AVG(column) - Average values in a column.
pub fn avg(column: Column(t, a)) -> RawExpr {
  raw("AVG(" <> column_name(column) <> ")")
}

/// MIN(column) - Minimum value in a column.
pub fn min(column: Column(t, a)) -> RawExpr {
  raw("MIN(" <> column_name(column) <> ")")
}

/// MAX(column) - Maximum value in a column.
pub fn max(column: Column(t, a)) -> RawExpr {
  raw("MAX(" <> column_name(column) <> ")")
}

/// COUNT(*) OVER () - Window function for total count.
/// Useful for pagination to get total count alongside results.
///
/// ## Example
/// ```gleam
/// select_raw(query, [count_over_with_alias("total_count")])
/// ```
pub fn count_over() -> RawExpr {
  raw("COUNT(*) OVER ()")
}

/// COUNT(*) OVER () with an alias.
pub fn count_over_with_alias(alias: String) -> RawExpr {
  raw("COUNT(*) OVER () AS " <> alias)
}

/// ROW_NUMBER() OVER (ORDER BY column).
pub fn row_number_over(
  column: Column(t, a),
  direction: ast.Direction,
) -> RawExpr {
  let dir_str = case direction {
    ast.Asc -> "ASC"
    ast.Desc -> "DESC"
  }
  raw(
    "ROW_NUMBER() OVER (ORDER BY "
    <> column_name(column)
    <> " "
    <> dir_str
    <> ")",
  )
}

/// LOWER(column) - Lowercase string.
pub fn lower(column: Column(t, String)) -> RawExpr {
  raw("LOWER(" <> column_name(column) <> ")")
}

/// UPPER(column) - Uppercase string.
pub fn upper(column: Column(t, String)) -> RawExpr {
  raw("UPPER(" <> column_name(column) <> ")")
}

/// TRIM(column) - Remove leading/trailing whitespace.
pub fn trim(column: Column(t, String)) -> RawExpr {
  raw("TRIM(" <> column_name(column) <> ")")
}

/// LENGTH(column) - String length.
pub fn length(column: Column(t, String)) -> RawExpr {
  raw("LENGTH(" <> column_name(column) <> ")")
}

/// CONCAT - Concatenate multiple expressions.
///
/// ## Example
/// ```gleam
/// concat([raw("first_name"), raw("' '"), raw("last_name")])
/// ```
pub fn concat(expressions: List(RawExpr)) -> RawExpr {
  let sqls = list.map(expressions, raw_sql)
  raw("CONCAT(" <> join_with_comma(sqls) <> ")")
}

/// Concatenate columns with a separator.
///
/// ## Example
/// ```gleam
/// concat_columns_with_separator([first_name, last_name], " ")
/// // first_name || ' ' || last_name
/// ```
pub fn concat_columns_with_separator(
  columns: List(Column(t, String)),
  separator: String,
) -> RawExpr {
  let names = list.map(columns, column_name)
  let joined = join_with(names, " || '" <> escape_string(separator) <> "' || ")
  raw(joined)
}

/// EXTRACT(field FROM column) - Extract part of a timestamp.
///
/// ## Example
/// ```gleam
/// extract("year", created_at)   // EXTRACT(year FROM created_at)
/// extract("month", created_at)  // EXTRACT(month FROM created_at)
/// ```
pub fn extract(field: String, column: Column(t, a)) -> RawExpr {
  raw("EXTRACT(" <> field <> " FROM " <> column_name(column) <> ")")
}

/// DATE_TRUNC - Truncate timestamp to specified precision.
///
/// ## Example
/// ```gleam
/// date_trunc("month", created_at)  // DATE_TRUNC('month', created_at)
/// ```
pub fn date_trunc(precision: String, column: Column(t, a)) -> RawExpr {
  raw("DATE_TRUNC('" <> precision <> "', " <> column_name(column) <> ")")
}

/// AGE - Calculate age between timestamps.
///
/// ## Example
/// ```gleam
/// age_from_now(birth_date)  // AGE(birth_date)
/// ```
pub fn age_from_now(column: Column(t, a)) -> RawExpr {
  raw("AGE(" <> column_name(column) <> ")")
}

/// Generate a CASE expression.
///
/// ## Example
/// ```gleam
/// case_when([
///   #(raw("status = 'active'"), raw("'Active'")),
///   #(raw("status = 'pending'"), raw("'Pending'")),
/// ], raw("'Unknown'"))
/// ```
pub fn case_when(
  conditions: List(#(RawExpr, RawExpr)),
  else_expr: RawExpr,
) -> RawExpr {
  let when_clauses =
    list.map(conditions, fn(pair) {
      let #(condition, result) = pair
      "WHEN " <> raw_sql(condition) <> " THEN " <> raw_sql(result)
    })
  let sql =
    "CASE "
    <> join_with_space(when_clauses)
    <> " ELSE "
    <> raw_sql(else_expr)
    <> " END"
  raw(sql)
}

/// EXISTS subquery check.
///
/// ## Example
/// ```gleam
/// exists(raw("SELECT 1 FROM orders WHERE orders.user_id = users.id"))
/// ```
pub fn exists(subquery: RawExpr) -> RawExpr {
  raw("EXISTS (" <> raw_sql(subquery) <> ")")
}

/// NOT EXISTS subquery check.
pub fn not_exists(subquery: RawExpr) -> RawExpr {
  raw("NOT EXISTS (" <> raw_sql(subquery) <> ")")
}

/// IN with a subquery.
///
/// ## Example
/// ```gleam
/// in_subquery(user_id, raw("SELECT id FROM active_users"))
/// ```
pub fn in_subquery(column: Column(t, a), subquery: RawExpr) -> RawExpr {
  raw(column_name(column) <> " IN (" <> raw_sql(subquery) <> ")")
}

/// NOT IN with a subquery.
pub fn not_in_subquery(column: Column(t, a), subquery: RawExpr) -> RawExpr {
  raw(column_name(column) <> " NOT IN (" <> raw_sql(subquery) <> ")")
}

// ============================================================================
// COMPARISON HELPERS
// ============================================================================

/// Create a raw comparison between a column and a raw expression.
///
/// ## Example
/// ```gleam
/// column_raw_eq(created_at, now())
/// // created_at = NOW()
/// ```
pub fn column_raw_eq(column: Column(t, a), expr: RawExpr) -> RawExpr {
  raw(column_name(column) <> " = " <> raw_sql(expr))
}

/// Column greater than raw expression.
pub fn column_raw_gt(column: Column(t, a), expr: RawExpr) -> RawExpr {
  raw(column_name(column) <> " > " <> raw_sql(expr))
}

/// Column greater than or equal to raw expression.
pub fn column_raw_gte(column: Column(t, a), expr: RawExpr) -> RawExpr {
  raw(column_name(column) <> " >= " <> raw_sql(expr))
}

/// Column less than raw expression.
pub fn column_raw_lt(column: Column(t, a), expr: RawExpr) -> RawExpr {
  raw(column_name(column) <> " < " <> raw_sql(expr))
}

/// Column less than or equal to raw expression.
pub fn column_raw_lte(column: Column(t, a), expr: RawExpr) -> RawExpr {
  raw(column_name(column) <> " <= " <> raw_sql(expr))
}

/// Column between two raw expressions.
pub fn column_raw_between(
  column: Column(t, a),
  low: RawExpr,
  high: RawExpr,
) -> RawExpr {
  raw(
    column_name(column)
    <> " BETWEEN "
    <> raw_sql(low)
    <> " AND "
    <> raw_sql(high),
  )
}

// ============================================================================
// ARITHMETIC HELPERS
// ============================================================================

/// Add two raw expressions.
pub fn add(left: RawExpr, right: RawExpr) -> RawExpr {
  raw("(" <> raw_sql(left) <> " + " <> raw_sql(right) <> ")")
}

/// Subtract right from left.
pub fn subtract(left: RawExpr, right: RawExpr) -> RawExpr {
  raw("(" <> raw_sql(left) <> " - " <> raw_sql(right) <> ")")
}

/// Multiply two expressions.
pub fn multiply(left: RawExpr, right: RawExpr) -> RawExpr {
  raw("(" <> raw_sql(left) <> " * " <> raw_sql(right) <> ")")
}

/// Divide left by right.
pub fn divide(left: RawExpr, right: RawExpr) -> RawExpr {
  raw("(" <> raw_sql(left) <> " / " <> raw_sql(right) <> ")")
}

/// Modulo operation.
pub fn modulo(left: RawExpr, right: RawExpr) -> RawExpr {
  raw("(" <> raw_sql(left) <> " % " <> raw_sql(right) <> ")")
}

/// Create a raw expression from a column name.
pub fn column_expr(column: Column(t, a)) -> RawExpr {
  raw(column_name(column))
}

/// Create a raw expression from a literal integer.
pub fn int_literal(value: Int) -> RawExpr {
  raw(int_to_string(value))
}

/// Create a raw expression from a literal float.
pub fn float_literal(value: Float) -> RawExpr {
  raw(float_to_string(value))
}

/// Create a raw expression from a literal string.
/// The string is properly escaped and quoted.
pub fn string_literal(value: String) -> RawExpr {
  raw("'" <> escape_string(value) <> "'")
}

/// Create a raw expression for boolean TRUE.
pub fn true_literal() -> RawExpr {
  raw("TRUE")
}

/// Create a raw expression for boolean FALSE.
pub fn false_literal() -> RawExpr {
  raw("FALSE")
}

/// Create a raw expression for NULL.
pub fn null_literal() -> RawExpr {
  raw("NULL")
}

// ============================================================================
// LOGICAL OPERATORS
// ============================================================================

/// AND multiple raw conditions.
pub fn and_raw(conditions: List(RawExpr)) -> RawExpr {
  let sqls = list.map(conditions, raw_sql)
  raw("(" <> join_with(sqls, " AND ") <> ")")
}

/// OR multiple raw conditions.
pub fn or_raw(conditions: List(RawExpr)) -> RawExpr {
  let sqls = list.map(conditions, raw_sql)
  raw("(" <> join_with(sqls, " OR ") <> ")")
}

/// NOT a raw condition.
pub fn not_raw(condition: RawExpr) -> RawExpr {
  raw("NOT (" <> raw_sql(condition) <> ")")
}

// ============================================================================
// HELPER FUNCTIONS (INTERNAL)
// ============================================================================

fn int_to_string(i: Int) -> String {
  case i < 0 {
    True -> "-" <> positive_int_to_string(-i)
    False -> positive_int_to_string(i)
  }
}

fn positive_int_to_string(i: Int) -> String {
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ -> positive_int_to_string(i / 10) <> positive_int_to_string(i % 10)
  }
}

fn float_to_string(f: Float) -> String {
  do_float_to_string(f)
}

@external(erlang, "erlang", "float_to_binary")
@external(javascript, "../../../cquill_ffi.mjs", "float_to_string")
fn do_float_to_string(f: Float) -> String

fn escape_string(s: String) -> String {
  // Basic SQL string escaping - replace single quotes with two single quotes
  do_escape_string(s)
}

@external(erlang, "cquill_ffi", "escape_sql_string")
@external(javascript, "../../../cquill_ffi.mjs", "escape_sql_string")
fn do_escape_string(s: String) -> String

fn join_with_comma(strings: List(String)) -> String {
  join_with(strings, ", ")
}

fn join_with_space(strings: List(String)) -> String {
  join_with(strings, " ")
}

fn join_with(strings: List(String), separator: String) -> String {
  case strings {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> separator <> join_with(rest, separator)
  }
}
