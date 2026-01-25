// cquill Query Builder
//
// This module provides the main query builder API - a typed DSL that builds
// abstract query structures (AST), not SQL strings.
//
// Queries are data: inspectable, serializable, and composable.
//
// Key principles:
// - Pipeline-friendly: All functions return Query for chaining
// - Composable: Reusable filter functions can be combined
// - Inspectable: Queries can be examined for testing
// - Pure: No I/O, no SQL generation
//
// SQL generation happens in the adapter layer, not here.

import cquill/query/ast.{
  type Condition, type Direction, type Join, type JoinType, type NullsOrder,
  type OrderBy, type Query, type Select, type SelectExpression, type Source,
  type Value, type Where, And, Asc, Between, BoolValue, CrossJoin, Desc, Eq,
  FloatValue, FullJoin, Gt, Gte, ILike, In, InnerJoin, IntValue, IsNotNull,
  IsNull, Join as JoinClause, LeftJoin, Like, ListValue, Lt, Lte, Not, NotEq,
  NotILike, NotIn, NotLike, NullValue, NullsDefault, NullsFirst, NullsLast, Or,
  OrderBy as OrderByClause, Query as QueryRecord, Raw, RightJoin, SelectAll,
  SelectExpr, SelectFields, StringValue, SubquerySource, TableSource,
  Where as WhereClause,
}
import cquill/schema.{type Schema}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// QUERY CONSTRUCTION
// ============================================================================

/// Create a new query from a schema.
/// This is the primary entry point for building queries.
///
/// ## Example
/// ```gleam
/// let query = query.from(user_schema)
/// ```
pub fn from(schema: Schema) -> Query(Schema) {
  let source = case schema.table_schema {
    Some(schema_name) ->
      TableSource(table: schema.source, schema_name: Some(schema_name))
    None -> TableSource(table: schema.source, schema_name: None)
  }

  QueryRecord(
    source:,
    select: SelectAll,
    wheres: [],
    order_bys: [],
    limit: None,
    offset: None,
    joins: [],
    distinct: False,
    group_bys: [],
    havings: [],
  )
}

/// Create a query from a table name directly (without schema metadata).
/// Useful for simple queries or testing.
pub fn from_table(table: String) -> Query(Nil) {
  ast.new(table)
}

/// Create a query from a qualified table name (schema.table).
pub fn from_qualified(schema_name: String, table: String) -> Query(Nil) {
  ast.new_qualified(schema_name, table)
}

/// Create a query from a subquery.
pub fn from_subquery(subquery: Query(a)) -> Query(Nil) {
  let source = SubquerySource(query: coerce_query(subquery))
  QueryRecord(
    source:,
    select: SelectAll,
    wheres: [],
    order_bys: [],
    limit: None,
    offset: None,
    joins: [],
    distinct: False,
    group_bys: [],
    havings: [],
  )
}

// Helper to coerce query schema parameter
fn coerce_query(query: Query(a)) -> Query(Nil) {
  let QueryRecord(
    source:,
    select:,
    wheres:,
    order_bys:,
    limit:,
    offset:,
    joins:,
    distinct:,
    group_bys:,
    havings:,
  ) = query
  QueryRecord(
    source:,
    select:,
    wheres:,
    order_bys:,
    limit:,
    offset:,
    joins:,
    distinct:,
    group_bys:,
    havings:,
  )
}

// ============================================================================
// SELECTION
// ============================================================================

/// Select specific fields by name.
///
/// ## Example
/// ```gleam
/// user_query
/// |> query.select(["id", "email", "name"])
/// ```
pub fn select(query: Query(s), fields: List(String)) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, select: SelectFields(fields))
}

/// Select all fields (SELECT *).
/// This is the default behavior.
pub fn select_all(query: Query(s)) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, select: SelectAll)
}

/// Select with expressions (for computed columns, aggregates).
pub fn select_expr(
  query: Query(s),
  expressions: List(SelectExpression),
) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, select: SelectExpr(expressions))
}

/// Add DISTINCT to the query.
pub fn distinct(query: Query(s)) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, distinct: True)
}

// ============================================================================
// WHERE CONDITIONS
// ============================================================================

/// Add a WHERE condition to the query.
/// Multiple where clauses are combined with AND.
///
/// ## Example
/// ```gleam
/// user_query
/// |> query.where(query.eq("active", True))
/// |> query.where(query.gt("age", 18))
/// ```
pub fn where(query: Query(s), condition: Condition) -> Query(s) {
  let QueryRecord(wheres:, ..) = query
  QueryRecord(..query, wheres: list.append(wheres, [WhereClause(condition)]))
}

/// Add an OR condition to the query.
/// This wraps existing conditions and the new one in an OR.
///
/// ## Example
/// ```gleam
/// user_query
/// |> query.where(query.eq("role", "admin"))
/// |> query.or_where(query.eq("role", "moderator"))
/// ```
pub fn or_where(query: Query(s), condition: Condition) -> Query(s) {
  let QueryRecord(wheres:, ..) = query
  case wheres {
    [] -> QueryRecord(..query, wheres: [WhereClause(condition)])
    existing -> {
      let existing_conditions = list.map(existing, fn(w) { w.condition })
      let combined = Or([And(existing_conditions), condition])
      QueryRecord(..query, wheres: [WhereClause(combined)])
    }
  }
}

/// Replace all WHERE conditions with the given condition.
pub fn where_replace(query: Query(s), condition: Condition) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, wheres: [WhereClause(condition)])
}

/// Clear all WHERE conditions.
pub fn where_clear(query: Query(s)) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, wheres: [])
}

// ============================================================================
// CONDITION BUILDERS
// ============================================================================

/// Create an equality condition: field = value
pub fn eq(field: String, value: a) -> Condition {
  Eq(field:, value: to_value(value))
}

/// Create a not-equal condition: field != value
pub fn not_eq(field: String, value: a) -> Condition {
  NotEq(field:, value: to_value(value))
}

/// Create a greater-than condition: field > value
pub fn gt(field: String, value: a) -> Condition {
  Gt(field:, value: to_value(value))
}

/// Create a greater-than-or-equal condition: field >= value
pub fn gte(field: String, value: a) -> Condition {
  Gte(field:, value: to_value(value))
}

/// Create a less-than condition: field < value
pub fn lt(field: String, value: a) -> Condition {
  Lt(field:, value: to_value(value))
}

/// Create a less-than-or-equal condition: field <= value
pub fn lte(field: String, value: a) -> Condition {
  Lte(field:, value: to_value(value))
}

/// Create an IN condition: field IN (values...)
pub fn is_in(field: String, values: List(a)) -> Condition {
  In(field:, values: list.map(values, to_value))
}

/// Alias for is_in - check if field value is in a list of values
/// This name may be more discoverable for users expecting `in_list`
pub fn in_list(field: String, values: List(a)) -> Condition {
  is_in(field, values)
}

/// Create a NOT IN condition: field NOT IN (values...)
pub fn is_not_in(field: String, values: List(a)) -> Condition {
  NotIn(field:, values: list.map(values, to_value))
}

/// Alias for is_not_in - check if field value is NOT in a list of values
pub fn not_in_list(field: String, values: List(a)) -> Condition {
  is_not_in(field, values)
}

/// Create a LIKE condition: field LIKE pattern
pub fn like(field: String, pattern: String) -> Condition {
  Like(field:, pattern:)
}

/// Create a NOT LIKE condition: field NOT LIKE pattern
pub fn not_like(field: String, pattern: String) -> Condition {
  NotLike(field:, pattern:)
}

/// Create a case-insensitive LIKE condition: field ILIKE pattern (PostgreSQL)
/// For case-insensitive pattern matching. The pattern uses SQL wildcards:
/// - `%` matches any sequence of characters
/// - `_` matches any single character
pub fn ilike(field: String, pattern: String) -> Condition {
  ILike(field:, pattern:)
}

/// Create a case-insensitive NOT LIKE condition: field NOT ILIKE pattern (PostgreSQL)
/// Negated case-insensitive pattern matching.
pub fn not_ilike(field: String, pattern: String) -> Condition {
  NotILike(field:, pattern:)
}

/// Create an IS NULL condition
pub fn is_null(field: String) -> Condition {
  IsNull(field)
}

/// Create an IS NOT NULL condition
pub fn is_not_null(field: String) -> Condition {
  IsNotNull(field)
}

/// Create a BETWEEN condition: field BETWEEN low AND high
pub fn between(field: String, low: a, high: a) -> Condition {
  Between(field:, low: to_value(low), high: to_value(high))
}

/// Combine conditions with AND
pub fn and(conditions: List(Condition)) -> Condition {
  And(conditions)
}

/// Combine conditions with OR
pub fn or(conditions: List(Condition)) -> Condition {
  Or(conditions)
}

/// Negate a condition
pub fn not(condition: Condition) -> Condition {
  Not(condition)
}

// ============================================================================
// VALUE CONVERSION
// ============================================================================

/// Convert a Gleam value to a query Value.
/// Uses dynamic dispatch to determine the type.
fn to_value(value: a) -> Value {
  // This uses Gleam's type inference to create the appropriate value
  // The compiler will optimize this based on the actual type
  case coerce_to_dynamic(value) {
    IntVal(i) -> IntValue(i)
    FloatVal(f) -> FloatValue(f)
    StringVal(s) -> StringValue(s)
    BoolVal(b) -> BoolValue(b)
    NilVal -> NullValue
    ListVal(vals) -> ListValue(list.map(vals, fn(v) { to_value_from_dyn(v) }))
    UnknownVal -> StringValue("")
  }
}

// Internal type for dynamic value handling
type DynValue {
  IntVal(Int)
  FloatVal(Float)
  StringVal(String)
  BoolVal(Bool)
  NilVal
  ListVal(List(DynValue))
  UnknownVal
}

@external(erlang, "cquill_ffi", "coerce_value")
@external(javascript, "../cquill_ffi.mjs", "coerce_value")
fn coerce_to_dynamic(value: a) -> DynValue

fn to_value_from_dyn(dyn: DynValue) -> Value {
  case dyn {
    IntVal(i) -> IntValue(i)
    FloatVal(f) -> FloatValue(f)
    StringVal(s) -> StringValue(s)
    BoolVal(b) -> BoolValue(b)
    NilVal -> NullValue
    ListVal(vals) -> ListValue(list.map(vals, to_value_from_dyn))
    UnknownVal -> StringValue("")
  }
}

// Type-safe value constructors for explicit use
/// Create an integer value
pub fn int_value(value: Int) -> Value {
  IntValue(value)
}

/// Create a float value
pub fn float_value(value: Float) -> Value {
  FloatValue(value)
}

/// Create a string value
pub fn string_value(value: String) -> Value {
  StringValue(value)
}

/// Create a boolean value
pub fn bool_value(value: Bool) -> Value {
  BoolValue(value)
}

/// Create a null value
pub fn null_value() -> Value {
  NullValue
}

/// Create a list value
pub fn list_value(values: List(Value)) -> Value {
  ListValue(values)
}

// Type-safe condition constructors using explicit Value types
/// Create an equality condition with explicit integer value
pub fn eq_int(field: String, value: Int) -> Condition {
  Eq(field:, value: IntValue(value))
}

/// Create an equality condition with explicit string value
pub fn eq_string(field: String, value: String) -> Condition {
  Eq(field:, value: StringValue(value))
}

/// Create an equality condition with explicit boolean value
pub fn eq_bool(field: String, value: Bool) -> Condition {
  Eq(field:, value: BoolValue(value))
}

/// Create a greater-than condition with explicit integer value
pub fn gt_int(field: String, value: Int) -> Condition {
  Gt(field:, value: IntValue(value))
}

/// Create a less-than condition with explicit integer value
pub fn lt_int(field: String, value: Int) -> Condition {
  Lt(field:, value: IntValue(value))
}

/// Create a greater-than-or-equal condition with explicit integer value
pub fn gte_int(field: String, value: Int) -> Condition {
  Gte(field:, value: IntValue(value))
}

/// Create a less-than-or-equal condition with explicit integer value
pub fn lte_int(field: String, value: Int) -> Condition {
  Lte(field:, value: IntValue(value))
}

/// Create an IN condition with explicit integer values
pub fn in_ints(field: String, values: List(Int)) -> Condition {
  In(field:, values: list.map(values, IntValue))
}

/// Create an IN condition with explicit string values
pub fn in_strings(field: String, values: List(String)) -> Condition {
  In(field:, values: list.map(values, StringValue))
}

// ============================================================================
// ORDER BY
// ============================================================================

/// Add an ORDER BY clause to the query.
///
/// ## Example
/// ```gleam
/// user_query
/// |> query.order_by("created_at", ast.Desc)
/// ```
pub fn order_by(
  query: Query(s),
  field: String,
  direction: Direction,
) -> Query(s) {
  let QueryRecord(order_bys:, ..) = query
  let new_order = OrderByClause(field:, direction:, nulls: NullsDefault)
  QueryRecord(..query, order_bys: list.append(order_bys, [new_order]))
}

/// Add an ORDER BY ASC clause.
pub fn order_by_asc(query: Query(s), field: String) -> Query(s) {
  order_by(query, field, Asc)
}

/// Add an ORDER BY DESC clause.
pub fn order_by_desc(query: Query(s), field: String) -> Query(s) {
  order_by(query, field, Desc)
}

/// Add an ORDER BY clause with null ordering.
pub fn order_by_with_nulls(
  query: Query(s),
  field: String,
  direction: Direction,
  nulls: NullsOrder,
) -> Query(s) {
  let QueryRecord(order_bys:, ..) = query
  let new_order = OrderByClause(field:, direction:, nulls:)
  QueryRecord(..query, order_bys: list.append(order_bys, [new_order]))
}

/// Clear all ORDER BY clauses.
pub fn order_by_clear(query: Query(s)) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, order_bys: [])
}

// ============================================================================
// PAGINATION
// ============================================================================

/// Set the LIMIT for the query.
///
/// ## Example
/// ```gleam
/// user_query
/// |> query.limit(10)
/// ```
pub fn limit(query: Query(s), count: Int) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, limit: Some(count))
}

/// Set the OFFSET for the query.
///
/// ## Example
/// ```gleam
/// user_query
/// |> query.offset(20)
/// |> query.limit(10)
/// ```
pub fn offset(query: Query(s), count: Int) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, offset: Some(count))
}

/// Set both LIMIT and OFFSET for pagination.
///
/// ## Example
/// ```gleam
/// // Page 3 with 10 items per page
/// user_query
/// |> query.paginate(page: 3, per_page: 10)
/// ```
pub fn paginate(
  query: Query(s),
  page page: Int,
  per_page per_page: Int,
) -> Query(s) {
  let offset_val = { page - 1 } * per_page
  query
  |> limit(per_page)
  |> offset(offset_val)
}

/// Remove LIMIT and OFFSET.
pub fn no_pagination(query: Query(s)) -> Query(s) {
  let QueryRecord(..) = query
  QueryRecord(..query, limit: None, offset: None)
}

// ============================================================================
// JOINS
// ============================================================================

/// Add an INNER JOIN to the query.
pub fn join(query: Query(s), table: String, on on: Condition) -> Query(s) {
  add_join(query, InnerJoin, table, None, on)
}

/// Add an INNER JOIN with an alias.
pub fn join_as(
  query: Query(s),
  table: String,
  alias alias: String,
  on on: Condition,
) -> Query(s) {
  add_join(query, InnerJoin, table, Some(alias), on)
}

/// Add a LEFT JOIN to the query.
pub fn left_join(query: Query(s), table: String, on on: Condition) -> Query(s) {
  add_join(query, LeftJoin, table, None, on)
}

/// Add a LEFT JOIN with an alias.
pub fn left_join_as(
  query: Query(s),
  table: String,
  alias alias: String,
  on on: Condition,
) -> Query(s) {
  add_join(query, LeftJoin, table, Some(alias), on)
}

/// Add a RIGHT JOIN to the query.
pub fn right_join(query: Query(s), table: String, on on: Condition) -> Query(s) {
  add_join(query, RightJoin, table, None, on)
}

/// Add a FULL OUTER JOIN to the query.
pub fn full_join(query: Query(s), table: String, on on: Condition) -> Query(s) {
  add_join(query, FullJoin, table, None, on)
}

/// Add a CROSS JOIN to the query.
pub fn cross_join(query: Query(s), table: String) -> Query(s) {
  // Cross join doesn't need an ON condition, use a dummy True condition
  add_join(query, CrossJoin, table, None, Eq("1", IntValue(1)))
}

fn add_join(
  query: Query(s),
  join_type: JoinType,
  table: String,
  table_alias: Option(String),
  on: Condition,
) -> Query(s) {
  let QueryRecord(joins:, ..) = query
  let new_join = JoinClause(join_type:, table:, table_alias:, on:)
  QueryRecord(..query, joins: list.append(joins, [new_join]))
}

// ============================================================================
// GROUP BY / HAVING
// ============================================================================

/// Add a GROUP BY field.
pub fn group_by(query: Query(s), field: String) -> Query(s) {
  let QueryRecord(group_bys:, ..) = query
  QueryRecord(..query, group_bys: list.append(group_bys, [field]))
}

/// Add multiple GROUP BY fields.
pub fn group_by_fields(query: Query(s), fields: List(String)) -> Query(s) {
  let QueryRecord(group_bys:, ..) = query
  QueryRecord(..query, group_bys: list.append(group_bys, fields))
}

/// Add a HAVING condition.
pub fn having(query: Query(s), condition: Condition) -> Query(s) {
  let QueryRecord(havings:, ..) = query
  QueryRecord(..query, havings: list.append(havings, [WhereClause(condition)]))
}

// ============================================================================
// QUERY INSPECTION
// ============================================================================

/// Get all WHERE conditions from the query.
pub fn get_conditions(query: Query(s)) -> List(Condition) {
  let QueryRecord(wheres:, ..) = query
  list.map(wheres, fn(w) { w.condition })
}

/// Get all ORDER BY clauses from the query.
pub fn get_order_bys(query: Query(s)) -> List(OrderBy) {
  let QueryRecord(order_bys:, ..) = query
  order_bys
}

/// Get the SELECT clause.
pub fn get_select(query: Query(s)) -> Select {
  let QueryRecord(select:, ..) = query
  select
}

/// Get the LIMIT value.
pub fn get_limit(query: Query(s)) -> Option(Int) {
  let QueryRecord(limit:, ..) = query
  limit
}

/// Get the OFFSET value.
pub fn get_offset(query: Query(s)) -> Option(Int) {
  let QueryRecord(offset:, ..) = query
  offset
}

/// Get all JOIN clauses.
pub fn get_joins(query: Query(s)) -> List(Join) {
  let QueryRecord(joins:, ..) = query
  joins
}

/// Get the source of the query.
pub fn get_source(query: Query(s)) -> Source {
  let QueryRecord(source:, ..) = query
  source
}

/// Check if the query has any WHERE conditions.
pub fn has_conditions(query: Query(s)) -> Bool {
  let QueryRecord(wheres:, ..) = query
  !list.is_empty(wheres)
}

/// Check if the query has any ORDER BY clauses.
pub fn has_order_by(query: Query(s)) -> Bool {
  let QueryRecord(order_bys:, ..) = query
  !list.is_empty(order_bys)
}

/// Check if the query has pagination.
pub fn has_pagination(query: Query(s)) -> Bool {
  let QueryRecord(limit:, offset:, ..) = query
  option.is_some(limit) || option.is_some(offset)
}

/// Check if the query is DISTINCT.
pub fn is_distinct(query: Query(s)) -> Bool {
  let QueryRecord(distinct:, ..) = query
  distinct
}

/// Count the number of WHERE conditions.
pub fn condition_count(query: Query(s)) -> Int {
  let QueryRecord(wheres:, ..) = query
  list.length(wheres)
}

// ============================================================================
// DEBUG / STRING REPRESENTATION
// ============================================================================

/// Convert a query to a debug string representation.
/// This is for debugging and testing, NOT for SQL generation.
pub fn to_debug_string(query: Query(s)) -> String {
  let QueryRecord(
    source:,
    select:,
    wheres:,
    order_bys:,
    limit:,
    offset:,
    joins:,
    distinct:,
    group_bys:,
    havings:,
  ) = query

  let parts = [
    "Query {",
    "  source: " <> source_to_string(source),
    "  select: " <> select_to_string(select),
    "  distinct: " <> bool_to_string(distinct),
    "  wheres: [" <> string.join(list.map(wheres, where_to_string), ", ") <> "]",
    "  joins: [" <> string.join(list.map(joins, join_to_string), ", ") <> "]",
    "  group_bys: [" <> string.join(group_bys, ", ") <> "]",
    "  havings: ["
      <> string.join(list.map(havings, where_to_string), ", ")
      <> "]",
    "  order_bys: ["
      <> string.join(list.map(order_bys, order_by_to_string), ", ")
      <> "]",
    "  limit: " <> option_int_to_string(limit),
    "  offset: " <> option_int_to_string(offset),
    "}",
  ]

  string.join(parts, "\n")
}

fn source_to_string(source: Source) -> String {
  case source {
    TableSource(table, None) -> table
    TableSource(table, Some(schema)) -> schema <> "." <> table
    SubquerySource(_) -> "(subquery)"
  }
}

fn select_to_string(select: Select) -> String {
  case select {
    SelectAll -> "*"
    SelectFields(fields) -> string.join(fields, ", ")
    SelectExpr(_) -> "(expressions)"
  }
}

fn where_to_string(w: Where) -> String {
  condition_to_string(w.condition)
}

fn condition_to_string(cond: Condition) -> String {
  case cond {
    Eq(field, value) -> field <> " = " <> value_to_string(value)
    NotEq(field, value) -> field <> " != " <> value_to_string(value)
    Gt(field, value) -> field <> " > " <> value_to_string(value)
    Gte(field, value) -> field <> " >= " <> value_to_string(value)
    Lt(field, value) -> field <> " < " <> value_to_string(value)
    Lte(field, value) -> field <> " <= " <> value_to_string(value)
    In(field, values) ->
      field
      <> " IN ("
      <> string.join(list.map(values, value_to_string), ", ")
      <> ")"
    NotIn(field, values) ->
      field
      <> " NOT IN ("
      <> string.join(list.map(values, value_to_string), ", ")
      <> ")"
    Like(field, pattern) -> field <> " LIKE '" <> pattern <> "'"
    NotLike(field, pattern) -> field <> " NOT LIKE '" <> pattern <> "'"
    ILike(field, pattern) -> field <> " ILIKE '" <> pattern <> "'"
    NotILike(field, pattern) -> field <> " NOT ILIKE '" <> pattern <> "'"
    IsNull(field) -> field <> " IS NULL"
    IsNotNull(field) -> field <> " IS NOT NULL"
    Between(field, low, high) ->
      field
      <> " BETWEEN "
      <> value_to_string(low)
      <> " AND "
      <> value_to_string(high)
    And(conditions) ->
      "("
      <> string.join(list.map(conditions, condition_to_string), " AND ")
      <> ")"
    Or(conditions) ->
      "("
      <> string.join(list.map(conditions, condition_to_string), " OR ")
      <> ")"
    Not(condition) -> "NOT (" <> condition_to_string(condition) <> ")"
    Raw(sql, _) -> "RAW: " <> sql
  }
}

fn value_to_string(value: Value) -> String {
  case value {
    IntValue(i) -> int.to_string(i)
    FloatValue(f) -> string.inspect(f)
    StringValue(s) -> "'" <> s <> "'"
    BoolValue(b) -> bool_to_string(b)
    NullValue -> "NULL"
    ast.ParamValue(p) -> "$" <> int.to_string(p)
    ListValue(values) ->
      "[" <> string.join(list.map(values, value_to_string), ", ") <> "]"
  }
}

fn order_by_to_string(ob: OrderBy) -> String {
  let dir = case ob.direction {
    Asc -> "ASC"
    Desc -> "DESC"
  }
  let nulls = case ob.nulls {
    NullsDefault -> ""
    NullsFirst -> " NULLS FIRST"
    NullsLast -> " NULLS LAST"
  }
  ob.field <> " " <> dir <> nulls
}

fn join_to_string(j: Join) -> String {
  let join_type = case j.join_type {
    InnerJoin -> "INNER JOIN"
    LeftJoin -> "LEFT JOIN"
    RightJoin -> "RIGHT JOIN"
    FullJoin -> "FULL JOIN"
    CrossJoin -> "CROSS JOIN"
  }
  let table = case j.table_alias {
    Some(alias) -> j.table <> " AS " <> alias
    None -> j.table
  }
  join_type <> " " <> table <> " ON " <> condition_to_string(j.on)
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn option_int_to_string(opt: Option(Int)) -> String {
  case opt {
    Some(i) -> int.to_string(i)
    None -> "None"
  }
}

// ============================================================================
// RE-EXPORTS FROM AST
// ============================================================================

// Re-export direction types for convenience
pub const asc = Asc

pub const desc = Desc

// Re-export nulls order types
pub const nulls_first = NullsFirst

pub const nulls_last = NullsLast

pub const nulls_default = NullsDefault
