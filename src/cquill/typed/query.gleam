// cquill Phantom-Typed Query Module
//
// This module provides a type-safe query builder that uses phantom types to
// ensure compile-time validation of queries. It wraps the existing query AST
// types but adds phantom type parameters to prevent invalid column usage.
//
// Key features:
// - Compile-time verification that columns belong to the queried table
// - Type-safe joins that compose table types
// - Conditions constrained to the query's table scope
//
// Example usage:
// ```gleam
// // This compiles - email belongs to UserTable
// typed_from(users)
// |> typed_where(typed_eq(email, "test@example.com"))
//
// // This fails to compile - post_title doesn't belong to UserTable
// typed_from(users)
// |> typed_where(typed_eq(post_title, "Hello"))
// ```

import cquill/query/ast
import cquill/typed/table.{
  type AliasedTable, type Column, type Join2, type Join3, type Table,
  aliased_table_alias, aliased_table_qualified_name, column_name,
  column_qualified_name, table_name, table_qualified_name, table_schema_name,
}
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// TYPED QUERY TYPE
// ============================================================================

/// A query carrying its source table type as a phantom parameter.
/// The phantom type `table_type` ensures that only columns belonging to
/// this table (or joined tables) can be used in the query.
pub opaque type TypedQuery(table_type) {
  TypedQuery(inner: ast.Query(Nil))
}

/// Extract the underlying AST query for adapter use.
/// This is used when passing the query to the adapter layer.
pub fn to_ast(query: TypedQuery(t)) -> ast.Query(Nil) {
  let TypedQuery(inner: inner) = query
  inner
}

// ============================================================================
// TYPED CONDITION TYPE
// ============================================================================

/// A condition constrained to a specific table scope.
/// The phantom type ensures conditions can only use columns from the
/// allowed table(s).
pub opaque type TypedCondition(table_type) {
  TypedCondition(inner: ast.Condition)
}

/// Extract the underlying AST condition.
pub fn condition_to_ast(condition: TypedCondition(t)) -> ast.Condition {
  let TypedCondition(inner: inner) = condition
  inner
}

// ============================================================================
// QUERY CONSTRUCTION
// ============================================================================

/// Create a new typed query from a Table.
/// This is the primary entry point for building type-safe queries.
///
/// ## Example
/// ```gleam
/// let query = typed_from(users)
/// ```
pub fn typed_from(table: Table(t)) -> TypedQuery(t) {
  let source =
    ast.TableSource(
      table: table_name(table),
      schema_name: case table_schema_name(table) {
        "public" -> None
        schema -> Some(schema)
      },
    )

  TypedQuery(
    inner: ast.Query(
      source: source,
      select: ast.SelectAll,
      wheres: [],
      order_bys: [],
      limit: None,
      offset: None,
      joins: [],
      distinct: False,
      group_bys: [],
      havings: [],
    ),
  )
}

// ============================================================================
// SELECTION
// ============================================================================

/// Select specific columns.
/// The columns must belong to the query's table type.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_select([id, email, name])
/// ```
pub fn typed_select(
  query: TypedQuery(t),
  columns: List(Column(t, a)),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let fields = list.map(columns, column_name)
  TypedQuery(inner: ast.Query(..inner, select: ast.SelectFields(fields)))
}

/// Select all columns (SELECT *).
pub fn typed_select_all(query: TypedQuery(t)) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, select: ast.SelectAll))
}

/// Select columns by name.
/// Use this when you need to select columns with different value types.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_select_by_names(["id", "email", "name"])
/// ```
pub fn typed_select_by_names(
  query: TypedQuery(t),
  names: List(String),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, select: ast.SelectFields(names)))
}

/// Add DISTINCT to the query.
pub fn typed_distinct(query: TypedQuery(t)) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, distinct: True))
}

// ============================================================================
// WHERE CONDITIONS
// ============================================================================

/// Add a WHERE condition to the query.
/// The condition must be for the same table type as the query.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_where(typed_eq(email, "test@example.com"))
/// ```
pub fn typed_where(
  query: TypedQuery(t),
  condition: TypedCondition(t),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_wheres = list.append(inner.wheres, [ast.Where(cond)])
  TypedQuery(inner: ast.Query(..inner, wheres: new_wheres))
}

/// Add an OR condition to the query.
pub fn typed_or_where(
  query: TypedQuery(t),
  condition: TypedCondition(t),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  case inner.wheres {
    [] -> TypedQuery(inner: ast.Query(..inner, wheres: [ast.Where(cond)]))
    existing -> {
      let existing_conditions = list.map(existing, fn(w) { w.condition })
      let combined = ast.Or([ast.And(existing_conditions), cond])
      TypedQuery(inner: ast.Query(..inner, wheres: [ast.Where(combined)]))
    }
  }
}

/// Clear all WHERE conditions.
pub fn typed_where_clear(query: TypedQuery(t)) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, wheres: []))
}

// ============================================================================
// TYPED CONDITION BUILDERS
// ============================================================================

/// Create an equality condition: column = value
/// The column's value type must match the value being compared.
///
/// ## Example
/// ```gleam
/// typed_eq(email, "test@example.com")  // Column(UserTable, String) = String
/// typed_eq(id, 42)                      // Column(UserTable, Int) = Int
/// ```
pub fn typed_eq(column: Column(t, v), value: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.Eq(
    field: column_qualified_name(column),
    value: to_ast_value(value),
  ))
}

/// Create a not-equal condition: column != value
pub fn typed_not_eq(column: Column(t, v), value: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.NotEq(
    field: column_qualified_name(column),
    value: to_ast_value(value),
  ))
}

/// Create a greater-than condition: column > value
pub fn typed_gt(column: Column(t, v), value: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.Gt(
    field: column_qualified_name(column),
    value: to_ast_value(value),
  ))
}

/// Create a greater-than-or-equal condition: column >= value
pub fn typed_gte(column: Column(t, v), value: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.Gte(
    field: column_qualified_name(column),
    value: to_ast_value(value),
  ))
}

/// Create a less-than condition: column < value
pub fn typed_lt(column: Column(t, v), value: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.Lt(
    field: column_qualified_name(column),
    value: to_ast_value(value),
  ))
}

/// Create a less-than-or-equal condition: column <= value
pub fn typed_lte(column: Column(t, v), value: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.Lte(
    field: column_qualified_name(column),
    value: to_ast_value(value),
  ))
}

/// Create an IN condition: column IN (values...)
pub fn typed_in(column: Column(t, v), values: List(v)) -> TypedCondition(t) {
  TypedCondition(inner: ast.In(
    field: column_qualified_name(column),
    values: list.map(values, to_ast_value),
  ))
}

/// Create a NOT IN condition: column NOT IN (values...)
pub fn typed_not_in(column: Column(t, v), values: List(v)) -> TypedCondition(t) {
  TypedCondition(inner: ast.NotIn(
    field: column_qualified_name(column),
    values: list.map(values, to_ast_value),
  ))
}

/// Create a LIKE condition: column LIKE pattern
/// Only works with String columns.
pub fn typed_like(
  column: Column(t, String),
  pattern: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.Like(
    field: column_qualified_name(column),
    pattern: pattern,
  ))
}

/// Create a NOT LIKE condition: column NOT LIKE pattern
pub fn typed_not_like(
  column: Column(t, String),
  pattern: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.NotLike(
    field: column_qualified_name(column),
    pattern: pattern,
  ))
}

/// Create a case-insensitive LIKE condition: column ILIKE pattern
/// Only works with String columns.
///
/// ## Example
/// ```gleam
/// typed_ilike(email, "%@EXAMPLE.COM")  // Matches case-insensitively
/// ```
pub fn typed_ilike(
  column: Column(t, String),
  pattern: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.ILike(
    field: column_qualified_name(column),
    pattern: pattern,
  ))
}

/// Create a NOT ILIKE condition: column NOT ILIKE pattern
pub fn typed_not_ilike(
  column: Column(t, String),
  pattern: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.NotILike(
    field: column_qualified_name(column),
    pattern: pattern,
  ))
}

/// Create a starts_with condition: column LIKE 'prefix%'
/// Convenience wrapper that generates the LIKE pattern automatically.
/// Only works with String columns.
///
/// ## Example
/// ```gleam
/// typed_starts_with(name, "John")  // LIKE 'John%'
/// ```
pub fn typed_starts_with(
  column: Column(t, String),
  prefix: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.Like(
    field: column_qualified_name(column),
    pattern: prefix <> "%",
  ))
}

/// Create an ends_with condition: column LIKE '%suffix'
/// Convenience wrapper that generates the LIKE pattern automatically.
/// Only works with String columns.
///
/// ## Example
/// ```gleam
/// typed_ends_with(email, "@example.com")  // LIKE '%@example.com'
/// ```
pub fn typed_ends_with(
  column: Column(t, String),
  suffix: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.Like(
    field: column_qualified_name(column),
    pattern: "%" <> suffix,
  ))
}

/// Create a contains condition: column LIKE '%substring%'
/// Convenience wrapper that generates the LIKE pattern automatically.
/// Only works with String columns.
///
/// ## Example
/// ```gleam
/// typed_contains(description, "important")  // LIKE '%important%'
/// ```
pub fn typed_contains(
  column: Column(t, String),
  substring: String,
) -> TypedCondition(t) {
  TypedCondition(inner: ast.Like(
    field: column_qualified_name(column),
    pattern: "%" <> substring <> "%",
  ))
}

/// Create an IS NULL condition.
/// Only works with Option columns.
pub fn typed_is_null(column: Column(t, Option(v))) -> TypedCondition(t) {
  TypedCondition(inner: ast.IsNull(column_qualified_name(column)))
}

/// Create an IS NOT NULL condition.
/// Only works with Option columns.
pub fn typed_is_not_null(column: Column(t, Option(v))) -> TypedCondition(t) {
  TypedCondition(inner: ast.IsNotNull(column_qualified_name(column)))
}

/// Create a BETWEEN condition: column BETWEEN low AND high
pub fn typed_between(column: Column(t, v), low: v, high: v) -> TypedCondition(t) {
  TypedCondition(inner: ast.Between(
    field: column_qualified_name(column),
    low: to_ast_value(low),
    high: to_ast_value(high),
  ))
}

/// Combine conditions with AND
pub fn typed_and(conditions: List(TypedCondition(t))) -> TypedCondition(t) {
  let inner_conditions =
    list.map(conditions, fn(c) {
      let TypedCondition(inner: inner) = c
      inner
    })
  TypedCondition(inner: ast.And(inner_conditions))
}

/// Combine conditions with OR
pub fn typed_or(conditions: List(TypedCondition(t))) -> TypedCondition(t) {
  let inner_conditions =
    list.map(conditions, fn(c) {
      let TypedCondition(inner: inner) = c
      inner
    })
  TypedCondition(inner: ast.Or(inner_conditions))
}

/// Negate a condition
pub fn typed_not(condition: TypedCondition(t)) -> TypedCondition(t) {
  let TypedCondition(inner: inner) = condition
  TypedCondition(inner: ast.Not(inner))
}

// ============================================================================
// ORDER BY
// ============================================================================

/// Add an ORDER BY ASC clause.
pub fn typed_order_by_asc(
  query: TypedQuery(t),
  column: Column(t, v),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let new_order =
    ast.OrderBy(
      field: column_qualified_name(column),
      direction: ast.Asc,
      nulls: ast.NullsDefault,
    )
  let new_order_bys = list.append(inner.order_bys, [new_order])
  TypedQuery(inner: ast.Query(..inner, order_bys: new_order_bys))
}

/// Add an ORDER BY DESC clause.
pub fn typed_order_by_desc(
  query: TypedQuery(t),
  column: Column(t, v),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let new_order =
    ast.OrderBy(
      field: column_qualified_name(column),
      direction: ast.Desc,
      nulls: ast.NullsDefault,
    )
  let new_order_bys = list.append(inner.order_bys, [new_order])
  TypedQuery(inner: ast.Query(..inner, order_bys: new_order_bys))
}

/// Add an ORDER BY clause with explicit direction and null ordering.
pub fn typed_order_by(
  query: TypedQuery(t),
  column: Column(t, v),
  direction: ast.Direction,
  nulls: ast.NullsOrder,
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let new_order =
    ast.OrderBy(
      field: column_qualified_name(column),
      direction: direction,
      nulls: nulls,
    )
  let new_order_bys = list.append(inner.order_bys, [new_order])
  TypedQuery(inner: ast.Query(..inner, order_bys: new_order_bys))
}

/// Clear all ORDER BY clauses.
pub fn typed_order_by_clear(query: TypedQuery(t)) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, order_bys: []))
}

// ============================================================================
// PAGINATION
// ============================================================================

/// Set the LIMIT for the query.
pub fn typed_limit(query: TypedQuery(t), count: Int) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, limit: Some(count)))
}

/// Set the OFFSET for the query.
pub fn typed_offset(query: TypedQuery(t), count: Int) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, offset: Some(count)))
}

/// Set both LIMIT and OFFSET for pagination.
pub fn typed_paginate(
  query: TypedQuery(t),
  page page: Int,
  per_page per_page: Int,
) -> TypedQuery(t) {
  let offset_val = { page - 1 } * per_page
  query
  |> typed_limit(per_page)
  |> typed_offset(offset_val)
}

/// Remove LIMIT and OFFSET.
pub fn typed_no_pagination(query: TypedQuery(t)) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  TypedQuery(inner: ast.Query(..inner, limit: None, offset: None))
}

// ============================================================================
// JOINS
// ============================================================================

/// Add an INNER JOIN to the query, returning a query with joined table types.
/// The resulting query can use columns from both tables.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_join(posts, on: typed_column_eq(user_id, id))
/// // Now the query type is TypedQuery(Join2(UserTable, PostTable))
/// ```
pub fn typed_join(
  query: TypedQuery(t1),
  table: Table(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.InnerJoin,
      table: table_qualified_name(table),
      table_alias: None,
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a LEFT JOIN to the query.
pub fn typed_left_join(
  query: TypedQuery(t1),
  table: Table(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.LeftJoin,
      table: table_qualified_name(table),
      table_alias: None,
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a RIGHT JOIN to the query.
pub fn typed_right_join(
  query: TypedQuery(t1),
  table: Table(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.RightJoin,
      table: table_qualified_name(table),
      table_alias: None,
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a FULL JOIN to the query.
pub fn typed_full_join(
  query: TypedQuery(t1),
  table: Table(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.FullJoin,
      table: table_qualified_name(table),
      table_alias: None,
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a third table to a joined query.
pub fn typed_join3(
  query: TypedQuery(Join2(t1, t2)),
  table: Table(t3),
  on condition: TypedCondition(Join3(t1, t2, t3)),
) -> TypedQuery(Join3(t1, t2, t3)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.InnerJoin,
      table: table_qualified_name(table),
      table_alias: None,
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a CROSS JOIN to the query.
/// Cross joins produce a Cartesian product and don't require a condition.
///
/// ## Example
/// ```gleam
/// typed_from(sizes)
/// |> typed_cross_join(colors)
/// // Produces all size/color combinations
/// ```
pub fn typed_cross_join(
  query: TypedQuery(t1),
  table: Table(t2),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  // For cross join, we use a true condition since there's no ON clause
  let new_join =
    ast.Join(
      join_type: ast.CrossJoin,
      table: table_qualified_name(table),
      table_alias: None,
      on: ast.Raw(sql: "1=1", params: []),
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

// ============================================================================
// ALIASED JOINS
// ============================================================================

/// Add an INNER JOIN with an aliased table.
/// Useful for self-joins where the same table needs different aliases.
///
/// ## Example
/// ```gleam
/// let managers = alias_table(employees, "m")
///
/// typed_from(employees)
/// |> typed_join_aliased(managers, on: join_condition)
/// ```
pub fn typed_join_aliased(
  query: TypedQuery(t1),
  aliased: AliasedTable(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.InnerJoin,
      table: aliased_table_qualified_name(aliased),
      table_alias: Some(aliased_table_alias(aliased)),
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a LEFT JOIN with an aliased table.
pub fn typed_left_join_aliased(
  query: TypedQuery(t1),
  aliased: AliasedTable(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.LeftJoin,
      table: aliased_table_qualified_name(aliased),
      table_alias: Some(aliased_table_alias(aliased)),
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a RIGHT JOIN with an aliased table.
pub fn typed_right_join_aliased(
  query: TypedQuery(t1),
  aliased: AliasedTable(t2),
  on condition: TypedCondition(Join2(t1, t2)),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_join =
    ast.Join(
      join_type: ast.RightJoin,
      table: aliased_table_qualified_name(aliased),
      table_alias: Some(aliased_table_alias(aliased)),
      on: cond,
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

/// Add a CROSS JOIN with an aliased table.
pub fn typed_cross_join_aliased(
  query: TypedQuery(t1),
  aliased: AliasedTable(t2),
) -> TypedQuery(Join2(t1, t2)) {
  let TypedQuery(inner: inner) = query
  let new_join =
    ast.Join(
      join_type: ast.CrossJoin,
      table: aliased_table_qualified_name(aliased),
      table_alias: Some(aliased_table_alias(aliased)),
      on: ast.Raw(sql: "1=1", params: []),
    )
  let new_joins = list.append(inner.joins, [new_join])
  TypedQuery(inner: ast.Query(..inner, joins: new_joins))
}

// ============================================================================
// JOIN CONDITION HELPERS
// ============================================================================

/// Create a simple join condition comparing columns from two different tables.
/// This is a convenience wrapper around `typed_eq_columns` for join conditions.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_join(posts, on: on(user_id, post_user_id))
/// ```
pub fn on(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " = " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a join condition with an additional condition.
/// The primary comparison is ANDed with the additional condition.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_join(posts, on: on_and(
///     user_id, post_user_id,
///     typed_eq(in_join2_right(post_published), True)
///   ))
/// ```
pub fn on_and(
  left: Column(t1, v),
  right: Column(t2, v),
  additional: TypedCondition(Join2(t1, t2)),
) -> TypedCondition(Join2(t1, t2)) {
  let TypedCondition(inner: add_cond) = additional
  TypedCondition(
    inner: ast.And([
      ast.Raw(
        sql: column_qualified_name(left)
          <> " = "
          <> column_qualified_name(right),
        params: [],
      ),
      add_cond,
    ]),
  )
}

/// Create a join condition with multiple additional conditions.
///
/// ## Example
/// ```gleam
/// on_and_all(user_id, post_user_id, [
///   typed_eq(in_join2_right(post_published), True),
///   typed_gt(in_join2_right(post_views), 100),
/// ])
/// ```
pub fn on_and_all(
  left: Column(t1, v),
  right: Column(t2, v),
  additional: List(TypedCondition(Join2(t1, t2))),
) -> TypedCondition(Join2(t1, t2)) {
  let base_cond =
    ast.Raw(
      sql: column_qualified_name(left) <> " = " <> column_qualified_name(right),
      params: [],
    )
  let add_conditions =
    list.map(additional, fn(c) {
      let TypedCondition(inner: inner) = c
      inner
    })
  TypedCondition(inner: ast.And([base_cond, ..add_conditions]))
}

// ============================================================================
// COLUMN COMPARISON CONDITIONS
// ============================================================================

/// Create a condition comparing two columns.
/// Both columns must belong to the same table scope.
/// Useful for join conditions.
///
/// ## Example
/// ```gleam
/// typed_column_eq(in_join2_right(user_id), in_join2_left(id))
/// ```
pub fn typed_column_eq(
  left: Column(t, v),
  right: Column(t, v),
) -> TypedCondition(t) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " = " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a condition comparing two columns for inequality.
pub fn typed_column_not_eq(
  left: Column(t, v),
  right: Column(t, v),
) -> TypedCondition(t) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " != " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a condition comparing two columns with greater-than.
pub fn typed_column_gt(
  left: Column(t, v),
  right: Column(t, v),
) -> TypedCondition(t) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " > " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a condition comparing two columns with less-than.
pub fn typed_column_lt(
  left: Column(t, v),
  right: Column(t, v),
) -> TypedCondition(t) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " < " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a condition comparing two columns with greater-than-or-equal.
pub fn typed_column_gte(
  left: Column(t, v),
  right: Column(t, v),
) -> TypedCondition(t) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " >= " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a condition comparing two columns with less-than-or-equal.
pub fn typed_column_lte(
  left: Column(t, v),
  right: Column(t, v),
) -> TypedCondition(t) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " <= " <> column_qualified_name(right),
      params: [],
    ),
  )
}

// ============================================================================
// CROSS-TABLE COLUMN COMPARISONS
// ============================================================================

/// Create an equality condition comparing columns from two different tables.
/// Returns a condition with Join2 scope, suitable for join conditions.
/// Both columns must have the same value type.
///
/// ## Example
/// ```gleam
/// // In a join between users and posts tables:
/// typed_eq_columns(posts_user_id, users_id)
/// // Returns: TypedCondition(Join2(PostTable, UserTable))
/// ```
pub fn typed_eq_columns(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " = " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a not-equal condition comparing columns from two different tables.
/// Returns a condition with Join2 scope.
pub fn typed_not_eq_columns(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " != " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a greater-than condition comparing columns from two different tables.
/// Returns a condition with Join2 scope.
pub fn typed_gt_columns(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " > " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a less-than condition comparing columns from two different tables.
/// Returns a condition with Join2 scope.
pub fn typed_lt_columns(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " < " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a greater-than-or-equal condition comparing columns from two different tables.
/// Returns a condition with Join2 scope.
pub fn typed_gte_columns(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " >= " <> column_qualified_name(right),
      params: [],
    ),
  )
}

/// Create a less-than-or-equal condition comparing columns from two different tables.
/// Returns a condition with Join2 scope.
pub fn typed_lte_columns(
  left: Column(t1, v),
  right: Column(t2, v),
) -> TypedCondition(Join2(t1, t2)) {
  TypedCondition(
    inner: ast.Raw(
      sql: column_qualified_name(left) <> " <= " <> column_qualified_name(right),
      params: [],
    ),
  )
}

// ============================================================================
// GROUP BY / HAVING
// ============================================================================

/// Add a GROUP BY column.
pub fn typed_group_by(
  query: TypedQuery(t),
  column: Column(t, v),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let new_group_bys =
    list.append(inner.group_bys, [column_qualified_name(column)])
  TypedQuery(inner: ast.Query(..inner, group_bys: new_group_bys))
}

/// Add a HAVING condition.
pub fn typed_having(
  query: TypedQuery(t),
  condition: TypedCondition(t),
) -> TypedQuery(t) {
  let TypedQuery(inner: inner) = query
  let TypedCondition(inner: cond) = condition
  let new_havings = list.append(inner.havings, [ast.Where(cond)])
  TypedQuery(inner: ast.Query(..inner, havings: new_havings))
}

// ============================================================================
// QUERY INSPECTION
// ============================================================================

/// Check if the query has any WHERE conditions.
pub fn typed_has_conditions(query: TypedQuery(t)) -> Bool {
  let TypedQuery(inner: inner) = query
  !list.is_empty(inner.wheres)
}

/// Check if the query has any ORDER BY clauses.
pub fn typed_has_order_by(query: TypedQuery(t)) -> Bool {
  let TypedQuery(inner: inner) = query
  !list.is_empty(inner.order_bys)
}

/// Check if the query has pagination.
pub fn typed_has_pagination(query: TypedQuery(t)) -> Bool {
  let TypedQuery(inner: inner) = query
  option.is_some(inner.limit) || option.is_some(inner.offset)
}

/// Check if the query is DISTINCT.
pub fn typed_is_distinct(query: TypedQuery(t)) -> Bool {
  let TypedQuery(inner: inner) = query
  inner.distinct
}

/// Get the LIMIT value.
pub fn typed_get_limit(query: TypedQuery(t)) -> Option(Int) {
  let TypedQuery(inner: inner) = query
  inner.limit
}

/// Get the OFFSET value.
pub fn typed_get_offset(query: TypedQuery(t)) -> Option(Int) {
  let TypedQuery(inner: inner) = query
  inner.offset
}

// ============================================================================
// VALUE CONVERSION (INTERNAL)
// ============================================================================

/// Convert a Gleam value to an AST Value.
/// Uses FFI for runtime type inspection.
fn to_ast_value(value: a) -> ast.Value {
  case coerce_to_dynamic(value) {
    IntVal(i) -> ast.IntValue(i)
    FloatVal(f) -> ast.FloatValue(f)
    StringVal(s) -> ast.StringValue(s)
    BoolVal(b) -> ast.BoolValue(b)
    NilVal -> ast.NullValue
    ListVal(vals) -> ast.ListValue(list.map(vals, to_ast_value_from_dyn))
    UnknownVal -> ast.StringValue("")
  }
}

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
@external(javascript, "../../cquill_ffi.mjs", "coerce_value")
fn coerce_to_dynamic(value: a) -> DynValue

fn to_ast_value_from_dyn(dyn: DynValue) -> ast.Value {
  case dyn {
    IntVal(i) -> ast.IntValue(i)
    FloatVal(f) -> ast.FloatValue(f)
    StringVal(s) -> ast.StringValue(s)
    BoolVal(b) -> ast.BoolValue(b)
    NilVal -> ast.NullValue
    ListVal(vals) -> ast.ListValue(list.map(vals, to_ast_value_from_dyn))
    UnknownVal -> ast.StringValue("")
  }
}
