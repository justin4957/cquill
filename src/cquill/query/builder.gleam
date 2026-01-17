// cquill Query Builder Composition
//
// This module provides advanced composition functions for building
// complex queries from reusable parts.
//
// The core philosophy is that query fragments should be:
// - First-class values that can be passed around
// - Composable via standard function composition
// - Reusable across different queries
//
// Example usage:
// ```gleam
// // Define reusable filters
// let active = builder.filter(query.eq_bool("active", True))
// let recent = builder.filter(query.gt_int("created_at", cutoff))
// let admin = builder.filter(query.eq_string("role", "admin"))
//
// // Compose them
// let active_recent_admins = builder.compose([active, recent, admin])
//
// // Apply to any query
// user_query |> active_recent_admins
// ```

import cquill/query
import cquill/query/ast.{
  type Condition, type Direction, type Join, type JoinType, type OrderBy,
  type Query, type Select, Asc, Desc, InnerJoin, Join as JoinClause,
  NullsDefault, OrderBy as OrderByClause, Query as QueryRecord, Raw, SelectAll,
  SelectFields,
}
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// QUERY MODIFIER TYPE
// ============================================================================

/// A QueryModifier is a function that transforms a query.
/// This is the foundation for composable query building.
pub type QueryModifier(s) =
  fn(Query(s)) -> Query(s)

// ============================================================================
// FILTER BUILDERS
// ============================================================================

/// Create a filter modifier from a condition.
/// Returns a function that adds the condition to any query.
///
/// ## Example
/// ```gleam
/// let active_filter = builder.filter(query.eq_bool("active", True))
/// user_query |> active_filter
/// ```
pub fn filter(condition: Condition) -> QueryModifier(s) {
  fn(q: Query(s)) { query.where(q, condition) }
}

/// Create a filter that requires a field to equal a specific integer.
pub fn filter_eq_int(field: String, value: Int) -> QueryModifier(s) {
  filter(query.eq_int(field, value))
}

/// Create a filter that requires a field to equal a specific string.
pub fn filter_eq_string(field: String, value: String) -> QueryModifier(s) {
  filter(query.eq_string(field, value))
}

/// Create a filter that requires a field to equal a specific boolean.
pub fn filter_eq_bool(field: String, value: Bool) -> QueryModifier(s) {
  filter(query.eq_bool(field, value))
}

/// Create a filter for non-null values.
pub fn filter_not_null(field: String) -> QueryModifier(s) {
  filter(query.is_not_null(field))
}

/// Create a filter for null values.
pub fn filter_null(field: String) -> QueryModifier(s) {
  filter(query.is_null(field))
}

/// Create a filter for values greater than a threshold.
pub fn filter_gt_int(field: String, value: Int) -> QueryModifier(s) {
  filter(query.gt_int(field, value))
}

// ============================================================================
// MODIFIER COMPOSITION
// ============================================================================

/// Compose multiple modifiers into a single modifier.
/// Modifiers are applied left to right.
///
/// ## Example
/// ```gleam
/// let active_admins = builder.compose([
///   builder.filter(query.eq_bool("active", True)),
///   builder.filter(query.eq_string("role", "admin")),
///   builder.with_limit(10),
/// ])
/// user_query |> active_admins
/// ```
pub fn compose(modifiers: List(QueryModifier(s))) -> QueryModifier(s) {
  fn(query: Query(s)) {
    list.fold(modifiers, query, fn(q, modifier) { modifier(q) })
  }
}

/// Compose two modifiers into a single modifier.
pub fn and_then(
  first: QueryModifier(s),
  second: QueryModifier(s),
) -> QueryModifier(s) {
  fn(query: Query(s)) { query |> first |> second }
}

/// Create an identity modifier (does nothing).
pub fn identity() -> QueryModifier(s) {
  fn(query: Query(s)) { query }
}

/// Conditionally apply a modifier.
/// If the condition is true, apply the modifier; otherwise, return unchanged.
pub fn when(condition: Bool, modifier: QueryModifier(s)) -> QueryModifier(s) {
  case condition {
    True -> modifier
    False -> identity()
  }
}

/// Apply a modifier based on an optional value.
/// If the value is Some, apply the modifier factory with that value.
pub fn when_some(
  maybe_value: Option(a),
  modifier_factory: fn(a) -> QueryModifier(s),
) -> QueryModifier(s) {
  case maybe_value {
    Some(value) -> modifier_factory(value)
    None -> identity()
  }
}

// ============================================================================
// COMMON MODIFIERS
// ============================================================================

/// Create a modifier that sets the limit.
pub fn with_limit(count: Int) -> QueryModifier(s) {
  fn(q: Query(s)) { query.limit(q, count) }
}

/// Create a modifier that sets the offset.
pub fn with_offset(count: Int) -> QueryModifier(s) {
  fn(q: Query(s)) { query.offset(q, count) }
}

/// Create a modifier that sets pagination.
pub fn with_pagination(page: Int, per_page: Int) -> QueryModifier(s) {
  fn(q: Query(s)) { query.paginate(q, page, per_page) }
}

/// Create a modifier that adds ORDER BY ASC.
pub fn order_asc(field: String) -> QueryModifier(s) {
  fn(q: Query(s)) { query.order_by_asc(q, field) }
}

/// Create a modifier that adds ORDER BY DESC.
pub fn order_desc(field: String) -> QueryModifier(s) {
  fn(q: Query(s)) { query.order_by_desc(q, field) }
}

/// Create a modifier that sets specific fields to select.
pub fn with_select(fields: List(String)) -> QueryModifier(s) {
  fn(q: Query(s)) { query.select(q, fields) }
}

/// Create a modifier that makes the query distinct.
pub fn with_distinct() -> QueryModifier(s) {
  fn(q: Query(s)) { query.distinct(q) }
}

// ============================================================================
// SCOPES (Named, Reusable Query Fragments)
// ============================================================================

/// A Scope is a named, reusable query modifier.
/// This is similar to Ecto's query scopes.
pub type Scope(s) {
  Scope(name: String, modifier: QueryModifier(s))
}

/// Create a named scope.
pub fn scope(name: String, modifier: QueryModifier(s)) -> Scope(s) {
  Scope(name:, modifier:)
}

/// Apply a scope to a query.
pub fn apply_scope(query: Query(s), scope: Scope(s)) -> Query(s) {
  scope.modifier(query)
}

/// Apply multiple scopes to a query.
pub fn apply_scopes(query: Query(s), scopes: List(Scope(s))) -> Query(s) {
  list.fold(scopes, query, fn(q, s) { apply_scope(q, s) })
}

// ============================================================================
// QUERY MERGING
// ============================================================================

/// Merge conditions from source query into target query.
/// Preserves target's source, select, and other settings.
pub fn merge_conditions(target: Query(s), source: Query(a)) -> Query(s) {
  let source_conditions = query.get_conditions(source)
  list.fold(source_conditions, target, fn(q, cond) { query.where(q, cond) })
}

/// Merge order_bys from source query into target query.
pub fn merge_order_bys(target: Query(s), source: Query(a)) -> Query(s) {
  let QueryRecord(order_bys: target_orders, ..) = target
  let source_orders = query.get_order_bys(source)
  QueryRecord(..target, order_bys: list.append(target_orders, source_orders))
}

/// Merge pagination (limit/offset) from source to target.
/// Only applies if target doesn't already have pagination.
pub fn merge_pagination(target: Query(s), source: Query(a)) -> Query(s) {
  let target_limit = query.get_limit(target)
  let target_offset = query.get_offset(target)
  let source_limit = query.get_limit(source)
  let source_offset = query.get_offset(source)

  let new_limit = case target_limit {
    Some(_) -> target_limit
    None -> source_limit
  }

  let new_offset = case target_offset {
    Some(_) -> target_offset
    None -> source_offset
  }

  QueryRecord(..target, limit: new_limit, offset: new_offset)
}

// ============================================================================
// COMMON PATTERNS
// ============================================================================

/// Create a "soft delete" filter that excludes soft-deleted records.
/// Assumes a `deleted_at` field that is NULL for non-deleted records.
pub fn not_deleted() -> QueryModifier(s) {
  filter_null("deleted_at")
}

/// Create a "published" filter for content models.
/// Assumes a `published` boolean field.
pub fn published() -> QueryModifier(s) {
  filter_eq_bool("published", True)
}

/// Create an "active" filter.
/// Assumes an `active` boolean field.
pub fn active() -> QueryModifier(s) {
  filter_eq_bool("active", True)
}

/// Create a "recent first" ordering modifier.
/// Assumes a `created_at` timestamp field.
pub fn recent_first() -> QueryModifier(s) {
  order_desc("created_at")
}

/// Create an "oldest first" ordering modifier.
/// Assumes a `created_at` timestamp field.
pub fn oldest_first() -> QueryModifier(s) {
  order_asc("created_at")
}

/// Create a modifier for "by user" filtering.
/// Common pattern for multi-tenant queries.
pub fn by_user(user_id: Int) -> QueryModifier(s) {
  filter_eq_int("user_id", user_id)
}

// ============================================================================
// QUERY CLONING / TRANSFORMATION
// ============================================================================

/// Clone a query, optionally resetting certain parts.
pub fn clone(q: Query(s)) -> Query(s) {
  q
}

/// Clone a query but clear all WHERE conditions.
pub fn clone_without_conditions(q: Query(s)) -> Query(s) {
  query.where_clear(q)
}

/// Clone a query but clear all ORDER BY.
pub fn clone_without_order(q: Query(s)) -> Query(s) {
  query.order_by_clear(q)
}

/// Clone a query but clear pagination.
pub fn clone_without_pagination(q: Query(s)) -> Query(s) {
  query.no_pagination(q)
}

// ============================================================================
// QUERY ANALYSIS
// ============================================================================

/// Check if two queries have equivalent conditions (order-independent).
pub fn conditions_equivalent(query_a: Query(a), query_b: Query(b)) -> Bool {
  let conds_a = query.get_conditions(query_a)
  let conds_b = query.get_conditions(query_b)

  list.length(conds_a) == list.length(conds_b)
  && list.all(conds_a, fn(c) { list.contains(conds_b, c) })
  && list.all(conds_b, fn(c) { list.contains(conds_a, c) })
}

/// Count total conditions including nested AND/OR.
pub fn count_conditions_deep(q: Query(s)) -> Int {
  let conditions = query.get_conditions(q)
  list.fold(conditions, 0, fn(acc, c) { acc + count_condition_nodes(c) })
}

fn count_condition_nodes(condition: Condition) -> Int {
  case condition {
    ast.And(conditions) ->
      1
      + list.fold(conditions, 0, fn(acc, c) { acc + count_condition_nodes(c) })
    ast.Or(conditions) ->
      1
      + list.fold(conditions, 0, fn(acc, c) { acc + count_condition_nodes(c) })
    ast.Not(inner) -> 1 + count_condition_nodes(inner)
    _ -> 1
  }
}

/// Extract all field names referenced in conditions.
pub fn get_condition_fields(q: Query(s)) -> List(String) {
  let conditions = query.get_conditions(q)
  conditions
  |> list.flat_map(extract_fields_from_condition)
  |> list.unique
}

fn extract_fields_from_condition(condition: Condition) -> List(String) {
  case condition {
    ast.Eq(field, _) -> [field]
    ast.NotEq(field, _) -> [field]
    ast.Gt(field, _) -> [field]
    ast.Gte(field, _) -> [field]
    ast.Lt(field, _) -> [field]
    ast.Lte(field, _) -> [field]
    ast.In(field, _) -> [field]
    ast.NotIn(field, _) -> [field]
    ast.Like(field, _) -> [field]
    ast.NotLike(field, _) -> [field]
    ast.IsNull(field) -> [field]
    ast.IsNotNull(field) -> [field]
    ast.Between(field, _, _) -> [field]
    ast.And(conditions) ->
      list.flat_map(conditions, extract_fields_from_condition)
    ast.Or(conditions) ->
      list.flat_map(conditions, extract_fields_from_condition)
    ast.Not(inner) -> extract_fields_from_condition(inner)
    Raw(_, _) -> []
  }
}
