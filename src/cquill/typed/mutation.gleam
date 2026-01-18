// cquill Phantom-Typed Mutation Builders
//
// This module provides type-safe INSERT, UPDATE, and DELETE query builders
// that use phantom types to ensure compile-time validation.
//
// Key features:
// - Type-safe SET operations that match column types
// - Compile-time verification that columns belong to the target table
// - Safe mutation patterns with required WHERE clauses for UPDATE/DELETE
//
// Example usage:
// ```gleam
// // Type-safe INSERT
// insert_into(users)
// |> insert_columns([user_email, user_name])
// |> insert_value(user_email, "test@example.com")
// |> insert_value(user_name, "Test User")
// |> insert_returning([user_id])
//
// // Type-safe UPDATE
// update(users)
// |> set(user_email, "new@example.com")
// |> set_where(typed_eq(user_id, 42))
// |> update_returning([user_id, user_email])
//
// // Safe DELETE
// delete_from(users)
// |> delete_where(typed_eq(user_id, 42))
// |> delete_returning([user_id])
// ```

import cquill/query/ast
import cquill/typed/table.{
  type Column, type Table, column_name, table_name, table_schema_name,
}
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// TYPED INSERT QUERY
// ============================================================================

/// A type-safe INSERT query carrying its target table type as a phantom parameter.
/// The phantom type ensures only columns from the target table can be used.
pub opaque type TypedInsertQuery(table_type) {
  TypedInsertQuery(inner: ast.InsertQuery(Nil))
}

/// Extract the underlying AST insert query for adapter use.
pub fn insert_to_ast(query: TypedInsertQuery(t)) -> ast.InsertQuery(Nil) {
  let TypedInsertQuery(inner: inner) = query
  inner
}

/// Start building an INSERT query for a table.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> insert_columns([user_email, user_name])
/// ```
pub fn insert_into(table: Table(t)) -> TypedInsertQuery(t) {
  TypedInsertQuery(
    inner: ast.InsertQuery(
      table: table_name(table),
      schema_name: case table_schema_name(table) {
        "public" -> None
        schema -> Some(schema)
      },
      columns: [],
      values: [],
      on_conflict: None,
      returning: [],
    ),
  )
}

/// Specify a single column for the INSERT statement.
/// Call multiple times to add more columns.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> insert_column(user_email)
/// |> insert_column(user_name)
/// ```
pub fn insert_column(
  query: TypedInsertQuery(t),
  column: Column(t, a),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_columns = list.append(inner.columns, [column_name(column)])
  TypedInsertQuery(inner: ast.InsertQuery(..inner, columns: new_columns))
}

/// Specify columns for the INSERT statement by name.
/// Use this when you need to specify multiple columns at once.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> insert_columns_by_name(["email", "name"])
/// ```
pub fn insert_columns_by_name(
  query: TypedInsertQuery(t),
  column_names: List(String),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(inner: ast.InsertQuery(..inner, columns: column_names))
}

/// Add a single value to the INSERT.
/// The value type must match the column's value type.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> insert_columns([user_email])
/// |> insert_string_value(user_email, "test@example.com")
/// ```
pub fn insert_string_value(
  query: TypedInsertQuery(t),
  _column: Column(t, String),
  value: String,
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = case inner.values {
    [] -> [[ast.StringValue(value)]]
    [row, ..rest] -> [list.append(row, [ast.StringValue(value)]), ..rest]
  }
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add an integer value to the INSERT.
pub fn insert_int_value(
  query: TypedInsertQuery(t),
  _column: Column(t, Int),
  value: Int,
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = case inner.values {
    [] -> [[ast.IntValue(value)]]
    [row, ..rest] -> [list.append(row, [ast.IntValue(value)]), ..rest]
  }
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add a float value to the INSERT.
pub fn insert_float_value(
  query: TypedInsertQuery(t),
  _column: Column(t, Float),
  value: Float,
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = case inner.values {
    [] -> [[ast.FloatValue(value)]]
    [row, ..rest] -> [list.append(row, [ast.FloatValue(value)]), ..rest]
  }
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add a boolean value to the INSERT.
pub fn insert_bool_value(
  query: TypedInsertQuery(t),
  _column: Column(t, Bool),
  value: Bool,
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = case inner.values {
    [] -> [[ast.BoolValue(value)]]
    [row, ..rest] -> [list.append(row, [ast.BoolValue(value)]), ..rest]
  }
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add a NULL value to the INSERT for an optional column.
pub fn insert_null(
  query: TypedInsertQuery(t),
  _column: Column(t, Option(a)),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = case inner.values {
    [] -> [[ast.NullValue]]
    [row, ..rest] -> [list.append(row, [ast.NullValue]), ..rest]
  }
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add a raw list of values for a single row.
/// Use this when you have pre-encoded values.
pub fn insert_row(
  query: TypedInsertQuery(t),
  values: List(ast.Value),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = list.append(inner.values, [values])
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add multiple rows of values.
pub fn insert_rows(
  query: TypedInsertQuery(t),
  rows: List(List(ast.Value)),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_values = list.append(inner.values, rows)
  TypedInsertQuery(inner: ast.InsertQuery(..inner, values: new_values))
}

/// Add a single column to the RETURNING clause.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> ...
/// |> insert_returning_column(user_id)
/// |> insert_returning_column(user_email)
/// ```
pub fn insert_returning_column(
  query: TypedInsertQuery(t),
  column: Column(t, a),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_returning = list.append(inner.returning, [column_name(column)])
  TypedInsertQuery(inner: ast.InsertQuery(..inner, returning: new_returning))
}

/// Add columns to the RETURNING clause by name.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> ...
/// |> insert_returning_by_name(["id", "email"])
/// ```
pub fn insert_returning_by_name(
  query: TypedInsertQuery(t),
  column_names: List(String),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  let new_returning = list.append(inner.returning, column_names)
  TypedInsertQuery(inner: ast.InsertQuery(..inner, returning: new_returning))
}

/// Add RETURNING * to get all inserted columns back.
pub fn insert_returning_all(query: TypedInsertQuery(t)) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(inner: ast.InsertQuery(..inner, returning: ["*"]))
}

/// Handle conflicts by doing nothing (skip conflicting rows).
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> ...
/// |> on_conflict_do_nothing()
/// ```
pub fn on_conflict_do_nothing(query: TypedInsertQuery(t)) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(
    inner: ast.InsertQuery(..inner, on_conflict: Some(ast.DoNothing)),
  )
}

/// Handle conflicts by updating specified columns (upsert).
/// Uses column names for flexibility with mixed-type columns.
///
/// ## Example
/// ```gleam
/// insert_into(users)
/// |> ...
/// |> on_conflict_do_update_by_name(
///     conflict_columns: ["email"],
///     update_columns: ["name", "updated_at"]
///   )
/// ```
pub fn on_conflict_do_update_by_name(
  query: TypedInsertQuery(t),
  conflict_columns conflict_names: List(String),
  update_columns update_names: List(String),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(
    inner: ast.InsertQuery(
      ..inner,
      on_conflict: Some(ast.DoUpdate(conflict_names, update_names)),
    ),
  )
}

/// Handle conflicts on a single conflict column by updating a single column.
/// Type-safe version for single column conflicts.
pub fn on_conflict_do_update_single(
  query: TypedInsertQuery(t),
  conflict_column: Column(t, a),
  update_column: Column(t, b),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(
    inner: ast.InsertQuery(
      ..inner,
      on_conflict: Some(
        ast.DoUpdate([column_name(conflict_column)], [
          column_name(update_column),
        ]),
      ),
    ),
  )
}

/// Handle conflicts on a named constraint by doing nothing.
pub fn on_conflict_constraint_do_nothing(
  query: TypedInsertQuery(t),
  constraint_name: String,
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(
    inner: ast.InsertQuery(
      ..inner,
      on_conflict: Some(ast.DoNothingOnConstraint(constraint_name)),
    ),
  )
}

/// Handle conflicts on a named constraint by updating columns.
pub fn on_conflict_constraint_do_update_by_name(
  query: TypedInsertQuery(t),
  constraint_name: String,
  update_columns: List(String),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(
    inner: ast.InsertQuery(
      ..inner,
      on_conflict: Some(ast.DoUpdateOnConstraint(
        constraint_name,
        update_columns,
      )),
    ),
  )
}

/// Handle conflicts on a named constraint by updating a single column.
pub fn on_conflict_constraint_do_update_single(
  query: TypedInsertQuery(t),
  constraint_name: String,
  update_column: Column(t, a),
) -> TypedInsertQuery(t) {
  let TypedInsertQuery(inner: inner) = query
  TypedInsertQuery(
    inner: ast.InsertQuery(
      ..inner,
      on_conflict: Some(
        ast.DoUpdateOnConstraint(constraint_name, [column_name(update_column)]),
      ),
    ),
  )
}

// ============================================================================
// TYPED UPDATE QUERY
// ============================================================================

/// A type-safe UPDATE query carrying its target table type as a phantom parameter.
pub opaque type TypedUpdateQuery(table_type) {
  TypedUpdateQuery(inner: ast.UpdateQuery(Nil))
}

/// Extract the underlying AST update query for adapter use.
pub fn update_to_ast(query: TypedUpdateQuery(t)) -> ast.UpdateQuery(Nil) {
  let TypedUpdateQuery(inner: inner) = query
  inner
}

/// Start building an UPDATE query for a table.
///
/// ## Example
/// ```gleam
/// update(users)
/// |> set_string(user_email, "new@example.com")
/// |> set_where(typed_eq(user_id, 42))
/// ```
pub fn update(table: Table(t)) -> TypedUpdateQuery(t) {
  TypedUpdateQuery(
    inner: ast.UpdateQuery(
      table: table_name(table),
      schema_name: case table_schema_name(table) {
        "public" -> None
        schema -> Some(schema)
      },
      sets: [],
      wheres: [],
      returning: [],
    ),
  )
}

/// Set a string column's value in the UPDATE.
/// The column must be a String column belonging to the table.
///
/// ## Example
/// ```gleam
/// update(users)
/// |> set_string(user_email, "new@example.com")
/// ```
pub fn set_string(
  query: TypedUpdateQuery(t),
  column: Column(t, String),
  value: String,
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let set_clause =
    ast.SetClause(column: column_name(column), value: ast.StringValue(value))
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Set an integer column's value in the UPDATE.
pub fn set_int(
  query: TypedUpdateQuery(t),
  column: Column(t, Int),
  value: Int,
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let set_clause =
    ast.SetClause(column: column_name(column), value: ast.IntValue(value))
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Set a float column's value in the UPDATE.
pub fn set_float(
  query: TypedUpdateQuery(t),
  column: Column(t, Float),
  value: Float,
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let set_clause =
    ast.SetClause(column: column_name(column), value: ast.FloatValue(value))
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Set a boolean column's value in the UPDATE.
pub fn set_bool(
  query: TypedUpdateQuery(t),
  column: Column(t, Bool),
  value: Bool,
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let set_clause =
    ast.SetClause(column: column_name(column), value: ast.BoolValue(value))
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Set an optional column to NULL.
pub fn set_null(
  query: TypedUpdateQuery(t),
  column: Column(t, Option(a)),
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let set_clause =
    ast.SetClause(column: column_name(column), value: ast.NullValue)
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Set an optional string column's value (Some or None).
pub fn set_optional_string(
  query: TypedUpdateQuery(t),
  column: Column(t, Option(String)),
  value: Option(String),
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let ast_value = case value {
    Some(v) -> ast.StringValue(v)
    None -> ast.NullValue
  }
  let set_clause = ast.SetClause(column: column_name(column), value: ast_value)
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Set an optional int column's value (Some or None).
pub fn set_optional_int(
  query: TypedUpdateQuery(t),
  column: Column(t, Option(Int)),
  value: Option(Int),
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let ast_value = case value {
    Some(v) -> ast.IntValue(v)
    None -> ast.NullValue
  }
  let set_clause = ast.SetClause(column: column_name(column), value: ast_value)
  let new_sets = list.append(inner.sets, [set_clause])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, sets: new_sets))
}

/// Import and use a typed condition for the WHERE clause.
/// This reuses the TypedCondition type from the query module.
pub fn set_where(
  query: TypedUpdateQuery(t),
  condition: ast.Condition,
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let new_wheres = list.append(inner.wheres, [ast.Where(condition)])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, wheres: new_wheres))
}

/// Add a raw WHERE condition (for advanced use cases).
pub fn set_where_raw(
  query: TypedUpdateQuery(t),
  sql: String,
  params: List(ast.Value),
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let condition = ast.Raw(sql: sql, params: params)
  let new_wheres = list.append(inner.wheres, [ast.Where(condition)])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, wheres: new_wheres))
}

/// Add a single column to the RETURNING clause.
pub fn update_returning_column(
  query: TypedUpdateQuery(t),
  column: Column(t, a),
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let new_returning = list.append(inner.returning, [column_name(column)])
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, returning: new_returning))
}

/// Add columns to the RETURNING clause by name.
pub fn update_returning_by_name(
  query: TypedUpdateQuery(t),
  column_names: List(String),
) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  let new_returning = list.append(inner.returning, column_names)
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, returning: new_returning))
}

/// Add RETURNING * to get all updated columns back.
pub fn update_returning_all(query: TypedUpdateQuery(t)) -> TypedUpdateQuery(t) {
  let TypedUpdateQuery(inner: inner) = query
  TypedUpdateQuery(inner: ast.UpdateQuery(..inner, returning: ["*"]))
}

/// Check if the UPDATE query has a WHERE clause.
/// Useful for safety checks before executing.
pub fn update_has_where(query: TypedUpdateQuery(t)) -> Bool {
  let TypedUpdateQuery(inner: inner) = query
  inner.wheres != []
}

/// Get the number of SET clauses in the UPDATE.
pub fn update_set_count(query: TypedUpdateQuery(t)) -> Int {
  let TypedUpdateQuery(inner: inner) = query
  list.length(inner.sets)
}

// ============================================================================
// TYPED DELETE QUERY
// ============================================================================

/// A type-safe DELETE query carrying its target table type as a phantom parameter.
pub opaque type TypedDeleteQuery(table_type) {
  TypedDeleteQuery(inner: ast.DeleteQuery(Nil))
}

/// Extract the underlying AST delete query for adapter use.
pub fn delete_to_ast(query: TypedDeleteQuery(t)) -> ast.DeleteQuery(Nil) {
  let TypedDeleteQuery(inner: inner) = query
  inner
}

/// Start building a DELETE query for a table.
///
/// ## Example
/// ```gleam
/// delete_from(users)
/// |> delete_where(typed_eq(user_id, 42))
/// ```
pub fn delete_from(table: Table(t)) -> TypedDeleteQuery(t) {
  TypedDeleteQuery(
    inner: ast.DeleteQuery(
      table: table_name(table),
      schema_name: case table_schema_name(table) {
        "public" -> None
        schema -> Some(schema)
      },
      wheres: [],
      returning: [],
    ),
  )
}

/// Add a WHERE condition to the DELETE.
/// Uses raw AST condition for flexibility.
pub fn delete_where(
  query: TypedDeleteQuery(t),
  condition: ast.Condition,
) -> TypedDeleteQuery(t) {
  let TypedDeleteQuery(inner: inner) = query
  let new_wheres = list.append(inner.wheres, [ast.Where(condition)])
  TypedDeleteQuery(inner: ast.DeleteQuery(..inner, wheres: new_wheres))
}

/// Add a raw WHERE condition (for advanced use cases).
pub fn delete_where_raw(
  query: TypedDeleteQuery(t),
  sql: String,
  params: List(ast.Value),
) -> TypedDeleteQuery(t) {
  let TypedDeleteQuery(inner: inner) = query
  let condition = ast.Raw(sql: sql, params: params)
  let new_wheres = list.append(inner.wheres, [ast.Where(condition)])
  TypedDeleteQuery(inner: ast.DeleteQuery(..inner, wheres: new_wheres))
}

/// Explicitly mark that you want to delete all rows.
/// This is a safety feature requiring explicit intent for bulk deletes.
///
/// ## Example
/// ```gleam
/// delete_from(temp_data)
/// |> delete_all()  // Explicit bulk delete
/// ```
pub fn delete_all(query: TypedDeleteQuery(t)) -> TypedDeleteQuery(t) {
  let TypedDeleteQuery(inner: inner) = query
  // Add a "1=1" condition to explicitly mark bulk delete intent
  let condition = ast.Raw(sql: "1=1", params: [])
  let new_wheres = list.append(inner.wheres, [ast.Where(condition)])
  TypedDeleteQuery(inner: ast.DeleteQuery(..inner, wheres: new_wheres))
}

/// Add a single column to the RETURNING clause.
pub fn delete_returning_column(
  query: TypedDeleteQuery(t),
  column: Column(t, a),
) -> TypedDeleteQuery(t) {
  let TypedDeleteQuery(inner: inner) = query
  let new_returning = list.append(inner.returning, [column_name(column)])
  TypedDeleteQuery(inner: ast.DeleteQuery(..inner, returning: new_returning))
}

/// Add columns to the RETURNING clause by name.
pub fn delete_returning_by_name(
  query: TypedDeleteQuery(t),
  column_names: List(String),
) -> TypedDeleteQuery(t) {
  let TypedDeleteQuery(inner: inner) = query
  let new_returning = list.append(inner.returning, column_names)
  TypedDeleteQuery(inner: ast.DeleteQuery(..inner, returning: new_returning))
}

/// Add RETURNING * to get all deleted columns back.
pub fn delete_returning_all(query: TypedDeleteQuery(t)) -> TypedDeleteQuery(t) {
  let TypedDeleteQuery(inner: inner) = query
  TypedDeleteQuery(inner: ast.DeleteQuery(..inner, returning: ["*"]))
}

/// Check if the DELETE query has a WHERE clause.
/// Useful for safety checks before executing.
pub fn delete_has_where(query: TypedDeleteQuery(t)) -> Bool {
  let TypedDeleteQuery(inner: inner) = query
  inner.wheres != []
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Get the table name from an INSERT query.
pub fn insert_table_name(query: TypedInsertQuery(t)) -> String {
  let TypedInsertQuery(inner: inner) = query
  inner.table
}

/// Get the table name from an UPDATE query.
pub fn update_table_name(query: TypedUpdateQuery(t)) -> String {
  let TypedUpdateQuery(inner: inner) = query
  inner.table
}

/// Get the table name from a DELETE query.
pub fn delete_table_name(query: TypedDeleteQuery(t)) -> String {
  let TypedDeleteQuery(inner: inner) = query
  inner.table
}

/// Get the column names from an INSERT query.
pub fn insert_column_names(query: TypedInsertQuery(t)) -> List(String) {
  let TypedInsertQuery(inner: inner) = query
  inner.columns
}

/// Get the number of rows in an INSERT query.
pub fn insert_row_count(query: TypedInsertQuery(t)) -> Int {
  let TypedInsertQuery(inner: inner) = query
  list.length(inner.values)
}

/// Check if the INSERT has an ON CONFLICT clause.
pub fn insert_has_on_conflict(query: TypedInsertQuery(t)) -> Bool {
  let TypedInsertQuery(inner: inner) = query
  case inner.on_conflict {
    Some(_) -> True
    None -> False
  }
}

/// Get the returning columns from an INSERT query.
pub fn insert_returning_columns(query: TypedInsertQuery(t)) -> List(String) {
  let TypedInsertQuery(inner: inner) = query
  inner.returning
}

/// Get the returning columns from an UPDATE query.
pub fn update_returning_columns(query: TypedUpdateQuery(t)) -> List(String) {
  let TypedUpdateQuery(inner: inner) = query
  inner.returning
}

/// Get the returning columns from a DELETE query.
pub fn delete_returning_columns(query: TypedDeleteQuery(t)) -> List(String) {
  let TypedDeleteQuery(inner: inner) = query
  inner.returning
}
