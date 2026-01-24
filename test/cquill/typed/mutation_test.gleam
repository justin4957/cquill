// Phantom-Typed Mutation Tests
//
// Tests for the type-safe INSERT, UPDATE, and DELETE builders
// that use phantom types to ensure compile-time validation.

import cquill/query/ast
import cquill/typed/mutation.{
  delete_all, delete_from, delete_has_where, delete_returning_all,
  delete_returning_by_name, delete_returning_column, delete_returning_columns,
  delete_table_name, delete_to_ast, delete_where, insert_bool_value,
  insert_column, insert_column_names, insert_columns_by_name,
  insert_has_on_conflict, insert_int_value, insert_into, insert_returning_all,
  insert_returning_by_name, insert_returning_column, insert_returning_columns,
  insert_row, insert_row_count, insert_rows, insert_string_value,
  insert_table_name, insert_to_ast, on_conflict_constraint_do_nothing,
  on_conflict_constraint_do_update_by_name, on_conflict_do_nothing,
  on_conflict_do_update_by_name, set_bool, set_int, set_string, set_where,
  set_where_raw, update, update_has_where, update_returning_all,
  update_returning_by_name, update_returning_columns, update_set_count,
  update_table_name, update_to_ast,
}
import cquill/typed/table.{type Column, type Table, column, table}
import gleam/list
import gleam/option.{Some}
import gleeunit/should

// ============================================================================
// MOCK TABLE TYPES (simulating generated code)
// ============================================================================

/// Phantom type for the users table
pub type UserTable

/// Phantom type for the posts table
pub type PostTable

// Mock table factories
fn users() -> Table(UserTable) {
  table("users")
}

fn posts() -> Table(PostTable) {
  table("posts")
}

// Mock column factories for users
fn user_id() -> Column(UserTable, Int) {
  column("id")
}

fn user_email() -> Column(UserTable, String) {
  column("email")
}

fn user_name() -> Column(UserTable, String) {
  column("name")
}

fn user_active() -> Column(UserTable, Bool) {
  column("active")
}

fn user_age() -> Column(UserTable, Int) {
  column("age")
}

// Mock column factories for posts
fn post_id() -> Column(PostTable, Int) {
  column("id")
}

fn post_title() -> Column(PostTable, String) {
  column("title")
}

fn post_published() -> Column(PostTable, Bool) {
  column("published")
}

// ============================================================================
// INSERT QUERY TESTS
// ============================================================================

pub fn insert_into_creates_basic_query_test() {
  let query = insert_into(users())

  insert_table_name(query)
  |> should.equal("users")

  insert_row_count(query)
  |> should.equal(0)
}

pub fn insert_column_sets_column_names_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_column(user_name())

  insert_column_names(query)
  |> should.equal(["email", "name"])
}

pub fn insert_columns_by_name_sets_column_names_test() {
  let query =
    insert_into(users())
    |> insert_columns_by_name(["email", "name"])

  insert_column_names(query)
  |> should.equal(["email", "name"])
}

pub fn insert_string_value_adds_value_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_string_value(user_email(), "test@example.com")

  insert_row_count(query)
  |> should.equal(1)

  let ast_query = insert_to_ast(query)
  case ast_query.values {
    [[ast.StringValue(value)]] -> {
      value |> should.equal("test@example.com")
    }
    _ -> should.fail()
  }
}

pub fn insert_int_value_adds_integer_test() {
  let query =
    insert_into(users())
    |> insert_column(user_age())
    |> insert_int_value(user_age(), 25)

  let ast_query = insert_to_ast(query)
  case ast_query.values {
    [[ast.IntValue(value)]] -> {
      value |> should.equal(25)
    }
    _ -> should.fail()
  }
}

pub fn insert_bool_value_adds_boolean_test() {
  let query =
    insert_into(users())
    |> insert_column(user_active())
    |> insert_bool_value(user_active(), True)

  let ast_query = insert_to_ast(query)
  case ast_query.values {
    [[ast.BoolValue(value)]] -> {
      value |> should.equal(True)
    }
    _ -> should.fail()
  }
}

pub fn insert_row_adds_complete_row_test() {
  let query =
    insert_into(users())
    |> insert_columns_by_name(["email", "name"])
    |> insert_row([ast.StringValue("test@example.com"), ast.StringValue("Test")])

  insert_row_count(query)
  |> should.equal(1)
}

pub fn insert_rows_adds_multiple_rows_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_rows([
      [ast.StringValue("user1@example.com")],
      [ast.StringValue("user2@example.com")],
      [ast.StringValue("user3@example.com")],
    ])

  insert_row_count(query)
  |> should.equal(3)
}

pub fn insert_returning_sets_columns_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_returning_by_name(["id", "email"])

  insert_returning_columns(query)
  |> should.equal(["id", "email"])
}

pub fn insert_returning_all_sets_star_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_returning_all()

  insert_returning_columns(query)
  |> should.equal(["*"])
}

pub fn on_conflict_do_nothing_sets_conflict_strategy_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> on_conflict_do_nothing()

  insert_has_on_conflict(query)
  |> should.be_true

  let ast_query = insert_to_ast(query)
  case ast_query.on_conflict {
    Some(ast.DoNothing) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn on_conflict_do_update_sets_upsert_strategy_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_column(user_name())
    |> on_conflict_do_update_by_name(
      conflict_columns: ["email"],
      update_columns: ["name"],
    )

  let ast_query = insert_to_ast(query)
  case ast_query.on_conflict {
    Some(ast.DoUpdate(conflict_cols, update_cols)) -> {
      conflict_cols |> should.equal(["email"])
      update_cols |> should.equal(["name"])
    }
    _ -> should.fail()
  }
}

pub fn on_conflict_constraint_do_nothing_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> on_conflict_constraint_do_nothing("users_email_unique")

  let ast_query = insert_to_ast(query)
  case ast_query.on_conflict {
    Some(ast.DoNothingOnConstraint(name)) -> {
      name |> should.equal("users_email_unique")
    }
    _ -> should.fail()
  }
}

pub fn on_conflict_constraint_do_update_test() {
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_column(user_name())
    |> on_conflict_constraint_do_update_by_name("users_email_unique", ["name"])

  let ast_query = insert_to_ast(query)
  case ast_query.on_conflict {
    Some(ast.DoUpdateOnConstraint(name, update_cols)) -> {
      name |> should.equal("users_email_unique")
      update_cols |> should.equal(["name"])
    }
    _ -> should.fail()
  }
}

// ============================================================================
// UPDATE QUERY TESTS
// ============================================================================

pub fn update_creates_basic_query_test() {
  let query = update(users())

  update_table_name(query)
  |> should.equal("users")

  update_set_count(query)
  |> should.equal(0)

  update_has_where(query)
  |> should.be_false
}

pub fn set_string_adds_set_clause_test() {
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")

  update_set_count(query)
  |> should.equal(1)

  let ast_query = update_to_ast(query)
  case ast_query.sets {
    [ast.SetClause(column, value)] -> {
      column |> should.equal("email")
      case value {
        ast.StringValue(v) -> v |> should.equal("new@example.com")
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn set_int_adds_integer_set_test() {
  let query =
    update(users())
    |> set_int(user_age(), 30)

  let ast_query = update_to_ast(query)
  case ast_query.sets {
    [ast.SetClause(column, value)] -> {
      column |> should.equal("age")
      case value {
        ast.IntValue(v) -> v |> should.equal(30)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn set_bool_adds_boolean_set_test() {
  let query =
    update(users())
    |> set_bool(user_active(), False)

  let ast_query = update_to_ast(query)
  case ast_query.sets {
    [ast.SetClause(column, value)] -> {
      column |> should.equal("active")
      case value {
        ast.BoolValue(v) -> v |> should.equal(False)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn multiple_set_clauses_test() {
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")
    |> set_string(user_name(), "New Name")
    |> set_bool(user_active(), True)

  update_set_count(query)
  |> should.equal(3)
}

pub fn set_where_adds_condition_test() {
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")
    |> set_where(ast.Eq("id", ast.IntValue(42)))

  update_has_where(query)
  |> should.be_true

  let ast_query = update_to_ast(query)
  list.length(ast_query.wheres)
  |> should.equal(1)
}

pub fn set_where_raw_adds_raw_condition_test() {
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")
    |> set_where_raw("id = $1", [ast.IntValue(42)])

  update_has_where(query)
  |> should.be_true
}

pub fn update_returning_sets_columns_test() {
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")
    |> update_returning_by_name(["id", "email"])

  update_returning_columns(query)
  |> should.equal(["id", "email"])
}

pub fn update_returning_all_sets_star_test() {
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")
    |> update_returning_all()

  update_returning_columns(query)
  |> should.equal(["*"])
}

// ============================================================================
// DELETE QUERY TESTS
// ============================================================================

pub fn delete_from_creates_basic_query_test() {
  let query = delete_from(users())

  delete_table_name(query)
  |> should.equal("users")

  delete_has_where(query)
  |> should.be_false
}

pub fn delete_where_adds_condition_test() {
  let query =
    delete_from(users())
    |> delete_where(ast.Eq("id", ast.IntValue(42)))

  delete_has_where(query)
  |> should.be_true

  let ast_query = delete_to_ast(query)
  list.length(ast_query.wheres)
  |> should.equal(1)
}

pub fn delete_all_marks_explicit_bulk_delete_test() {
  let query =
    delete_from(users())
    |> delete_all()

  delete_has_where(query)
  |> should.be_true

  let ast_query = delete_to_ast(query)
  case ast_query.wheres {
    [ast.Where(condition)] -> {
      case condition {
        ast.Raw(sql, _) -> sql |> should.equal("1=1")
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn delete_returning_sets_columns_test() {
  let query =
    delete_from(users())
    |> delete_where(ast.Eq("id", ast.IntValue(42)))
    |> delete_returning_column(user_id())

  delete_returning_columns(query)
  |> should.equal(["id"])
}

pub fn delete_returning_all_sets_star_test() {
  let query =
    delete_from(users())
    |> delete_where(ast.Eq("id", ast.IntValue(42)))
    |> delete_returning_all()

  delete_returning_columns(query)
  |> should.equal(["*"])
}

pub fn multiple_delete_where_clauses_test() {
  let query =
    delete_from(users())
    |> delete_where(ast.Eq("active", ast.BoolValue(False)))
    |> delete_where(ast.Lt("age", ast.IntValue(18)))

  let ast_query = delete_to_ast(query)
  list.length(ast_query.wheres)
  |> should.equal(2)
}

// ============================================================================
// TYPE SAFETY DEMONSTRATION TESTS
// ============================================================================

pub fn insert_uses_correct_table_columns_test() {
  // This demonstrates type safety - we can only use user columns with users table
  let query =
    insert_into(users())
    |> insert_column(user_email())
    |> insert_column(user_name())
    |> insert_string_value(user_email(), "test@example.com")

  insert_table_name(query)
  |> should.equal("users")
}

pub fn update_uses_correct_table_columns_test() {
  // This demonstrates type safety - we can only set user columns on users table
  let query =
    update(users())
    |> set_string(user_email(), "new@example.com")
    |> set_bool(user_active(), True)

  update_table_name(query)
  |> should.equal("users")
}

pub fn delete_uses_correct_table_test() {
  // This demonstrates type safety - we can only use user columns with users table
  let query =
    delete_from(users())
    |> delete_returning_column(user_id())

  delete_table_name(query)
  |> should.equal("users")
}

// ============================================================================
// COMPLEX QUERY TESTS
// ============================================================================

pub fn complete_insert_query_test() {
  // Test a complete INSERT query with all features
  let query =
    insert_into(users())
    |> insert_columns_by_name(["email", "name", "active"])
    |> insert_row([
      ast.StringValue("test@example.com"),
      ast.StringValue("Test User"),
      ast.BoolValue(True),
    ])
    |> on_conflict_do_update_by_name(
      conflict_columns: ["email"],
      update_columns: ["name"],
    )
    |> insert_returning_column(user_id())

  insert_table_name(query) |> should.equal("users")
  insert_column_names(query) |> should.equal(["email", "name", "active"])
  insert_row_count(query) |> should.equal(1)
  insert_has_on_conflict(query) |> should.be_true
  insert_returning_columns(query) |> should.equal(["id"])
}

pub fn complete_update_query_test() {
  // Test a complete UPDATE query with all features
  let query =
    update(users())
    |> set_string(user_email(), "updated@example.com")
    |> set_string(user_name(), "Updated Name")
    |> set_bool(user_active(), True)
    |> set_where(ast.Eq("id", ast.IntValue(42)))
    |> update_returning_all()

  update_table_name(query) |> should.equal("users")
  update_set_count(query) |> should.equal(3)
  update_has_where(query) |> should.be_true
  update_returning_columns(query) |> should.equal(["*"])
}

pub fn complete_delete_query_test() {
  // Test a complete DELETE query with all features
  let query =
    delete_from(users())
    |> delete_where(ast.Eq("id", ast.IntValue(42)))
    |> delete_returning_by_name(["id", "email"])

  delete_table_name(query) |> should.equal("users")
  delete_has_where(query) |> should.be_true
  delete_returning_columns(query) |> should.equal(["id", "email"])
}

// ============================================================================
// POSTS TABLE TESTS (verify different tables work)
// ============================================================================

pub fn insert_into_different_table_test() {
  let query =
    insert_into(posts())
    |> insert_column(post_title())
    |> insert_column(post_published())
    |> insert_string_value(post_title(), "Hello World")

  insert_table_name(query)
  |> should.equal("posts")
}

pub fn update_different_table_test() {
  let query =
    update(posts())
    |> set_bool(post_published(), True)

  update_table_name(query)
  |> should.equal("posts")
}

pub fn delete_from_different_table_test() {
  let query =
    delete_from(posts())
    |> delete_where(ast.Eq("id", ast.IntValue(1)))
    |> delete_returning_column(post_id())

  delete_table_name(query)
  |> should.equal("posts")
}
