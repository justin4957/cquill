// Phantom-Typed Table and Column Tests
//
// Tests for the phantom type infrastructure that provides compile-time
// type safety for database queries.

import cquill/typed/table.{
  type Column, type Join2, type Table, column, column_aliased, column_name,
  column_names, column_qualified_name, column_table_alias, in_join2_left,
  in_join2_right, table, table_in_schema, table_name, table_qualified_name,
  table_schema_name,
}
import gleeunit/should

// ============================================================================
// MOCK TABLE TYPES (simulating generated code)
// ============================================================================

/// Phantom type representing the "users" table
pub type UserTable

/// Phantom type representing the "posts" table
pub type PostTable

/// Phantom type representing the "comments" table
pub type CommentTable

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

fn user_name() -> Column(UserTable, String) {
  column("name")
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
// TABLE CONSTRUCTION TESTS
// ============================================================================

pub fn table_creates_with_name_test() {
  let t: Table(UserTable) = table("users")
  table_name(t)
  |> should.equal("users")
}

pub fn table_defaults_to_public_schema_test() {
  let t: Table(UserTable) = table("users")
  table_schema_name(t)
  |> should.equal("public")
}

pub fn table_in_schema_sets_schema_name_test() {
  let t: Table(UserTable) = table_in_schema("my_schema", "users")
  table_schema_name(t)
  |> should.equal("my_schema")
  table_name(t)
  |> should.equal("users")
}

pub fn table_qualified_name_for_public_schema_test() {
  let t: Table(UserTable) = table("users")
  table_qualified_name(t)
  |> should.equal("users")
}

pub fn table_qualified_name_for_custom_schema_test() {
  let t: Table(UserTable) = table_in_schema("my_schema", "users")
  table_qualified_name(t)
  |> should.equal("my_schema.users")
}

// ============================================================================
// COLUMN CONSTRUCTION TESTS
// ============================================================================

pub fn column_creates_with_name_test() {
  let c: Column(UserTable, String) = column("email")
  column_name(c)
  |> should.equal("email")
}

pub fn column_has_empty_alias_by_default_test() {
  let c: Column(UserTable, String) = column("email")
  column_table_alias(c)
  |> should.equal("")
}

pub fn column_aliased_sets_table_alias_test() {
  let c: Column(UserTable, String) = column_aliased("u", "email")
  column_name(c)
  |> should.equal("email")
  column_table_alias(c)
  |> should.equal("u")
}

pub fn column_qualified_name_without_alias_test() {
  let c: Column(UserTable, String) = column("email")
  column_qualified_name(c)
  |> should.equal("email")
}

pub fn column_qualified_name_with_alias_test() {
  let c: Column(UserTable, String) = column_aliased("u", "email")
  column_qualified_name(c)
  |> should.equal("u.email")
}

// ============================================================================
// COLUMN NAMES LIST TESTS
// ============================================================================

pub fn column_names_extracts_all_names_test() {
  let cols: List(Column(UserTable, String)) = [user_email(), user_name()]
  column_names(cols)
  |> should.equal(["email", "name"])
}

pub fn column_names_empty_list_test() {
  let cols: List(Column(UserTable, String)) = []
  column_names(cols)
  |> should.equal([])
}

pub fn column_names_single_column_test() {
  let cols: List(Column(UserTable, String)) = [user_email()]
  column_names(cols)
  |> should.equal(["email"])
}

// ============================================================================
// JOIN TYPE COERCION TESTS
// ============================================================================

pub fn in_join2_left_coerces_column_test() {
  let original: Column(UserTable, String) = user_email()
  let coerced: Column(Join2(UserTable, PostTable), String) =
    in_join2_left(original)
  column_name(coerced)
  |> should.equal("email")
}

pub fn in_join2_right_coerces_column_test() {
  let original: Column(PostTable, String) = post_title()
  let coerced: Column(Join2(UserTable, PostTable), String) =
    in_join2_right(original)
  column_name(coerced)
  |> should.equal("title")
}

pub fn join2_columns_can_be_used_together_test() {
  // Both columns coerced to the same join type can be used together
  let left: Column(Join2(UserTable, PostTable), Int) = in_join2_left(user_id())
  let right: Column(Join2(UserTable, PostTable), Int) =
    in_join2_right(post_user_id())

  // They have the same table type now
  column_name(left)
  |> should.equal("id")
  column_name(right)
  |> should.equal("user_id")
}

// ============================================================================
// PHANTOM TYPE SAFETY TESTS (Compile-time verification)
// ============================================================================

// The following tests verify that the type system correctly tracks table types.
// These tests compile successfully because the types are used correctly.

pub fn same_table_columns_can_be_used_together_test() {
  // All these columns belong to UserTable
  let id: Column(UserTable, Int) = user_id()
  let email: Column(UserTable, String) = user_email()
  let name: Column(UserTable, String) = user_name()

  // They can all be collected into a list of UserTable columns
  column_name(id)
  |> should.equal("id")
  column_name(email)
  |> should.equal("email")
  column_name(name)
  |> should.equal("name")
}

pub fn different_value_types_on_same_table_test() {
  // Columns with different value types but same table type
  let int_col: Column(UserTable, Int) = user_id()
  let str_col: Column(UserTable, String) = user_email()

  column_name(int_col)
  |> should.equal("id")
  column_name(str_col)
  |> should.equal("email")
}

// ============================================================================
// PREDEFINED COLUMN TESTS
// ============================================================================

pub fn user_columns_have_correct_names_test() {
  column_name(user_id())
  |> should.equal("id")
  column_name(user_email())
  |> should.equal("email")
  column_name(user_name())
  |> should.equal("name")
}

pub fn post_columns_have_correct_names_test() {
  column_name(post_id())
  |> should.equal("id")
  column_name(post_user_id())
  |> should.equal("user_id")
  column_name(post_title())
  |> should.equal("title")
}

pub fn table_references_have_correct_names_test() {
  table_name(users())
  |> should.equal("users")
  table_name(posts())
  |> should.equal("posts")
}
