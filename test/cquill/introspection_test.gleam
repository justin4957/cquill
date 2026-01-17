// Tests for the schema introspection module
//
// These tests verify:
// - SQL query string generation
// - Parsing functions for various data types
// - Building functions for schema structures
// - Schema assembly from raw query results
// - Utility functions for schema navigation
// - Type mapping helpers

import cquill/introspection.{
  Cascade, IntrospectedColumn, IntrospectedEnum, IntrospectedForeignKey,
  IntrospectedSchema, IntrospectedTable, NoAction, RawCheckRow, RawColumnRow,
  RawEnumRow, RawForeignKeyRow, RawPrimaryKeyRow, RawUniqueRow, Restrict,
  SetDefault, SetNull,
}
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// SQL QUERY TESTS
// ============================================================================

pub fn columns_query_contains_required_columns_test() {
  let query = introspection.columns_query()

  // Should contain all the columns we need
  query |> query_should_contain("table_name") |> should.be_true
  query |> query_should_contain("column_name") |> should.be_true
  query |> query_should_contain("ordinal_position") |> should.be_true
  query |> query_should_contain("data_type") |> should.be_true
  query |> query_should_contain("udt_name") |> should.be_true
  query |> query_should_contain("is_nullable") |> should.be_true
  query |> query_should_contain("column_default") |> should.be_true
}

fn query_should_contain(query: String, substring: String) -> Bool {
  string.contains(query, substring)
}

pub fn columns_query_uses_information_schema_test() {
  let query = introspection.columns_query()

  query |> query_should_contain("information_schema.tables") |> should.be_true
  query |> query_should_contain("information_schema.columns") |> should.be_true
}

pub fn columns_query_filters_base_tables_test() {
  let query = introspection.columns_query()

  // Should only select base tables, not views
  query |> query_should_contain("BASE TABLE") |> should.be_true
}

pub fn columns_query_has_schema_parameter_test() {
  let query = introspection.columns_query()

  // Should have a schema parameter placeholder
  query |> query_should_contain("$1") |> should.be_true
}

pub fn primary_keys_query_has_required_structure_test() {
  let query = introspection.primary_keys_query()

  query |> query_should_contain("table_name") |> should.be_true
  query |> query_should_contain("column_name") |> should.be_true
  query |> query_should_contain("ordinal_position") |> should.be_true
  query |> query_should_contain("PRIMARY KEY") |> should.be_true
  query |> query_should_contain("table_constraints") |> should.be_true
  query |> query_should_contain("key_column_usage") |> should.be_true
}

pub fn foreign_keys_query_has_required_structure_test() {
  let query = introspection.foreign_keys_query()

  query |> query_should_contain("table_name") |> should.be_true
  query |> query_should_contain("column_name") |> should.be_true
  query |> query_should_contain("foreign_table_name") |> should.be_true
  query |> query_should_contain("foreign_column_name") |> should.be_true
  query |> query_should_contain("update_rule") |> should.be_true
  query |> query_should_contain("delete_rule") |> should.be_true
  query |> query_should_contain("FOREIGN KEY") |> should.be_true
}

pub fn unique_constraints_query_has_required_structure_test() {
  let query = introspection.unique_constraints_query()

  query |> query_should_contain("table_name") |> should.be_true
  query |> query_should_contain("constraint_name") |> should.be_true
  query |> query_should_contain("column_name") |> should.be_true
  query |> query_should_contain("UNIQUE") |> should.be_true
}

pub fn check_constraints_query_has_required_structure_test() {
  let query = introspection.check_constraints_query()

  query |> query_should_contain("table_name") |> should.be_true
  query |> query_should_contain("constraint_name") |> should.be_true
  query |> query_should_contain("check_clause") |> should.be_true
  query |> query_should_contain("CHECK") |> should.be_true
}

pub fn check_constraints_query_excludes_not_null_test() {
  let query = introspection.check_constraints_query()

  // Should exclude system-generated NOT NULL constraints
  query |> query_should_contain("_not_null") |> should.be_true
}

pub fn enums_query_has_required_structure_test() {
  let query = introspection.enums_query()

  query |> query_should_contain("enum_name") |> should.be_true
  query |> query_should_contain("enum_value") |> should.be_true
  query |> query_should_contain("enumsortorder") |> should.be_true
  query |> query_should_contain("pg_type") |> should.be_true
  query |> query_should_contain("pg_enum") |> should.be_true
}

// ============================================================================
// PARSING FUNCTION TESTS
// ============================================================================

pub fn parse_fk_action_no_action_test() {
  introspection.parse_fk_action("NO ACTION")
  |> should.equal(NoAction)
}

pub fn parse_fk_action_restrict_test() {
  introspection.parse_fk_action("RESTRICT")
  |> should.equal(Restrict)
}

pub fn parse_fk_action_cascade_test() {
  introspection.parse_fk_action("CASCADE")
  |> should.equal(Cascade)
}

pub fn parse_fk_action_set_null_test() {
  introspection.parse_fk_action("SET NULL")
  |> should.equal(SetNull)
}

pub fn parse_fk_action_set_default_test() {
  introspection.parse_fk_action("SET DEFAULT")
  |> should.equal(SetDefault)
}

pub fn parse_fk_action_lowercase_test() {
  // Should handle lowercase input
  introspection.parse_fk_action("cascade")
  |> should.equal(Cascade)
}

pub fn parse_fk_action_unknown_defaults_to_no_action_test() {
  introspection.parse_fk_action("UNKNOWN")
  |> should.equal(NoAction)
}

pub fn parse_nullable_yes_test() {
  introspection.parse_nullable("YES")
  |> should.be_true
}

pub fn parse_nullable_no_test() {
  introspection.parse_nullable("NO")
  |> should.be_false
}

pub fn parse_nullable_lowercase_y_test() {
  introspection.parse_nullable("y")
  |> should.be_true
}

pub fn parse_nullable_true_string_test() {
  introspection.parse_nullable("TRUE")
  |> should.be_true
}

pub fn parse_nullable_one_string_test() {
  introspection.parse_nullable("1")
  |> should.be_true
}

pub fn parse_nullable_unknown_defaults_false_test() {
  introspection.parse_nullable("MAYBE")
  |> should.be_false
}

// ============================================================================
// BUILD FUNCTION TESTS
// ============================================================================

pub fn build_column_creates_introspected_column_test() {
  let raw_row =
    RawColumnRow(
      table_name: "users",
      column_name: "email",
      ordinal_position: 2,
      data_type: "character varying",
      udt_name: "varchar",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: Some(255),
      numeric_precision: None,
      numeric_scale: None,
    )

  let column = introspection.build_column(raw_row)

  column.name |> should.equal("email")
  column.position |> should.equal(2)
  column.data_type |> should.equal("character varying")
  column.udt_name |> should.equal("varchar")
  column.is_nullable |> should.be_false
  column.default |> should.equal(None)
  column.max_length |> should.equal(Some(255))
}

pub fn build_column_with_default_value_test() {
  let raw_row =
    RawColumnRow(
      table_name: "users",
      column_name: "created_at",
      ordinal_position: 5,
      data_type: "timestamp with time zone",
      udt_name: "timestamptz",
      is_nullable: "YES",
      column_default: Some("now()"),
      character_maximum_length: None,
      numeric_precision: None,
      numeric_scale: None,
    )

  let column = introspection.build_column(raw_row)

  column.is_nullable |> should.be_true
  column.default |> should.equal(Some("now()"))
}

pub fn build_column_with_numeric_precision_test() {
  let raw_row =
    RawColumnRow(
      table_name: "products",
      column_name: "price",
      ordinal_position: 3,
      data_type: "numeric",
      udt_name: "numeric",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(10),
      numeric_scale: Some(2),
    )

  let column = introspection.build_column(raw_row)

  column.numeric_precision |> should.equal(Some(10))
  column.numeric_scale |> should.equal(Some(2))
}

pub fn build_foreign_key_test() {
  let raw_row =
    RawForeignKeyRow(
      table_name: "posts",
      column_name: "user_id",
      foreign_table_name: "users",
      foreign_column_name: "id",
      update_rule: "CASCADE",
      delete_rule: "SET NULL",
    )

  let foreign_key = introspection.build_foreign_key(raw_row)

  foreign_key.column_name |> should.equal("user_id")
  foreign_key.foreign_table |> should.equal("users")
  foreign_key.foreign_column |> should.equal("id")
  foreign_key.on_update |> should.equal(Cascade)
  foreign_key.on_delete |> should.equal(SetNull)
}

pub fn build_check_test() {
  let raw_row =
    RawCheckRow(
      table_name: "users",
      constraint_name: "users_age_check",
      check_clause: "((age >= 0) AND (age <= 150))",
    )

  let check = introspection.build_check(raw_row)

  check.constraint_name |> should.equal("users_age_check")
  check.check_clause |> should.equal("((age >= 0) AND (age <= 150))")
}

// ============================================================================
// SCHEMA ASSEMBLY TESTS
// ============================================================================

pub fn build_schema_empty_inputs_test() {
  let schema = introspection.build_schema([], [], [], [], [], [])

  schema.tables |> should.equal([])
  schema.enums |> should.equal([])
}

pub fn build_schema_single_table_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "users",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "users",
      column_name: "name",
      ordinal_position: 2,
      data_type: "text",
      udt_name: "text",
      is_nullable: "YES",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: None,
      numeric_scale: None,
    ),
  ]

  let pk_rows = [
    RawPrimaryKeyRow(
      table_name: "users",
      column_name: "id",
      ordinal_position: 1,
    ),
  ]

  let schema = introspection.build_schema(column_rows, pk_rows, [], [], [], [])

  // Should have one table
  schema.tables |> should.not_equal([])
  let assert [table] = schema.tables

  table.name |> should.equal("users")
  table.columns |> should.not_equal([])
  table.primary_key |> should.equal(["id"])
}

pub fn build_schema_composite_primary_key_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "order_items",
      column_name: "order_id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "order_items",
      column_name: "product_id",
      ordinal_position: 2,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "order_items",
      column_name: "quantity",
      ordinal_position: 3,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
  ]

  let pk_rows = [
    RawPrimaryKeyRow(
      table_name: "order_items",
      column_name: "order_id",
      ordinal_position: 1,
    ),
    RawPrimaryKeyRow(
      table_name: "order_items",
      column_name: "product_id",
      ordinal_position: 2,
    ),
  ]

  let schema = introspection.build_schema(column_rows, pk_rows, [], [], [], [])

  let assert [table] = schema.tables
  table.primary_key |> should.equal(["order_id", "product_id"])
}

pub fn build_schema_with_foreign_keys_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "posts",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "posts",
      column_name: "user_id",
      ordinal_position: 2,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
  ]

  let pk_rows = [
    RawPrimaryKeyRow(
      table_name: "posts",
      column_name: "id",
      ordinal_position: 1,
    ),
  ]

  let fk_rows = [
    RawForeignKeyRow(
      table_name: "posts",
      column_name: "user_id",
      foreign_table_name: "users",
      foreign_column_name: "id",
      update_rule: "CASCADE",
      delete_rule: "CASCADE",
    ),
  ]

  let schema =
    introspection.build_schema(column_rows, pk_rows, fk_rows, [], [], [])

  let assert [table] = schema.tables
  table.foreign_keys |> should.not_equal([])
  let assert [fk] = table.foreign_keys
  fk.foreign_table |> should.equal("users")
}

pub fn build_schema_with_unique_constraints_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "users",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "users",
      column_name: "email",
      ordinal_position: 2,
      data_type: "text",
      udt_name: "text",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: None,
      numeric_scale: None,
    ),
  ]

  let unique_rows = [
    RawUniqueRow(
      table_name: "users",
      constraint_name: "users_email_unique",
      column_name: "email",
    ),
  ]

  let schema =
    introspection.build_schema(column_rows, [], [], unique_rows, [], [])

  let assert [table] = schema.tables
  table.unique_constraints |> should.not_equal([])
  let assert [unique] = table.unique_constraints
  unique.constraint_name |> should.equal("users_email_unique")
  unique.columns |> should.equal(["email"])
}

pub fn build_schema_with_multi_column_unique_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "users",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "users",
      column_name: "first_name",
      ordinal_position: 2,
      data_type: "text",
      udt_name: "text",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: None,
      numeric_scale: None,
    ),
    RawColumnRow(
      table_name: "users",
      column_name: "last_name",
      ordinal_position: 3,
      data_type: "text",
      udt_name: "text",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: None,
      numeric_scale: None,
    ),
  ]

  let unique_rows = [
    RawUniqueRow(
      table_name: "users",
      constraint_name: "users_name_unique",
      column_name: "first_name",
    ),
    RawUniqueRow(
      table_name: "users",
      constraint_name: "users_name_unique",
      column_name: "last_name",
    ),
  ]

  let schema =
    introspection.build_schema(column_rows, [], [], unique_rows, [], [])

  let assert [table] = schema.tables
  let assert [unique] = table.unique_constraints
  unique.columns |> should.equal(["first_name", "last_name"])
}

pub fn build_schema_with_check_constraints_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "users",
      column_name: "age",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "YES",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
  ]

  let check_rows = [
    RawCheckRow(
      table_name: "users",
      constraint_name: "users_age_check",
      check_clause: "((age >= 0))",
    ),
  ]

  let schema =
    introspection.build_schema(column_rows, [], [], [], check_rows, [])

  let assert [table] = schema.tables
  table.check_constraints |> should.not_equal([])
  let assert [check] = table.check_constraints
  check.constraint_name |> should.equal("users_age_check")
}

pub fn build_schema_with_enums_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "users",
      column_name: "status",
      ordinal_position: 1,
      data_type: "USER-DEFINED",
      udt_name: "user_status",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: None,
      numeric_scale: None,
    ),
  ]

  let enum_rows = [
    RawEnumRow(
      enum_name: "user_status",
      enum_value: "active",
      enum_sort_order: 1.0,
    ),
    RawEnumRow(
      enum_name: "user_status",
      enum_value: "inactive",
      enum_sort_order: 2.0,
    ),
    RawEnumRow(
      enum_name: "user_status",
      enum_value: "banned",
      enum_sort_order: 3.0,
    ),
  ]

  let schema =
    introspection.build_schema(column_rows, [], [], [], [], enum_rows)

  schema.enums |> should.not_equal([])
  let assert [enum_type] = schema.enums
  enum_type.name |> should.equal("user_status")
  enum_type.values |> should.equal(["active", "inactive", "banned"])
}

pub fn build_schema_multiple_tables_test() {
  let column_rows = [
    RawColumnRow(
      table_name: "users",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "posts",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
    RawColumnRow(
      table_name: "comments",
      column_name: "id",
      ordinal_position: 1,
      data_type: "integer",
      udt_name: "int4",
      is_nullable: "NO",
      column_default: None,
      character_maximum_length: None,
      numeric_precision: Some(32),
      numeric_scale: Some(0),
    ),
  ]

  let schema = introspection.build_schema(column_rows, [], [], [], [], [])

  // Should have three tables
  schema.tables
  |> should.not_equal([])

  let table_count = case schema.tables {
    [_, _, _] -> 3
    _ -> 0
  }
  table_count |> should.equal(3)
}

// ============================================================================
// UTILITY FUNCTION TESTS
// ============================================================================

pub fn empty_schema_test() {
  let schema = introspection.empty_schema()

  schema.tables |> should.equal([])
  schema.enums |> should.equal([])
}

pub fn empty_table_test() {
  let table = introspection.empty_table("users")

  table.name |> should.equal("users")
  table.columns |> should.equal([])
  table.primary_key |> should.equal([])
  table.foreign_keys |> should.equal([])
  table.unique_constraints |> should.equal([])
  table.check_constraints |> should.equal([])
}

pub fn find_table_existing_test() {
  let schema =
    IntrospectedSchema(
      tables: [
        introspection.empty_table("users"),
        introspection.empty_table("posts"),
      ],
      enums: [],
    )

  introspection.find_table(schema, "users")
  |> should.be_some
}

pub fn find_table_not_existing_test() {
  let schema = introspection.empty_schema()

  introspection.find_table(schema, "users")
  |> should.be_none
}

pub fn find_column_existing_test() {
  let column =
    IntrospectedColumn(
      name: "email",
      position: 1,
      data_type: "text",
      udt_name: "text",
      is_nullable: False,
      default: None,
      max_length: None,
      numeric_precision: None,
      numeric_scale: None,
    )

  let table =
    IntrospectedTable(..introspection.empty_table("users"), columns: [column])

  introspection.find_column(table, "email")
  |> should.be_some
}

pub fn find_column_not_existing_test() {
  let table = introspection.empty_table("users")

  introspection.find_column(table, "email")
  |> should.be_none
}

pub fn find_enum_existing_test() {
  let schema =
    IntrospectedSchema(tables: [], enums: [
      IntrospectedEnum(name: "status", values: ["a", "b"]),
    ])

  introspection.find_enum(schema, "status")
  |> should.be_some
}

pub fn find_enum_not_existing_test() {
  let schema = introspection.empty_schema()

  introspection.find_enum(schema, "status")
  |> should.be_none
}

pub fn table_names_test() {
  let schema =
    IntrospectedSchema(
      tables: [
        introspection.empty_table("users"),
        introspection.empty_table("posts"),
        introspection.empty_table("comments"),
      ],
      enums: [],
    )

  introspection.table_names(schema)
  |> should.equal(["users", "posts", "comments"])
}

pub fn column_names_test() {
  let table =
    IntrospectedTable(
      name: "users",
      columns: [
        IntrospectedColumn(
          name: "id",
          position: 1,
          data_type: "integer",
          udt_name: "int4",
          is_nullable: False,
          default: None,
          max_length: None,
          numeric_precision: None,
          numeric_scale: None,
        ),
        IntrospectedColumn(
          name: "email",
          position: 2,
          data_type: "text",
          udt_name: "text",
          is_nullable: False,
          default: None,
          max_length: None,
          numeric_precision: None,
          numeric_scale: None,
        ),
      ],
      primary_key: ["id"],
      foreign_keys: [],
      unique_constraints: [],
      check_constraints: [],
    )

  introspection.column_names(table)
  |> should.equal(["id", "email"])
}

pub fn has_primary_key_true_test() {
  let table =
    IntrospectedTable(..introspection.empty_table("users"), primary_key: ["id"])

  introspection.has_primary_key(table)
  |> should.be_true
}

pub fn has_primary_key_false_test() {
  let table = introspection.empty_table("users")

  introspection.has_primary_key(table)
  |> should.be_false
}

pub fn has_composite_primary_key_true_test() {
  let table =
    IntrospectedTable(..introspection.empty_table("order_items"), primary_key: [
      "order_id",
      "product_id",
    ])

  introspection.has_composite_primary_key(table)
  |> should.be_true
}

pub fn has_composite_primary_key_false_single_test() {
  let table =
    IntrospectedTable(..introspection.empty_table("users"), primary_key: ["id"])

  introspection.has_composite_primary_key(table)
  |> should.be_false
}

pub fn has_composite_primary_key_false_no_pk_test() {
  let table = introspection.empty_table("users")

  introspection.has_composite_primary_key(table)
  |> should.be_false
}

pub fn tables_referencing_test() {
  let users_table = introspection.empty_table("users")
  let posts_table =
    IntrospectedTable(..introspection.empty_table("posts"), foreign_keys: [
      IntrospectedForeignKey(
        column_name: "user_id",
        foreign_table: "users",
        foreign_column: "id",
        on_update: NoAction,
        on_delete: Cascade,
      ),
    ])
  let comments_table =
    IntrospectedTable(..introspection.empty_table("comments"), foreign_keys: [
      IntrospectedForeignKey(
        column_name: "user_id",
        foreign_table: "users",
        foreign_column: "id",
        on_update: NoAction,
        on_delete: Cascade,
      ),
    ])

  let schema =
    IntrospectedSchema(
      tables: [users_table, posts_table, comments_table],
      enums: [],
    )

  let referencing = introspection.tables_referencing(schema, "users")
  // posts and comments should both reference users
  let count = case referencing {
    [_, _] -> 2
    _ -> 0
  }
  count |> should.equal(2)
}

pub fn is_primary_key_column_true_test() {
  let table =
    IntrospectedTable(..introspection.empty_table("users"), primary_key: ["id"])

  introspection.is_primary_key_column(table, "id")
  |> should.be_true
}

pub fn is_primary_key_column_false_test() {
  let table =
    IntrospectedTable(..introspection.empty_table("users"), primary_key: ["id"])

  introspection.is_primary_key_column(table, "email")
  |> should.be_false
}

pub fn has_foreign_key_true_test() {
  let table =
    IntrospectedTable(..introspection.empty_table("posts"), foreign_keys: [
      IntrospectedForeignKey(
        column_name: "user_id",
        foreign_table: "users",
        foreign_column: "id",
        on_update: NoAction,
        on_delete: Cascade,
      ),
    ])

  introspection.has_foreign_key(table, "user_id")
  |> should.be_true
}

pub fn has_foreign_key_false_test() {
  let table = introspection.empty_table("posts")

  introspection.has_foreign_key(table, "user_id")
  |> should.be_false
}

pub fn get_column_foreign_key_some_test() {
  let fk =
    IntrospectedForeignKey(
      column_name: "user_id",
      foreign_table: "users",
      foreign_column: "id",
      on_update: NoAction,
      on_delete: Cascade,
    )
  let table =
    IntrospectedTable(..introspection.empty_table("posts"), foreign_keys: [fk])

  introspection.get_column_foreign_key(table, "user_id")
  |> should.be_some
}

pub fn get_column_foreign_key_none_test() {
  let table = introspection.empty_table("posts")

  introspection.get_column_foreign_key(table, "user_id")
  |> should.be_none
}

pub fn get_foreign_keys_test() {
  let fk =
    IntrospectedForeignKey(
      column_name: "user_id",
      foreign_table: "users",
      foreign_column: "id",
      on_update: NoAction,
      on_delete: Cascade,
    )
  let table =
    IntrospectedTable(..introspection.empty_table("posts"), foreign_keys: [fk])

  introspection.get_foreign_keys(table)
  |> should.equal([fk])
}

// ============================================================================
// TYPE MAPPING TESTS
// ============================================================================

pub fn gleam_type_for_integer_test() {
  introspection.gleam_type_for_postgres("integer", "int4", False)
  |> should.equal("Int")
}

pub fn gleam_type_for_bigint_test() {
  introspection.gleam_type_for_postgres("bigint", "int8", False)
  |> should.equal("Int")
}

pub fn gleam_type_for_smallint_test() {
  introspection.gleam_type_for_postgres("smallint", "int2", False)
  |> should.equal("Int")
}

pub fn gleam_type_for_serial_test() {
  introspection.gleam_type_for_postgres("serial", "serial4", False)
  |> should.equal("Int")
}

pub fn gleam_type_for_bigserial_test() {
  introspection.gleam_type_for_postgres("bigserial", "serial8", False)
  |> should.equal("Int")
}

pub fn gleam_type_for_real_test() {
  introspection.gleam_type_for_postgres("real", "float4", False)
  |> should.equal("Float")
}

pub fn gleam_type_for_double_precision_test() {
  introspection.gleam_type_for_postgres("double precision", "float8", False)
  |> should.equal("Float")
}

pub fn gleam_type_for_numeric_test() {
  introspection.gleam_type_for_postgres("numeric", "numeric", False)
  |> should.equal("Float")
}

pub fn gleam_type_for_boolean_test() {
  introspection.gleam_type_for_postgres("boolean", "bool", False)
  |> should.equal("Bool")
}

pub fn gleam_type_for_varchar_test() {
  introspection.gleam_type_for_postgres("character varying", "varchar", False)
  |> should.equal("String")
}

pub fn gleam_type_for_char_test() {
  introspection.gleam_type_for_postgres("character", "char", False)
  |> should.equal("String")
}

pub fn gleam_type_for_text_test() {
  introspection.gleam_type_for_postgres("text", "text", False)
  |> should.equal("String")
}

pub fn gleam_type_for_uuid_test() {
  introspection.gleam_type_for_postgres("uuid", "uuid", False)
  |> should.equal("String")
}

pub fn gleam_type_for_json_test() {
  introspection.gleam_type_for_postgres("json", "json", False)
  |> should.equal("String")
}

pub fn gleam_type_for_jsonb_test() {
  introspection.gleam_type_for_postgres("jsonb", "jsonb", False)
  |> should.equal("String")
}

pub fn gleam_type_for_bytea_test() {
  introspection.gleam_type_for_postgres("bytea", "bytea", False)
  |> should.equal("BitArray")
}

pub fn gleam_type_for_timestamp_test() {
  introspection.gleam_type_for_postgres(
    "timestamp without time zone",
    "timestamp",
    False,
  )
  |> should.equal("String")
}

pub fn gleam_type_for_timestamptz_test() {
  introspection.gleam_type_for_postgres(
    "timestamp with time zone",
    "timestamptz",
    False,
  )
  |> should.equal("String")
}

pub fn gleam_type_for_date_test() {
  introspection.gleam_type_for_postgres("date", "date", False)
  |> should.equal("String")
}

pub fn gleam_type_for_time_test() {
  introspection.gleam_type_for_postgres("time", "time", False)
  |> should.equal("String")
}

pub fn gleam_type_for_interval_test() {
  introspection.gleam_type_for_postgres("interval", "interval", False)
  |> should.equal("String")
}

pub fn gleam_type_for_user_defined_test() {
  introspection.gleam_type_for_postgres("user-defined", "user_status", False)
  |> should.equal("UserStatus")
}

pub fn gleam_type_for_array_test() {
  introspection.gleam_type_for_postgres("array", "_int4", False)
  |> should.equal("List(Dynamic)")
}

pub fn gleam_type_for_unknown_test() {
  introspection.gleam_type_for_postgres("some_unknown_type", "unknown", False)
  |> should.equal("Dynamic")
}

pub fn gleam_type_nullable_wraps_in_option_test() {
  introspection.gleam_type_for_postgres("integer", "int4", True)
  |> should.equal("Option(Int)")
}

pub fn gleam_type_nullable_string_test() {
  introspection.gleam_type_for_postgres("text", "text", True)
  |> should.equal("Option(String)")
}

pub fn gleam_type_nullable_user_defined_test() {
  introspection.gleam_type_for_postgres("user-defined", "order_status", True)
  |> should.equal("Option(OrderStatus)")
}

// ============================================================================
// PASCAL CASE TESTS
// ============================================================================

pub fn pascal_case_simple_test() {
  introspection.pascal_case("user")
  |> should.equal("User")
}

pub fn pascal_case_snake_case_test() {
  introspection.pascal_case("user_status")
  |> should.equal("UserStatus")
}

pub fn pascal_case_multiple_underscores_test() {
  introspection.pascal_case("order_line_item")
  |> should.equal("OrderLineItem")
}

pub fn pascal_case_single_char_test() {
  introspection.pascal_case("a")
  |> should.equal("A")
}

pub fn pascal_case_empty_test() {
  introspection.pascal_case("")
  |> should.equal("")
}

pub fn pascal_case_already_capitalized_test() {
  introspection.pascal_case("User")
  |> should.equal("User")
}
