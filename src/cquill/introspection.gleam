// Schema Introspection Module
//
// This module provides PostgreSQL schema introspection capabilities,
// extracting table structure, columns, constraints, and relationships
// from a database schema.
//
// Usage:
// 1. Use the query functions to get SQL for introspection
// 2. Execute queries against your Postgres database
// 3. Use the parse functions to convert results to typed structures
//
// The introspection data can be used to:
// - Generate Gleam schema types
// - Validate existing schemas against the database
// - Create migration diffs
// - Build documentation

import gleam/list
import gleam/option.{type Option}
import gleam/string

// ============================================================================
// INTROSPECTED SCHEMA TYPES
// ============================================================================

/// Complete introspected database schema
pub type IntrospectedSchema {
  IntrospectedSchema(
    /// All tables in the schema
    tables: List(IntrospectedTable),
    /// All enum types in the schema
    enums: List(IntrospectedEnum),
  )
}

/// A database table with its complete structure
pub type IntrospectedTable {
  IntrospectedTable(
    /// Table name
    name: String,
    /// Columns in ordinal position order
    columns: List(IntrospectedColumn),
    /// Primary key column names (supports composite keys)
    primary_key: List(String),
    /// Foreign key relationships
    foreign_keys: List(IntrospectedForeignKey),
    /// Unique constraints
    unique_constraints: List(IntrospectedUnique),
    /// Check constraints
    check_constraints: List(IntrospectedCheck),
  )
}

/// A database column with metadata
pub type IntrospectedColumn {
  IntrospectedColumn(
    /// Column name
    name: String,
    /// Ordinal position (1-based)
    position: Int,
    /// SQL data type (e.g., "integer", "character varying")
    data_type: String,
    /// Underlying type name (useful for custom types/enums)
    udt_name: String,
    /// Whether the column allows NULL values
    is_nullable: Bool,
    /// Default value expression (if any)
    default: Option(String),
    /// Maximum character length (for string types)
    max_length: Option(Int),
    /// Numeric precision (for numeric types)
    numeric_precision: Option(Int),
    /// Numeric scale (for numeric types)
    numeric_scale: Option(Int),
  )
}

/// A foreign key relationship
pub type IntrospectedForeignKey {
  IntrospectedForeignKey(
    /// Column name in this table
    column_name: String,
    /// Referenced table name
    foreign_table: String,
    /// Referenced column name
    foreign_column: String,
    /// ON UPDATE action
    on_update: ForeignKeyAction,
    /// ON DELETE action
    on_delete: ForeignKeyAction,
  )
}

/// Foreign key referential action
pub type ForeignKeyAction {
  NoAction
  Restrict
  Cascade
  SetNull
  SetDefault
}

/// A unique constraint
pub type IntrospectedUnique {
  IntrospectedUnique(
    /// Constraint name
    constraint_name: String,
    /// Columns covered by this constraint (supports composite)
    columns: List(String),
  )
}

/// A check constraint
pub type IntrospectedCheck {
  IntrospectedCheck(
    /// Constraint name
    constraint_name: String,
    /// Check expression (SQL)
    check_clause: String,
  )
}

/// A database enum type
pub type IntrospectedEnum {
  IntrospectedEnum(
    /// Enum type name
    name: String,
    /// Enum values in order
    values: List(String),
  )
}

// ============================================================================
// RAW ROW TYPES (for parsing query results)
// ============================================================================

/// Raw column data from introspection query
pub type RawColumnRow {
  RawColumnRow(
    table_name: String,
    column_name: String,
    ordinal_position: Int,
    data_type: String,
    udt_name: String,
    is_nullable: String,
    column_default: Option(String),
    character_maximum_length: Option(Int),
    numeric_precision: Option(Int),
    numeric_scale: Option(Int),
  )
}

/// Raw primary key data from introspection query
pub type RawPrimaryKeyRow {
  RawPrimaryKeyRow(
    table_name: String,
    column_name: String,
    ordinal_position: Int,
  )
}

/// Raw foreign key data from introspection query
pub type RawForeignKeyRow {
  RawForeignKeyRow(
    table_name: String,
    column_name: String,
    foreign_table_name: String,
    foreign_column_name: String,
    update_rule: String,
    delete_rule: String,
  )
}

/// Raw unique constraint data from introspection query
pub type RawUniqueRow {
  RawUniqueRow(table_name: String, constraint_name: String, column_name: String)
}

/// Raw check constraint data from introspection query
pub type RawCheckRow {
  RawCheckRow(table_name: String, constraint_name: String, check_clause: String)
}

/// Raw enum data from introspection query
pub type RawEnumRow {
  RawEnumRow(enum_name: String, enum_value: String, enum_sort_order: Float)
}

// ============================================================================
// SQL QUERY FUNCTIONS
// ============================================================================

/// Get SQL query for table and column information
/// Parameter $1 is the schema name (e.g., "public")
pub fn columns_query() -> String {
  "SELECT
  t.table_name,
  c.column_name,
  c.ordinal_position,
  c.data_type,
  c.udt_name,
  c.is_nullable,
  c.column_default,
  c.character_maximum_length,
  c.numeric_precision,
  c.numeric_scale
FROM information_schema.tables t
JOIN information_schema.columns c
  ON t.table_name = c.table_name
  AND t.table_schema = c.table_schema
WHERE t.table_schema = $1
  AND t.table_type = 'BASE TABLE'
ORDER BY t.table_name, c.ordinal_position"
}

/// Get SQL query for primary keys
/// Parameter $1 is the schema name
pub fn primary_keys_query() -> String {
  "SELECT
  tc.table_name,
  kcu.column_name,
  kcu.ordinal_position
FROM information_schema.table_constraints tc
JOIN information_schema.key_column_usage kcu
  ON tc.constraint_name = kcu.constraint_name
  AND tc.table_schema = kcu.table_schema
WHERE tc.constraint_type = 'PRIMARY KEY'
  AND tc.table_schema = $1
ORDER BY tc.table_name, kcu.ordinal_position"
}

/// Get SQL query for foreign keys
/// Parameter $1 is the schema name
pub fn foreign_keys_query() -> String {
  "SELECT
  tc.table_name,
  kcu.column_name,
  ccu.table_name AS foreign_table_name,
  ccu.column_name AS foreign_column_name,
  rc.update_rule,
  rc.delete_rule
FROM information_schema.table_constraints tc
JOIN information_schema.key_column_usage kcu
  ON tc.constraint_name = kcu.constraint_name
  AND tc.table_schema = kcu.table_schema
JOIN information_schema.constraint_column_usage ccu
  ON ccu.constraint_name = tc.constraint_name
  AND ccu.table_schema = tc.table_schema
JOIN information_schema.referential_constraints rc
  ON rc.constraint_name = tc.constraint_name
  AND rc.constraint_schema = tc.table_schema
WHERE tc.constraint_type = 'FOREIGN KEY'
  AND tc.table_schema = $1"
}

/// Get SQL query for unique constraints
/// Parameter $1 is the schema name
pub fn unique_constraints_query() -> String {
  "SELECT
  tc.table_name,
  tc.constraint_name,
  kcu.column_name
FROM information_schema.table_constraints tc
JOIN information_schema.key_column_usage kcu
  ON tc.constraint_name = kcu.constraint_name
  AND tc.table_schema = kcu.table_schema
WHERE tc.constraint_type = 'UNIQUE'
  AND tc.table_schema = $1
ORDER BY tc.table_name, tc.constraint_name, kcu.ordinal_position"
}

/// Get SQL query for check constraints
/// Parameter $1 is the schema name
pub fn check_constraints_query() -> String {
  "SELECT
  tc.table_name,
  tc.constraint_name,
  cc.check_clause
FROM information_schema.table_constraints tc
JOIN information_schema.check_constraints cc
  ON tc.constraint_name = cc.constraint_name
  AND tc.table_schema = cc.constraint_schema
WHERE tc.constraint_type = 'CHECK'
  AND tc.table_schema = $1
  AND tc.constraint_name NOT LIKE '%_not_null'"
}

/// Get SQL query for enum types
/// Parameter $1 is the schema name
pub fn enums_query() -> String {
  "SELECT
  t.typname AS enum_name,
  e.enumlabel AS enum_value,
  e.enumsortorder
FROM pg_type t
JOIN pg_enum e ON t.oid = e.enumtypid
JOIN pg_namespace n ON t.typnamespace = n.oid
WHERE n.nspname = $1
ORDER BY t.typname, e.enumsortorder"
}

// ============================================================================
// PARSING FUNCTIONS
// ============================================================================

/// Parse foreign key action from SQL string
pub fn parse_fk_action(action: String) -> ForeignKeyAction {
  case string.uppercase(action) {
    "NO ACTION" -> NoAction
    "RESTRICT" -> Restrict
    "CASCADE" -> Cascade
    "SET NULL" -> SetNull
    "SET DEFAULT" -> SetDefault
    _ -> NoAction
  }
}

/// Parse is_nullable string to boolean
pub fn parse_nullable(value: String) -> Bool {
  case string.uppercase(value) {
    "YES" -> True
    "Y" -> True
    "TRUE" -> True
    "1" -> True
    _ -> False
  }
}

/// Build IntrospectedColumn from raw row data
pub fn build_column(row: RawColumnRow) -> IntrospectedColumn {
  IntrospectedColumn(
    name: row.column_name,
    position: row.ordinal_position,
    data_type: row.data_type,
    udt_name: row.udt_name,
    is_nullable: parse_nullable(row.is_nullable),
    default: row.column_default,
    max_length: row.character_maximum_length,
    numeric_precision: row.numeric_precision,
    numeric_scale: row.numeric_scale,
  )
}

/// Build IntrospectedForeignKey from raw row data
pub fn build_foreign_key(row: RawForeignKeyRow) -> IntrospectedForeignKey {
  IntrospectedForeignKey(
    column_name: row.column_name,
    foreign_table: row.foreign_table_name,
    foreign_column: row.foreign_column_name,
    on_update: parse_fk_action(row.update_rule),
    on_delete: parse_fk_action(row.delete_rule),
  )
}

/// Build IntrospectedCheck from raw row data
pub fn build_check(row: RawCheckRow) -> IntrospectedCheck {
  IntrospectedCheck(
    constraint_name: row.constraint_name,
    check_clause: row.check_clause,
  )
}

// ============================================================================
// SCHEMA ASSEMBLY FUNCTIONS
// ============================================================================

/// Build a complete IntrospectedSchema from raw query results
pub fn build_schema(
  column_rows: List(RawColumnRow),
  pk_rows: List(RawPrimaryKeyRow),
  fk_rows: List(RawForeignKeyRow),
  unique_rows: List(RawUniqueRow),
  check_rows: List(RawCheckRow),
  enum_rows: List(RawEnumRow),
) -> IntrospectedSchema {
  // Get unique table names from columns
  let table_names = get_unique_table_names(column_rows)

  // Build tables
  let tables =
    list.map(table_names, fn(table_name) {
      build_table(
        table_name,
        column_rows,
        pk_rows,
        fk_rows,
        unique_rows,
        check_rows,
      )
    })

  // Build enums
  let enums = build_enums(enum_rows)

  IntrospectedSchema(tables:, enums:)
}

/// Build a single table from raw data
fn build_table(
  table_name: String,
  column_rows: List(RawColumnRow),
  pk_rows: List(RawPrimaryKeyRow),
  fk_rows: List(RawForeignKeyRow),
  unique_rows: List(RawUniqueRow),
  check_rows: List(RawCheckRow),
) -> IntrospectedTable {
  // Filter and build columns for this table
  let columns =
    column_rows
    |> list.filter(fn(row) { row.table_name == table_name })
    |> list.map(build_column)

  // Get primary key columns for this table (in order)
  let primary_key =
    pk_rows
    |> list.filter(fn(row) { row.table_name == table_name })
    |> list.sort(fn(a, b) {
      int_compare(a.ordinal_position, b.ordinal_position)
    })
    |> list.map(fn(row) { row.column_name })

  // Build foreign keys for this table
  let foreign_keys =
    fk_rows
    |> list.filter(fn(row) { row.table_name == table_name })
    |> list.map(build_foreign_key)

  // Build unique constraints for this table
  let unique_constraints = build_unique_constraints(table_name, unique_rows)

  // Build check constraints for this table
  let check_constraints =
    check_rows
    |> list.filter(fn(row) { row.table_name == table_name })
    |> list.map(build_check)

  IntrospectedTable(
    name: table_name,
    columns:,
    primary_key:,
    foreign_keys:,
    unique_constraints:,
    check_constraints:,
  )
}

/// Build unique constraints, grouping multi-column constraints
fn build_unique_constraints(
  table_name: String,
  unique_rows: List(RawUniqueRow),
) -> List(IntrospectedUnique) {
  // Filter for this table
  let table_rows =
    unique_rows
    |> list.filter(fn(row) { row.table_name == table_name })

  // Get unique constraint names
  let constraint_names = get_unique_constraint_names(table_rows)

  // Build each constraint with its columns
  list.map(constraint_names, fn(constraint_name) {
    let columns =
      table_rows
      |> list.filter(fn(row) { row.constraint_name == constraint_name })
      |> list.map(fn(row) { row.column_name })

    IntrospectedUnique(constraint_name:, columns:)
  })
}

/// Build enum types from raw rows
fn build_enums(enum_rows: List(RawEnumRow)) -> List(IntrospectedEnum) {
  // Get unique enum names
  let enum_names = get_unique_enum_names(enum_rows)

  // Build each enum with its values
  list.map(enum_names, fn(enum_name) {
    let values =
      enum_rows
      |> list.filter(fn(row) { row.enum_name == enum_name })
      |> list.sort(fn(a, b) {
        float_compare(a.enum_sort_order, b.enum_sort_order)
      })
      |> list.map(fn(row) { row.enum_value })

    IntrospectedEnum(name: enum_name, values:)
  })
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Get unique table names from column rows, preserving order
fn get_unique_table_names(rows: List(RawColumnRow)) -> List(String) {
  rows
  |> list.map(fn(row) { row.table_name })
  |> list.unique
}

/// Get unique constraint names from unique rows
fn get_unique_constraint_names(rows: List(RawUniqueRow)) -> List(String) {
  rows
  |> list.map(fn(row) { row.constraint_name })
  |> list.unique
}

/// Get unique enum names from enum rows
fn get_unique_enum_names(rows: List(RawEnumRow)) -> List(String) {
  rows
  |> list.map(fn(row) { row.enum_name })
  |> list.unique
}

/// Compare two integers for sorting
fn int_compare(a: Int, b: Int) -> order.Order {
  case a < b {
    True -> order.Lt
    False ->
      case a > b {
        True -> order.Gt
        False -> order.Eq
      }
  }
}

/// Compare two floats for sorting
fn float_compare(a: Float, b: Float) -> order.Order {
  case a <. b {
    True -> order.Lt
    False ->
      case a >. b {
        True -> order.Gt
        False -> order.Eq
      }
  }
}

// ============================================================================
// SCHEMA UTILITIES
// ============================================================================

/// Create an empty introspected schema
pub fn empty_schema() -> IntrospectedSchema {
  IntrospectedSchema(tables: [], enums: [])
}

/// Create an empty introspected table
pub fn empty_table(name: String) -> IntrospectedTable {
  IntrospectedTable(
    name:,
    columns: [],
    primary_key: [],
    foreign_keys: [],
    unique_constraints: [],
    check_constraints: [],
  )
}

/// Find a table by name in the schema
pub fn find_table(
  schema: IntrospectedSchema,
  name: String,
) -> Option(IntrospectedTable) {
  list.find(schema.tables, fn(table) { table.name == name })
  |> option.from_result
}

/// Find a column by name in a table
pub fn find_column(
  table: IntrospectedTable,
  name: String,
) -> Option(IntrospectedColumn) {
  list.find(table.columns, fn(col) { col.name == name })
  |> option.from_result
}

/// Find an enum by name in the schema
pub fn find_enum(
  schema: IntrospectedSchema,
  name: String,
) -> Option(IntrospectedEnum) {
  list.find(schema.enums, fn(e) { e.name == name })
  |> option.from_result
}

/// Get all table names in the schema
pub fn table_names(schema: IntrospectedSchema) -> List(String) {
  list.map(schema.tables, fn(table) { table.name })
}

/// Get all column names in a table
pub fn column_names(table: IntrospectedTable) -> List(String) {
  list.map(table.columns, fn(col) { col.name })
}

/// Check if a table has a primary key
pub fn has_primary_key(table: IntrospectedTable) -> Bool {
  !list.is_empty(table.primary_key)
}

/// Check if a table has a composite primary key
pub fn has_composite_primary_key(table: IntrospectedTable) -> Bool {
  list.length(table.primary_key) > 1
}

/// Get all tables that reference a given table via foreign key
pub fn tables_referencing(
  schema: IntrospectedSchema,
  table_name: String,
) -> List(IntrospectedTable) {
  list.filter(schema.tables, fn(table) {
    list.any(table.foreign_keys, fn(fk) { fk.foreign_table == table_name })
  })
}

/// Get all foreign keys from a table
pub fn get_foreign_keys(
  table: IntrospectedTable,
) -> List(IntrospectedForeignKey) {
  table.foreign_keys
}

/// Check if a column is part of the primary key
pub fn is_primary_key_column(
  table: IntrospectedTable,
  column_name: String,
) -> Bool {
  list.contains(table.primary_key, column_name)
}

/// Check if a column has a foreign key constraint
pub fn has_foreign_key(table: IntrospectedTable, column_name: String) -> Bool {
  list.any(table.foreign_keys, fn(fk) { fk.column_name == column_name })
}

/// Get the foreign key for a column (if any)
pub fn get_column_foreign_key(
  table: IntrospectedTable,
  column_name: String,
) -> Option(IntrospectedForeignKey) {
  list.find(table.foreign_keys, fn(fk) { fk.column_name == column_name })
  |> option.from_result
}

// ============================================================================
// TYPE MAPPING HELPERS
// ============================================================================

/// Map Postgres data type to a Gleam-friendly type name
pub fn gleam_type_for_postgres(
  data_type: String,
  udt_name: String,
  is_nullable: Bool,
) -> String {
  let base_type = case string.lowercase(data_type) {
    "integer" | "int" | "int4" -> "Int"
    "bigint" | "int8" -> "Int"
    "smallint" | "int2" -> "Int"
    "serial" | "serial4" -> "Int"
    "bigserial" | "serial8" -> "Int"
    "real" | "float4" -> "Float"
    "double precision" | "float8" -> "Float"
    "numeric" | "decimal" -> "Float"
    "boolean" | "bool" -> "Bool"
    "character varying" | "varchar" -> "String"
    "character" | "char" -> "String"
    "text" -> "String"
    "uuid" -> "String"
    "json" | "jsonb" -> "String"
    "bytea" -> "BitArray"
    "date" -> "String"
    "time" | "time without time zone" -> "String"
    "time with time zone" | "timetz" -> "String"
    "timestamp" | "timestamp without time zone" -> "String"
    "timestamp with time zone" | "timestamptz" -> "String"
    "interval" -> "String"
    "user-defined" -> pascal_case(udt_name)
    "array" -> "List(Dynamic)"
    _ -> "Dynamic"
  }

  case is_nullable {
    True -> "Option(" <> base_type <> ")"
    False -> base_type
  }
}

/// Convert snake_case to PascalCase
pub fn pascal_case(input: String) -> String {
  input
  |> string.split("_")
  |> list.map(capitalize_first)
  |> string.join("")
}

/// Capitalize the first letter of a string
fn capitalize_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}

// Import order module for sorting
import gleam/order
