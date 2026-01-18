// cquill Phantom-Typed Table and Column Module
//
// This module provides compile-time type safety for database queries through
// phantom types. Phantom types are type parameters that don't appear at runtime
// but enable the type system to catch errors at compile time.
//
// Key types:
// - Table(table_type): A table reference carrying its type as a phantom parameter
// - Column(table_type, value_type): A column reference with table and value types
//
// How it works:
// Generated code creates specific table types (e.g., UserTable, PostTable) and
// uses them as phantom parameters. The compiler then ensures that columns from
// one table can't be used in queries for another table.
//
// Example generated code:
// ```gleam
// pub opaque type UserTable
// pub const users: Table(UserTable) = table("users")
// pub const email: Column(UserTable, String) = column("email")
// ```
//
// Example compile-time safety:
// ```gleam
// // This compiles - email belongs to UserTable
// from(users) |> where(eq(email, "test@example.com"))
//
// // This fails to compile - email doesn't belong to PostTable
// from(posts) |> where(eq(email, "test@example.com"))
// ```

// ============================================================================
// TABLE TYPE
// ============================================================================

/// A table reference carrying its type as a phantom parameter.
/// The phantom type `table_type` enables compile-time verification that
/// columns used in queries belong to the correct table.
///
/// ## Example
/// ```gleam
/// pub opaque type UserTable
/// pub const users: Table(UserTable) = table("users")
/// ```
pub opaque type Table(table_type) {
  Table(name: String, schema_name: String)
}

/// Create a table reference.
/// This is typically used in generated code.
///
/// ## Example
/// ```gleam
/// pub const users: Table(UserTable) = table("users")
/// ```
pub fn table(name: String) -> Table(t) {
  Table(name: name, schema_name: "public")
}

/// Create a table reference with a specific schema.
///
/// ## Example
/// ```gleam
/// pub const users: Table(UserTable) = table_in_schema("my_schema", "users")
/// ```
pub fn table_in_schema(schema_name: String, name: String) -> Table(t) {
  Table(name: name, schema_name: schema_name)
}

/// Get the table name from a Table reference.
/// Used at runtime when building actual queries.
pub fn table_name(table: Table(t)) -> String {
  let Table(name: name, ..) = table
  name
}

/// Get the schema name from a Table reference.
pub fn table_schema_name(table: Table(t)) -> String {
  let Table(schema_name: schema_name, ..) = table
  schema_name
}

/// Get the fully qualified table name (schema.table).
pub fn table_qualified_name(table: Table(t)) -> String {
  let Table(name: name, schema_name: schema_name) = table
  case schema_name {
    "public" -> name
    schema -> schema <> "." <> name
  }
}

// ============================================================================
// TABLE ALIASING
// ============================================================================

/// An aliased table reference carrying the original table type.
/// Used for self-joins and queries that need multiple references to the same table.
///
/// ## Example
/// ```gleam
/// let managers = alias_table(users, "managers")
///
/// typed_from(users)
/// |> typed_join_aliased(managers, on: ...)
/// ```
pub opaque type AliasedTable(table_type) {
  AliasedTable(table: Table(table_type), alias_name: String)
}

/// Create an aliased table reference.
/// This creates a new reference to a table with a different alias,
/// useful for self-joins or when you need to reference the same table multiple times.
///
/// ## Example
/// ```gleam
/// // For a self-join to find employees and their managers
/// let managers = alias_table(employees, "managers")
/// ```
pub fn alias_table(table: Table(t), alias_name: String) -> AliasedTable(t) {
  AliasedTable(table: table, alias_name: alias_name)
}

/// Get the alias name from an aliased table.
pub fn aliased_table_alias(aliased: AliasedTable(t)) -> String {
  let AliasedTable(alias_name: alias, ..) = aliased
  alias
}

/// Get the underlying table from an aliased table.
pub fn aliased_table_table(aliased: AliasedTable(t)) -> Table(t) {
  let AliasedTable(table: table, ..) = aliased
  table
}

/// Get the original table name from an aliased table.
pub fn aliased_table_name(aliased: AliasedTable(t)) -> String {
  let AliasedTable(table: table, ..) = aliased
  table_name(table)
}

/// Get the qualified name with alias (schema.table AS alias).
pub fn aliased_table_qualified_name(aliased: AliasedTable(t)) -> String {
  let AliasedTable(table: table, alias_name: alias) = aliased
  table_qualified_name(table) <> " AS " <> alias
}

/// Create a column reference using an aliased table.
/// The column will be prefixed with the alias in generated SQL.
///
/// ## Example
/// ```gleam
/// let managers = alias_table(users, "m")
/// let manager_id = aliased_column(managers, "id")
/// // Generates: m.id
/// ```
pub fn aliased_column(
  aliased_table: AliasedTable(t),
  column_name: String,
) -> Column(t, v) {
  let AliasedTable(alias_name: alias, ..) = aliased_table
  Column(name: column_name, table_alias: alias)
}

// ============================================================================
// COLUMN TYPE
// ============================================================================

/// A column reference carrying table and value types as phantom parameters.
/// - `table_type`: The phantom type of the table this column belongs to
/// - `value_type`: The Gleam type of the column's values
///
/// ## Example
/// ```gleam
/// pub const id: Column(UserTable, Int) = column("id")
/// pub const email: Column(UserTable, String) = column("email")
/// pub const name: Column(UserTable, Option(String)) = column("name")
/// ```
pub opaque type Column(table_type, value_type) {
  Column(name: String, table_alias: String)
}

/// Create a column reference.
/// This is typically used in generated code.
///
/// ## Example
/// ```gleam
/// pub const email: Column(UserTable, String) = column("email")
/// ```
pub fn column(name: String) -> Column(t, v) {
  Column(name: name, table_alias: "")
}

/// Create a column reference with a table alias.
/// Useful for self-joins or when using table aliases.
///
/// ## Example
/// ```gleam
/// let aliased_email = column_aliased("u", "email")
/// ```
pub fn column_aliased(table_alias: String, name: String) -> Column(t, v) {
  Column(name: name, table_alias: table_alias)
}

/// Get the column name from a Column reference.
/// Used at runtime when building actual queries.
pub fn column_name(column: Column(t, v)) -> String {
  let Column(name: name, ..) = column
  name
}

/// Get the table alias from a Column reference.
pub fn column_table_alias(column: Column(t, v)) -> String {
  let Column(table_alias: alias, ..) = column
  alias
}

/// Get the fully qualified column name (table_alias.column or just column).
pub fn column_qualified_name(column: Column(t, v)) -> String {
  let Column(name: name, table_alias: alias) = column
  case alias {
    "" -> name
    table_alias -> table_alias <> "." <> name
  }
}

/// Add or change the table alias for a column.
/// Returns a new column with the specified alias prefix.
///
/// ## Example
/// ```gleam
/// let u_email = with_alias(email, "u")
/// // In SQL: u.email
/// ```
pub fn with_alias(column: Column(t, v), alias: String) -> Column(t, v) {
  let Column(name: name, ..) = column
  Column(name: name, table_alias: alias)
}

// ============================================================================
// JOIN TYPES
// ============================================================================

/// Phantom type representing a join between two tables.
/// Enables type-safe access to columns from either table in a joined query.
/// Note: This is an empty type used only as a phantom type parameter.
pub type Join2(t1, t2)

/// Phantom type representing a join between three tables.
pub type Join3(t1, t2, t3)

/// Phantom type representing a join between four tables.
pub type Join4(t1, t2, t3, t4)

/// Phantom type representing a join between five tables.
pub type Join5(t1, t2, t3, t4, t5)

// ============================================================================
// TYPE-SAFE COLUMN ACCESS
// ============================================================================

/// Coerce a column to work with a Join2 type.
/// This is used when a column from table t1 needs to be used in a query
/// that has been joined with another table.
///
/// ## Example
/// ```gleam
/// from(users)
/// |> typed_join(posts, on: eq(user_id_col, id_col))
/// |> where(eq(in_join2_left(email), "test@example.com"))
/// ```
pub fn in_join2_left(column: Column(t1, v)) -> Column(Join2(t1, t2), v) {
  let Column(name: name, table_alias: alias) = column
  Column(name: name, table_alias: alias)
}

/// Coerce a column from the right table of a Join2.
pub fn in_join2_right(column: Column(t2, v)) -> Column(Join2(t1, t2), v) {
  let Column(name: name, table_alias: alias) = column
  Column(name: name, table_alias: alias)
}

/// Short alias for `in_join2_left`.
/// Lifts a column from the left table into a Join2 scope.
///
/// ## Example
/// ```gleam
/// typed_from(users)
/// |> typed_join(posts, on: typed_eq_columns(left(user_id), right(post_user_id)))
/// ```
pub fn left(column: Column(t1, v)) -> Column(Join2(t1, t2), v) {
  in_join2_left(column)
}

/// Short alias for `in_join2_right`.
/// Lifts a column from the right table into a Join2 scope.
pub fn right(column: Column(t2, v)) -> Column(Join2(t1, t2), v) {
  in_join2_right(column)
}

/// Coerce a column to the first table of a Join3.
pub fn in_join3_first(column: Column(t1, v)) -> Column(Join3(t1, t2, t3), v) {
  let Column(name: name, table_alias: alias) = column
  Column(name: name, table_alias: alias)
}

/// Coerce a column to the second table of a Join3.
pub fn in_join3_second(column: Column(t2, v)) -> Column(Join3(t1, t2, t3), v) {
  let Column(name: name, table_alias: alias) = column
  Column(name: name, table_alias: alias)
}

/// Coerce a column to the third table of a Join3.
pub fn in_join3_third(column: Column(t3, v)) -> Column(Join3(t1, t2, t3), v) {
  let Column(name: name, table_alias: alias) = column
  Column(name: name, table_alias: alias)
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/// Create a list of column names from a list of columns.
/// Useful for SELECT statements.
pub fn column_names(columns: List(Column(t, a))) -> List(String) {
  do_column_names(columns, [])
}

fn do_column_names(
  columns: List(Column(t, a)),
  acc: List(String),
) -> List(String) {
  case columns {
    [] -> reverse(acc)
    [first, ..rest] -> do_column_names(rest, [column_name(first), ..acc])
  }
}

fn reverse(list: List(a)) -> List(a) {
  do_reverse(list, [])
}

fn do_reverse(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [first, ..rest] -> do_reverse(rest, [first, ..acc])
  }
}
