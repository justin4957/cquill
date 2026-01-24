// cquill Schema Layer
//
// This module defines the Schema type — pure metadata describing database tables.
// It is the foundation of cquill's type-safe query building.
//
// ARCHITECTURAL PRINCIPLE: This layer is PURE — no I/O, no side effects.
// It defines structure and metadata only.
//
// DESIGN PATTERN: Multiple schemas per domain
// Following Ecto's pattern, you can define different schemas for:
// - Full schema (all fields for reads)
// - Insert schema (no auto-generated fields)
// - Public schema (excluding sensitive fields)
//
// Example:
// ```gleam
// // Full schema for reads
// let user_schema = schema.new("users")
//   |> schema.field(field.integer("id") |> field.primary_key)
//   |> schema.field(field.string("email") |> field.not_null)
//   |> schema.field(field.string("password_hash") |> field.not_null)
//   |> schema.field(field.datetime("inserted_at"))
//
// // Insert schema (no id, no timestamps)
// let new_user_schema = schema.new("users")
//   |> schema.field(field.string("email") |> field.not_null)
//   |> schema.field(field.string("password_hash") |> field.not_null)
// ```

import cquill/schema/field.{type Field, type FieldType}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// SCHEMA TYPE
// ============================================================================

/// A schema describes the structure of a database table.
/// It contains metadata about fields, primary keys, and table-level constraints.
pub type Schema {
  Schema(
    /// The source table name in the database
    source: String,
    /// Schema/namespace (e.g., "public" in Postgres)
    table_schema: Option(String),
    /// Ordered list of fields
    fields: List(Field),
    /// Primary key column names (supports composite keys)
    primary_key: List(String),
    /// Table-level constraints (multi-column unique, etc.)
    table_constraints: List(TableConstraint),
    /// Table comment/documentation
    comment: Option(String),
  )
}

/// Table-level constraints (apply to multiple columns)
pub type TableConstraint {
  /// Multi-column unique constraint
  UniqueConstraint(name: String, columns: List(String))

  /// Multi-column index (not enforced, but for documentation)
  Index(name: String, columns: List(String), unique: Bool)

  /// Table-level check constraint
  TableCheck(name: String, expression: String)

  /// Composite foreign key
  CompositeForeignKey(
    name: String,
    columns: List(String),
    references_table: String,
    references_columns: List(String),
  )
}

// ============================================================================
// SCHEMA CONSTRUCTORS
// ============================================================================

/// Create a new empty schema for a table
pub fn new(source: String) -> Schema {
  Schema(
    source: source,
    table_schema: None,
    fields: [],
    primary_key: [],
    table_constraints: [],
    comment: None,
  )
}

/// Create a new schema with a specific database schema/namespace
pub fn new_with_schema(source: String, table_schema: String) -> Schema {
  Schema(
    source: source,
    table_schema: Some(table_schema),
    fields: [],
    primary_key: [],
    table_constraints: [],
    comment: None,
  )
}

// ============================================================================
// SCHEMA BUILDERS (Pipeline-friendly)
// ============================================================================

/// Add a field to the schema
/// Fields are added in order and the order is preserved
///
/// If the field has a PrimaryKey constraint and no schema-level primary key
/// is set yet, the field will automatically be registered as the primary key.
/// This makes `field.primary_key()` work intuitively without requiring a
/// separate call to `schema.single_primary_key()`.
pub fn add_field(schema: Schema, new_field: Field) -> Schema {
  // Add to end to preserve insertion order
  let updated_schema =
    Schema(..schema, fields: list.append(schema.fields, [new_field]))

  // Auto-register primary key if field has PrimaryKey constraint
  // and no schema-level primary key is set yet
  case list.is_empty(schema.primary_key) && field.is_primary_key(new_field) {
    True -> Schema(..updated_schema, primary_key: [field.get_name(new_field)])
    False -> updated_schema
  }
}

/// Alias for add_field - more concise in pipelines
///
/// Note: This also auto-registers primary keys. See `add_field` for details.
pub fn field(schema: Schema, new_field: Field) -> Schema {
  add_field(schema, new_field)
}

/// Set the primary key columns
/// Supports composite primary keys
pub fn primary_key(schema: Schema, columns: List(String)) -> Schema {
  Schema(..schema, primary_key: columns)
}

/// Set a single-column primary key
pub fn single_primary_key(schema: Schema, column: String) -> Schema {
  Schema(..schema, primary_key: [column])
}

/// Set the table schema/namespace
pub fn in_schema(schema: Schema, table_schema: String) -> Schema {
  Schema(..schema, table_schema: Some(table_schema))
}

/// Add a table-level constraint
pub fn add_constraint(schema: Schema, constraint: TableConstraint) -> Schema {
  Schema(
    ..schema,
    table_constraints: list.append(schema.table_constraints, [constraint]),
  )
}

/// Add a multi-column unique constraint
pub fn unique_constraint(
  schema: Schema,
  name: String,
  columns: List(String),
) -> Schema {
  add_constraint(schema, UniqueConstraint(name, columns))
}

/// Add an index
pub fn index(
  schema: Schema,
  name: String,
  columns: List(String),
  unique: Bool,
) -> Schema {
  add_constraint(schema, Index(name, columns, unique))
}

/// Add a table-level check constraint
pub fn table_check(schema: Schema, name: String, expression: String) -> Schema {
  add_constraint(schema, TableCheck(name, expression))
}

/// Add a comment to the schema
pub fn with_comment(schema: Schema, text: String) -> Schema {
  Schema(..schema, comment: Some(text))
}

// ============================================================================
// SCHEMA ACCESSORS
// ============================================================================

/// Get the source table name
pub fn get_source(schema: Schema) -> String {
  schema.source
}

/// Get the fully qualified table name (schema.table or just table)
pub fn get_qualified_name(schema: Schema) -> String {
  case schema.table_schema {
    Some(s) -> s <> "." <> schema.source
    None -> schema.source
  }
}

/// Get all fields in order
pub fn get_fields(schema: Schema) -> List(Field) {
  schema.fields
}

/// Get field count
pub fn field_count(schema: Schema) -> Int {
  list.length(schema.fields)
}

/// Get the primary key columns
pub fn get_primary_key(schema: Schema) -> List(String) {
  schema.primary_key
}

/// Check if schema has a primary key defined
pub fn has_primary_key(schema: Schema) -> Bool {
  !list.is_empty(schema.primary_key)
}

/// Get a field by name
pub fn get_field(schema: Schema, name: String) -> Option(Field) {
  list.find(schema.fields, fn(f) { field.get_name(f) == name })
  |> option.from_result
}

/// Check if schema has a field with the given name
pub fn has_field(schema: Schema, name: String) -> Bool {
  option.is_some(get_field(schema, name))
}

/// Get all field names in order
pub fn field_names(schema: Schema) -> List(String) {
  list.map(schema.fields, field.get_name)
}

/// Get fields that are part of the primary key
pub fn primary_key_fields(schema: Schema) -> List(Field) {
  list.filter(schema.fields, fn(f) {
    list.contains(schema.primary_key, field.get_name(f))
  })
}

/// Get fields that are NOT part of the primary key
pub fn non_primary_key_fields(schema: Schema) -> List(Field) {
  list.filter(schema.fields, fn(f) {
    !list.contains(schema.primary_key, field.get_name(f))
  })
}

/// Get all nullable fields
pub fn nullable_fields(schema: Schema) -> List(Field) {
  list.filter(schema.fields, field.is_nullable)
}

/// Get all required (non-nullable, no default) fields
pub fn required_fields(schema: Schema) -> List(Field) {
  list.filter(schema.fields, field.is_required)
}

/// Get all auto-increment fields
pub fn auto_increment_fields(schema: Schema) -> List(Field) {
  list.filter(schema.fields, field.is_auto_increment)
}

/// Get fields with foreign key constraints
pub fn foreign_key_fields(schema: Schema) -> List(Field) {
  list.filter(schema.fields, fn(f) {
    field.has_constraint(f, field.is_foreign_key_constraint)
  })
}

// ============================================================================
// SCHEMA TRANSFORMATIONS
// ============================================================================

/// Create a new schema with only the specified fields
/// Useful for creating insert/update schemas without auto-generated fields
pub fn select_fields(schema: Schema, names: List(String)) -> Schema {
  let selected_fields =
    list.filter(schema.fields, fn(f) { list.contains(names, field.get_name(f)) })
  Schema(..schema, fields: selected_fields)
}

/// Create a new schema excluding the specified fields
/// Useful for creating public schemas without sensitive fields
pub fn exclude_fields(schema: Schema, names: List(String)) -> Schema {
  let remaining_fields =
    list.filter(schema.fields, fn(f) {
      !list.contains(names, field.get_name(f))
    })
  Schema(..schema, fields: remaining_fields)
}

/// Create a new schema with only non-auto-generated fields
/// Useful for insert schemas
pub fn insertable_fields(schema: Schema) -> Schema {
  let fields = list.filter(schema.fields, fn(f) { !field.is_auto_increment(f) })
  Schema(..schema, fields: fields)
}

/// Rename a field in the schema
pub fn rename_field(
  schema: Schema,
  old_name: String,
  new_name: String,
) -> Schema {
  let updated_fields =
    list.map(schema.fields, fn(f) {
      case field.get_name(f) == old_name {
        True -> field.Field(..f, name: new_name)
        False -> f
      }
    })
  Schema(..schema, fields: updated_fields)
}

// ============================================================================
// SCHEMA VALIDATION
// ============================================================================

/// Validation errors for schemas
pub type SchemaError {
  /// Primary key references non-existent field
  InvalidPrimaryKey(column: String)

  /// Duplicate field name
  DuplicateField(name: String)

  /// Empty schema (no fields)
  EmptySchema

  /// Invalid table name
  InvalidTableName(name: String)

  /// Table constraint references invalid column
  InvalidConstraintColumn(constraint_name: String, column: String)
}

/// Validate a schema and return any errors
pub fn validate(schema: Schema) -> List(SchemaError) {
  let errors = []

  // Check for empty table name
  let errors = case string.is_empty(schema.source) {
    True -> [InvalidTableName(""), ..errors]
    False -> errors
  }

  // Check for empty schema
  let errors = case list.is_empty(schema.fields) {
    True -> [EmptySchema, ..errors]
    False -> errors
  }

  // Check for duplicate field names
  let field_name_list = field_names(schema)
  let errors = check_duplicates(field_name_list, errors)

  // Check primary key references valid fields
  let errors =
    list.fold(schema.primary_key, errors, fn(errs, pk_col) {
      case has_field(schema, pk_col) {
        True -> errs
        False -> [InvalidPrimaryKey(pk_col), ..errs]
      }
    })

  // Check table constraints reference valid columns
  let errors =
    list.fold(schema.table_constraints, errors, fn(errs, constraint) {
      case constraint {
        UniqueConstraint(name, columns) ->
          validate_constraint_columns(name, columns, schema, errs)
        Index(name, columns, _) ->
          validate_constraint_columns(name, columns, schema, errs)
        TableCheck(_, _) -> errs
        CompositeForeignKey(name, columns, _, _) ->
          validate_constraint_columns(name, columns, schema, errs)
      }
    })

  list.reverse(errors)
}

/// Check if schema is valid
pub fn is_valid(schema: Schema) -> Bool {
  list.is_empty(validate(schema))
}

// Helper: check for duplicate field names
fn check_duplicates(
  names: List(String),
  errors: List(SchemaError),
) -> List(SchemaError) {
  case names {
    [] -> errors
    [name, ..rest] ->
      case list.contains(rest, name) {
        True -> check_duplicates(rest, [DuplicateField(name), ..errors])
        False -> check_duplicates(rest, errors)
      }
  }
}

// Helper: validate constraint columns exist
fn validate_constraint_columns(
  constraint_name: String,
  columns: List(String),
  schema: Schema,
  errors: List(SchemaError),
) -> List(SchemaError) {
  list.fold(columns, errors, fn(errs, col) {
    case has_field(schema, col) {
      True -> errs
      False -> [InvalidConstraintColumn(constraint_name, col), ..errs]
    }
  })
}

// ============================================================================
// SCHEMA COMPARISON
// ============================================================================

/// Compare two schemas and find differences
/// Useful for schema migration planning
pub type SchemaDiff {
  FieldAdded(Field)
  FieldRemoved(String)
  FieldTypeChanged(name: String, old_type: FieldType, new_type: FieldType)
  PrimaryKeyChanged(old_pk: List(String), new_pk: List(String))
}

/// Find differences between two schemas
pub fn diff(old_schema: Schema, new_schema: Schema) -> List(SchemaDiff) {
  let diffs = []

  // Find added fields
  let old_names = field_names(old_schema)
  let added_diffs =
    list.filter_map(new_schema.fields, fn(f) {
      case list.contains(old_names, field.get_name(f)) {
        True -> Error(Nil)
        False -> Ok(FieldAdded(f))
      }
    })

  // Find removed fields
  let new_names = field_names(new_schema)
  let removed_diffs =
    list.filter_map(old_schema.fields, fn(f) {
      let name = field.get_name(f)
      case list.contains(new_names, name) {
        True -> Error(Nil)
        False -> Ok(FieldRemoved(name))
      }
    })

  // Find type changes
  let type_change_diffs =
    list.filter_map(old_schema.fields, fn(old_field) {
      let name = field.get_name(old_field)
      case get_field(new_schema, name) {
        None -> Error(Nil)
        Some(new_field) -> {
          let old_type = field.get_type(old_field)
          let new_type = field.get_type(new_field)
          case old_type == new_type {
            True -> Error(Nil)
            False -> Ok(FieldTypeChanged(name, old_type, new_type))
          }
        }
      }
    })

  // Check primary key changes
  let pk_diffs = case old_schema.primary_key == new_schema.primary_key {
    True -> []
    False -> [PrimaryKeyChanged(old_schema.primary_key, new_schema.primary_key)]
  }

  list.flatten([diffs, added_diffs, removed_diffs, type_change_diffs, pk_diffs])
}
