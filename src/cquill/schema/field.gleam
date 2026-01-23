// cquill Schema Field Types
//
// This module defines field types and metadata for database schemas.
// It is part of the Schema layer which is PURE — no I/O, no side effects.
//
// The types here map to common SQL database types while remaining
// database-agnostic. Adapters are responsible for translating these
// to their specific SQL dialects.

import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// FIELD TYPES
// ============================================================================

/// Represents the type of a database field.
/// These map to common SQL types across different databases.
pub type FieldType {
  /// Integer type (32-bit signed)
  /// SQL: INTEGER, INT, INT4
  Integer

  /// Big integer type (64-bit signed)
  /// SQL: BIGINT, INT8, BIGSERIAL
  BigInteger

  /// Floating point number
  /// SQL: FLOAT, DOUBLE PRECISION, REAL
  Float

  /// Decimal/numeric with precision
  /// SQL: DECIMAL, NUMERIC
  Decimal(precision: Int, scale: Int)

  /// Variable-length string
  /// SQL: VARCHAR, TEXT, CHARACTER VARYING
  String

  /// Fixed-length string
  /// SQL: CHAR, CHARACTER
  Char(length: Int)

  /// Boolean value
  /// SQL: BOOLEAN, BOOL
  Boolean

  /// Timestamp with timezone
  /// SQL: TIMESTAMP WITH TIME ZONE, TIMESTAMPTZ
  DateTime

  /// Date only (no time)
  /// SQL: DATE
  Date

  /// Time only (no date)
  /// SQL: TIME, TIME WITHOUT TIME ZONE
  Time

  /// Binary data
  /// SQL: BYTEA, BLOB, BINARY
  Binary

  /// UUID/GUID
  /// SQL: UUID
  Uuid

  /// JSON data
  /// SQL: JSON, JSONB
  Json

  /// Array of another type
  /// SQL: type[], ARRAY
  Array(FieldType)

  /// Nullable wrapper - field can be NULL
  /// This is separate from constraints to allow type composition
  Nullable(FieldType)

  /// Enum type with allowed values
  /// SQL: ENUM (Postgres), CHECK constraint (others)
  Enum(name: String, values: List(String))

  /// Custom/adapter-specific type
  /// For types not covered by the standard set
  Custom(type_name: String)
}

// ============================================================================
// FIELD CONSTRAINTS
// ============================================================================

/// Constraints that can be applied to fields
pub type Constraint {
  /// Field cannot be NULL
  NotNull

  /// Field must have unique values (per-column unique)
  Unique

  /// Field is the primary key (or part of composite key)
  PrimaryKey

  /// Field references another table
  ForeignKey(table: String, column: String, on_delete: ForeignKeyAction)

  /// Custom check constraint
  Check(name: String, expression: String)

  /// Minimum value constraint (for numeric types)
  MinValue(min: Int)

  /// Maximum value constraint (for numeric types)
  MaxValue(max: Int)

  /// Minimum length constraint (for string types)
  MinLength(min: Int)

  /// Maximum length constraint (for string types)
  MaxLength(max: Int)

  /// Regex pattern constraint (for string types)
  Pattern(regex: String)
}

/// Actions for foreign key ON DELETE/ON UPDATE
pub type ForeignKeyAction {
  NoAction
  Restrict
  Cascade
  SetNull
  SetDefault
}

// ============================================================================
// DEFAULT VALUES
// ============================================================================

/// Default value for a field
pub type Default {
  /// Static default value
  DefaultValue(Dynamic)

  /// Database function as default (e.g., "now()", "gen_random_uuid()")
  DefaultFunction(function_name: String)

  /// Use database's auto-increment/serial
  DefaultAutoIncrement
}

// ============================================================================
// FIELD DEFINITION
// ============================================================================

/// Complete field definition with all metadata
pub type Field {
  Field(
    /// Field name (column name in database)
    name: String,
    /// Field type
    field_type: FieldType,
    /// Default value if any
    default: Option(Default),
    /// Constraints applied to this field
    constraints: List(Constraint),
    /// Documentation/comment for this field
    comment: Option(String),
  )
}

// ============================================================================
// FIELD CONSTRUCTORS
// ============================================================================

/// Create a new field with just name and type
pub fn new(name: String, field_type: FieldType) -> Field {
  Field(
    name: name,
    field_type: field_type,
    default: None,
    constraints: [],
    comment: None,
  )
}

/// Create an integer field
pub fn integer(name: String) -> Field {
  new(name, Integer)
}

/// Create a big integer field
pub fn big_integer(name: String) -> Field {
  new(name, BigInteger)
}

/// Create a float field
pub fn float(name: String) -> Field {
  new(name, Float)
}

/// Create a decimal field with precision and scale
pub fn decimal(name: String, precision: Int, scale: Int) -> Field {
  new(name, Decimal(precision, scale))
}

/// Create a string/text field
pub fn string(name: String) -> Field {
  new(name, String)
}

/// Create a boolean field
pub fn boolean(name: String) -> Field {
  new(name, Boolean)
}

/// Create a datetime/timestamp field
pub fn datetime(name: String) -> Field {
  new(name, DateTime)
}

/// Create a date-only field
pub fn date(name: String) -> Field {
  new(name, Date)
}

/// Create a time-only field
pub fn time(name: String) -> Field {
  new(name, Time)
}

/// Create a binary/bytea field
pub fn binary(name: String) -> Field {
  new(name, Binary)
}

/// Create a UUID field
pub fn uuid(name: String) -> Field {
  new(name, Uuid)
}

/// Create a JSON field
pub fn json(name: String) -> Field {
  new(name, Json)
}

/// Create an array field
pub fn array(name: String, element_type: FieldType) -> Field {
  new(name, Array(element_type))
}

/// Create an enum field
pub fn enum_(name: String, enum_name: String, values: List(String)) -> Field {
  new(name, Enum(enum_name, values))
}

// ============================================================================
// FIELD MODIFIERS (Pipeline-friendly)
// ============================================================================

/// Mark field as nullable
pub fn nullable(field: Field) -> Field {
  Field(..field, field_type: Nullable(field.field_type))
}

/// Add a default value
pub fn with_default(field: Field, default: Default) -> Field {
  Field(..field, default: Some(default))
}

/// Add a static default value
pub fn default_value(field: Field, value: Dynamic) -> Field {
  Field(..field, default: Some(DefaultValue(value)))
}

/// Add a database function as default
pub fn default_function(field: Field, function_name: String) -> Field {
  Field(..field, default: Some(DefaultFunction(function_name)))
}

/// Mark field as auto-increment
pub fn auto_increment(field: Field) -> Field {
  Field(..field, default: Some(DefaultAutoIncrement))
}

/// Add NOT NULL constraint
pub fn not_null(field: Field) -> Field {
  Field(..field, constraints: [NotNull, ..field.constraints])
}

/// Add UNIQUE constraint
pub fn unique(field: Field) -> Field {
  Field(..field, constraints: [Unique, ..field.constraints])
}

/// Mark as primary key
pub fn primary_key(field: Field) -> Field {
  Field(..field, constraints: [PrimaryKey, ..field.constraints])
}

/// Add foreign key constraint
pub fn references(field: Field, table: String, column: String) -> Field {
  let fk = ForeignKey(table: table, column: column, on_delete: NoAction)
  Field(..field, constraints: [fk, ..field.constraints])
}

/// Add foreign key with ON DELETE action
pub fn references_with_action(
  field: Field,
  table: String,
  column: String,
  on_delete: ForeignKeyAction,
) -> Field {
  let fk = ForeignKey(table: table, column: column, on_delete: on_delete)
  Field(..field, constraints: [fk, ..field.constraints])
}

/// Add check constraint
pub fn check(field: Field, name: String, expression: String) -> Field {
  Field(..field, constraints: [Check(name, expression), ..field.constraints])
}

/// Add minimum value constraint
pub fn min_value(field: Field, min: Int) -> Field {
  Field(..field, constraints: [MinValue(min), ..field.constraints])
}

/// Add maximum value constraint
pub fn max_value(field: Field, max: Int) -> Field {
  Field(..field, constraints: [MaxValue(max), ..field.constraints])
}

/// Add minimum length constraint
pub fn min_length(field: Field, min: Int) -> Field {
  Field(..field, constraints: [MinLength(min), ..field.constraints])
}

/// Add maximum length constraint
pub fn max_length(field: Field, max: Int) -> Field {
  Field(..field, constraints: [MaxLength(max), ..field.constraints])
}

/// Add regex pattern constraint
pub fn pattern(field: Field, regex: String) -> Field {
  Field(..field, constraints: [Pattern(regex), ..field.constraints])
}

/// Add a comment/documentation
pub fn comment(field: Field, text: String) -> Field {
  Field(..field, comment: Some(text))
}

// ============================================================================
// FIELD ACCESSORS
// ============================================================================

/// Get the field name
pub fn get_name(field: Field) -> String {
  field.name
}

/// Get the field type
pub fn get_type(field: Field) -> FieldType {
  field.field_type
}

/// Get the field's base type (unwrapping Nullable if present)
pub fn get_base_type(field: Field) -> FieldType {
  case field.field_type {
    Nullable(inner) -> inner
    other -> other
  }
}

/// Check if field is nullable
pub fn is_nullable(field: Field) -> Bool {
  case field.field_type {
    Nullable(_) -> True
    _ -> !has_constraint(field, is_not_null_constraint)
  }
}

/// Check if field is NOT nullable (positive assertion helper)
/// Use this instead of `!is_nullable(f)` for better readability
pub fn is_not_nullable(field: Field) -> Bool {
  !is_nullable(field)
}

/// Check if field is a primary key
pub fn is_primary_key(field: Field) -> Bool {
  has_constraint(field, is_primary_key_constraint)
}

/// Check if field has auto-increment
pub fn is_auto_increment(field: Field) -> Bool {
  case field.default {
    Some(DefaultAutoIncrement) -> True
    _ -> False
  }
}

/// Check if field is required (non-nullable and has no default value)
/// This is a positive assertion helper - use instead of `!is_nullable(f) && is_none(f.default)`
pub fn is_required(field: Field) -> Bool {
  is_not_nullable(field) && option.is_none(field.default)
}

/// Check if field has a specific constraint
pub fn has_constraint(field: Field, predicate: fn(Constraint) -> Bool) -> Bool {
  list.any(field.constraints, predicate)
}

/// Get all constraints of a specific type
pub fn get_constraints(
  field: Field,
  predicate: fn(Constraint) -> Bool,
) -> List(Constraint) {
  list.filter(field.constraints, predicate)
}

// ============================================================================
// CONSTRAINT PREDICATES
// ============================================================================

/// Check if constraint is NotNull
pub fn is_not_null_constraint(constraint: Constraint) -> Bool {
  case constraint {
    NotNull -> True
    _ -> False
  }
}

/// Check if constraint is PrimaryKey
pub fn is_primary_key_constraint(constraint: Constraint) -> Bool {
  case constraint {
    PrimaryKey -> True
    _ -> False
  }
}

/// Check if constraint is Unique
pub fn is_unique_constraint(constraint: Constraint) -> Bool {
  case constraint {
    Unique -> True
    _ -> False
  }
}

/// Check if constraint is ForeignKey
pub fn is_foreign_key_constraint(constraint: Constraint) -> Bool {
  case constraint {
    ForeignKey(..) -> True
    _ -> False
  }
}

// ============================================================================
// TYPE UTILITIES
// ============================================================================

/// Get a human-readable name for a field type
pub fn type_name(field_type: FieldType) -> String {
  case field_type {
    Integer -> "integer"
    BigInteger -> "bigint"
    Float -> "float"
    Decimal(p, s) ->
      "decimal(" <> int.to_string(p) <> "," <> int.to_string(s) <> ")"
    String -> "string"
    Char(l) -> "char(" <> int.to_string(l) <> ")"
    Boolean -> "boolean"
    DateTime -> "datetime"
    Date -> "date"
    Time -> "time"
    Binary -> "binary"
    Uuid -> "uuid"
    Json -> "json"
    Array(inner) -> "array(" <> type_name(inner) <> ")"
    Nullable(inner) -> type_name(inner) <> "?"
    Enum(name, _) -> "enum:" <> name
    Custom(name) -> "custom:" <> name
  }
}

/// Check if a field type is numeric
pub fn is_numeric_type(field_type: FieldType) -> Bool {
  case field_type {
    Integer | BigInteger | Float | Decimal(..) -> True
    Nullable(inner) -> is_numeric_type(inner)
    _ -> False
  }
}

/// Check if a field type is text-like
pub fn is_text_type(field_type: FieldType) -> Bool {
  case field_type {
    String | Char(_) -> True
    Nullable(inner) -> is_text_type(inner)
    _ -> False
  }
}

/// Check if a field type is temporal (date/time related)
pub fn is_temporal_type(field_type: FieldType) -> Bool {
  case field_type {
    DateTime | Date | Time -> True
    Nullable(inner) -> is_temporal_type(inner)
    _ -> False
  }
}
