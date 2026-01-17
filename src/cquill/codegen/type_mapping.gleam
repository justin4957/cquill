// Type Mapping Module
//
// This module provides type mapping logic that converts PostgreSQL column types
// to appropriate Gleam types for code generation.
//
// Usage:
// 1. Get introspected columns from the introspection module
// 2. Use map_column() to get full column metadata including Gleam type
// 3. Use the GleamType for code generation
//
// Features:
// - Maps all common PostgreSQL types to Gleam types
// - Handles nullable columns with Option wrapper
// - Handles arrays with List wrapper
// - Detects custom enum types
// - Tracks auto-generated columns (serial, defaults)

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// GLEAM TYPE REPRESENTATION
// ============================================================================

/// Represents a Gleam type that a PostgreSQL column maps to
pub type GleamType {
  /// Gleam Int (for integer, smallint, bigint, serial, bigserial)
  GleamInt
  /// Gleam Float (for real, double precision)
  GleamFloat
  /// Gleam String (for varchar, char, text)
  GleamString
  /// Gleam Bool (for boolean)
  GleamBool
  /// Gleam BitArray (for bytea)
  GleamBitArray
  /// Time type (for timestamp with/without time zone)
  GleamTime
  /// Date type (for date)
  GleamDate
  /// Time of day type (for time with/without time zone)
  GleamTimeOfDay
  /// Decimal type (for numeric, decimal) - external package
  GleamDecimal
  /// UUID type - typically a newtype wrapper around String
  GleamUuid
  /// JSON type (for json, jsonb) - opaque type
  GleamJson
  /// List type for PostgreSQL arrays
  GleamList(element_type: GleamType)
  /// Option type for nullable columns
  GleamOption(inner_type: GleamType)
  /// Custom type (for user-defined enums or types)
  GleamCustom(module: String, type_name: String)
}

// ============================================================================
// COLUMN METADATA
// ============================================================================

/// Extended column metadata including Gleam type and generation info
pub type ColumnMeta {
  ColumnMeta(
    /// The mapped Gleam type
    gleam_type: GleamType,
    /// Whether this column is auto-generated (serial, has default, etc.)
    /// Auto-generated columns are excluded from insert types
    is_auto_generated: Bool,
    /// The original PostgreSQL data type
    postgres_type: String,
    /// The underlying type name (useful for enums)
    udt_name: String,
    /// Column name
    column_name: String,
    /// Whether the column is nullable
    is_nullable: Bool,
  )
}

// ============================================================================
// ERROR TYPES
// ============================================================================

/// Errors that can occur during type mapping
pub type TypeMappingError {
  /// Unknown PostgreSQL type that cannot be mapped
  UnknownType(postgres_type: String, udt_name: String)
  /// Array type with unsupported element type
  UnsupportedArray(element_type: String)
}

// ============================================================================
// TYPE MAPPING FUNCTIONS
// ============================================================================

/// Map a PostgreSQL column to its corresponding Gleam type
///
/// Parameters:
/// - data_type: The PostgreSQL data type (e.g., "integer", "character varying")
/// - udt_name: The underlying type name (e.g., "int4", "varchar", or enum name)
/// - is_nullable: Whether the column allows NULL values
/// - enum_names: List of known enum type names in the schema
///
/// Returns:
/// - Ok(GleamType) with the mapped type (wrapped in Option if nullable)
/// - Error(TypeMappingError) if the type cannot be mapped
pub fn map_postgres_type(
  data_type: String,
  udt_name: String,
  is_nullable: Bool,
  enum_names: List(String),
) -> Result(GleamType, TypeMappingError) {
  // First, map the base type
  use base_type <- result.try(map_base_type(data_type, udt_name, enum_names))

  // Wrap in Option if nullable
  let final_type = case is_nullable {
    True -> GleamOption(base_type)
    False -> base_type
  }

  Ok(final_type)
}

/// Map a PostgreSQL type to a base Gleam type (without Option wrapper)
fn map_base_type(
  data_type: String,
  udt_name: String,
  enum_names: List(String),
) -> Result(GleamType, TypeMappingError) {
  let lowercase_data_type = string.lowercase(data_type)
  let lowercase_udt_name = string.lowercase(udt_name)

  case lowercase_data_type {
    // Integer types
    "integer" | "int" | "int4" -> Ok(GleamInt)
    "smallint" | "int2" -> Ok(GleamInt)
    "bigint" | "int8" -> Ok(GleamInt)

    // Serial types (auto-incrementing integers)
    "serial" | "serial4" -> Ok(GleamInt)
    "smallserial" | "serial2" -> Ok(GleamInt)
    "bigserial" | "serial8" -> Ok(GleamInt)

    // Floating point types
    "real" | "float4" -> Ok(GleamFloat)
    "double precision" | "float8" -> Ok(GleamFloat)

    // Decimal types (for precision)
    "numeric" | "decimal" -> Ok(GleamDecimal)

    // String types
    "character varying" | "varchar" -> Ok(GleamString)
    "character" | "char" | "bpchar" -> Ok(GleamString)
    "text" -> Ok(GleamString)
    "name" -> Ok(GleamString)

    // Boolean
    "boolean" | "bool" -> Ok(GleamBool)

    // Binary data
    "bytea" -> Ok(GleamBitArray)

    // Timestamp types
    "timestamp without time zone" | "timestamp" -> Ok(GleamTime)
    "timestamp with time zone" | "timestamptz" -> Ok(GleamTime)

    // Date type
    "date" -> Ok(GleamDate)

    // Time types
    "time without time zone" | "time" -> Ok(GleamTimeOfDay)
    "time with time zone" | "timetz" -> Ok(GleamTimeOfDay)

    // Interval (map to String for now, could be custom type)
    "interval" -> Ok(GleamString)

    // UUID
    "uuid" -> Ok(GleamUuid)

    // JSON types
    "json" -> Ok(GleamJson)
    "jsonb" -> Ok(GleamJson)

    // Array types - detected by ARRAY data_type or _ prefix in udt_name
    "array" -> map_array_type(lowercase_udt_name, enum_names)

    // User-defined types (enums)
    "user-defined" -> map_user_defined_type(udt_name, enum_names)

    // Try to match by udt_name for cases where data_type isn't specific
    _ -> map_by_udt_name(lowercase_udt_name, enum_names)
  }
}

/// Map an array type based on its element type (from udt_name with _ prefix)
fn map_array_type(
  udt_name: String,
  enum_names: List(String),
) -> Result(GleamType, TypeMappingError) {
  // PostgreSQL array types have udt_name starting with underscore
  // e.g., _int4 for integer[], _varchar for varchar[]
  case string.starts_with(udt_name, "_") {
    True -> {
      let element_udt = string.drop_start(udt_name, 1)
      case map_element_type(element_udt, enum_names) {
        Ok(element_type) -> Ok(GleamList(element_type))
        Error(_) -> Error(UnsupportedArray(element_udt))
      }
    }
    False -> Error(UnsupportedArray(udt_name))
  }
}

/// Map array element types
fn map_element_type(
  element_udt: String,
  enum_names: List(String),
) -> Result(GleamType, TypeMappingError) {
  let lowercase_udt = string.lowercase(element_udt)

  case lowercase_udt {
    // Integer types
    "int4" | "int" | "integer" -> Ok(GleamInt)
    "int2" | "smallint" -> Ok(GleamInt)
    "int8" | "bigint" -> Ok(GleamInt)

    // Float types
    "float4" | "real" -> Ok(GleamFloat)
    "float8" | "double precision" -> Ok(GleamFloat)

    // Decimal
    "numeric" | "decimal" -> Ok(GleamDecimal)

    // String types
    "varchar" | "character varying" -> Ok(GleamString)
    "bpchar" | "char" | "character" -> Ok(GleamString)
    "text" -> Ok(GleamString)
    "name" -> Ok(GleamString)

    // Boolean
    "bool" | "boolean" -> Ok(GleamBool)

    // Binary
    "bytea" -> Ok(GleamBitArray)

    // Timestamp
    "timestamp" | "timestamptz" -> Ok(GleamTime)

    // Date
    "date" -> Ok(GleamDate)

    // Time
    "time" | "timetz" -> Ok(GleamTimeOfDay)

    // UUID
    "uuid" -> Ok(GleamUuid)

    // JSON
    "json" | "jsonb" -> Ok(GleamJson)

    // Check if it's a custom enum
    _ -> {
      case list.contains(enum_names, element_udt) {
        True ->
          Ok(GleamCustom(
            module: "schema/enums",
            type_name: pascal_case(element_udt),
          ))
        False -> Error(UnknownType(element_udt, element_udt))
      }
    }
  }
}

/// Map user-defined types (primarily enums)
fn map_user_defined_type(
  udt_name: String,
  enum_names: List(String),
) -> Result(GleamType, TypeMappingError) {
  // Check if this is a known enum
  case list.contains(enum_names, udt_name) {
    True ->
      Ok(GleamCustom(module: "schema/enums", type_name: pascal_case(udt_name)))
    False -> {
      // Could be another user-defined type, treat as custom
      Ok(GleamCustom(module: "schema/types", type_name: pascal_case(udt_name)))
    }
  }
}

/// Try to map by udt_name when data_type is not specific enough
fn map_by_udt_name(
  udt_name: String,
  enum_names: List(String),
) -> Result(GleamType, TypeMappingError) {
  case udt_name {
    // Integer types
    "int4" | "int" -> Ok(GleamInt)
    "int2" -> Ok(GleamInt)
    "int8" -> Ok(GleamInt)

    // Float types
    "float4" -> Ok(GleamFloat)
    "float8" -> Ok(GleamFloat)

    // Decimal
    "numeric" -> Ok(GleamDecimal)

    // String types
    "varchar" -> Ok(GleamString)
    "bpchar" -> Ok(GleamString)
    "text" -> Ok(GleamString)
    "name" -> Ok(GleamString)

    // Boolean
    "bool" -> Ok(GleamBool)

    // Binary
    "bytea" -> Ok(GleamBitArray)

    // Timestamp
    "timestamp" | "timestamptz" -> Ok(GleamTime)

    // Date
    "date" -> Ok(GleamDate)

    // Time
    "time" | "timetz" -> Ok(GleamTimeOfDay)

    // UUID
    "uuid" -> Ok(GleamUuid)

    // JSON
    "json" | "jsonb" -> Ok(GleamJson)

    // Check for array types (start with underscore) or enums
    _ -> {
      case string.starts_with(udt_name, "_") {
        True -> map_array_type(udt_name, enum_names)
        False -> {
          case list.contains(enum_names, udt_name) {
            True ->
              Ok(GleamCustom(
                module: "schema/enums",
                type_name: pascal_case(udt_name),
              ))
            False -> Error(UnknownType("unknown", udt_name))
          }
        }
      }
    }
  }
}

// ============================================================================
// COLUMN METADATA MAPPING
// ============================================================================

/// Map an introspected column to full column metadata
///
/// Parameters:
/// - column_name: The column name
/// - data_type: PostgreSQL data type
/// - udt_name: Underlying type name
/// - is_nullable: Whether the column allows NULL
/// - column_default: Default value expression (if any)
/// - enum_names: List of known enum type names
///
/// Returns:
/// - Ok(ColumnMeta) with full metadata
/// - Error(TypeMappingError) if type cannot be mapped
pub fn map_column(
  column_name: String,
  data_type: String,
  udt_name: String,
  is_nullable: Bool,
  column_default: Option(String),
  enum_names: List(String),
) -> Result(ColumnMeta, TypeMappingError) {
  use gleam_type <- result.try(map_postgres_type(
    data_type,
    udt_name,
    is_nullable,
    enum_names,
  ))

  let is_auto_generated = detect_auto_generated(data_type, column_default)

  Ok(ColumnMeta(
    gleam_type:,
    is_auto_generated:,
    postgres_type: data_type,
    udt_name:,
    column_name:,
    is_nullable:,
  ))
}

/// Detect if a column is auto-generated based on type and default
fn detect_auto_generated(
  data_type: String,
  column_default: Option(String),
) -> Bool {
  let lowercase_type = string.lowercase(data_type)

  // Serial types are always auto-generated
  let is_serial = case lowercase_type {
    "serial" | "serial2" | "serial4" | "smallserial" | "bigserial" | "serial8" ->
      True
    _ -> False
  }

  // Check for sequence-based defaults (nextval)
  let has_sequence_default = case column_default {
    Some(default_expr) ->
      string.contains(string.lowercase(default_expr), "nextval")
    None -> False
  }

  // Check for other auto-generated defaults
  let has_auto_default = case column_default {
    Some(default_expr) -> {
      let lower = string.lowercase(default_expr)
      string.contains(lower, "now()")
      || string.contains(lower, "current_timestamp")
      || string.contains(lower, "current_date")
      || string.contains(lower, "gen_random_uuid")
      || string.contains(lower, "uuid_generate")
    }
    None -> False
  }

  is_serial || has_sequence_default || has_auto_default
}

// ============================================================================
// TYPE RENDERING FUNCTIONS
// ============================================================================

/// Render a GleamType as a Gleam type string for code generation
pub fn render_type(gleam_type: GleamType) -> String {
  case gleam_type {
    GleamInt -> "Int"
    GleamFloat -> "Float"
    GleamString -> "String"
    GleamBool -> "Bool"
    GleamBitArray -> "BitArray"
    GleamTime -> "Time"
    GleamDate -> "Date"
    GleamTimeOfDay -> "TimeOfDay"
    GleamDecimal -> "Decimal"
    GleamUuid -> "Uuid"
    GleamJson -> "Json"
    GleamList(element_type) -> "List(" <> render_type(element_type) <> ")"
    GleamOption(inner_type) -> "Option(" <> render_type(inner_type) <> ")"
    GleamCustom(_module, type_name) -> type_name
  }
}

/// Render a GleamType as a fully qualified Gleam type string (with imports)
pub fn render_qualified_type(gleam_type: GleamType) -> String {
  case gleam_type {
    GleamInt -> "Int"
    GleamFloat -> "Float"
    GleamString -> "String"
    GleamBool -> "Bool"
    GleamBitArray -> "BitArray"
    GleamTime -> "birl.Time"
    GleamDate -> "birl.Date"
    GleamTimeOfDay -> "birl.TimeOfDay"
    GleamDecimal -> "decimal.Decimal"
    GleamUuid -> "uuid.Uuid"
    GleamJson -> "json.Json"
    GleamList(element_type) ->
      "List(" <> render_qualified_type(element_type) <> ")"
    GleamOption(inner_type) ->
      "option.Option(" <> render_qualified_type(inner_type) <> ")"
    GleamCustom(module, type_name) -> module <> "." <> type_name
  }
}

/// Get the imports needed for a GleamType
pub fn get_imports(gleam_type: GleamType) -> List(String) {
  case gleam_type {
    GleamInt | GleamFloat | GleamString | GleamBool | GleamBitArray -> []
    GleamTime | GleamDate | GleamTimeOfDay -> ["birl"]
    GleamDecimal -> ["decimal"]
    GleamUuid -> ["uuid"]
    GleamJson -> ["gleam/json"]
    GleamList(element_type) -> get_imports(element_type)
    GleamOption(inner_type) -> ["gleam/option", ..get_imports(inner_type)]
    GleamCustom(module, _) -> [module]
  }
}

/// Collect all unique imports needed for a list of column metadata
pub fn collect_imports(columns: List(ColumnMeta)) -> List(String) {
  columns
  |> list.flat_map(fn(col) { get_imports(col.gleam_type) })
  |> list.unique
  |> list.sort(string.compare)
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

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

/// Convert PascalCase or snake_case to snake_case
pub fn snake_case(input: String) -> String {
  input
  |> string.to_graphemes
  |> list.index_fold("", fn(acc, char, index) {
    case is_uppercase(char), index {
      True, 0 -> string.lowercase(char)
      True, _ -> acc <> "_" <> string.lowercase(char)
      False, _ -> acc <> char
    }
  })
}

/// Check if a character is uppercase
fn is_uppercase(char: String) -> Bool {
  char == string.uppercase(char) && char != string.lowercase(char)
}

// ============================================================================
// TYPE CHECKING FUNCTIONS
// ============================================================================

/// Check if a GleamType is nullable (wrapped in Option)
pub fn is_nullable_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    GleamOption(_) -> True
    _ -> False
  }
}

/// Check if a GleamType is a list type
pub fn is_list_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    GleamList(_) -> True
    _ -> False
  }
}

/// Check if a GleamType is a custom type (enum or user-defined)
pub fn is_custom_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    GleamCustom(_, _) -> True
    GleamOption(GleamCustom(_, _)) -> True
    GleamList(GleamCustom(_, _)) -> True
    _ -> False
  }
}

/// Get the inner type from an Option type
pub fn unwrap_option(gleam_type: GleamType) -> GleamType {
  case gleam_type {
    GleamOption(inner) -> inner
    other -> other
  }
}

/// Get the element type from a List type
pub fn unwrap_list(gleam_type: GleamType) -> GleamType {
  case gleam_type {
    GleamList(element) -> element
    other -> other
  }
}

/// Check if two GleamTypes are equal
pub fn types_equal(type_a: GleamType, type_b: GleamType) -> Bool {
  case type_a, type_b {
    GleamInt, GleamInt -> True
    GleamFloat, GleamFloat -> True
    GleamString, GleamString -> True
    GleamBool, GleamBool -> True
    GleamBitArray, GleamBitArray -> True
    GleamTime, GleamTime -> True
    GleamDate, GleamDate -> True
    GleamTimeOfDay, GleamTimeOfDay -> True
    GleamDecimal, GleamDecimal -> True
    GleamUuid, GleamUuid -> True
    GleamJson, GleamJson -> True
    GleamList(a), GleamList(b) -> types_equal(a, b)
    GleamOption(a), GleamOption(b) -> types_equal(a, b)
    GleamCustom(mod_a, name_a), GleamCustom(mod_b, name_b) ->
      mod_a == mod_b && name_a == name_b
    _, _ -> False
  }
}
