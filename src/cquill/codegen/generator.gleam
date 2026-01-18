// Code Generation Module
//
// This module generates valid Gleam source files from introspected schema
// metadata. It produces type definitions, schema metadata, decoders, and
// encoders for database tables.
//
// Generated file structure:
// src/db/
// ├── schema.gleam        # Re-exports all schemas
// ├── schema/
// │   ├── user.gleam      # User types, schema, decoders
// │   └── ...
// ├── enums.gleam         # All enum types
// └── types.gleam         # Shared types

import cquill/codegen/type_mapping.{
  type ColumnMeta, type GleamType, ColumnMeta, GleamBitArray, GleamBool,
  GleamCustom, GleamDate, GleamDecimal, GleamFloat, GleamInt, GleamJson,
  GleamList, GleamOption, GleamString, GleamTime, GleamTimeOfDay, GleamUuid,
}
import cquill/introspection.{
  type IntrospectedColumn, type IntrospectedEnum, type IntrospectedTable,
}
import gleam/int
import gleam/list
import gleam/option
import gleam/string

// ============================================================================
// CONFIGURATION
// ============================================================================

/// Configuration for code generation
pub type GeneratorConfig {
  GeneratorConfig(
    /// Module prefix for generated files (e.g., "db" -> "db/schema/user")
    module_prefix: String,
    /// Whether to include timestamp fields (inserted_at, updated_at)
    include_timestamps: Bool,
    /// Names of timestamp fields to treat as auto-generated
    timestamp_fields: List(String),
    /// Whether to generate decoders
    generate_decoders: Bool,
    /// Whether to generate encoders
    generate_encoders: Bool,
  )
}

/// Create a default generator configuration
pub fn default_config() -> GeneratorConfig {
  GeneratorConfig(
    module_prefix: "db",
    include_timestamps: True,
    timestamp_fields: ["inserted_at", "updated_at", "created_at", "modified_at"],
    generate_decoders: True,
    generate_encoders: True,
  )
}

/// Create a generator configuration with a custom module prefix
pub fn with_module_prefix(
  config: GeneratorConfig,
  prefix: String,
) -> GeneratorConfig {
  GeneratorConfig(..config, module_prefix: prefix)
}

/// Set timestamp fields for a configuration
pub fn with_timestamp_fields(
  config: GeneratorConfig,
  fields: List(String),
) -> GeneratorConfig {
  GeneratorConfig(..config, timestamp_fields: fields)
}

// ============================================================================
// GENERATION RESULT
// ============================================================================

/// Result of generating a module
pub type GeneratedModule {
  GeneratedModule(
    /// Module path (e.g., "db/schema/user")
    path: String,
    /// File name (e.g., "user.gleam")
    filename: String,
    /// Generated source code
    content: String,
  )
}

// ============================================================================
// MAIN GENERATION FUNCTIONS
// ============================================================================

/// Generate a complete schema module for a table
pub fn generate_schema_module(
  table: IntrospectedTable,
  enums: List(IntrospectedEnum),
  config: GeneratorConfig,
) -> GeneratedModule {
  let enum_names = list.map(enums, fn(e) { e.name })
  let columns = map_columns(table, enum_names, config)
  let type_name = pascal_case(table.name)
  let module_name = snake_case(table.name)

  let content =
    generate_module_header(table.name, config)
    <> "\n"
    <> generate_imports(columns, config)
    <> "\n"
    <> generate_record_type(type_name, columns)
    <> "\n"
    <> generate_new_type(type_name, columns)
    <> "\n"
    <> generate_changes_type(type_name, columns)
    <> "\n"
    <> generate_schema_const(table, columns, config)
    <> case config.generate_decoders {
      True -> "\n" <> generate_decoder(type_name, columns)
      False -> ""
    }
    <> case config.generate_encoders {
      True -> "\n" <> generate_new_encoder(type_name, columns)
      False -> ""
    }

  GeneratedModule(
    path: config.module_prefix <> "/schema/" <> module_name,
    filename: module_name <> ".gleam",
    content:,
  )
}

/// Generate the enum module containing all enum types
pub fn generate_enum_module(
  enums: List(IntrospectedEnum),
  config: GeneratorConfig,
) -> GeneratedModule {
  let content =
    generate_enum_module_header(config)
    <> "\n"
    <> generate_enum_imports()
    <> "\n"
    <> string.join(list.map(enums, generate_enum_type), "\n\n")

  GeneratedModule(
    path: config.module_prefix <> "/enums",
    filename: "enums.gleam",
    content:,
  )
}

/// Generate the index module that re-exports all schemas
pub fn generate_index_module(
  tables: List(IntrospectedTable),
  enums: List(IntrospectedEnum),
  config: GeneratorConfig,
) -> GeneratedModule {
  let content =
    generate_index_module_header(config)
    <> "\n"
    <> generate_index_imports(tables, enums, config)

  GeneratedModule(
    path: config.module_prefix <> "/schema",
    filename: "schema.gleam",
    content:,
  )
}

// ============================================================================
// MODULE HEADERS
// ============================================================================

fn generate_module_header(
  table_name: String,
  _config: GeneratorConfig,
) -> String {
  "// "
  <> table_name
  <> ".gleam (GENERATED)\n"
  <> "//// Schema and types for "
  <> table_name
  <> " table\n"
  <> "//// Generated by cquill - DO NOT EDIT\n"
}

fn generate_enum_module_header(_config: GeneratorConfig) -> String {
  "// enums.gleam (GENERATED)\n"
  <> "//// Database enum types\n"
  <> "//// Generated by cquill - DO NOT EDIT\n"
}

fn generate_index_module_header(_config: GeneratorConfig) -> String {
  "// schema.gleam (GENERATED)\n"
  <> "//// Database schema definitions\n"
  <> "//// Generated by cquill - DO NOT EDIT\n"
}

// ============================================================================
// IMPORT GENERATION
// ============================================================================

fn generate_imports(
  columns: List(ColumnMeta),
  _config: GeneratorConfig,
) -> String {
  let imports = type_mapping.collect_imports(columns)

  let standard_imports = ["import cquill/schema.{type Schema}"]

  let type_imports =
    imports
    |> list.filter_map(fn(imp) {
      case imp {
        "gleam/option" -> Ok("import gleam/option.{type Option}")
        "birl" -> Ok("import birl.{type Time}")
        "uuid" -> Ok("import uuid.{type Uuid}")
        "decimal" -> Ok("import decimal.{type Decimal}")
        "gleam/json" -> Ok("import gleam/json.{type Json}")
        _ -> Error(Nil)
      }
    })

  let all_imports = list.append(standard_imports, type_imports)
  string.join(all_imports, "\n")
}

fn generate_enum_imports() -> String {
  "import gleam/dynamic.{type Dynamic}\n" <> "import gleam/result"
}

fn generate_index_imports(
  tables: List(IntrospectedTable),
  enums: List(IntrospectedEnum),
  config: GeneratorConfig,
) -> String {
  let table_imports =
    tables
    |> list.map(fn(table) {
      let type_name = pascal_case(table.name)
      let module_name = snake_case(table.name)
      "import "
      <> config.module_prefix
      <> "/schema/"
      <> module_name
      <> ".{"
      <> type_name
      <> ", New"
      <> type_name
      <> ", "
      <> type_name
      <> "Changes, "
      <> module_name
      <> "_schema}"
    })

  let enum_import = case list.is_empty(enums) {
    True -> []
    False -> {
      let enum_types =
        enums
        |> list.map(fn(e) { pascal_case(e.name) })
        |> string.join(", ")
      ["import " <> config.module_prefix <> "/enums.{" <> enum_types <> "}"]
    }
  }

  string.join(list.append(table_imports, enum_import), "\n")
}

// ============================================================================
// TYPE GENERATION
// ============================================================================

/// Generate the main record type (for SELECT results)
fn generate_record_type(type_name: String, columns: List(ColumnMeta)) -> String {
  let fields =
    columns
    |> list.map(fn(col) {
      "    " <> col.column_name <> ": " <> render_type(col.gleam_type) <> ","
    })
    |> string.join("\n")

  "/// Full "
  <> type_name
  <> " record (for SELECT results)\n"
  <> "pub type "
  <> type_name
  <> " {\n"
  <> "  "
  <> type_name
  <> "(\n"
  <> fields
  <> "\n  )\n"
  <> "}"
}

/// Generate the New type (for INSERT operations, excludes auto-generated)
fn generate_new_type(type_name: String, columns: List(ColumnMeta)) -> String {
  let insert_columns =
    columns
    |> list.filter(fn(col) { !col.is_auto_generated })

  let fields =
    insert_columns
    |> list.map(fn(col) {
      "    " <> col.column_name <> ": " <> render_type(col.gleam_type) <> ","
    })
    |> string.join("\n")

  "/// For INSERT operations (excludes auto-generated fields)\n"
  <> "pub type New"
  <> type_name
  <> " {\n"
  <> "  New"
  <> type_name
  <> "(\n"
  <> fields
  <> "\n  )\n"
  <> "}"
}

/// Generate the Changes type (for UPDATE operations, all fields optional)
fn generate_changes_type(type_name: String, columns: List(ColumnMeta)) -> String {
  let update_columns =
    columns
    |> list.filter(fn(col) { !col.is_auto_generated })

  let fields =
    update_columns
    |> list.map(fn(col) {
      let field_type = case col.gleam_type {
        // For nullable fields, wrap in Option(Option(T)) to distinguish
        // between "not updating" and "setting to null"
        GleamOption(inner) -> "Option(Option(" <> render_type(inner) <> "))"
        other -> "Option(" <> render_type(other) <> ")"
      }
      "    " <> col.column_name <> ": " <> field_type <> ","
    })
    |> string.join("\n")

  "/// For UPDATE operations (all fields optional)\n"
  <> "pub type "
  <> type_name
  <> "Changes {\n"
  <> "  "
  <> type_name
  <> "Changes(\n"
  <> fields
  <> "\n  )\n"
  <> "}"
}

// ============================================================================
// SCHEMA METADATA GENERATION
// ============================================================================

fn generate_schema_const(
  table: IntrospectedTable,
  columns: List(ColumnMeta),
  _config: GeneratorConfig,
) -> String {
  let schema_name = snake_case(table.name) <> "_schema"

  let fields =
    columns
    |> list.map(fn(col) { generate_field_definition(col, table) })
    |> string.join(",\n")

  let pk_str =
    table.primary_key
    |> list.map(fn(pk) { "\"" <> pk <> "\"" })
    |> string.join(", ")

  "/// Schema metadata for "
  <> table.name
  <> "\n"
  <> "pub const "
  <> schema_name
  <> " = Schema(\n"
  <> "  source: \""
  <> table.name
  <> "\",\n"
  <> "  fields: [\n"
  <> fields
  <> "\n  ],\n"
  <> "  primary_key: ["
  <> pk_str
  <> "],\n"
  <> ")"
}

fn generate_field_definition(
  col: ColumnMeta,
  table: IntrospectedTable,
) -> String {
  let constraints = generate_constraints(col, table)

  "    #(\""
  <> col.column_name
  <> "\", \""
  <> render_field_type(col.gleam_type)
  <> "\", ["
  <> constraints
  <> "])"
}

fn generate_constraints(col: ColumnMeta, table: IntrospectedTable) -> String {
  let mut_constraints = []

  // Check if primary key
  let is_pk = list.contains(table.primary_key, col.column_name)
  let constraints = case is_pk {
    True -> ["\"primary_key\"", ..mut_constraints]
    False -> mut_constraints
  }

  // Check if not null (and not nullable)
  let constraints = case col.is_nullable {
    False -> ["\"not_null\"", ..constraints]
    True -> constraints
  }

  // Check for unique constraint
  let is_unique =
    list.any(table.unique_constraints, fn(u) { u.columns == [col.column_name] })
  let constraints = case is_unique {
    True -> ["\"unique\"", ..constraints]
    False -> constraints
  }

  string.join(list.reverse(constraints), ", ")
}

// ============================================================================
// DECODER GENERATION
// ============================================================================

fn generate_decoder(type_name: String, columns: List(ColumnMeta)) -> String {
  let decoder_name = snake_case(type_name) <> "_decoder"

  let field_count = list.length(columns)
  let field_params =
    columns
    |> list.index_map(fn(col, _idx) {
      "    use " <> col.column_name <> " <- decoder.parameter"
    })
    |> string.join("\n")

  let constructor_args =
    columns
    |> list.map(fn(col) { col.column_name <> ":" })
    |> string.join(", ")

  let field_decoders =
    columns
    |> list.index_map(fn(col, idx) {
      "  |> decoder.field("
      <> int.to_string(idx)
      <> ", "
      <> generate_field_decoder(col.gleam_type)
      <> ")"
    })
    |> string.join("\n")

  "/// Decoder for "
  <> type_name
  <> " from database rows\n"
  <> "pub fn "
  <> decoder_name
  <> "() {\n"
  <> "  decoder.into({\n"
  <> field_params
  <> "\n"
  <> "    "
  <> type_name
  <> "("
  <> constructor_args
  <> ")\n"
  <> "  })\n"
  <> field_decoders
  <> "\n"
  <> "}\n"
  <> "\n"
  <> "// Field count: "
  <> int.to_string(field_count)
}

fn generate_field_decoder(gleam_type: GleamType) -> String {
  case gleam_type {
    GleamInt -> "decoder.int()"
    GleamFloat -> "decoder.float()"
    GleamString -> "decoder.string()"
    GleamBool -> "decoder.bool()"
    GleamBitArray -> "decoder.bit_array()"
    GleamTime -> "decoder.time()"
    GleamDate -> "decoder.date()"
    GleamTimeOfDay -> "decoder.time_of_day()"
    GleamDecimal -> "decoder.decimal()"
    GleamUuid -> "decoder.uuid()"
    GleamJson -> "decoder.json()"
    GleamList(element) ->
      "decoder.list(" <> generate_field_decoder(element) <> ")"
    GleamOption(inner) ->
      "decoder.optional(" <> generate_field_decoder(inner) <> ")"
    GleamCustom(_, type_name) -> snake_case(type_name) <> "_decoder()"
  }
}

// ============================================================================
// ENCODER GENERATION
// ============================================================================

fn generate_new_encoder(type_name: String, columns: List(ColumnMeta)) -> String {
  let encoder_name = "new_" <> snake_case(type_name) <> "_encoder"
  let param_name = snake_case(type_name)

  let insert_columns =
    columns
    |> list.filter(fn(col) { !col.is_auto_generated })

  let field_encoders =
    insert_columns
    |> list.map(fn(col) {
      "    #(\""
      <> col.column_name
      <> "\", "
      <> generate_field_encoder(
        col.gleam_type,
        param_name <> "." <> col.column_name,
      )
      <> "),"
    })
    |> string.join("\n")

  "/// Encoder for New"
  <> type_name
  <> "\n"
  <> "pub fn "
  <> encoder_name
  <> "("
  <> param_name
  <> ": New"
  <> type_name
  <> ") {\n"
  <> "  [\n"
  <> field_encoders
  <> "\n  ]\n"
  <> "}"
}

fn generate_field_encoder(gleam_type: GleamType, value_expr: String) -> String {
  case gleam_type {
    GleamInt -> "encoder.int(" <> value_expr <> ")"
    GleamFloat -> "encoder.float(" <> value_expr <> ")"
    GleamString -> "encoder.string(" <> value_expr <> ")"
    GleamBool -> "encoder.bool(" <> value_expr <> ")"
    GleamBitArray -> "encoder.bit_array(" <> value_expr <> ")"
    GleamTime -> "encoder.time(" <> value_expr <> ")"
    GleamDate -> "encoder.date(" <> value_expr <> ")"
    GleamTimeOfDay -> "encoder.time_of_day(" <> value_expr <> ")"
    GleamDecimal -> "encoder.decimal(" <> value_expr <> ")"
    GleamUuid -> "encoder.uuid(" <> value_expr <> ")"
    GleamJson -> "encoder.json(" <> value_expr <> ")"
    GleamList(_element) -> "encoder.list(" <> value_expr <> ")"
    GleamOption(inner) ->
      "encoder.optional("
      <> generate_encoder_fn(inner)
      <> ", "
      <> value_expr
      <> ")"
    GleamCustom(_, type_name) ->
      snake_case(type_name) <> "_encoder(" <> value_expr <> ")"
  }
}

fn generate_encoder_fn(gleam_type: GleamType) -> String {
  case gleam_type {
    GleamInt -> "encoder.int"
    GleamFloat -> "encoder.float"
    GleamString -> "encoder.string"
    GleamBool -> "encoder.bool"
    GleamBitArray -> "encoder.bit_array"
    GleamTime -> "encoder.time"
    GleamDate -> "encoder.date"
    GleamTimeOfDay -> "encoder.time_of_day"
    GleamDecimal -> "encoder.decimal"
    GleamUuid -> "encoder.uuid"
    GleamJson -> "encoder.json"
    GleamList(_) -> "encoder.list"
    GleamOption(_) -> "encoder.optional"
    GleamCustom(_, type_name) -> snake_case(type_name) <> "_encoder"
  }
}

// ============================================================================
// ENUM GENERATION
// ============================================================================

fn generate_enum_type(enum_type: IntrospectedEnum) -> String {
  let type_name = pascal_case(enum_type.name)

  let variants =
    enum_type.values
    |> list.map(pascal_case)
    |> string.join("\n  ")

  let decoder = generate_enum_decoder(enum_type)
  let encoder = generate_enum_encoder(enum_type)
  let to_string_fn = generate_enum_to_string(enum_type)

  "pub type "
  <> type_name
  <> " {\n  "
  <> variants
  <> "\n}\n\n"
  <> decoder
  <> "\n\n"
  <> encoder
  <> "\n\n"
  <> to_string_fn
}

fn generate_enum_decoder(enum_type: IntrospectedEnum) -> String {
  let type_name = pascal_case(enum_type.name)
  let decoder_name = snake_case(enum_type.name) <> "_decoder"

  let cases =
    enum_type.values
    |> list.map(fn(value) {
      "      \"" <> value <> "\" -> Ok(" <> pascal_case(value) <> ")"
    })
    |> string.join("\n")

  "pub fn "
  <> decoder_name
  <> "() {\n"
  <> "  fn(dyn: Dynamic) {\n"
  <> "    use s <- result.try(dynamic.string(dyn))\n"
  <> "    case s {\n"
  <> cases
  <> "\n"
  <> "      _ -> Error([dynamic.DecodeError(expected: \""
  <> type_name
  <> "\", found: s, path: [])])\n"
  <> "    }\n"
  <> "  }\n"
  <> "}"
}

fn generate_enum_encoder(enum_type: IntrospectedEnum) -> String {
  let type_name = pascal_case(enum_type.name)
  let encoder_name = snake_case(enum_type.name) <> "_encoder"
  let param_name = "value"

  let cases =
    enum_type.values
    |> list.map(fn(value) {
      "    " <> pascal_case(value) <> " -> \"" <> value <> "\""
    })
    |> string.join("\n")

  "pub fn "
  <> encoder_name
  <> "("
  <> param_name
  <> ": "
  <> type_name
  <> ") -> String {\n"
  <> "  case "
  <> param_name
  <> " {\n"
  <> cases
  <> "\n  }\n"
  <> "}"
}

fn generate_enum_to_string(enum_type: IntrospectedEnum) -> String {
  let type_name = pascal_case(enum_type.name)
  let fn_name = snake_case(enum_type.name) <> "_to_string"
  let param_name = "value"

  let cases =
    enum_type.values
    |> list.map(fn(value) {
      "    " <> pascal_case(value) <> " -> \"" <> value <> "\""
    })
    |> string.join("\n")

  "pub fn "
  <> fn_name
  <> "("
  <> param_name
  <> ": "
  <> type_name
  <> ") -> String {\n"
  <> "  case "
  <> param_name
  <> " {\n"
  <> cases
  <> "\n  }\n"
  <> "}"
}

// ============================================================================
// TYPE RENDERING HELPERS
// ============================================================================

fn render_type(gleam_type: GleamType) -> String {
  type_mapping.render_type(gleam_type)
}

fn render_field_type(gleam_type: GleamType) -> String {
  case gleam_type {
    GleamInt -> "integer"
    GleamFloat -> "float"
    GleamString -> "string"
    GleamBool -> "boolean"
    GleamBitArray -> "binary"
    GleamTime -> "timestamp"
    GleamDate -> "date"
    GleamTimeOfDay -> "time"
    GleamDecimal -> "decimal"
    GleamUuid -> "uuid"
    GleamJson -> "json"
    GleamList(element) -> "array:" <> render_field_type(element)
    GleamOption(inner) -> "optional:" <> render_field_type(inner)
    GleamCustom(_, type_name) -> "custom:" <> type_name
  }
}

// ============================================================================
// COLUMN MAPPING
// ============================================================================

fn map_columns(
  table: IntrospectedTable,
  enum_names: List(String),
  config: GeneratorConfig,
) -> List(ColumnMeta) {
  table.columns
  |> list.filter_map(fn(col) { map_column_to_meta(col, enum_names, config) })
}

fn map_column_to_meta(
  col: IntrospectedColumn,
  enum_names: List(String),
  config: GeneratorConfig,
) -> Result(ColumnMeta, Nil) {
  case
    type_mapping.map_column(
      col.name,
      col.data_type,
      col.udt_name,
      col.is_nullable,
      col.default,
      enum_names,
    )
  {
    Ok(meta) -> {
      // Also check for timestamp fields
      let is_timestamp_field = list.contains(config.timestamp_fields, col.name)
      let final_meta = case is_timestamp_field {
        True -> ColumnMeta(..meta, is_auto_generated: True)
        False -> meta
      }
      Ok(final_meta)
    }
    Error(_) -> Error(Nil)
  }
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/// Convert snake_case to PascalCase
pub fn pascal_case(input: String) -> String {
  type_mapping.pascal_case(input)
}

/// Convert PascalCase to snake_case
pub fn snake_case(input: String) -> String {
  type_mapping.snake_case(input)
}

// ============================================================================
// BATCH GENERATION
// ============================================================================

/// Generate all modules for a complete schema
pub fn generate_all(
  tables: List(IntrospectedTable),
  enums: List(IntrospectedEnum),
  config: GeneratorConfig,
) -> List(GeneratedModule) {
  let table_modules =
    tables
    |> list.map(fn(table) { generate_schema_module(table, enums, config) })

  let enum_module = case list.is_empty(enums) {
    True -> []
    False -> [generate_enum_module(enums, config)]
  }

  let index_module = [generate_index_module(tables, enums, config)]

  list.flatten([table_modules, enum_module, index_module])
}

/// Get the full file path for a generated module
pub fn module_file_path(module: GeneratedModule) -> String {
  "src/" <> module.path <> ".gleam"
}
