// Tests for the type mapping module
//
// These tests verify:
// - All standard PostgreSQL types map correctly
// - Nullable types are wrapped in Option
// - Arrays map to List
// - Unknown types produce clear errors
// - Serial types marked as auto-generated
// - Custom enum detection
// - Type rendering for code generation

import cquill/codegen/type_mapping.{
  type ColumnMeta, ColumnMeta, GleamBitArray, GleamBool, GleamCustom, GleamDate,
  GleamDecimal, GleamFloat, GleamInt, GleamJson, GleamList, GleamOption,
  GleamString, GleamTime, GleamTimeOfDay, GleamUuid, UnknownType,
  UnsupportedArray,
}
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// INTEGER TYPE MAPPING TESTS
// ============================================================================

pub fn map_integer_test() {
  type_mapping.map_postgres_type("integer", "int4", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_int_test() {
  type_mapping.map_postgres_type("int", "int4", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_int4_test() {
  type_mapping.map_postgres_type("int4", "int4", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_smallint_test() {
  type_mapping.map_postgres_type("smallint", "int2", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_bigint_test() {
  type_mapping.map_postgres_type("bigint", "int8", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_serial_test() {
  type_mapping.map_postgres_type("serial", "serial4", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_bigserial_test() {
  type_mapping.map_postgres_type("bigserial", "serial8", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_smallserial_test() {
  type_mapping.map_postgres_type("smallserial", "serial2", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

// ============================================================================
// FLOAT TYPE MAPPING TESTS
// ============================================================================

pub fn map_real_test() {
  type_mapping.map_postgres_type("real", "float4", False, [])
  |> should.be_ok
  |> should.equal(GleamFloat)
}

pub fn map_double_precision_test() {
  type_mapping.map_postgres_type("double precision", "float8", False, [])
  |> should.be_ok
  |> should.equal(GleamFloat)
}

pub fn map_float4_test() {
  type_mapping.map_postgres_type("float4", "float4", False, [])
  |> should.be_ok
  |> should.equal(GleamFloat)
}

pub fn map_float8_test() {
  type_mapping.map_postgres_type("float8", "float8", False, [])
  |> should.be_ok
  |> should.equal(GleamFloat)
}

// ============================================================================
// DECIMAL TYPE MAPPING TESTS
// ============================================================================

pub fn map_numeric_test() {
  type_mapping.map_postgres_type("numeric", "numeric", False, [])
  |> should.be_ok
  |> should.equal(GleamDecimal)
}

pub fn map_decimal_test() {
  type_mapping.map_postgres_type("decimal", "numeric", False, [])
  |> should.be_ok
  |> should.equal(GleamDecimal)
}

// ============================================================================
// STRING TYPE MAPPING TESTS
// ============================================================================

pub fn map_varchar_test() {
  type_mapping.map_postgres_type("character varying", "varchar", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

pub fn map_char_test() {
  type_mapping.map_postgres_type("character", "bpchar", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

pub fn map_bpchar_test() {
  type_mapping.map_postgres_type("bpchar", "bpchar", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

pub fn map_text_test() {
  type_mapping.map_postgres_type("text", "text", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

pub fn map_name_test() {
  type_mapping.map_postgres_type("name", "name", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

// ============================================================================
// BOOLEAN TYPE MAPPING TESTS
// ============================================================================

pub fn map_boolean_test() {
  type_mapping.map_postgres_type("boolean", "bool", False, [])
  |> should.be_ok
  |> should.equal(GleamBool)
}

pub fn map_bool_test() {
  type_mapping.map_postgres_type("bool", "bool", False, [])
  |> should.be_ok
  |> should.equal(GleamBool)
}

// ============================================================================
// BINARY TYPE MAPPING TESTS
// ============================================================================

pub fn map_bytea_test() {
  type_mapping.map_postgres_type("bytea", "bytea", False, [])
  |> should.be_ok
  |> should.equal(GleamBitArray)
}

// ============================================================================
// TIMESTAMP TYPE MAPPING TESTS
// ============================================================================

pub fn map_timestamp_test() {
  type_mapping.map_postgres_type(
    "timestamp without time zone",
    "timestamp",
    False,
    [],
  )
  |> should.be_ok
  |> should.equal(GleamTime)
}

pub fn map_timestamptz_test() {
  type_mapping.map_postgres_type(
    "timestamp with time zone",
    "timestamptz",
    False,
    [],
  )
  |> should.be_ok
  |> should.equal(GleamTime)
}

pub fn map_timestamp_short_test() {
  type_mapping.map_postgres_type("timestamp", "timestamp", False, [])
  |> should.be_ok
  |> should.equal(GleamTime)
}

// ============================================================================
// DATE TYPE MAPPING TESTS
// ============================================================================

pub fn map_date_test() {
  type_mapping.map_postgres_type("date", "date", False, [])
  |> should.be_ok
  |> should.equal(GleamDate)
}

// ============================================================================
// TIME TYPE MAPPING TESTS
// ============================================================================

pub fn map_time_test() {
  type_mapping.map_postgres_type("time without time zone", "time", False, [])
  |> should.be_ok
  |> should.equal(GleamTimeOfDay)
}

pub fn map_timetz_test() {
  type_mapping.map_postgres_type("time with time zone", "timetz", False, [])
  |> should.be_ok
  |> should.equal(GleamTimeOfDay)
}

pub fn map_time_short_test() {
  type_mapping.map_postgres_type("time", "time", False, [])
  |> should.be_ok
  |> should.equal(GleamTimeOfDay)
}

// ============================================================================
// UUID TYPE MAPPING TESTS
// ============================================================================

pub fn map_uuid_test() {
  type_mapping.map_postgres_type("uuid", "uuid", False, [])
  |> should.be_ok
  |> should.equal(GleamUuid)
}

// ============================================================================
// JSON TYPE MAPPING TESTS
// ============================================================================

pub fn map_json_test() {
  type_mapping.map_postgres_type("json", "json", False, [])
  |> should.be_ok
  |> should.equal(GleamJson)
}

pub fn map_jsonb_test() {
  type_mapping.map_postgres_type("jsonb", "jsonb", False, [])
  |> should.be_ok
  |> should.equal(GleamJson)
}

// ============================================================================
// INTERVAL TYPE MAPPING TESTS
// ============================================================================

pub fn map_interval_test() {
  type_mapping.map_postgres_type("interval", "interval", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

// ============================================================================
// NULLABLE TYPE TESTS
// ============================================================================

pub fn nullable_integer_test() {
  type_mapping.map_postgres_type("integer", "int4", True, [])
  |> should.be_ok
  |> should.equal(GleamOption(GleamInt))
}

pub fn nullable_string_test() {
  type_mapping.map_postgres_type("text", "text", True, [])
  |> should.be_ok
  |> should.equal(GleamOption(GleamString))
}

pub fn nullable_boolean_test() {
  type_mapping.map_postgres_type("boolean", "bool", True, [])
  |> should.be_ok
  |> should.equal(GleamOption(GleamBool))
}

pub fn nullable_timestamp_test() {
  type_mapping.map_postgres_type(
    "timestamp with time zone",
    "timestamptz",
    True,
    [],
  )
  |> should.be_ok
  |> should.equal(GleamOption(GleamTime))
}

pub fn nullable_uuid_test() {
  type_mapping.map_postgres_type("uuid", "uuid", True, [])
  |> should.be_ok
  |> should.equal(GleamOption(GleamUuid))
}

// ============================================================================
// ARRAY TYPE TESTS
// ============================================================================

pub fn map_integer_array_test() {
  type_mapping.map_postgres_type("array", "_int4", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamInt))
}

pub fn map_text_array_test() {
  type_mapping.map_postgres_type("array", "_text", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamString))
}

pub fn map_varchar_array_test() {
  type_mapping.map_postgres_type("array", "_varchar", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamString))
}

pub fn map_boolean_array_test() {
  type_mapping.map_postgres_type("array", "_bool", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamBool))
}

pub fn map_uuid_array_test() {
  type_mapping.map_postgres_type("array", "_uuid", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamUuid))
}

pub fn map_float_array_test() {
  type_mapping.map_postgres_type("array", "_float8", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamFloat))
}

pub fn nullable_array_test() {
  type_mapping.map_postgres_type("array", "_int4", True, [])
  |> should.be_ok
  |> should.equal(GleamOption(GleamList(GleamInt)))
}

pub fn map_enum_array_test() {
  type_mapping.map_postgres_type("array", "_user_role", False, ["user_role"])
  |> should.be_ok
  |> should.equal(GleamList(GleamCustom("schema/enums", "UserRole")))
}

// ============================================================================
// ENUM TYPE TESTS
// ============================================================================

pub fn map_enum_test() {
  type_mapping.map_postgres_type("user-defined", "user_role", False, [
    "user_role",
  ])
  |> should.be_ok
  |> should.equal(GleamCustom("schema/enums", "UserRole"))
}

pub fn map_enum_nullable_test() {
  type_mapping.map_postgres_type("user-defined", "user_status", True, [
    "user_status",
  ])
  |> should.be_ok
  |> should.equal(GleamOption(GleamCustom("schema/enums", "UserStatus")))
}

pub fn map_unknown_user_defined_test() {
  // Unknown user-defined types are treated as custom types
  type_mapping.map_postgres_type("user-defined", "my_custom_type", False, [])
  |> should.be_ok
  |> should.equal(GleamCustom("schema/types", "MyCustomType"))
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

pub fn unknown_type_error_test() {
  type_mapping.map_postgres_type(
    "unknown_postgres_type",
    "unknown_udt",
    False,
    [],
  )
  |> should.be_error
  |> should.equal(UnknownType("unknown", "unknown_udt"))
}

pub fn unsupported_array_error_test() {
  // An array with a truly unknown element type
  type_mapping.map_postgres_type("array", "_totally_unknown", False, [])
  |> should.be_error
  |> should.equal(UnsupportedArray("totally_unknown"))
}

// ============================================================================
// COLUMN METADATA TESTS
// ============================================================================

pub fn map_column_basic_test() {
  type_mapping.map_column("id", "integer", "int4", False, None, [])
  |> should.be_ok
  |> fn(meta: ColumnMeta) {
    meta.gleam_type |> should.equal(GleamInt)
    meta.is_auto_generated |> should.be_false
    meta.column_name |> should.equal("id")
    meta.is_nullable |> should.be_false
  }
}

pub fn map_column_serial_auto_generated_test() {
  type_mapping.map_column("id", "serial", "serial4", False, None, [])
  |> should.be_ok
  |> fn(meta: ColumnMeta) {
    meta.gleam_type |> should.equal(GleamInt)
    meta.is_auto_generated |> should.be_true
  }
}

pub fn map_column_bigserial_auto_generated_test() {
  type_mapping.map_column("id", "bigserial", "serial8", False, None, [])
  |> should.be_ok
  |> fn(meta: ColumnMeta) { meta.is_auto_generated |> should.be_true }
}

pub fn map_column_nextval_auto_generated_test() {
  type_mapping.map_column(
    "id",
    "integer",
    "int4",
    False,
    Some("nextval('users_id_seq'::regclass)"),
    [],
  )
  |> should.be_ok
  |> fn(meta: ColumnMeta) { meta.is_auto_generated |> should.be_true }
}

pub fn map_column_now_auto_generated_test() {
  type_mapping.map_column(
    "created_at",
    "timestamp with time zone",
    "timestamptz",
    False,
    Some("now()"),
    [],
  )
  |> should.be_ok
  |> fn(meta: ColumnMeta) { meta.is_auto_generated |> should.be_true }
}

pub fn map_column_current_timestamp_auto_generated_test() {
  type_mapping.map_column(
    "updated_at",
    "timestamp with time zone",
    "timestamptz",
    True,
    Some("CURRENT_TIMESTAMP"),
    [],
  )
  |> should.be_ok
  |> fn(meta: ColumnMeta) { meta.is_auto_generated |> should.be_true }
}

pub fn map_column_gen_random_uuid_auto_generated_test() {
  type_mapping.map_column(
    "uuid",
    "uuid",
    "uuid",
    False,
    Some("gen_random_uuid()"),
    [],
  )
  |> should.be_ok
  |> fn(meta: ColumnMeta) { meta.is_auto_generated |> should.be_true }
}

pub fn map_column_regular_default_not_auto_generated_test() {
  type_mapping.map_column(
    "status",
    "text",
    "text",
    False,
    Some("'pending'::text"),
    [],
  )
  |> should.be_ok
  |> fn(meta: ColumnMeta) {
    // Regular defaults are NOT auto-generated
    meta.is_auto_generated |> should.be_false
  }
}

pub fn map_column_nullable_test() {
  type_mapping.map_column("name", "text", "text", True, None, [])
  |> should.be_ok
  |> fn(meta: ColumnMeta) {
    meta.gleam_type |> should.equal(GleamOption(GleamString))
    meta.is_nullable |> should.be_true
  }
}

pub fn map_column_with_enum_test() {
  type_mapping.map_column("role", "user-defined", "user_role", False, None, [
    "user_role",
  ])
  |> should.be_ok
  |> fn(meta: ColumnMeta) {
    meta.gleam_type
    |> should.equal(GleamCustom("schema/enums", "UserRole"))
  }
}

// ============================================================================
// TYPE RENDERING TESTS
// ============================================================================

pub fn render_int_test() {
  type_mapping.render_type(GleamInt)
  |> should.equal("Int")
}

pub fn render_float_test() {
  type_mapping.render_type(GleamFloat)
  |> should.equal("Float")
}

pub fn render_string_test() {
  type_mapping.render_type(GleamString)
  |> should.equal("String")
}

pub fn render_bool_test() {
  type_mapping.render_type(GleamBool)
  |> should.equal("Bool")
}

pub fn render_bit_array_test() {
  type_mapping.render_type(GleamBitArray)
  |> should.equal("BitArray")
}

pub fn render_time_test() {
  type_mapping.render_type(GleamTime)
  |> should.equal("Time")
}

pub fn render_date_test() {
  type_mapping.render_type(GleamDate)
  |> should.equal("Date")
}

pub fn render_time_of_day_test() {
  type_mapping.render_type(GleamTimeOfDay)
  |> should.equal("TimeOfDay")
}

pub fn render_decimal_test() {
  type_mapping.render_type(GleamDecimal)
  |> should.equal("Decimal")
}

pub fn render_uuid_test() {
  type_mapping.render_type(GleamUuid)
  |> should.equal("Uuid")
}

pub fn render_json_test() {
  type_mapping.render_type(GleamJson)
  |> should.equal("Json")
}

pub fn render_list_test() {
  type_mapping.render_type(GleamList(GleamInt))
  |> should.equal("List(Int)")
}

pub fn render_option_test() {
  type_mapping.render_type(GleamOption(GleamString))
  |> should.equal("Option(String)")
}

pub fn render_custom_test() {
  type_mapping.render_type(GleamCustom("schema/enums", "UserRole"))
  |> should.equal("UserRole")
}

pub fn render_nested_option_list_test() {
  type_mapping.render_type(GleamOption(GleamList(GleamInt)))
  |> should.equal("Option(List(Int))")
}

// ============================================================================
// QUALIFIED TYPE RENDERING TESTS
// ============================================================================

pub fn render_qualified_time_test() {
  type_mapping.render_qualified_type(GleamTime)
  |> should.equal("birl.Time")
}

pub fn render_qualified_decimal_test() {
  type_mapping.render_qualified_type(GleamDecimal)
  |> should.equal("decimal.Decimal")
}

pub fn render_qualified_uuid_test() {
  type_mapping.render_qualified_type(GleamUuid)
  |> should.equal("uuid.Uuid")
}

pub fn render_qualified_option_test() {
  type_mapping.render_qualified_type(GleamOption(GleamString))
  |> should.equal("option.Option(String)")
}

pub fn render_qualified_custom_test() {
  type_mapping.render_qualified_type(GleamCustom("schema/enums", "UserRole"))
  |> should.equal("schema/enums.UserRole")
}

// ============================================================================
// IMPORTS COLLECTION TESTS
// ============================================================================

pub fn get_imports_basic_types_test() {
  type_mapping.get_imports(GleamInt)
  |> should.equal([])
}

pub fn get_imports_time_test() {
  type_mapping.get_imports(GleamTime)
  |> should.equal(["birl"])
}

pub fn get_imports_option_test() {
  type_mapping.get_imports(GleamOption(GleamString))
  |> should.equal(["gleam/option"])
}

pub fn get_imports_nested_test() {
  type_mapping.get_imports(GleamOption(GleamTime))
  |> should.equal(["gleam/option", "birl"])
}

pub fn collect_imports_test() {
  let columns = [
    ColumnMeta(
      gleam_type: GleamInt,
      is_auto_generated: False,
      postgres_type: "integer",
      udt_name: "int4",
      column_name: "id",
      is_nullable: False,
    ),
    ColumnMeta(
      gleam_type: GleamOption(GleamTime),
      is_auto_generated: True,
      postgres_type: "timestamp with time zone",
      udt_name: "timestamptz",
      column_name: "created_at",
      is_nullable: True,
    ),
    ColumnMeta(
      gleam_type: GleamUuid,
      is_auto_generated: False,
      postgres_type: "uuid",
      udt_name: "uuid",
      column_name: "external_id",
      is_nullable: False,
    ),
  ]

  type_mapping.collect_imports(columns)
  |> should.equal(["birl", "gleam/option", "uuid"])
}

// ============================================================================
// UTILITY FUNCTION TESTS
// ============================================================================

pub fn pascal_case_simple_test() {
  type_mapping.pascal_case("user")
  |> should.equal("User")
}

pub fn pascal_case_snake_test() {
  type_mapping.pascal_case("user_role")
  |> should.equal("UserRole")
}

pub fn pascal_case_multiple_words_test() {
  type_mapping.pascal_case("order_line_item")
  |> should.equal("OrderLineItem")
}

pub fn pascal_case_empty_test() {
  type_mapping.pascal_case("")
  |> should.equal("")
}

pub fn snake_case_simple_test() {
  type_mapping.snake_case("User")
  |> should.equal("user")
}

pub fn snake_case_pascal_test() {
  type_mapping.snake_case("UserRole")
  |> should.equal("user_role")
}

pub fn snake_case_multiple_words_test() {
  type_mapping.snake_case("OrderLineItem")
  |> should.equal("order_line_item")
}

// ============================================================================
// TYPE CHECKING FUNCTION TESTS
// ============================================================================

pub fn is_nullable_type_true_test() {
  type_mapping.is_nullable_type(GleamOption(GleamString))
  |> should.be_true
}

pub fn is_nullable_type_false_test() {
  type_mapping.is_nullable_type(GleamString)
  |> should.be_false
}

pub fn is_list_type_true_test() {
  type_mapping.is_list_type(GleamList(GleamInt))
  |> should.be_true
}

pub fn is_list_type_false_test() {
  type_mapping.is_list_type(GleamInt)
  |> should.be_false
}

pub fn is_custom_type_direct_test() {
  type_mapping.is_custom_type(GleamCustom("schema/enums", "UserRole"))
  |> should.be_true
}

pub fn is_custom_type_in_option_test() {
  type_mapping.is_custom_type(
    GleamOption(GleamCustom("schema/enums", "UserRole")),
  )
  |> should.be_true
}

pub fn is_custom_type_in_list_test() {
  type_mapping.is_custom_type(
    GleamList(GleamCustom("schema/enums", "UserRole")),
  )
  |> should.be_true
}

pub fn is_custom_type_false_test() {
  type_mapping.is_custom_type(GleamString)
  |> should.be_false
}

pub fn unwrap_option_test() {
  type_mapping.unwrap_option(GleamOption(GleamString))
  |> should.equal(GleamString)
}

pub fn unwrap_option_not_option_test() {
  type_mapping.unwrap_option(GleamString)
  |> should.equal(GleamString)
}

pub fn unwrap_list_test() {
  type_mapping.unwrap_list(GleamList(GleamInt))
  |> should.equal(GleamInt)
}

pub fn unwrap_list_not_list_test() {
  type_mapping.unwrap_list(GleamInt)
  |> should.equal(GleamInt)
}

pub fn types_equal_same_test() {
  type_mapping.types_equal(GleamInt, GleamInt)
  |> should.be_true
}

pub fn types_equal_different_test() {
  type_mapping.types_equal(GleamInt, GleamString)
  |> should.be_false
}

pub fn types_equal_list_test() {
  type_mapping.types_equal(GleamList(GleamInt), GleamList(GleamInt))
  |> should.be_true
}

pub fn types_equal_list_different_element_test() {
  type_mapping.types_equal(GleamList(GleamInt), GleamList(GleamString))
  |> should.be_false
}

pub fn types_equal_option_test() {
  type_mapping.types_equal(GleamOption(GleamString), GleamOption(GleamString))
  |> should.be_true
}

pub fn types_equal_custom_test() {
  type_mapping.types_equal(
    GleamCustom("schema/enums", "UserRole"),
    GleamCustom("schema/enums", "UserRole"),
  )
  |> should.be_true
}

pub fn types_equal_custom_different_test() {
  type_mapping.types_equal(
    GleamCustom("schema/enums", "UserRole"),
    GleamCustom("schema/enums", "UserStatus"),
  )
  |> should.be_false
}

// ============================================================================
// UDT NAME FALLBACK TESTS
// ============================================================================

pub fn map_by_udt_name_int4_test() {
  // When data_type is not recognized, fall back to udt_name
  type_mapping.map_postgres_type("", "int4", False, [])
  |> should.be_ok
  |> should.equal(GleamInt)
}

pub fn map_by_udt_name_text_test() {
  type_mapping.map_postgres_type("", "text", False, [])
  |> should.be_ok
  |> should.equal(GleamString)
}

pub fn map_by_udt_name_array_test() {
  type_mapping.map_postgres_type("", "_int4", False, [])
  |> should.be_ok
  |> should.equal(GleamList(GleamInt))
}

pub fn map_by_udt_name_enum_test() {
  type_mapping.map_postgres_type("", "user_role", False, ["user_role"])
  |> should.be_ok
  |> should.equal(GleamCustom("schema/enums", "UserRole"))
}
