// Generator Tests
//
// Comprehensive tests for the code generation module that generates
// Gleam source files from introspected schema metadata.

import cquill/codegen/generator.{
  GeneratedModule, GeneratorConfig, default_config, generate_all,
  generate_enum_module, generate_index_module, generate_schema_module,
  generate_typed_index_module, generate_typed_module, module_file_path,
  pascal_case, snake_case, with_module_prefix, with_timestamp_fields,
  with_typed_columns,
}
import cquill/introspection.{
  type IntrospectedEnum, type IntrospectedTable, IntrospectedColumn,
  IntrospectedEnum, IntrospectedTable, IntrospectedUnique,
}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// TEST FIXTURES
// ============================================================================

/// Create a simple user table for testing
fn make_user_table() -> IntrospectedTable {
  IntrospectedTable(
    name: "users",
    columns: [
      IntrospectedColumn(
        name: "id",
        position: 1,
        data_type: "serial",
        udt_name: "serial",
        is_nullable: False,
        default: Some("nextval('users_id_seq'::regclass)"),
        max_length: None,
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "email",
        position: 2,
        data_type: "character varying",
        udt_name: "varchar",
        is_nullable: False,
        default: None,
        max_length: Some(255),
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "name",
        position: 3,
        data_type: "character varying",
        udt_name: "varchar",
        is_nullable: True,
        default: None,
        max_length: Some(100),
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "inserted_at",
        position: 4,
        data_type: "timestamp without time zone",
        udt_name: "timestamp",
        is_nullable: False,
        default: Some("now()"),
        max_length: None,
        numeric_precision: None,
        numeric_scale: None,
      ),
    ],
    primary_key: ["id"],
    foreign_keys: [],
    unique_constraints: [
      IntrospectedUnique(constraint_name: "users_email_key", columns: ["email"]),
    ],
    check_constraints: [],
  )
}

/// Create a posts table with foreign key for testing
fn make_posts_table() -> IntrospectedTable {
  IntrospectedTable(
    name: "posts",
    columns: [
      IntrospectedColumn(
        name: "id",
        position: 1,
        data_type: "serial",
        udt_name: "serial",
        is_nullable: False,
        default: Some("nextval('posts_id_seq'::regclass)"),
        max_length: None,
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "user_id",
        position: 2,
        data_type: "integer",
        udt_name: "int4",
        is_nullable: False,
        default: None,
        max_length: None,
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "title",
        position: 3,
        data_type: "character varying",
        udt_name: "varchar",
        is_nullable: False,
        default: None,
        max_length: Some(200),
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "body",
        position: 4,
        data_type: "text",
        udt_name: "text",
        is_nullable: False,
        default: None,
        max_length: None,
        numeric_precision: None,
        numeric_scale: None,
      ),
      IntrospectedColumn(
        name: "status",
        position: 5,
        data_type: "user-defined",
        udt_name: "post_status",
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
}

/// Create a test enum
fn make_post_status_enum() -> IntrospectedEnum {
  IntrospectedEnum(name: "post_status", values: [
    "draft",
    "published",
    "archived",
  ])
}

/// Create a test enum with underscores
fn make_user_role_enum() -> IntrospectedEnum {
  IntrospectedEnum(name: "user_role", values: ["admin", "moderator", "member"])
}

// ============================================================================
// CONFIGURATION TESTS
// ============================================================================

pub fn default_config_has_expected_values_test() {
  let config = default_config()

  config.module_prefix
  |> should.equal("db")

  config.include_timestamps
  |> should.be_true

  config.generate_decoders
  |> should.be_true

  config.generate_encoders
  |> should.be_true

  config.timestamp_fields
  |> should.equal(["inserted_at", "updated_at", "created_at", "modified_at"])
}

pub fn with_module_prefix_updates_prefix_test() {
  let config =
    default_config()
    |> with_module_prefix("custom_db")

  config.module_prefix
  |> should.equal("custom_db")
}

pub fn with_timestamp_fields_updates_fields_test() {
  let config =
    default_config()
    |> with_timestamp_fields(["created", "modified"])

  config.timestamp_fields
  |> should.equal(["created", "modified"])
}

// ============================================================================
// CASE CONVERSION TESTS
// ============================================================================

pub fn pascal_case_converts_snake_case_test() {
  pascal_case("user_account")
  |> should.equal("UserAccount")
}

pub fn pascal_case_handles_single_word_test() {
  pascal_case("users")
  |> should.equal("Users")
}

pub fn pascal_case_handles_multiple_underscores_test() {
  pascal_case("user_role_permission")
  |> should.equal("UserRolePermission")
}

pub fn snake_case_converts_pascal_case_test() {
  snake_case("UserAccount")
  |> should.equal("user_account")
}

pub fn snake_case_handles_single_word_test() {
  snake_case("Users")
  |> should.equal("users")
}

pub fn snake_case_handles_consecutive_capitals_test() {
  snake_case("HTTPRequest")
  |> should.equal("h_t_t_p_request")
}

pub fn snake_case_handles_already_lowercase_test() {
  snake_case("users")
  |> should.equal("users")
}

// ============================================================================
// SCHEMA MODULE GENERATION TESTS
// ============================================================================

pub fn generate_schema_module_creates_correct_path_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  module.path
  |> should.equal("db/schema/users")

  module.filename
  |> should.equal("users.gleam")
}

pub fn generate_schema_module_includes_header_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  module.content
  |> string.contains("// users.gleam (GENERATED)")
  |> should.be_true

  module.content
  |> string.contains("DO NOT EDIT")
  |> should.be_true
}

pub fn generate_schema_module_generates_record_type_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Check for main type definition
  module.content
  |> string.contains("pub type Users {")
  |> should.be_true

  module.content
  |> string.contains("Users(")
  |> should.be_true

  // Check for field types
  module.content
  |> string.contains("id: Int,")
  |> should.be_true

  module.content
  |> string.contains("email: String,")
  |> should.be_true

  // Nullable field should be Option
  module.content
  |> string.contains("name: Option(String),")
  |> should.be_true
}

pub fn generate_schema_module_generates_new_type_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Check for NewUsers type
  module.content
  |> string.contains("pub type NewUsers {")
  |> should.be_true

  module.content
  |> string.contains("NewUsers(")
  |> should.be_true

  // NewUsers should NOT have id (auto-generated) or inserted_at (timestamp)
  // It should have email and name
  module.content
  |> string.contains("For INSERT operations")
  |> should.be_true
}

pub fn generate_schema_module_generates_changes_type_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Check for UsersChanges type
  module.content
  |> string.contains("pub type UsersChanges {")
  |> should.be_true

  module.content
  |> string.contains("For UPDATE operations")
  |> should.be_true
}

pub fn generate_schema_module_generates_schema_const_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Check for schema constant
  module.content
  |> string.contains("pub const users_schema = Schema(")
  |> should.be_true

  module.content
  |> string.contains("source: \"users\"")
  |> should.be_true

  module.content
  |> string.contains("primary_key: [\"id\"]")
  |> should.be_true
}

pub fn generate_schema_module_generates_decoder_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Check for decoder function
  module.content
  |> string.contains("pub fn users_decoder()")
  |> should.be_true

  module.content
  |> string.contains("decoder.into")
  |> should.be_true

  module.content
  |> string.contains("decoder.field")
  |> should.be_true
}

pub fn generate_schema_module_generates_encoder_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Check for encoder function
  module.content
  |> string.contains("pub fn new_users_encoder(")
  |> should.be_true

  module.content
  |> string.contains(": NewUsers)")
  |> should.be_true
}

pub fn generate_schema_module_handles_custom_prefix_test() {
  let table = make_user_table()
  let config =
    default_config()
    |> with_module_prefix("myapp/database")

  let module = generate_schema_module(table, [], config)

  module.path
  |> should.equal("myapp/database/schema/users")
}

pub fn generate_schema_module_respects_generate_decoders_false_test() {
  let table = make_user_table()
  let config =
    GeneratorConfig(
      module_prefix: "db",
      include_timestamps: True,
      timestamp_fields: ["inserted_at", "updated_at"],
      generate_decoders: False,
      generate_encoders: True,
      generate_typed: True,
    )

  let module = generate_schema_module(table, [], config)

  module.content
  |> string.contains("users_decoder()")
  |> should.be_false
}

pub fn generate_schema_module_respects_generate_encoders_false_test() {
  let table = make_user_table()
  let config =
    GeneratorConfig(
      module_prefix: "db",
      include_timestamps: True,
      timestamp_fields: ["inserted_at", "updated_at"],
      generate_decoders: True,
      generate_encoders: False,
      generate_typed: True,
    )

  let module = generate_schema_module(table, [], config)

  module.content
  |> string.contains("new_users_encoder(")
  |> should.be_false
}

pub fn generate_schema_module_handles_enum_columns_test() {
  let table = make_posts_table()
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_schema_module(table, enums, config)

  // Enum column should use custom type name
  module.content
  |> string.contains("status: PostStatus,")
  |> should.be_true
}

// ============================================================================
// ENUM MODULE GENERATION TESTS
// ============================================================================

pub fn generate_enum_module_creates_correct_path_test() {
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  module.path
  |> should.equal("db/enums")

  module.filename
  |> should.equal("enums.gleam")
}

pub fn generate_enum_module_includes_header_test() {
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  module.content
  |> string.contains("// enums.gleam (GENERATED)")
  |> should.be_true

  module.content
  |> string.contains("DO NOT EDIT")
  |> should.be_true
}

pub fn generate_enum_module_generates_type_definition_test() {
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  module.content
  |> string.contains("pub type PostStatus {")
  |> should.be_true

  // Check for variant names (PascalCase)
  module.content
  |> string.contains("Draft")
  |> should.be_true

  module.content
  |> string.contains("Published")
  |> should.be_true

  module.content
  |> string.contains("Archived")
  |> should.be_true
}

pub fn generate_enum_module_generates_decoder_test() {
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  module.content
  |> string.contains("pub fn post_status_decoder()")
  |> should.be_true

  // Check pattern matching cases
  module.content
  |> string.contains("\"draft\" -> Ok(Draft)")
  |> should.be_true

  module.content
  |> string.contains("\"published\" -> Ok(Published)")
  |> should.be_true
}

pub fn generate_enum_module_generates_encoder_test() {
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  module.content
  |> string.contains("pub fn post_status_encoder(value: PostStatus) -> String")
  |> should.be_true

  module.content
  |> string.contains("Draft -> \"draft\"")
  |> should.be_true
}

pub fn generate_enum_module_generates_to_string_test() {
  let enums = [make_post_status_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  module.content
  |> string.contains(
    "pub fn post_status_to_string(value: PostStatus) -> String",
  )
  |> should.be_true
}

pub fn generate_enum_module_handles_multiple_enums_test() {
  let enums = [make_post_status_enum(), make_user_role_enum()]
  let config = default_config()

  let module = generate_enum_module(enums, config)

  // Both enum types should be present
  module.content
  |> string.contains("pub type PostStatus {")
  |> should.be_true

  module.content
  |> string.contains("pub type UserRole {")
  |> should.be_true

  // Both decoders should be present
  module.content
  |> string.contains("pub fn post_status_decoder()")
  |> should.be_true

  module.content
  |> string.contains("pub fn user_role_decoder()")
  |> should.be_true
}

// ============================================================================
// INDEX MODULE GENERATION TESTS
// ============================================================================

pub fn generate_index_module_creates_correct_path_test() {
  let tables = [make_user_table()]
  let enums = []
  let config = default_config()

  let module = generate_index_module(tables, enums, config)

  module.path
  |> should.equal("db/schema")

  module.filename
  |> should.equal("schema.gleam")
}

pub fn generate_index_module_includes_header_test() {
  let tables = [make_user_table()]
  let enums = []
  let config = default_config()

  let module = generate_index_module(tables, enums, config)

  module.content
  |> string.contains("// schema.gleam (GENERATED)")
  |> should.be_true
}

pub fn generate_index_module_imports_table_types_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = []
  let config = default_config()

  let module = generate_index_module(tables, enums, config)

  // Check for imports of both tables
  module.content
  |> string.contains(
    "import db/schema/users.{Users, NewUsers, UsersChanges, users_schema}",
  )
  |> should.be_true

  module.content
  |> string.contains(
    "import db/schema/posts.{Posts, NewPosts, PostsChanges, posts_schema}",
  )
  |> should.be_true
}

pub fn generate_index_module_imports_enums_test() {
  let tables = [make_user_table()]
  let enums = [make_post_status_enum(), make_user_role_enum()]
  let config = default_config()

  let module = generate_index_module(tables, enums, config)

  module.content
  |> string.contains("import db/enums.{PostStatus, UserRole}")
  |> should.be_true
}

pub fn generate_index_module_no_enum_import_when_empty_test() {
  let tables = [make_user_table()]
  let enums = []
  let config = default_config()

  let module = generate_index_module(tables, enums, config)

  module.content
  |> string.contains("import db/enums")
  |> should.be_false
}

// ============================================================================
// BATCH GENERATION TESTS
// ============================================================================

pub fn generate_all_creates_all_modules_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = [make_post_status_enum()]
  let config =
    default_config()
    |> with_typed_columns(False)

  let modules = generate_all(tables, enums, config)

  // Should have: 2 table modules + 1 enum module + 1 index module = 4
  list.length(modules)
  |> should.equal(4)
}

pub fn generate_all_includes_table_modules_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = []
  let config = default_config()

  let modules = generate_all(tables, enums, config)
  let paths = list.map(modules, fn(m) { m.path })

  paths
  |> list.contains("db/schema/users")
  |> should.be_true

  paths
  |> list.contains("db/schema/posts")
  |> should.be_true
}

pub fn generate_all_includes_enum_module_test() {
  let tables = [make_user_table()]
  let enums = [make_post_status_enum()]
  let config = default_config()

  let modules = generate_all(tables, enums, config)
  let paths = list.map(modules, fn(m) { m.path })

  paths
  |> list.contains("db/enums")
  |> should.be_true
}

pub fn generate_all_includes_index_module_test() {
  let tables = [make_user_table()]
  let enums = []
  let config = default_config()

  let modules = generate_all(tables, enums, config)
  let paths = list.map(modules, fn(m) { m.path })

  paths
  |> list.contains("db/schema")
  |> should.be_true
}

pub fn generate_all_skips_enum_module_when_no_enums_test() {
  let tables = [make_user_table()]
  let enums = []
  let config =
    default_config()
    |> with_typed_columns(False)

  let modules = generate_all(tables, enums, config)
  let paths = list.map(modules, fn(m) { m.path })

  paths
  |> list.contains("db/enums")
  |> should.be_false

  // Should have: 1 table module + 1 index module = 2
  list.length(modules)
  |> should.equal(2)
}

// ============================================================================
// FILE PATH TESTS
// ============================================================================

pub fn module_file_path_generates_correct_path_test() {
  let module =
    GeneratedModule(
      path: "db/schema/users",
      filename: "users.gleam",
      content: "",
    )

  module_file_path(module)
  |> should.equal("src/db/schema/users.gleam")
}

// ============================================================================
// IMPORT GENERATION TESTS
// ============================================================================

pub fn generate_schema_module_imports_option_for_nullable_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  module.content
  |> string.contains("import gleam/option.{type Option}")
  |> should.be_true
}

pub fn generate_schema_module_imports_schema_type_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  module.content
  |> string.contains("import cquill/schema.{type Schema}")
  |> should.be_true
}

// ============================================================================
// CONSTRAINT GENERATION TESTS
// ============================================================================

pub fn generate_schema_module_includes_primary_key_constraint_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // The id field should have "primary_key" constraint
  module.content
  |> string.contains("\"primary_key\"")
  |> should.be_true
}

pub fn generate_schema_module_includes_not_null_constraint_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Non-nullable fields should have "not_null" constraint
  module.content
  |> string.contains("\"not_null\"")
  |> should.be_true
}

pub fn generate_schema_module_includes_unique_constraint_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Email has a unique constraint
  module.content
  |> string.contains("\"unique\"")
  |> should.be_true
}

// ============================================================================
// DECODER FIELD INDEX TESTS
// ============================================================================

pub fn generate_schema_module_decoder_uses_correct_indices_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Fields should be indexed 0, 1, 2, 3...
  module.content
  |> string.contains("decoder.field(0,")
  |> should.be_true

  module.content
  |> string.contains("decoder.field(1,")
  |> should.be_true

  module.content
  |> string.contains("decoder.field(2,")
  |> should.be_true
}

// ============================================================================
// CHANGES TYPE OPTION WRAPPING TESTS
// ============================================================================

pub fn changes_type_wraps_non_nullable_in_option_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // In UsersChanges, email (String) should become Option(String)
  // The content includes "email: Option(String)," in the changes type
  module.content
  |> string.contains("UsersChanges")
  |> should.be_true
}

pub fn changes_type_double_wraps_nullable_fields_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // In UsersChanges, name (Option(String)) should become Option(Option(String))
  module.content
  |> string.contains("Option(Option(String))")
  |> should.be_true
}

// ============================================================================
// EDGE CASE TESTS
// ============================================================================

pub fn generate_schema_module_handles_table_with_no_primary_key_test() {
  let table =
    IntrospectedTable(
      name: "logs",
      columns: [
        IntrospectedColumn(
          name: "message",
          position: 1,
          data_type: "text",
          udt_name: "text",
          is_nullable: False,
          default: None,
          max_length: None,
          numeric_precision: None,
          numeric_scale: None,
        ),
      ],
      primary_key: [],
      foreign_keys: [],
      unique_constraints: [],
      check_constraints: [],
    )
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Should still generate with empty primary_key array
  module.content
  |> string.contains("primary_key: []")
  |> should.be_true
}

pub fn generate_schema_module_handles_all_nullable_columns_test() {
  let table =
    IntrospectedTable(
      name: "metadata",
      columns: [
        IntrospectedColumn(
          name: "key",
          position: 1,
          data_type: "text",
          udt_name: "text",
          is_nullable: True,
          default: None,
          max_length: None,
          numeric_precision: None,
          numeric_scale: None,
        ),
        IntrospectedColumn(
          name: "value",
          position: 2,
          data_type: "text",
          udt_name: "text",
          is_nullable: True,
          default: None,
          max_length: None,
          numeric_precision: None,
          numeric_scale: None,
        ),
      ],
      primary_key: [],
      foreign_keys: [],
      unique_constraints: [],
      check_constraints: [],
    )
  let config = default_config()

  let module = generate_schema_module(table, [], config)

  // Both fields should be Option types
  module.content
  |> string.contains("key: Option(String),")
  |> should.be_true

  module.content
  |> string.contains("value: Option(String),")
  |> should.be_true
}

// ============================================================================
// TYPED COLUMN GENERATION TESTS
// ============================================================================

pub fn generate_typed_module_creates_correct_path_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  module.path
  |> should.equal("db/typed/users")

  module.filename
  |> should.equal("users.gleam")
}

pub fn generate_typed_module_includes_header_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  module.content
  |> string.contains("// users.gleam (GENERATED)")
  |> should.be_true

  module.content
  |> string.contains("DO NOT EDIT")
  |> should.be_true

  module.content
  |> string.contains("Typed columns for users table")
  |> should.be_true
}

pub fn generate_typed_module_imports_table_and_column_types_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  module.content
  |> string.contains(
    "import cquill/typed/table.{type Table, type Column, table, column}",
  )
  |> should.be_true
}

pub fn generate_typed_module_imports_option_for_nullable_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  // name column is nullable, so Option should be imported
  module.content
  |> string.contains("import gleam/option.{type Option}")
  |> should.be_true
}

pub fn generate_typed_module_generates_phantom_type_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  module.content
  |> string.contains("pub type UsersTable")
  |> should.be_true

  module.content
  |> string.contains("/// Phantom type for UsersTable table")
  |> should.be_true
}

pub fn generate_typed_module_generates_table_function_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  module.content
  |> string.contains("pub fn users() -> Table(UsersTable)")
  |> should.be_true

  module.content
  |> string.contains("table(\"users\")")
  |> should.be_true
}

pub fn generate_typed_module_generates_column_functions_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  // id column - Int type
  module.content
  |> string.contains("pub fn id() -> Column(UsersTable, Int)")
  |> should.be_true

  module.content
  |> string.contains("column(\"id\")")
  |> should.be_true

  // email column - String type
  module.content
  |> string.contains("pub fn email() -> Column(UsersTable, String)")
  |> should.be_true

  module.content
  |> string.contains("column(\"email\")")
  |> should.be_true

  // name column - Option(String) type (nullable)
  module.content
  |> string.contains("pub fn name() -> Column(UsersTable, Option(String))")
  |> should.be_true

  module.content
  |> string.contains("column(\"name\")")
  |> should.be_true
}

pub fn generate_typed_module_includes_column_comments_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  module.content
  |> string.contains("/// Column: id")
  |> should.be_true

  module.content
  |> string.contains("/// Column: email")
  |> should.be_true
}

pub fn generate_typed_module_respects_module_prefix_test() {
  let table = make_user_table()
  let config =
    default_config()
    |> with_module_prefix("myapp/database")

  let module = generate_typed_module(table, [], config)

  module.path
  |> should.equal("myapp/database/typed/users")
}

pub fn generate_typed_module_handles_timestamp_columns_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  // inserted_at column should be Time type
  module.content
  |> string.contains("pub fn inserted_at() -> Column(UsersTable, Time)")
  |> should.be_true
}

pub fn generate_typed_module_imports_birl_for_time_test() {
  let table = make_user_table()
  let config = default_config()

  let module = generate_typed_module(table, [], config)

  // inserted_at is a timestamp, so birl should be imported
  module.content
  |> string.contains("import birl.{type Time}")
  |> should.be_true
}

// ============================================================================
// TYPED INDEX MODULE TESTS
// ============================================================================

pub fn generate_typed_index_module_creates_correct_path_test() {
  let tables = [make_user_table()]
  let config = default_config()

  let module = generate_typed_index_module(tables, config)

  module.path
  |> should.equal("db/typed")

  module.filename
  |> should.equal("typed.gleam")
}

pub fn generate_typed_index_module_includes_header_test() {
  let tables = [make_user_table()]
  let config = default_config()

  let module = generate_typed_index_module(tables, config)

  module.content
  |> string.contains("// typed.gleam (GENERATED)")
  |> should.be_true

  module.content
  |> string.contains("DO NOT EDIT")
  |> should.be_true
}

pub fn generate_typed_index_module_imports_typed_table_modules_test() {
  let tables = [make_user_table(), make_posts_table()]
  let config = default_config()

  let module = generate_typed_index_module(tables, config)

  module.content
  |> string.contains("import db/typed/users")
  |> should.be_true

  module.content
  |> string.contains("import db/typed/posts")
  |> should.be_true
}

// ============================================================================
// TYPED GENERATION CONFIG TESTS
// ============================================================================

pub fn with_typed_columns_enables_typed_generation_test() {
  let config =
    default_config()
    |> with_typed_columns(True)

  config.generate_typed
  |> should.be_true
}

pub fn with_typed_columns_disables_typed_generation_test() {
  let config =
    default_config()
    |> with_typed_columns(False)

  config.generate_typed
  |> should.be_false
}

pub fn default_config_enables_typed_generation_test() {
  let config = default_config()

  config.generate_typed
  |> should.be_true
}

// ============================================================================
// GENERATE ALL WITH TYPED TESTS
// ============================================================================

pub fn generate_all_includes_typed_modules_by_default_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = []
  let config = default_config()

  let modules = generate_all(tables, enums, config)
  let paths = list.map(modules, fn(m) { m.path })

  // Should include typed modules
  paths
  |> list.contains("db/typed/users")
  |> should.be_true

  paths
  |> list.contains("db/typed/posts")
  |> should.be_true

  paths
  |> list.contains("db/typed")
  |> should.be_true
}

pub fn generate_all_with_typed_creates_correct_count_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = []
  let config = default_config()

  let modules = generate_all(tables, enums, config)

  // Should have: 2 table modules + 1 index module + 2 typed table modules + 1 typed index = 6
  list.length(modules)
  |> should.equal(6)
}

pub fn generate_all_skips_typed_modules_when_disabled_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = []
  let config =
    default_config()
    |> with_typed_columns(False)

  let modules = generate_all(tables, enums, config)
  let paths = list.map(modules, fn(m) { m.path })

  // Should NOT include typed modules
  paths
  |> list.contains("db/typed/users")
  |> should.be_false

  paths
  |> list.contains("db/typed/posts")
  |> should.be_false

  paths
  |> list.contains("db/typed")
  |> should.be_false

  // Should have: 2 table modules + 1 index module = 3
  list.length(modules)
  |> should.equal(3)
}

pub fn generate_all_with_enums_and_typed_creates_all_modules_test() {
  let tables = [make_user_table(), make_posts_table()]
  let enums = [make_post_status_enum()]
  let config = default_config()

  let modules = generate_all(tables, enums, config)

  // Should have: 2 table modules + 1 enum module + 1 index module + 2 typed table modules + 1 typed index = 7
  list.length(modules)
  |> should.equal(7)
}
