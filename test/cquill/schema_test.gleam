import cquill/schema.{
  DuplicateField, EmptySchema, FieldAdded, FieldRemoved, FieldTypeChanged, Index,
  InvalidConstraintColumn, InvalidPrimaryKey, InvalidTableName,
  PrimaryKeyChanged, TableCheck, UniqueConstraint,
}
import cquill/schema/field.{Integer}
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// SCHEMA CONSTRUCTOR TESTS
// ============================================================================

pub fn new_schema_test() {
  let s = schema.new("users")

  schema.get_source(s) |> should.equal("users")
  schema.get_fields(s) |> should.equal([])
  schema.get_primary_key(s) |> should.equal([])
  s.table_schema |> should.equal(None)
}

pub fn new_with_schema_test() {
  let s = schema.new_with_schema("users", "public")

  schema.get_source(s) |> should.equal("users")
  s.table_schema |> should.equal(Some("public"))
  schema.get_qualified_name(s) |> should.equal("public.users")
}

pub fn qualified_name_without_schema_test() {
  let s = schema.new("users")

  schema.get_qualified_name(s) |> should.equal("users")
}

// ============================================================================
// FIELD ADDITION TESTS
// ============================================================================

pub fn add_single_field_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))

  schema.field_count(s) |> should.equal(1)
  schema.has_field(s, "id") |> should.be_true
}

pub fn add_multiple_fields_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name") |> field.nullable)

  schema.field_count(s) |> should.equal(3)
  schema.has_field(s, "id") |> should.be_true
  schema.has_field(s, "email") |> should.be_true
  schema.has_field(s, "name") |> should.be_true
}

pub fn field_order_preserved_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name"))

  schema.field_names(s) |> should.equal(["id", "email", "name"])
}

pub fn get_field_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))

  case schema.get_field(s, "email") {
    Some(f) -> field.get_name(f) |> should.equal("email")
    None -> should.fail()
  }

  schema.get_field(s, "nonexistent") |> should.equal(None)
}

// ============================================================================
// PRIMARY KEY TESTS
// ============================================================================

pub fn single_primary_key_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.single_primary_key("id")

  schema.get_primary_key(s) |> should.equal(["id"])
  schema.has_primary_key(s) |> should.be_true
}

pub fn composite_primary_key_test() {
  let s =
    schema.new("user_roles")
    |> schema.field(field.integer("user_id"))
    |> schema.field(field.integer("role_id"))
    |> schema.primary_key(["user_id", "role_id"])

  schema.get_primary_key(s) |> should.equal(["user_id", "role_id"])
}

pub fn no_primary_key_test() {
  let s = schema.new("logs")

  schema.has_primary_key(s) |> should.be_false
}

pub fn primary_key_fields_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.single_primary_key("id")

  let pk_fields = schema.primary_key_fields(s)
  pk_fields |> list.length |> should.equal(1)
  case pk_fields {
    [f] -> field.get_name(f) |> should.equal("id")
    _ -> should.fail()
  }
}

pub fn non_primary_key_fields_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name"))
    |> schema.single_primary_key("id")

  let non_pk_fields = schema.non_primary_key_fields(s)
  non_pk_fields |> list.length |> should.equal(2)

  let names = list.map(non_pk_fields, field.get_name)
  names |> should.equal(["email", "name"])
}

// ============================================================================
// TABLE CONSTRAINT TESTS
// ============================================================================

pub fn unique_constraint_test() {
  let s =
    schema.new("users")
    |> schema.field(field.string("email"))
    |> schema.unique_constraint("users_email_unique", ["email"])

  s.table_constraints |> list.length |> should.equal(1)
  case s.table_constraints {
    [UniqueConstraint(name, cols)] -> {
      name |> should.equal("users_email_unique")
      cols |> should.equal(["email"])
    }
    _ -> should.fail()
  }
}

pub fn multi_column_unique_test() {
  let s =
    schema.new("user_roles")
    |> schema.field(field.integer("user_id"))
    |> schema.field(field.integer("role_id"))
    |> schema.unique_constraint("user_roles_unique", ["user_id", "role_id"])

  case s.table_constraints {
    [UniqueConstraint(_, cols)] -> cols |> should.equal(["user_id", "role_id"])
    _ -> should.fail()
  }
}

pub fn index_test() {
  let s =
    schema.new("users")
    |> schema.field(field.string("email"))
    |> schema.index("users_email_idx", ["email"], False)

  case s.table_constraints {
    [Index(name, cols, unique)] -> {
      name |> should.equal("users_email_idx")
      cols |> should.equal(["email"])
      unique |> should.be_false
    }
    _ -> should.fail()
  }
}

pub fn table_check_constraint_test() {
  let s =
    schema.new("products")
    |> schema.field(field.decimal("price", 10, 2))
    |> schema.field(field.decimal("sale_price", 10, 2))
    |> schema.table_check("price_check", "sale_price <= price")

  case s.table_constraints {
    [TableCheck(name, expr)] -> {
      name |> should.equal("price_check")
      expr |> should.equal("sale_price <= price")
    }
    _ -> should.fail()
  }
}

pub fn schema_comment_test() {
  let s =
    schema.new("users")
    |> schema.with_comment("Main user accounts table")

  s.comment |> should.equal(Some("Main user accounts table"))
}

pub fn in_schema_test() {
  let s =
    schema.new("users")
    |> schema.in_schema("auth")

  s.table_schema |> should.equal(Some("auth"))
  schema.get_qualified_name(s) |> should.equal("auth.users")
}

// ============================================================================
// FIELD FILTERING TESTS
// ============================================================================

pub fn nullable_fields_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.not_null)
    |> schema.field(field.string("email") |> field.not_null)
    |> schema.field(field.string("name") |> field.nullable)
    |> schema.field(field.string("bio") |> field.nullable)

  let nullable = schema.nullable_fields(s)
  nullable |> list.length |> should.equal(2)

  let names = list.map(nullable, field.get_name)
  list.contains(names, "name") |> should.be_true
  list.contains(names, "bio") |> should.be_true
}

pub fn required_fields_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.auto_increment)
    |> schema.field(field.string("email") |> field.not_null)
    |> schema.field(field.string("name") |> field.nullable)
    |> schema.field(
      field.boolean("active") |> field.default_value(dynamic.bool(True)),
    )

  // Only email is required (not null, no default)
  // id has auto_increment, name is nullable, active has default
  let required = schema.required_fields(s)
  required |> list.length |> should.equal(1)
  case required {
    [f] -> field.get_name(f) |> should.equal("email")
    _ -> should.fail()
  }
}

pub fn auto_increment_fields_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.auto_increment)
    |> schema.field(field.string("email"))

  let auto_inc = schema.auto_increment_fields(s)
  auto_inc |> list.length |> should.equal(1)
  case auto_inc {
    [f] -> field.get_name(f) |> should.equal("id")
    _ -> should.fail()
  }
}

pub fn foreign_key_fields_test() {
  let s =
    schema.new("posts")
    |> schema.field(field.integer("id"))
    |> schema.field(field.integer("user_id") |> field.references("users", "id"))
    |> schema.field(field.string("title"))

  let fk_fields = schema.foreign_key_fields(s)
  fk_fields |> list.length |> should.equal(1)
  case fk_fields {
    [f] -> field.get_name(f) |> should.equal("user_id")
    _ -> should.fail()
  }
}

// ============================================================================
// SCHEMA TRANSFORMATION TESTS
// ============================================================================

pub fn select_fields_test() {
  let full_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.field(field.string("password_hash"))
    |> schema.field(field.string("name"))

  let public_schema = schema.select_fields(full_schema, ["id", "email", "name"])

  schema.field_count(public_schema) |> should.equal(3)
  schema.has_field(public_schema, "password_hash") |> should.be_false
}

pub fn exclude_fields_test() {
  let full_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.field(field.string("password_hash"))
    |> schema.field(field.string("name"))

  let public_schema = schema.exclude_fields(full_schema, ["password_hash"])

  schema.field_count(public_schema) |> should.equal(3)
  schema.has_field(public_schema, "password_hash") |> should.be_false
  schema.has_field(public_schema, "email") |> should.be_true
}

pub fn insertable_fields_test() {
  let full_schema =
    schema.new("users")
    |> schema.field(field.integer("id") |> field.auto_increment)
    |> schema.field(field.string("email"))
    |> schema.field(field.string("name"))

  let insert_schema = schema.insertable_fields(full_schema)

  schema.field_count(insert_schema) |> should.equal(2)
  schema.has_field(insert_schema, "id") |> should.be_false
  schema.has_field(insert_schema, "email") |> should.be_true
}

pub fn rename_field_test() {
  let s =
    schema.new("users")
    |> schema.field(field.string("email"))
    |> schema.rename_field("email", "email_address")

  schema.has_field(s, "email") |> should.be_false
  schema.has_field(s, "email_address") |> should.be_true
}

// ============================================================================
// VALIDATION TESTS
// ============================================================================

pub fn valid_schema_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))
    |> schema.single_primary_key("id")

  schema.is_valid(s) |> should.be_true
  schema.validate(s) |> should.equal([])
}

pub fn empty_schema_invalid_test() {
  let s = schema.new("users")

  schema.is_valid(s) |> should.be_false
  let errors = schema.validate(s)
  list.contains(errors, EmptySchema) |> should.be_true
}

pub fn empty_table_name_invalid_test() {
  let s =
    schema.new("")
    |> schema.field(field.integer("id"))

  schema.is_valid(s) |> should.be_false
  let errors = schema.validate(s)
  list.contains(errors, InvalidTableName("")) |> should.be_true
}

pub fn invalid_primary_key_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.single_primary_key("nonexistent")

  schema.is_valid(s) |> should.be_false
  let errors = schema.validate(s)
  list.contains(errors, InvalidPrimaryKey("nonexistent")) |> should.be_true
}

pub fn duplicate_field_invalid_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("id"))
  // Duplicate!

  schema.is_valid(s) |> should.be_false
  let errors = schema.validate(s)
  list.contains(errors, DuplicateField("id")) |> should.be_true
}

pub fn invalid_constraint_column_test() {
  let s =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.unique_constraint("bad_constraint", ["nonexistent"])

  schema.is_valid(s) |> should.be_false
  let errors = schema.validate(s)
  list.contains(
    errors,
    InvalidConstraintColumn("bad_constraint", "nonexistent"),
  )
  |> should.be_true
}

// ============================================================================
// SCHEMA DIFF TESTS
// ============================================================================

pub fn diff_no_changes_test() {
  let s1 =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))

  let s2 =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))

  schema.diff(s1, s2) |> should.equal([])
}

pub fn diff_field_added_test() {
  let old_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))

  let new_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))

  let diffs = schema.diff(old_schema, new_schema)
  diffs |> list.length |> should.equal(1)

  case diffs {
    [FieldAdded(f)] -> field.get_name(f) |> should.equal("email")
    _ -> should.fail()
  }
}

pub fn diff_field_removed_test() {
  let old_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.string("email"))

  let new_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))

  let diffs = schema.diff(old_schema, new_schema)
  diffs |> list.length |> should.equal(1)

  case diffs {
    [FieldRemoved(name)] -> name |> should.equal("email")
    _ -> should.fail()
  }
}

pub fn diff_field_type_changed_test() {
  let old_schema =
    schema.new("users")
    |> schema.field(field.integer("count"))

  let new_schema =
    schema.new("users")
    |> schema.field(field.big_integer("count"))

  let diffs = schema.diff(old_schema, new_schema)
  diffs |> list.length |> should.equal(1)

  case diffs {
    [FieldTypeChanged(name, old_type, new_type)] -> {
      name |> should.equal("count")
      old_type |> should.equal(Integer)
      new_type |> should.equal(field.BigInteger)
    }
    _ -> should.fail()
  }
}

pub fn diff_primary_key_changed_test() {
  let old_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.single_primary_key("id")

  let new_schema =
    schema.new("users")
    |> schema.field(field.integer("id"))
    |> schema.field(field.integer("org_id"))
    |> schema.primary_key(["id", "org_id"])

  let diffs = schema.diff(old_schema, new_schema)

  let pk_changed =
    list.find(diffs, fn(d) {
      case d {
        PrimaryKeyChanged(_, _) -> True
        _ -> False
      }
    })

  case pk_changed {
    Ok(PrimaryKeyChanged(old_pk, new_pk)) -> {
      old_pk |> should.equal(["id"])
      new_pk |> should.equal(["id", "org_id"])
    }
    _ -> should.fail()
  }
}

// ============================================================================
// MULTIPLE SCHEMAS PER DOMAIN PATTERN TEST
// ============================================================================

pub fn multiple_schemas_pattern_test() {
  // Full schema for reads
  let user_full =
    schema.new("users")
    |> schema.field(
      field.integer("id") |> field.primary_key |> field.auto_increment,
    )
    |> schema.field(field.string("email") |> field.not_null |> field.unique)
    |> schema.field(field.string("password_hash") |> field.not_null)
    |> schema.field(field.string("name") |> field.nullable)
    |> schema.field(
      field.datetime("inserted_at") |> field.default_function("now()"),
    )
    |> schema.single_primary_key("id")

  // Insert schema (no auto-generated fields)
  let user_insert = schema.insertable_fields(user_full)

  // Public schema (no sensitive fields)
  let user_public = schema.exclude_fields(user_full, ["password_hash"])

  // Verify full schema
  schema.field_count(user_full) |> should.equal(5)

  // Verify insert schema has no id (auto-increment)
  schema.has_field(user_insert, "id") |> should.be_false
  schema.field_count(user_insert) |> should.equal(4)

  // Verify public schema has no password_hash
  schema.has_field(user_public, "password_hash") |> should.be_false
  schema.field_count(user_public) |> should.equal(4)
}
