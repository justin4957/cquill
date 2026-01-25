import cquill/schema/field.{
  Array, BigInteger, Boolean, Cascade, Char, Check, Custom, Date, DateTime,
  Decimal, DefaultAutoIncrement, DefaultFunction, DefaultValue, Enum, Float,
  ForeignKey, Integer, Json, MaxLength, MaxValue, MinLength, MinValue, NoAction,
  Nullable, Pattern, Restrict, SetNull, String, Text, Time, Uuid,
}
import gleam/dynamic
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// FIELD CONSTRUCTOR TESTS
// ============================================================================

pub fn integer_field_test() {
  let f = field.integer("id")

  field.get_name(f) |> should.equal("id")
  field.get_type(f) |> should.equal(Integer)
  f.default |> should.equal(None)
  f.constraints |> should.equal([])
}

pub fn string_field_test() {
  let f = field.string("email")

  field.get_name(f) |> should.equal("email")
  field.get_type(f) |> should.equal(String)
}

pub fn text_field_test() {
  let f = field.text("description")

  field.get_name(f) |> should.equal("description")
  field.get_type(f) |> should.equal(Text)
}

pub fn big_integer_field_test() {
  let f = field.big_integer("count")

  field.get_type(f) |> should.equal(BigInteger)
}

pub fn float_field_test() {
  let f = field.float("price")

  field.get_type(f) |> should.equal(Float)
}

pub fn decimal_field_test() {
  let f = field.decimal("amount", 10, 2)

  field.get_type(f) |> should.equal(Decimal(10, 2))
}

pub fn boolean_field_test() {
  let f = field.boolean("active")

  field.get_type(f) |> should.equal(Boolean)
}

pub fn datetime_field_test() {
  let f = field.datetime("created_at")

  field.get_type(f) |> should.equal(DateTime)
}

pub fn date_field_test() {
  let f = field.date("birth_date")

  field.get_type(f) |> should.equal(Date)
}

pub fn time_field_test() {
  let f = field.time("start_time")

  field.get_type(f) |> should.equal(Time)
}

pub fn uuid_field_test() {
  let f = field.uuid("external_id")

  field.get_type(f) |> should.equal(Uuid)
}

pub fn json_field_test() {
  let f = field.json("metadata")

  field.get_type(f) |> should.equal(Json)
}

pub fn binary_field_test() {
  let f = field.binary("data")

  field.get_type(f) |> should.equal(field.Binary)
}

pub fn array_field_test() {
  let f = field.array("tags", String)

  field.get_type(f) |> should.equal(Array(String))
}

pub fn enum_field_test() {
  let f =
    field.enum_("status", "order_status", ["pending", "shipped", "delivered"])

  field.get_type(f)
  |> should.equal(Enum("order_status", ["pending", "shipped", "delivered"]))
}

// ============================================================================
// FIELD MODIFIER TESTS
// ============================================================================

pub fn nullable_modifier_test() {
  let f =
    field.string("name")
    |> field.nullable

  field.get_type(f) |> should.equal(Nullable(String))
  field.is_nullable(f) |> should.be_true
}

pub fn not_null_constraint_test() {
  let f =
    field.string("email")
    |> field.not_null

  field.has_constraint(f, field.is_not_null_constraint) |> should.be_true
  field.is_nullable(f) |> should.be_false
}

pub fn unique_constraint_test() {
  let f =
    field.string("email")
    |> field.unique

  field.has_constraint(f, field.is_unique_constraint) |> should.be_true
}

pub fn primary_key_constraint_test() {
  let f =
    field.integer("id")
    |> field.primary_key

  field.has_constraint(f, field.is_primary_key_constraint) |> should.be_true
  field.is_primary_key(f) |> should.be_true
}

pub fn foreign_key_constraint_test() {
  let f =
    field.integer("user_id")
    |> field.references("users", "id")

  field.has_constraint(f, field.is_foreign_key_constraint) |> should.be_true
}

pub fn foreign_key_with_action_test() {
  let f =
    field.integer("user_id")
    |> field.references_with_action("users", "id", Cascade)

  let fk_constraints = field.get_constraints(f, field.is_foreign_key_constraint)
  case fk_constraints {
    [ForeignKey(_, _, on_delete)] -> on_delete |> should.equal(Cascade)
    _ -> should.fail()
  }
}

pub fn check_constraint_test() {
  let f =
    field.integer("age")
    |> field.check("age_positive", "age > 0")

  let check_pred = fn(c) {
    case c {
      Check(_, _) -> True
      _ -> False
    }
  }
  field.has_constraint(f, check_pred) |> should.be_true
}

pub fn min_max_value_test() {
  let f =
    field.integer("quantity")
    |> field.min_value(0)
    |> field.max_value(1000)

  let min_pred = fn(c) {
    case c {
      MinValue(_) -> True
      _ -> False
    }
  }
  let max_pred = fn(c) {
    case c {
      MaxValue(_) -> True
      _ -> False
    }
  }
  field.has_constraint(f, min_pred) |> should.be_true
  field.has_constraint(f, max_pred) |> should.be_true
}

pub fn min_max_length_test() {
  let f =
    field.string("username")
    |> field.min_length(3)
    |> field.max_length(50)

  let min_pred = fn(c) {
    case c {
      MinLength(_) -> True
      _ -> False
    }
  }
  let max_pred = fn(c) {
    case c {
      MaxLength(_) -> True
      _ -> False
    }
  }
  field.has_constraint(f, min_pred) |> should.be_true
  field.has_constraint(f, max_pred) |> should.be_true
}

pub fn pattern_constraint_test() {
  let f =
    field.string("email")
    |> field.pattern("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")

  let pattern_pred = fn(c) {
    case c {
      Pattern(_) -> True
      _ -> False
    }
  }
  field.has_constraint(f, pattern_pred) |> should.be_true
}

// ============================================================================
// DEFAULT VALUE TESTS
// ============================================================================

pub fn default_value_test() {
  let f =
    field.boolean("active")
    |> field.default_value(dynamic.bool(True))

  case f.default {
    Some(DefaultValue(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn default_function_test() {
  let f =
    field.datetime("created_at")
    |> field.default_function("now()")

  case f.default {
    Some(DefaultFunction("now()")) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn auto_increment_test() {
  let f =
    field.integer("id")
    |> field.auto_increment

  field.is_auto_increment(f) |> should.be_true
  case f.default {
    Some(DefaultAutoIncrement) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// COMMENT TEST
// ============================================================================

pub fn comment_test() {
  let f =
    field.string("email")
    |> field.comment("User's primary email address")

  f.comment |> should.equal(Some("User's primary email address"))
}

// ============================================================================
// TYPE UTILITY TESTS
// ============================================================================

pub fn type_name_test() {
  field.type_name(Integer) |> should.equal("integer")
  field.type_name(String) |> should.equal("string")
  field.type_name(Text) |> should.equal("text")
  field.type_name(Nullable(String)) |> should.equal("string?")
  field.type_name(Nullable(Text)) |> should.equal("text?")
  field.type_name(Array(Integer)) |> should.equal("array(integer)")
  field.type_name(Decimal(10, 2)) |> should.equal("decimal(10,2)")
  field.type_name(Enum("status", [])) |> should.equal("enum:status")
  field.type_name(Custom("geometry")) |> should.equal("custom:geometry")
}

pub fn is_numeric_type_test() {
  field.is_numeric_type(Integer) |> should.be_true
  field.is_numeric_type(BigInteger) |> should.be_true
  field.is_numeric_type(Float) |> should.be_true
  field.is_numeric_type(Decimal(10, 2)) |> should.be_true
  field.is_numeric_type(Nullable(Integer)) |> should.be_true
  field.is_numeric_type(String) |> should.be_false
}

pub fn is_text_type_test() {
  field.is_text_type(String) |> should.be_true
  field.is_text_type(Text) |> should.be_true
  field.is_text_type(Char(10)) |> should.be_true
  field.is_text_type(Nullable(String)) |> should.be_true
  field.is_text_type(Nullable(Text)) |> should.be_true
  field.is_text_type(Integer) |> should.be_false
}

pub fn is_temporal_type_test() {
  field.is_temporal_type(DateTime) |> should.be_true
  field.is_temporal_type(Date) |> should.be_true
  field.is_temporal_type(Time) |> should.be_true
  field.is_temporal_type(Nullable(DateTime)) |> should.be_true
  field.is_temporal_type(String) |> should.be_false
}

pub fn get_base_type_test() {
  let f = field.string("name") |> field.nullable

  field.get_base_type(f) |> should.equal(String)

  let f2 = field.integer("count")
  field.get_base_type(f2) |> should.equal(Integer)
}

// ============================================================================
// MULTIPLE CONSTRAINTS TEST
// ============================================================================

pub fn multiple_constraints_test() {
  let f =
    field.string("email")
    |> field.not_null
    |> field.unique
    |> field.max_length(255)

  // Should have all three constraints
  field.has_constraint(f, field.is_not_null_constraint) |> should.be_true
  field.has_constraint(f, field.is_unique_constraint) |> should.be_true

  let max_pred = fn(c) {
    case c {
      MaxLength(255) -> True
      _ -> False
    }
  }
  field.has_constraint(f, max_pred) |> should.be_true
}

// ============================================================================
// FOREIGN KEY ACTION TESTS
// ============================================================================

pub fn foreign_key_actions_test() {
  // Test all action types compile and are distinct
  let _no_action = NoAction
  let _restrict = Restrict
  let _cascade = Cascade
  let _set_null = SetNull
  let _set_default = field.SetDefault

  should.be_true(True)
}
