import cquill/changeset.{
  Acceptance, Confirmation, Custom, ExactLength, Exclusion, Format, Inclusion,
  Length, MaxLength, MinLength, Range, RangeLength, Required, ValidationError,
}
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// CONSTRUCTOR TESTS
// ============================================================================

pub fn new_creates_valid_changeset_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))

  let cs = changeset.new(changes)

  changeset.is_valid(cs) |> should.be_true
  changeset.get_data(cs) |> should.equal(None)
}

pub fn change_creates_changeset_with_existing_data_test() {
  let existing =
    dict.new()
    |> dict.insert("id", dynamic.int(1))
    |> dict.insert("email", dynamic.string("old@example.com"))

  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("new@example.com"))

  let cs = changeset.change(existing, changes)

  changeset.is_valid(cs) |> should.be_true
  changeset.get_data(cs) |> should.equal(Some(existing))
  changeset.get_changes(cs) |> should.equal(changes)
}

pub fn empty_creates_empty_changeset_test() {
  let cs = changeset.empty()

  changeset.is_valid(cs) |> should.be_true
  changeset.get_changes(cs) |> dict.size |> should.equal(0)
}

// ============================================================================
// VALIDATE_REQUIRED TESTS
// ============================================================================

pub fn validate_required_passes_for_present_field_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email"])

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_required_fails_for_missing_field_test() {
  let changes = dict.new()

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email"])

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "email") |> should.be_true

  let errors = changeset.get_errors(cs, "email")
  errors |> should.not_equal([])

  case errors {
    [ValidationError(message: msg, validation: Required)] -> {
      msg |> should.equal("is required")
    }
    _ -> should.fail()
  }
}

pub fn validate_required_fails_for_empty_string_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string(""))

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email"])

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "email") |> should.be_true
}

pub fn validate_required_multiple_fields_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email", "name"])

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "email") |> should.be_false
  changeset.has_error(cs, "name") |> should.be_true
}

// ============================================================================
// VALIDATE_FORMAT TESTS
// ============================================================================

pub fn validate_format_passes_for_matching_pattern_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_format_fails_for_non_matching_pattern_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("invalid-email"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "email") |> should.be_true

  let errors = changeset.get_errors(cs, "email")
  case errors {
    [ValidationError(message: _, validation: Format)] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn validate_format_skips_missing_field_test() {
  let changes = dict.new()

  let cs =
    changeset.new(changes)
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")

  changeset.is_valid(cs) |> should.be_true
}

// ============================================================================
// VALIDATE_LENGTH TESTS
// ============================================================================

pub fn validate_length_min_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("name", dynamic.string("John"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length_min("name", 2)

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_length_min_fails_test() {
  let changes =
    dict.new()
    |> dict.insert("name", dynamic.string("J"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length_min("name", 2)

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "name") |> should.be_true

  let errors = changeset.get_errors(cs, "name")
  case errors {
    [ValidationError(message: _, validation: Length(MinLength(2)))] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn validate_length_max_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("name", dynamic.string("John"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length_max("name", 10)

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_length_max_fails_test() {
  let changes =
    dict.new()
    |> dict.insert("name", dynamic.string("This is a very long name"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length_max("name", 10)

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "name")
  case errors {
    [ValidationError(message: _, validation: Length(MaxLength(10)))] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn validate_length_exact_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("code", dynamic.string("ABC123"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length_exact("code", 6)

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_length_exact_fails_test() {
  let changes =
    dict.new()
    |> dict.insert("code", dynamic.string("ABC12"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length_exact("code", 6)

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "code")
  case errors {
    [ValidationError(message: _, validation: Length(ExactLength(6)))] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn validate_length_range_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("name", dynamic.string("John"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length("name", min: 2, max: 100)

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_length_range_fails_below_min_test() {
  let changes =
    dict.new()
    |> dict.insert("name", dynamic.string("J"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_length("name", min: 2, max: 100)

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "name")
  case errors {
    [ValidationError(message: _, validation: Length(RangeLength(2, 100)))] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// VALIDATE_INCLUSION TESTS
// ============================================================================

pub fn validate_inclusion_passes_for_allowed_value_test() {
  let changes =
    dict.new()
    |> dict.insert("role", dynamic.string("admin"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_inclusion("role", ["admin", "user", "guest"])

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_inclusion_fails_for_disallowed_value_test() {
  let changes =
    dict.new()
    |> dict.insert("role", dynamic.string("superadmin"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_inclusion("role", ["admin", "user", "guest"])

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "role")
  case errors {
    [ValidationError(message: _, validation: Inclusion)] -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// VALIDATE_EXCLUSION TESTS
// ============================================================================

pub fn validate_exclusion_passes_for_allowed_value_test() {
  let changes =
    dict.new()
    |> dict.insert("username", dynamic.string("john"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_exclusion("username", ["admin", "root", "system"])

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_exclusion_fails_for_excluded_value_test() {
  let changes =
    dict.new()
    |> dict.insert("username", dynamic.string("admin"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_exclusion("username", ["admin", "root", "system"])

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "username")
  case errors {
    [ValidationError(message: _, validation: Exclusion)] -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// VALIDATE_NUMBER_RANGE TESTS
// ============================================================================

pub fn validate_number_range_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("age", dynamic.int(25))

  let cs =
    changeset.new(changes)
    |> changeset.validate_number_range("age", min: Some(18), max: Some(120))

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_number_range_fails_below_min_test() {
  let changes =
    dict.new()
    |> dict.insert("age", dynamic.int(15))

  let cs =
    changeset.new(changes)
    |> changeset.validate_number_range("age", min: Some(18), max: Some(120))

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "age")
  case errors {
    [ValidationError(message: _, validation: Range)] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn validate_number_range_fails_above_max_test() {
  let changes =
    dict.new()
    |> dict.insert("age", dynamic.int(150))

  let cs =
    changeset.new(changes)
    |> changeset.validate_number_range("age", min: Some(18), max: Some(120))

  changeset.is_valid(cs) |> should.be_false
}

// ============================================================================
// VALIDATE_CONFIRMATION TESTS
// ============================================================================

pub fn validate_confirmation_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("password", dynamic.string("secret123"))
    |> dict.insert("password_confirmation", dynamic.string("secret123"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_confirmation("password", "password_confirmation")

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_confirmation_fails_test() {
  let changes =
    dict.new()
    |> dict.insert("password", dynamic.string("secret123"))
    |> dict.insert("password_confirmation", dynamic.string("different"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_confirmation("password", "password_confirmation")

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "password_confirmation") |> should.be_true

  let errors = changeset.get_errors(cs, "password_confirmation")
  case errors {
    [ValidationError(message: _, validation: Confirmation)] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// VALIDATE_ACCEPTANCE TESTS
// ============================================================================

pub fn validate_acceptance_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("terms", dynamic.bool(True))

  let cs =
    changeset.new(changes)
    |> changeset.validate_acceptance("terms")

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_acceptance_fails_for_false_test() {
  let changes =
    dict.new()
    |> dict.insert("terms", dynamic.bool(False))

  let cs =
    changeset.new(changes)
    |> changeset.validate_acceptance("terms")

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "terms")
  case errors {
    [ValidationError(message: _, validation: Acceptance)] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn validate_acceptance_fails_for_missing_test() {
  let changes = dict.new()

  let cs =
    changeset.new(changes)
    |> changeset.validate_acceptance("terms")

  changeset.is_valid(cs) |> should.be_false
}

// ============================================================================
// VALIDATE_CUSTOM TESTS
// ============================================================================

pub fn validate_custom_passes_test() {
  let changes =
    dict.new()
    |> dict.insert("value", dynamic.int(10))

  let validator = fn(_cs) { Ok(Nil) }

  let cs =
    changeset.new(changes)
    |> changeset.validate_custom("value", "always_valid", validator)

  changeset.is_valid(cs) |> should.be_true
}

pub fn validate_custom_fails_test() {
  let changes =
    dict.new()
    |> dict.insert("value", dynamic.int(10))

  let validator = fn(_cs) { Error("custom validation failed") }

  let cs =
    changeset.new(changes)
    |> changeset.validate_custom("value", "custom_check", validator)

  changeset.is_valid(cs) |> should.be_false

  let errors = changeset.get_errors(cs, "value")
  case errors {
    [ValidationError(message: msg, validation: Custom("custom_check"))] -> {
      msg |> should.equal("custom validation failed")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// VALIDATE_IF_PRESENT TESTS
// ============================================================================

pub fn validate_if_present_runs_when_present_test() {
  let changes =
    dict.new()
    |> dict.insert("nickname", dynamic.string("AB"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_if_present("nickname", fn(cs) {
      changeset.validate_length_min(cs, "nickname", 3)
    })

  changeset.is_valid(cs) |> should.be_false
}

pub fn validate_if_present_skips_when_missing_test() {
  let changes = dict.new()

  let cs =
    changeset.new(changes)
    |> changeset.validate_if_present("nickname", fn(cs) {
      changeset.validate_length_min(cs, "nickname", 3)
    })

  changeset.is_valid(cs) |> should.be_true
}

// ============================================================================
// CHANGE FUNCTIONS TESTS
// ============================================================================

pub fn put_change_adds_change_test() {
  let cs =
    changeset.empty()
    |> changeset.put_change("email", dynamic.string("test@example.com"))

  changeset.get_change(cs, "email")
  |> should.equal(Some(dynamic.string("test@example.com")))
}

pub fn delete_change_removes_change_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))

  let cs =
    changeset.new(changes)
    |> changeset.delete_change("email")

  changeset.get_change(cs, "email") |> should.equal(None)
}

pub fn force_change_adds_and_clears_errors_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("invalid"))

  let cs =
    changeset.new(changes)
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")

  changeset.is_valid(cs) |> should.be_false

  let cs2 =
    cs
    |> changeset.force_change("email", dynamic.string("valid@example.com"))

  changeset.is_valid(cs2) |> should.be_true
  changeset.has_error(cs2, "email") |> should.be_false
}

// ============================================================================
// APPLY TESTS
// ============================================================================

pub fn apply_returns_ok_for_valid_changeset_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))

  let result =
    changeset.new(changes)
    |> changeset.validate_required(["email"])
    |> changeset.apply()

  case result {
    Ok(data) -> dict.size(data) |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn apply_returns_error_for_invalid_changeset_test() {
  let changes = dict.new()

  let result =
    changeset.new(changes)
    |> changeset.validate_required(["email"])
    |> changeset.apply()

  case result {
    Ok(_) -> should.fail()
    Error(errors) -> dict.has_key(errors, "email") |> should.be_true
  }
}

// ============================================================================
// MERGE_CHANGES TESTS
// ============================================================================

pub fn merge_changes_combines_data_and_changes_test() {
  let existing =
    dict.new()
    |> dict.insert("id", dynamic.int(1))
    |> dict.insert("email", dynamic.string("old@example.com"))

  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("new@example.com"))

  let cs = changeset.change(existing, changes)
  let merged = changeset.merge_changes(cs)

  dict.size(merged) |> should.equal(2)
}

// ============================================================================
// ERROR FORMATTING TESTS
// ============================================================================

pub fn format_errors_returns_readable_string_test() {
  let changes = dict.new()

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email", "name"])

  let formatted = changeset.format_errors(cs)

  formatted |> string.contains("email is required") |> should.be_true
  formatted |> string.contains("name is required") |> should.be_true
}

pub fn format_field_errors_returns_field_errors_test() {
  let changes = dict.new()

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email"])

  let formatted = changeset.format_field_errors(cs, "email")

  formatted |> should.equal("is required")
}

// ============================================================================
// PIPELINE TESTS
// ============================================================================

pub fn full_validation_pipeline_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("test@example.com"))
    |> dict.insert("name", dynamic.string("John Doe"))
    |> dict.insert("age", dynamic.int(25))
    |> dict.insert("role", dynamic.string("user"))
    |> dict.insert("terms", dynamic.bool(True))

  let result =
    changeset.new(changes)
    |> changeset.validate_required(["email", "name"])
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")
    |> changeset.validate_length("name", min: 2, max: 100)
    |> changeset.validate_number_range("age", min: Some(18), max: Some(120))
    |> changeset.validate_inclusion("role", ["admin", "user", "guest"])
    |> changeset.validate_acceptance("terms")
    |> changeset.apply()

  case result {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

pub fn full_validation_pipeline_with_errors_test() {
  let changes =
    dict.new()
    |> dict.insert("email", dynamic.string("invalid-email"))
    |> dict.insert("name", dynamic.string("J"))
    |> dict.insert("age", dynamic.int(15))
    |> dict.insert("role", dynamic.string("superadmin"))
    |> dict.insert("terms", dynamic.bool(False))

  let cs =
    changeset.new(changes)
    |> changeset.validate_required(["email", "name"])
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")
    |> changeset.validate_length("name", min: 2, max: 100)
    |> changeset.validate_number_range("age", min: Some(18), max: Some(120))
    |> changeset.validate_inclusion("role", ["admin", "user", "guest"])
    |> changeset.validate_acceptance("terms")

  changeset.is_valid(cs) |> should.be_false
  changeset.has_error(cs, "email") |> should.be_true
  changeset.has_error(cs, "name") |> should.be_true
  changeset.has_error(cs, "age") |> should.be_true
  changeset.has_error(cs, "role") |> should.be_true
  changeset.has_error(cs, "terms") |> should.be_true

  changeset.all_errors(cs) |> list.length |> should.equal(5)
}
