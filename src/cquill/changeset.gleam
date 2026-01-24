// cquill Changeset Layer
//
// This module provides data validation and transformation before persistence.
// It is part of the pure core — no I/O, no side effects.
//
// ARCHITECTURAL PRINCIPLE: Changesets are pure data transformations.
// They validate and transform input data, producing either valid output
// or structured error information.
//
// DESIGN PATTERN: Inspired by Ecto's Changeset
// - Changesets track changes to data
// - Validations are composable via pipelines
// - Errors are structured and field-specific
// - Valid changesets can be applied to get the result
//
// Example:
// ```gleam
// let result = changeset.new(user_data)
//   |> changeset.validate_required(["email", "name"])
//   |> changeset.validate_format("email", "^[^@]+@[^@]+$")
//   |> changeset.validate_length("name", min: 2, max: 100)
//   |> changeset.apply()
//
// case result {
//   Ok(valid_data) -> // use valid_data
//   Error(errors) -> // handle validation errors
// }
// ```

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string

// ============================================================================
// TYPES
// ============================================================================

/// A changeset represents a set of changes to be validated and applied.
/// It tracks the original data, changes, and any validation errors.
pub type Changeset {
  Changeset(
    /// The original data (if updating existing record)
    data: Option(Dict(String, Dynamic)),
    /// The changes/new values being applied
    changes: Dict(String, Dynamic),
    /// Validation errors by field name
    errors: Dict(String, List(ValidationError)),
    /// Whether the changeset is valid (no errors)
    valid: Bool,
    /// Fields that were validated
    validated_fields: List(String),
  )
}

/// A validation error for a specific field
pub type ValidationError {
  ValidationError(
    /// The error message
    message: String,
    /// The validation type that failed
    validation: ValidationType,
  )
}

/// Types of validations that can fail
pub type ValidationType {
  /// Field is required but missing or empty
  Required
  /// Field format doesn't match pattern
  Format
  /// Field length is invalid
  Length(constraint: LengthConstraint)
  /// Field value is not in allowed list
  Inclusion
  /// Field value is in excluded list
  Exclusion
  /// Numeric value out of range
  Range
  /// Custom validation failed
  Custom(name: String)
  /// Field must be unique (checked externally)
  Uniqueness
  /// Confirmation field doesn't match
  Confirmation
  /// Field must be accepted (for terms, etc.)
  Acceptance
}

/// Length constraint details
pub type LengthConstraint {
  MinLength(min: Int)
  MaxLength(max: Int)
  ExactLength(length: Int)
  RangeLength(min: Int, max: Int)
}

// ============================================================================
// CONSTRUCTORS
// ============================================================================

/// Create a new changeset from input data (for inserts)
pub fn new(changes: Dict(String, Dynamic)) -> Changeset {
  Changeset(
    data: None,
    changes: changes,
    errors: dict.new(),
    valid: True,
    validated_fields: [],
  )
}

/// Create a changeset for updating existing data
pub fn change(
  existing: Dict(String, Dynamic),
  changes: Dict(String, Dynamic),
) -> Changeset {
  Changeset(
    data: Some(existing),
    changes: changes,
    errors: dict.new(),
    valid: True,
    validated_fields: [],
  )
}

/// Create an empty changeset (useful for forms)
pub fn empty() -> Changeset {
  Changeset(
    data: None,
    changes: dict.new(),
    errors: dict.new(),
    valid: True,
    validated_fields: [],
  )
}

// ============================================================================
// VALIDATION FUNCTIONS
// ============================================================================

/// Validate that the specified fields are present and non-empty
pub fn validate_required(
  changeset: Changeset,
  fields: List(String),
) -> Changeset {
  list.fold(fields, changeset, fn(cs, field) {
    let value = get_field(cs, field)
    case is_present(value) {
      True -> mark_validated(cs, field)
      False -> add_error(cs, field, "is required", Required)
    }
  })
}

/// Validate that a string field matches a regex pattern
pub fn validate_format(
  changeset: Changeset,
  field: String,
  pattern: String,
) -> Changeset {
  case get_field_string(changeset, field) {
    None -> mark_validated(changeset, field)
    Some(value) -> {
      case regexp.from_string(pattern) {
        Error(_) -> add_error(changeset, field, "has invalid format", Format)
        Ok(re) -> {
          case regexp.check(re, value) {
            True -> mark_validated(changeset, field)
            False -> add_error(changeset, field, "has invalid format", Format)
          }
        }
      }
    }
  }
}

/// Validate string length with minimum constraint
pub fn validate_length_min(
  changeset: Changeset,
  field: String,
  min: Int,
) -> Changeset {
  validate_length_impl(changeset, field, Some(min), None, None)
}

/// Validate string length with maximum constraint
pub fn validate_length_max(
  changeset: Changeset,
  field: String,
  max: Int,
) -> Changeset {
  validate_length_impl(changeset, field, None, Some(max), None)
}

/// Validate string length with exact length constraint
pub fn validate_length_exact(
  changeset: Changeset,
  field: String,
  length: Int,
) -> Changeset {
  validate_length_impl(changeset, field, None, None, Some(length))
}

/// Validate string length with min and max constraints
pub fn validate_length(
  changeset: Changeset,
  field: String,
  min min: Int,
  max max: Int,
) -> Changeset {
  validate_length_impl(changeset, field, Some(min), Some(max), None)
}

fn validate_length_impl(
  changeset: Changeset,
  field: String,
  min: Option(Int),
  max: Option(Int),
  exact: Option(Int),
) -> Changeset {
  case get_field_string(changeset, field) {
    None -> mark_validated(changeset, field)
    Some(value) -> {
      let len = string.length(value)
      case exact {
        Some(exact_len) if len != exact_len ->
          add_error(
            changeset,
            field,
            "must be exactly " <> int.to_string(exact_len) <> " characters",
            Length(ExactLength(exact_len)),
          )
        Some(_) -> mark_validated(changeset, field)
        None -> {
          case min, max {
            Some(min_len), Some(max_len) if len < min_len || len > max_len ->
              add_error(
                changeset,
                field,
                "must be between "
                  <> int.to_string(min_len)
                  <> " and "
                  <> int.to_string(max_len)
                  <> " characters",
                Length(RangeLength(min_len, max_len)),
              )
            Some(min_len), None if len < min_len ->
              add_error(
                changeset,
                field,
                "must be at least " <> int.to_string(min_len) <> " characters",
                Length(MinLength(min_len)),
              )
            None, Some(max_len) if len > max_len ->
              add_error(
                changeset,
                field,
                "must be at most " <> int.to_string(max_len) <> " characters",
                Length(MaxLength(max_len)),
              )
            _, _ -> mark_validated(changeset, field)
          }
        }
      }
    }
  }
}

/// Validate that a field value is in a list of allowed values
pub fn validate_inclusion(
  changeset: Changeset,
  field: String,
  allowed: List(String),
) -> Changeset {
  case get_field_string(changeset, field) {
    None -> mark_validated(changeset, field)
    Some(value) -> {
      case list.contains(allowed, value) {
        True -> mark_validated(changeset, field)
        False -> add_error(changeset, field, "is not a valid option", Inclusion)
      }
    }
  }
}

/// Validate that a field value is NOT in a list of excluded values
pub fn validate_exclusion(
  changeset: Changeset,
  field: String,
  excluded: List(String),
) -> Changeset {
  case get_field_string(changeset, field) {
    None -> mark_validated(changeset, field)
    Some(value) -> {
      case list.contains(excluded, value) {
        True -> add_error(changeset, field, "is not allowed", Exclusion)
        False -> mark_validated(changeset, field)
      }
    }
  }
}

/// Validate that a numeric field is within a range
pub fn validate_number_range(
  changeset: Changeset,
  field: String,
  min min: Option(Int),
  max max: Option(Int),
) -> Changeset {
  case get_field_int(changeset, field) {
    None -> mark_validated(changeset, field)
    Some(value) -> {
      case min, max {
        Some(min_val), Some(max_val) if value < min_val || value > max_val ->
          add_error(
            changeset,
            field,
            "must be between "
              <> int.to_string(min_val)
              <> " and "
              <> int.to_string(max_val),
            Range,
          )
        Some(min_val), None if value < min_val ->
          add_error(
            changeset,
            field,
            "must be at least " <> int.to_string(min_val),
            Range,
          )
        None, Some(max_val) if value > max_val ->
          add_error(
            changeset,
            field,
            "must be at most " <> int.to_string(max_val),
            Range,
          )
        _, _ -> mark_validated(changeset, field)
      }
    }
  }
}

/// Validate that a confirmation field matches the original field
/// e.g., password and password_confirmation
pub fn validate_confirmation(
  changeset: Changeset,
  field: String,
  confirmation_field: String,
) -> Changeset {
  let original = get_field_string(changeset, field)
  let confirmation = get_field_string(changeset, confirmation_field)

  case original, confirmation {
    Some(orig), Some(conf) if orig != conf ->
      add_error(changeset, confirmation_field, "does not match", Confirmation)
    _, _ -> mark_validated(changeset, confirmation_field)
  }
}

/// Validate that a boolean field is true (for accepting terms, etc.)
pub fn validate_acceptance(changeset: Changeset, field: String) -> Changeset {
  case get_field_bool(changeset, field) {
    Some(True) -> mark_validated(changeset, field)
    Some(False) -> add_error(changeset, field, "must be accepted", Acceptance)
    None -> add_error(changeset, field, "must be accepted", Acceptance)
  }
}

/// Apply a custom validation function
/// The function should return Ok(Nil) if valid, or Error(message) if invalid
pub fn validate_custom(
  changeset: Changeset,
  field: String,
  name: String,
  validator: fn(Changeset) -> Result(Nil, String),
) -> Changeset {
  case validator(changeset) {
    Ok(_) -> mark_validated(changeset, field)
    Error(message) -> add_error(changeset, field, message, Custom(name))
  }
}

/// Validate a field only if it is present (non-nil)
/// Useful for optional fields that have constraints when provided
pub fn validate_if_present(
  changeset: Changeset,
  field: String,
  validator: fn(Changeset) -> Changeset,
) -> Changeset {
  case get_field(changeset, field) {
    None -> changeset
    Some(_) -> validator(changeset)
  }
}

// ============================================================================
// CHANGE FUNCTIONS
// ============================================================================

/// Put a change into the changeset
pub fn put_change(
  changeset: Changeset,
  field: String,
  value: Dynamic,
) -> Changeset {
  Changeset(..changeset, changes: dict.insert(changeset.changes, field, value))
}

/// Delete a change from the changeset
pub fn delete_change(changeset: Changeset, field: String) -> Changeset {
  Changeset(..changeset, changes: dict.delete(changeset.changes, field))
}

/// Get a change value from the changeset
pub fn get_change(changeset: Changeset, field: String) -> Option(Dynamic) {
  dict.get(changeset.changes, field)
  |> option.from_result
}

/// Force a change even if validation would fail
/// Use with caution — this bypasses validation for the field
pub fn force_change(
  changeset: Changeset,
  field: String,
  value: Dynamic,
) -> Changeset {
  changeset
  |> put_change(field, value)
  |> clear_errors(field)
}

// ============================================================================
// ERROR FUNCTIONS
// ============================================================================

/// Add an error to the changeset
pub fn add_error(
  changeset: Changeset,
  field: String,
  message: String,
  validation: ValidationType,
) -> Changeset {
  let error = ValidationError(message:, validation:)
  let existing_errors =
    dict.get(changeset.errors, field)
    |> result.unwrap([])
  let updated_errors =
    dict.insert(changeset.errors, field, [error, ..existing_errors])
  Changeset(..changeset, errors: updated_errors, valid: False)
}

/// Clear all errors for a specific field
pub fn clear_errors(changeset: Changeset, field: String) -> Changeset {
  let updated_errors = dict.delete(changeset.errors, field)
  let is_valid = dict.is_empty(updated_errors)
  Changeset(..changeset, errors: updated_errors, valid: is_valid)
}

/// Clear all errors from the changeset
pub fn clear_all_errors(changeset: Changeset) -> Changeset {
  Changeset(..changeset, errors: dict.new(), valid: True)
}

/// Get errors for a specific field
pub fn get_errors(changeset: Changeset, field: String) -> List(ValidationError) {
  dict.get(changeset.errors, field)
  |> result.unwrap([])
}

/// Get all errors as a list of (field, error) tuples
pub fn all_errors(changeset: Changeset) -> List(#(String, ValidationError)) {
  dict.to_list(changeset.errors)
  |> list.flat_map(fn(pair) {
    let #(field, errors) = pair
    list.map(errors, fn(err) { #(field, err) })
  })
}

/// Check if a specific field has errors
pub fn has_error(changeset: Changeset, field: String) -> Bool {
  case dict.get(changeset.errors, field) {
    Ok(errors) -> !list.is_empty(errors)
    Error(_) -> False
  }
}

// ============================================================================
// RESULT FUNCTIONS
// ============================================================================

/// Apply the changeset and return the result
/// Returns Ok(changes) if valid, Error(errors) if invalid
pub fn apply(
  changeset: Changeset,
) -> Result(Dict(String, Dynamic), Dict(String, List(ValidationError))) {
  case changeset.valid {
    True -> Ok(changeset.changes)
    False -> Error(changeset.errors)
  }
}

/// Check if the changeset is valid
pub fn is_valid(changeset: Changeset) -> Bool {
  changeset.valid
}

/// Get the changes from the changeset (regardless of validity)
pub fn get_changes(changeset: Changeset) -> Dict(String, Dynamic) {
  changeset.changes
}

/// Get the original data (if this is an update changeset)
pub fn get_data(changeset: Changeset) -> Option(Dict(String, Dynamic)) {
  changeset.data
}

/// Merge changes from the changeset with the original data
/// For updates, this combines old data with new changes
pub fn merge_changes(changeset: Changeset) -> Dict(String, Dynamic) {
  case changeset.data {
    None -> changeset.changes
    Some(data) -> dict.merge(data, changeset.changes)
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Get a field value from changes or original data
fn get_field(changeset: Changeset, field: String) -> Option(Dynamic) {
  case dict.get(changeset.changes, field) {
    Ok(value) -> Some(value)
    Error(_) -> {
      case changeset.data {
        None -> None
        Some(data) ->
          dict.get(data, field)
          |> option.from_result
      }
    }
  }
}

/// Get a field value as a string
fn get_field_string(changeset: Changeset, field: String) -> Option(String) {
  case get_field(changeset, field) {
    None -> None
    Some(value) -> {
      case decode.run(value, decode.string) {
        Ok(s) -> Some(s)
        Error(_) -> None
      }
    }
  }
}

/// Get a field value as an int
fn get_field_int(changeset: Changeset, field: String) -> Option(Int) {
  case get_field(changeset, field) {
    None -> None
    Some(value) -> {
      case decode.run(value, decode.int) {
        Ok(i) -> Some(i)
        Error(_) -> None
      }
    }
  }
}

/// Get a field value as a bool
fn get_field_bool(changeset: Changeset, field: String) -> Option(Bool) {
  case get_field(changeset, field) {
    None -> None
    Some(value) -> {
      case decode.run(value, decode.bool) {
        Ok(b) -> Some(b)
        Error(_) -> None
      }
    }
  }
}

/// Check if a value is present (not None and not empty string)
fn is_present(value: Option(Dynamic)) -> Bool {
  case value {
    None -> False
    Some(v) -> {
      case decode.run(v, decode.string) {
        Ok(s) -> !string.is_empty(s)
        Error(_) -> True
      }
    }
  }
}

/// Mark a field as validated
fn mark_validated(changeset: Changeset, field: String) -> Changeset {
  case list.contains(changeset.validated_fields, field) {
    True -> changeset
    False ->
      Changeset(..changeset, validated_fields: [
        field,
        ..changeset.validated_fields
      ])
  }
}

// ============================================================================
// FORMATTING
// ============================================================================

/// Format all errors as a human-readable string
pub fn format_errors(changeset: Changeset) -> String {
  all_errors(changeset)
  |> list.map(fn(pair) {
    let #(field, error) = pair
    field <> " " <> error.message
  })
  |> string.join(", ")
}

/// Format errors for a specific field
pub fn format_field_errors(changeset: Changeset, field: String) -> String {
  get_errors(changeset, field)
  |> list.map(fn(error) { error.message })
  |> string.join(", ")
}
