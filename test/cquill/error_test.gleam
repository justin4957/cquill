// Tests for cquill/error.gleam
//
// These tests verify error mapping functions work correctly,
// particularly the SQLite constraint classification.

import cquill/error.{
  type AdapterError, AdapterSpecific, CheckViolation, ConnectionFailed,
  ConstraintViolation, ForeignKeyViolation, NotNullViolation, QueryFailed,
  UniqueViolation, from_sqlite_error,
}
import gleam/option.{Some}
import gleeunit/should

// ============================================================================
// SQLITE ERROR MAPPING TESTS
// ============================================================================

pub fn sqlite_unique_constraint_test() {
  // SQLite CONSTRAINT error (19) with "unique" in message
  let result = from_sqlite_error(19, "UNIQUE constraint failed: users.email")

  case result {
    UniqueViolation(_, detail) -> {
      detail
      |> should.equal("UNIQUE constraint failed: users.email")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_unique_constraint_uppercase_test() {
  // Test case-insensitive matching
  let result = from_sqlite_error(19, "UNIQUE CONSTRAINT FAILED: users.email")

  case result {
    UniqueViolation(..) -> Nil
    _ -> should.fail()
  }
}

pub fn sqlite_foreign_key_constraint_test() {
  // SQLite CONSTRAINT error (19) with "foreign key" in message
  let result = from_sqlite_error(19, "FOREIGN KEY constraint failed")

  case result {
    ForeignKeyViolation(_, detail) -> {
      detail
      |> should.equal("FOREIGN KEY constraint failed")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_not_null_constraint_test() {
  // SQLite CONSTRAINT error (19) with "not null" in message
  let result = from_sqlite_error(19, "NOT NULL constraint failed: users.name")

  case result {
    NotNullViolation(_) -> Nil
    _ -> should.fail()
  }
}

pub fn sqlite_check_constraint_test() {
  // SQLite CONSTRAINT error (19) with "check" in message
  let result = from_sqlite_error(19, "CHECK constraint failed: price_positive")

  case result {
    CheckViolation(_, detail) -> {
      detail
      |> should.equal("CHECK constraint failed: price_positive")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_unknown_constraint_test() {
  // SQLite CONSTRAINT error (19) with unrecognized message
  let result = from_sqlite_error(19, "constraint failed")

  case result {
    ConstraintViolation(constraint, detail) -> {
      constraint
      |> should.equal("unknown")
      detail
      |> should.equal("constraint failed")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_busy_error_test() {
  // SQLITE_BUSY (5)
  let result = from_sqlite_error(5, "database is locked")

  case result {
    ConnectionFailed(reason) -> {
      reason
      |> should.equal("Database is locked")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_locked_error_test() {
  // SQLITE_LOCKED (6)
  let result = from_sqlite_error(6, "table is locked")

  case result {
    ConnectionFailed(reason) -> {
      reason
      |> should.equal("Table is locked")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_notadb_error_test() {
  // SQLITE_NOTADB (26)
  let result = from_sqlite_error(26, "file is not a database")

  case result {
    ConnectionFailed(reason) -> {
      reason
      |> should.equal("Not a database file")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_sql_error_test() {
  // SQLITE_ERROR (1) - SQL error
  let result = from_sqlite_error(1, "no such table: users")

  case result {
    QueryFailed(message, Some(code)) -> {
      message
      |> should.equal("no such table: users")
      code
      |> should.equal("1")
    }
    _ -> should.fail()
  }
}

pub fn sqlite_unknown_error_code_test() {
  // Unknown error code
  let result = from_sqlite_error(999, "unknown error")

  case result {
    AdapterSpecific(code, message) -> {
      code
      |> should.equal("999")
      message
      |> should.equal("unknown error")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// CONSTRAINT PATTERN ORDER TESTS
// ============================================================================

// These tests verify that constraint patterns are checked in the correct order
// For example, a message containing both "unique" and "check" should match "unique" first

pub fn sqlite_constraint_order_unique_first_test() {
  // Message contains both "unique" and "check" - should match "unique"
  let result = from_sqlite_error(19, "unique constraint check failed")

  case result {
    UniqueViolation(..) -> Nil
    _ -> should.fail()
  }
}
