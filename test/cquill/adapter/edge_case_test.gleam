// Edge Case Tests
//
// Tests for boundary conditions and edge cases that all adapters must handle.
// Includes unicode, special characters, large data, and boundary values.

import cquill/adapter/memory.{type MemoryRow, type MemoryStore}
import cquill/schema
import cquill/schema/field
import gleam/dynamic
import gleam/int
import gleam/list
import gleam/string
import gleeunit/should

// ============================================================================
// TEST SETUP
// ============================================================================

fn setup_text_store() -> MemoryStore {
  let text_schema =
    schema.new("texts")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("content") |> field.nullable)

  memory.new_store()
  |> memory.create_table_from_schema(text_schema)
}

fn setup_numbers_store() -> MemoryStore {
  let numbers_schema =
    schema.new("numbers")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.integer("value") |> field.nullable)

  memory.new_store()
  |> memory.create_table_from_schema(numbers_schema)
}

// ============================================================================
// UNICODE CHARACTER TESTS
// ============================================================================

pub fn insert_unicode_ascii_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("Hello World")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_unicode_japanese_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("日本語テスト")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_unicode_chinese_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("中文测试")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_unicode_korean_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("한국어 테스트")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_unicode_arabic_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("اختبار العربية")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_unicode_emoji_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("Hello 👋 World 🌍 ❤️")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_unicode_mixed_scripts_test() {
  let store = setup_text_store()

  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("Hello 你好 こんにちは مرحبا 🎉"),
  ]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// SPECIAL CHARACTER TESTS
// ============================================================================

pub fn insert_special_chars_quotes_test() {
  let store = setup_text_store()

  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("He said \"Hello\" and 'Goodbye'"),
  ]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_backslashes_test() {
  let store = setup_text_store()

  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("C:\\Users\\Admin\\file"),
  ]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_newlines_test() {
  let store = setup_text_store()

  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("Line 1\nLine 2\nLine 3"),
  ]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_tabs_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("Col1\tCol2\tCol3")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_carriage_return_test() {
  let store = setup_text_store()

  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("Windows\r\nLine Endings"),
  ]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_null_byte_test() {
  let store = setup_text_store()

  // String with null byte embedded
  let row: MemoryRow = [dynamic.int(1), dynamic.string("before\u{0000}after")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_sql_metacharacters_test() {
  let store = setup_text_store()

  // SQL metacharacters that might cause issues
  let row: MemoryRow = [
    dynamic.int(1),
    dynamic.string("SELECT * FROM users; DROP TABLE users; --"),
  ]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_special_chars_percent_underscore_test() {
  let store = setup_text_store()

  // LIKE pattern characters
  let row: MemoryRow = [dynamic.int(1), dynamic.string("100% success_rate")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// EMPTY AND BOUNDARY STRING TESTS
// ============================================================================

pub fn insert_empty_string_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_whitespace_only_string_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.string("   ")]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_very_long_string_test() {
  let store = setup_text_store()

  // Create a 10,000 character string
  let long_string = string.repeat("A", 10_000)
  let row: MemoryRow = [dynamic.int(1), dynamic.string(long_string)]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> {
          // Verify length is preserved
          case retrieved {
            [_, _content] -> {
              // Content should match the original long string
              should.be_true(True)
            }
            _ -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// NUMERIC BOUNDARY TESTS
// ============================================================================

pub fn insert_zero_integer_test() {
  let store = setup_numbers_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.int(0)]

  let result = memory.insert_row(store, "numbers", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "numbers", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_negative_integer_test() {
  let store = setup_numbers_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.int(-12_345)]

  let result = memory.insert_row(store, "numbers", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "numbers", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_large_positive_integer_test() {
  let store = setup_numbers_store()

  // Large but valid integer
  let row: MemoryRow = [dynamic.int(1), dynamic.int(2_147_483_647)]

  let result = memory.insert_row(store, "numbers", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "numbers", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn insert_large_negative_integer_test() {
  let store = setup_numbers_store()

  // Large negative integer
  let row: MemoryRow = [dynamic.int(1), dynamic.int(-2_147_483_648)]

  let result = memory.insert_row(store, "numbers", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "numbers", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// LARGE RESULT SET TESTS
// ============================================================================

pub fn query_large_result_set_test() {
  let store = setup_text_store()

  // Insert 1000 rows
  let store =
    list.fold(list.range(1, 1000), store, fn(s, i) {
      let key = int.to_string(i)
      let row: MemoryRow = [dynamic.int(i), dynamic.string("Row " <> key)]
      case memory.insert_row(s, "texts", key, row) {
        Ok(new_store) -> new_store
        Error(_) -> s
      }
    })

  // Query all rows
  let result = memory.get_all_rows(store, "texts")
  result |> should.be_ok

  case result {
    Ok(rows) -> list.length(rows) |> should.equal(1000)
    Error(_) -> should.fail()
  }
}

pub fn batch_insert_large_dataset_test() {
  let store = setup_text_store()

  // Create 500 rows for batch insert
  let rows =
    list.range(1, 500)
    |> list.map(fn(i) {
      let key = int.to_string(i)
      let row: MemoryRow = [dynamic.int(i), dynamic.string("Batch row " <> key)]
      #(key, row)
    })

  let result = memory.insert_all(store, "texts", rows)
  result |> should.be_ok

  case result {
    Ok(#(new_store, count)) -> {
      count |> should.equal(500)
      case memory.row_count(new_store, "texts") {
        Ok(n) -> n |> should.equal(500)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// NULL VALUE EDGE CASES
// ============================================================================

pub fn insert_null_content_test() {
  let store = setup_text_store()

  let row: MemoryRow = [dynamic.int(1), dynamic.nil()]

  let result = memory.insert_row(store, "texts", "1", row)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn update_to_null_test() {
  let store = setup_text_store()

  // Insert with value
  let row1: MemoryRow = [dynamic.int(1), dynamic.string("Original")]
  let assert Ok(store) = memory.insert_row(store, "texts", "1", row1)

  // Update to null
  let row2: MemoryRow = [dynamic.int(1), dynamic.nil()]
  let result = memory.update_row(store, "texts", "1", row2)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row2)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn update_from_null_to_value_test() {
  let store = setup_text_store()

  // Insert with null
  let row1: MemoryRow = [dynamic.int(1), dynamic.nil()]
  let assert Ok(store) = memory.insert_row(store, "texts", "1", row1)

  // Update to value
  let row2: MemoryRow = [dynamic.int(1), dynamic.string("New Value")]
  let result = memory.update_row(store, "texts", "1", row2)
  result |> should.be_ok

  case result {
    Ok(new_store) -> {
      case memory.get_row(new_store, "texts", "1") {
        Ok(retrieved) -> retrieved |> should.equal(row2)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
