# Testing with cquill

This guide covers testing strategies for applications using cquill, from unit tests to integration tests.

## Testing Philosophy

cquill encourages a layered testing approach:

```
┌────────────────────────────────────┐
│  Ring 3: System/Integration Tests  │  Real database, full flows
├────────────────────────────────────┤
│  Ring 2: Adapter Contract Tests    │  Verify adapter behavior
├────────────────────────────────────┤
│  Ring 1: Unit Tests (Pure Code)    │  No I/O, fast, isolated
└────────────────────────────────────┘
```

## Ring 1: Unit Tests (Pure Code)

Test pure functions without any database:

### Testing Query Builders

```gleam
import gleeunit/should
import cquill/query
import cquill/query/ast

pub fn query_builder_creates_correct_ast_test() {
  let q =
    query.from("users")
    |> query.where(query.eq("active", ast.BoolValue(True)))
    |> query.order_by_asc("name")
    |> query.limit(10)

  // Verify the AST structure
  case q.source {
    ast.TableSource(table, _) -> table |> should.equal("users")
    _ -> should.fail()
  }

  q.wheres |> list.length() |> should.equal(1)
  q.order_bys |> list.length() |> should.equal(1)
  q.limit |> should.equal(Some(10))
}

pub fn where_conditions_compose_correctly_test() {
  let q =
    query.from("users")
    |> query.where(query.eq("a", ast.IntValue(1)))
    |> query.where(query.eq("b", ast.IntValue(2)))

  // Both conditions are added
  q.wheres |> list.length() |> should.equal(2)
}
```

### Testing Business Logic

```gleam
import cquill/query/ast

// Business logic that builds queries
pub fn build_user_search_query(
  search_term: Option(String),
  active_only: Bool,
  page: Int,
  per_page: Int,
) {
  query.from("users")
  |> apply_search(search_term)
  |> apply_active_filter(active_only)
  |> query.order_by_asc("name")
  |> query.limit(per_page)
  |> query.offset((page - 1) * per_page)
}

// Tests
pub fn search_query_applies_search_term_test() {
  let q = build_user_search_query(Some("john"), False, 1, 20)

  // Check that search condition was added
  q.wheres |> list.length() |> should.equal(1)
}

pub fn search_query_applies_pagination_test() {
  let q = build_user_search_query(None, False, 3, 20)

  q.limit |> should.equal(Some(20))
  q.offset |> should.equal(Some(40))  // (3-1) * 20
}

pub fn search_query_filters_active_when_requested_test() {
  let q = build_user_search_query(None, True, 1, 20)

  q.wheres |> list.length() |> should.equal(1)
}
```

### Testing Validation Logic

```gleam
pub type UserInput {
  UserInput(email: String, name: String, age: Int)
}

pub type ValidationError {
  InvalidEmail
  NameTooShort
  AgeTooYoung
}

pub fn validate_user_input(input: UserInput) -> Result(UserInput, List(ValidationError)) {
  let errors = []
  let errors = case string.contains(input.email, "@") {
    True -> errors
    False -> [InvalidEmail, ..errors]
  }
  let errors = case string.length(input.name) >= 2 {
    True -> errors
    False -> [NameTooShort, ..errors]
  }
  let errors = case input.age >= 18 {
    True -> errors
    False -> [AgeTooYoung, ..errors]
  }

  case errors {
    [] -> Ok(input)
    _ -> Error(errors)
  }
}

// Tests
pub fn validate_accepts_valid_input_test() {
  let input = UserInput("test@example.com", "Alice", 25)
  validate_user_input(input) |> should.be_ok()
}

pub fn validate_rejects_invalid_email_test() {
  let input = UserInput("invalid", "Alice", 25)
  case validate_user_input(input) {
    Error(errors) -> errors |> should.equal([InvalidEmail])
    Ok(_) -> should.fail()
  }
}

pub fn validate_collects_multiple_errors_test() {
  let input = UserInput("invalid", "A", 10)
  case validate_user_input(input) {
    Error(errors) -> list.length(errors) |> should.equal(3)
    Ok(_) -> should.fail()
  }
}
```

## Ring 2: Adapter Tests with Memory Adapter

Test database operations using the memory adapter:

### Basic CRUD Tests

```gleam
import cquill/adapter/memory
import cquill/query
import cquill/query/ast

pub fn memory_adapter_insert_and_select_test() {
  // Setup
  let store = memory.new_store()
    |> memory.create_table("users", ["id", "email", "name"])

  // Insert
  let store = memory.insert_row(store, "users", dict.from_list([
    #("id", ast.IntValue(1)),
    #("email", ast.StringValue("alice@example.com")),
    #("name", ast.StringValue("Alice")),
  ]))

  // Query
  let query = query.from("users")
    |> query.where(query.eq("id", ast.IntValue(1)))

  // Assert
  case memory.execute_select(store, query) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
      case list.first(rows) {
        Ok(row) -> {
          dict.get(row, "email")
          |> should.equal(Ok(ast.StringValue("alice@example.com")))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn memory_adapter_update_test() {
  let store = memory.new_store()
    |> memory.create_table("users", ["id", "name"])
    |> memory.insert_row("users", dict.from_list([
      #("id", ast.IntValue(1)),
      #("name", ast.StringValue("Alice")),
    ]))

  let update = ast.new_update("users")
    |> ast.set("name", ast.StringValue("Alice Smith"))
    |> ast.update_where(query.eq("id", ast.IntValue(1)))

  let store = case memory.execute_update(store, update) {
    Ok(#(new_store, _)) -> new_store
    Error(_) -> { should.fail() store }
  }

  // Verify update
  let query = query.from("users")
    |> query.where(query.eq("id", ast.IntValue(1)))

  case memory.execute_select(store, query) {
    Ok([row]) -> {
      dict.get(row, "name")
      |> should.equal(Ok(ast.StringValue("Alice Smith")))
    }
    _ -> should.fail()
  }
}

pub fn memory_adapter_delete_test() {
  let store = memory.new_store()
    |> memory.create_table("users", ["id"])
    |> memory.insert_row("users", dict.from_list([#("id", ast.IntValue(1))]))
    |> memory.insert_row("users", dict.from_list([#("id", ast.IntValue(2))]))

  let delete = ast.new_delete("users")
    |> ast.delete_where(query.eq("id", ast.IntValue(1)))

  let store = case memory.execute_delete(store, delete) {
    Ok(#(new_store, count)) -> {
      count |> should.equal(1)
      new_store
    }
    Error(_) -> { should.fail() store }
  }

  // Verify only one remains
  case memory.execute_select(store, query.from("users")) {
    Ok(rows) -> list.length(rows) |> should.equal(1)
    Error(_) -> should.fail()
  }
}
```

### Transaction Tests

```gleam
pub fn memory_adapter_transaction_commits_test() {
  let store = memory.new_store()
    |> memory.create_table("accounts", ["id", "balance"])
    |> memory.insert_row("accounts", dict.from_list([
      #("id", ast.IntValue(1)),
      #("balance", ast.IntValue(100)),
    ]))

  // Start transaction
  let #(store, tx) = memory.begin_transaction(store)

  // Make changes within transaction
  let update = ast.new_update("accounts")
    |> ast.set("balance", ast.IntValue(50))
    |> ast.update_where(query.eq("id", ast.IntValue(1)))

  let #(store, _) = case memory.execute_update_tx(store, tx, update) {
    Ok(result) -> result
    Error(_) -> { should.fail() #(store, []) }
  }

  // Commit
  let store = memory.commit_transaction(store, tx)

  // Verify change persisted
  case memory.execute_select(store, query.from("accounts")) {
    Ok([row]) -> {
      dict.get(row, "balance") |> should.equal(Ok(ast.IntValue(50)))
    }
    _ -> should.fail()
  }
}

pub fn memory_adapter_transaction_rollback_test() {
  let store = memory.new_store()
    |> memory.create_table("accounts", ["id", "balance"])
    |> memory.insert_row("accounts", dict.from_list([
      #("id", ast.IntValue(1)),
      #("balance", ast.IntValue(100)),
    ]))

  // Start transaction
  let #(store, tx) = memory.begin_transaction(store)

  // Make changes
  let update = ast.new_update("accounts")
    |> ast.set("balance", ast.IntValue(50))
    |> ast.update_where(query.eq("id", ast.IntValue(1)))

  let #(store, _) = case memory.execute_update_tx(store, tx, update) {
    Ok(result) -> result
    Error(_) -> { should.fail() #(store, []) }
  }

  // Rollback
  let store = memory.rollback_transaction(store, tx)

  // Verify change was reverted
  case memory.execute_select(store, query.from("accounts")) {
    Ok([row]) -> {
      dict.get(row, "balance") |> should.equal(Ok(ast.IntValue(100)))
    }
    _ -> should.fail()
  }
}
```

### Savepoint Tests

```gleam
pub fn memory_adapter_savepoint_rollback_test() {
  let store = memory.new_store()
    |> memory.create_table("items", ["id", "status"])
    |> memory.insert_row("items", dict.from_list([
      #("id", ast.IntValue(1)),
      #("status", ast.StringValue("pending")),
    ]))

  // Start transaction
  let #(store, tx) = memory.begin_transaction(store)

  // First update
  let update1 = ast.new_update("items")
    |> ast.set("status", ast.StringValue("processing"))
    |> ast.update_where(query.eq("id", ast.IntValue(1)))
  let #(store, _) = memory.execute_update_tx(store, tx, update1) |> result.unwrap(#(store, []))

  // Create savepoint
  let #(store, sp) = memory.create_savepoint(store, tx, "before_complete")

  // Second update
  let update2 = ast.new_update("items")
    |> ast.set("status", ast.StringValue("complete"))
    |> ast.update_where(query.eq("id", ast.IntValue(1)))
  let #(store, _) = memory.execute_update_tx(store, tx, update2) |> result.unwrap(#(store, []))

  // Rollback to savepoint
  let store = memory.rollback_to_savepoint(store, tx, sp)

  // Commit transaction
  let store = memory.commit_transaction(store, tx)

  // Verify: should be "processing" (savepoint state), not "complete"
  case memory.execute_select(store, query.from("items")) {
    Ok([row]) -> {
      dict.get(row, "status") |> should.equal(Ok(ast.StringValue("processing")))
    }
    _ -> should.fail()
  }
}
```

## Ring 3: Integration Tests

Test against real databases when needed:

### PostgreSQL Integration Tests

```gleam
import cquill/adapter/postgres

// Only run when DATABASE_URL is set
pub fn postgres_integration_test() {
  case get_test_database_url() {
    Error(_) -> {
      io.println("Skipping: DATABASE_URL not set")
    }
    Ok(url) -> {
      use pool <- setup_test_database(url)
      run_postgres_tests(pool)
    }
  }
}

fn setup_test_database(url: String) -> fn(fn(Pool) -> a) -> a {
  fn(test_fn) {
    // Connect
    let pool = postgres.connect(postgres.parse_url(url))
      |> result.unwrap_or_panic()

    // Start transaction (will be rolled back)
    postgres.begin_transaction(pool)

    // Run test
    let result = test_fn(pool)

    // Rollback to clean up
    postgres.rollback_transaction(pool)

    result
  }
}

fn run_postgres_tests(pool) {
  // Your actual tests here
  let insert = ast.new_insert("users")
    |> ast.insert_columns(["email"])
    |> ast.insert_values([[ast.StringValue("test@example.com")]])
    |> ast.returning(["id"])

  case postgres.execute_insert(pool, insert) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
    }
    Error(e) -> {
      io.println("Test failed: " <> error.format(e))
      should.fail()
    }
  }
}
```

### Test Fixtures

Create reusable test data:

```gleam
pub fn with_test_users(store, test_fn: fn(MemoryStore) -> a) -> a {
  let store = store
    |> memory.create_table("users", ["id", "email", "name", "active"])
    |> memory.insert_row("users", dict.from_list([
      #("id", ast.IntValue(1)),
      #("email", ast.StringValue("alice@example.com")),
      #("name", ast.StringValue("Alice")),
      #("active", ast.BoolValue(True)),
    ]))
    |> memory.insert_row("users", dict.from_list([
      #("id", ast.IntValue(2)),
      #("email", ast.StringValue("bob@example.com")),
      #("name", ast.StringValue("Bob")),
      #("active", ast.BoolValue(False)),
    ]))
    |> memory.insert_row("users", dict.from_list([
      #("id", ast.IntValue(3)),
      #("email", ast.StringValue("carol@example.com")),
      #("name", ast.StringValue("Carol")),
      #("active", ast.BoolValue(True)),
    ]))

  test_fn(store)
}

// Usage
pub fn query_active_users_test() {
  with_test_users(memory.new_store(), fn(store) {
    let query = query.from("users")
      |> query.where(query.eq("active", ast.BoolValue(True)))

    case memory.execute_select(store, query) {
      Ok(rows) -> list.length(rows) |> should.equal(2)
      Error(_) -> should.fail()
    }
  })
}
```

## Testing Error Handling

```gleam
pub fn handles_not_found_error_test() {
  let store = memory.new_store()
    |> memory.create_table("users", ["id"])

  let query = query.from("users")
    |> query.where(query.eq("id", ast.IntValue(999)))

  case memory.execute_one(store, query) {
    Error(error.NotFound) -> should.be_true(True)  // Expected
    Ok(_) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn handles_constraint_violation_test() {
  let store = memory.new_store()
    |> memory.create_table_with_constraints("users", [
      #("email", field.String |> field.unique()),
    ])
    |> memory.insert_row("users", dict.from_list([
      #("email", ast.StringValue("taken@example.com")),
    ]))

  // Try to insert duplicate
  let result = memory.insert_row(store, "users", dict.from_list([
    #("email", ast.StringValue("taken@example.com")),
  ]))

  case result {
    Error(error.UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}
```

## Running Tests

```bash
# Run all tests
gleam test

# Run specific test module
gleam test test/my_module_test.gleam

# Run with verbose output
gleam test -- --verbose
```

## Best Practices

1. **Use memory adapter for unit tests** - Fast and isolated
2. **Test error paths** - Don't just test happy paths
3. **Use fixtures for common setup** - Reduce duplication
4. **Keep integration tests separate** - They're slower
5. **Clean up after tests** - Use transactions that rollback

## Next Steps

- See [CRUD Operations](./crud.md) for operations to test
- Read about [Transactions](./transactions.md) for testing atomic operations
- Check [Error Reference](../reference/errors.md) for error types to test
