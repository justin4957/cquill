# Testing with cquill

This guide covers testing strategies for applications using cquill, from unit tests to integration tests with real databases.

## Table of Contents

- [Testing Philosophy](#testing-philosophy)
- [The Three Rings of Testing](#the-three-rings-of-testing)
- [Ring 1: Unit Tests (Pure Code)](#ring-1-unit-tests-pure-code)
- [Ring 2: Adapter Tests with Memory Adapter](#ring-2-adapter-tests-with-memory-adapter)
- [Ring 3: Integration Tests](#ring-3-integration-tests)
- [Test Helpers Module](#test-helpers-module)
- [Factory Pattern](#factory-pattern)
- [Test Context Pattern](#test-context-pattern)
- [PostgreSQL Sandbox Testing](#postgresql-sandbox-testing)
- [Property-Based Testing](#property-based-testing)
- [Best Practices](#best-practices)

## Testing Philosophy

cquill encourages a layered testing approach that maximizes test speed while ensuring confidence in your code:

```
┌─────────────────────────────────────────┐
│  Ring 3: System/Integration Tests       │  Real database, full flows
│  ┌─────────────────────────────────┐    │
│  │  Ring 2: Adapter Contract Tests │    │  Memory adapter, I/O boundary
│  │  ┌─────────────────────────┐    │    │
│  │  │  Ring 1: Unit Tests     │    │    │  Pure functions, no I/O
│  │  │  (fastest, most tests)  │    │    │
│  │  └─────────────────────────┘    │    │
│  └─────────────────────────────────┘    │
└─────────────────────────────────────────┘
```

**Key principles:**

1. **Most tests should be in Ring 1** - Pure function tests are fast and reliable
2. **Use the memory adapter for Ring 2** - Fast, isolated, deterministic
3. **Reserve Ring 3 for critical paths** - Real database tests catch integration issues
4. **Every layer uses different tools** - Appropriate assertions for each context

## The Three Rings of Testing

### Ring 1: Pure Functions (No I/O)

Test schemas, query builders, validation, and business logic without any database.

**What to test:**
- Query AST generation
- Schema metadata
- Validation functions
- Business logic that transforms data
- Encoder/decoder functions

**Advantages:**
- Extremely fast (milliseconds)
- No setup required
- 100% deterministic
- Easy to debug

### Ring 2: Memory Adapter (I/O Boundary)

Test database operations using the in-memory adapter.

**What to test:**
- CRUD operations
- Transaction behavior
- Constraint enforcement
- Service layer logic

**Advantages:**
- Fast (no network/disk I/O)
- Isolated (each test gets fresh state)
- Deterministic
- Tests adapter contract compliance

### Ring 3: Real Database (Integration)

Test against PostgreSQL for critical paths and edge cases.

**What to test:**
- Complex queries
- Database-specific behavior
- Performance characteristics
- Migration compatibility

**Advantages:**
- Catches real integration issues
- Tests actual SQL generation
- Validates against production-like environment

## Ring 1: Unit Tests (Pure Code)

### Testing Query Builders

```gleam
import gleeunit/should
import cquill/query
import cquill/query/ast

pub fn query_builder_creates_correct_ast_test() {
  let q =
    query.from_table("users")
    |> query.where(query.eq_bool("active", True))
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
    query.from_table("users")
    |> query.where(query.eq_int("a", 1))
    |> query.where(query.eq_int("b", 2))

  // Both conditions are added
  q.wheres |> list.length() |> should.equal(2)
}
```

### Testing Composable Query Functions

```gleam
import cquill/query

// Define composable query functions
pub fn published(q: query.Query(a)) -> query.Query(a) {
  q |> query.where(query.eq_string("status", "published"))
}

pub fn by_author(q: query.Query(a), user_id: Int) -> query.Query(a) {
  q |> query.where(query.eq_int("user_id", user_id))
}

pub fn newest_first(q: query.Query(a)) -> query.Query(a) {
  q |> query.order_by_desc("published_at")
}

// Test that composition works
pub fn query_composition_test() {
  let base = query.from_table("posts")

  let q = base
    |> published()
    |> by_author(42)
    |> newest_first()
    |> query.limit(10)

  // Verify each component was added
  q.wheres |> list.length() |> should.equal(2)
  q.order_bys |> list.length() |> should.equal(1)
  q.limit |> should.equal(Some(10))
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

pub fn validate_user_input(
  input: UserInput,
) -> Result(UserInput, List(ValidationError)) {
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

### Basic CRUD Tests

```gleam
import cquill/adapter/memory
import cquill/testing
import gleam/dynamic
import gleeunit/should

pub fn memory_adapter_insert_and_get_test() {
  use store <- testing.with_memory_store()

  // Setup table
  let store = memory.create_table(store, "users", "id")

  // Insert - the memory adapter uses List(Dynamic) for rows
  let row = [
    dynamic.from(1),           // id
    dynamic.from("alice@example.com"),  // email
    dynamic.from("Alice"),     // name
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Query by key
  case memory.get_row(store, "users", "1") {
    Ok(found_row) -> {
      // Row was found
      list.length(found_row) |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

pub fn memory_adapter_get_all_rows_test() {
  use store <- testing.with_memory_store()

  let store = memory.create_table(store, "users", "id")

  // Insert multiple rows
  let assert Ok(store) = memory.insert_row(store, "users", "1", [
    dynamic.from(1), dynamic.from("alice@example.com"),
  ])
  let assert Ok(store) = memory.insert_row(store, "users", "2", [
    dynamic.from(2), dynamic.from("bob@example.com"),
  ])

  // Get all rows
  let assert Ok(rows) = memory.get_all_rows(store, "users")
  list.length(rows) |> should.equal(2)
}
```

### Transaction Tests

The memory adapter supports transactions with `execute_transaction`:

```gleam
import cquill/adapter/memory
import cquill/testing
import gleam/dynamic
import gleeunit/should

pub fn transaction_commits_on_success_test() {
  use store <- testing.with_memory_store()

  let store = memory.create_table(store, "accounts", "id")
  let assert Ok(store) = memory.insert_row(store, "accounts", "1", [
    dynamic.from(1), dynamic.from(100),
  ])

  // Execute a transaction that succeeds
  let result = memory.execute_transaction(store, fn(tx_store) {
    // Insert a new row in the transaction
    case memory.insert_row(tx_store, "accounts", "2", [
      dynamic.from(2), dynamic.from(200),
    ]) {
      Ok(new_store) -> Ok(#(new_store, "success"))
      Error(e) -> Error(e)
    }
  })

  // Transaction should succeed
  let assert Ok(#(new_store, _)) = result

  // Verify both rows exist
  let assert Ok(rows) = memory.get_all_rows(new_store, "accounts")
  list.length(rows) |> should.equal(2)
}

pub fn transaction_rollback_on_error_test() {
  use store <- testing.with_memory_store()

  let store = memory.create_table(store, "accounts", "id")
  let assert Ok(store) = memory.insert_row(store, "accounts", "1", [
    dynamic.from(1), dynamic.from(100),
  ])

  // Execute a transaction that fails
  let result = memory.execute_transaction(store, fn(tx_store) {
    // Insert succeeds
    let assert Ok(tx_store) = memory.insert_row(tx_store, "accounts", "2", [
      dynamic.from(2), dynamic.from(200),
    ])
    // Then try to insert duplicate key (fails)
    memory.insert_row(tx_store, "accounts", "1", [
      dynamic.from(1), dynamic.from(999),
    ])
  })

  // Transaction should fail
  result |> should.be_error()

  // Original row should still exist, new row should not
  let assert Ok(rows) = memory.get_all_rows(store, "accounts")
  list.length(rows) |> should.equal(1)
}
```

### Testing Row Existence

```gleam
import cquill/adapter/memory
import cquill/testing
import gleam/dynamic
import gleeunit/should

pub fn verify_row_exists_test() {
  use store <- testing.with_memory_store()

  let store = memory.create_table(store, "users", "id")
  let assert Ok(store) = memory.insert_row(store, "users", "1", [
    dynamic.from(1), dynamic.from("alice@example.com"),
  ])
  let assert Ok(store) = memory.insert_row(store, "users", "2", [
    dynamic.from(2), dynamic.from("bob@example.com"),
  ])

  // Verify row exists by key
  memory.get_row(store, "users", "1") |> should.be_ok()
  memory.get_row(store, "users", "2") |> should.be_ok()

  // Verify row does not exist
  memory.get_row(store, "users", "999") |> should.be_error()

  // Verify count
  let assert Ok(rows) = memory.get_all_rows(store, "users")
  list.length(rows) |> should.equal(2)
}
```

## Ring 3: Integration Tests

### PostgreSQL Integration Tests

```gleam
import cquill/adapter/postgres
import gleam/os

// Only run when DATABASE_URL is set
pub fn postgres_integration_test() {
  case os.get_env("DATABASE_URL") {
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
    let assert Ok(pool) = postgres.connect(postgres.parse_url(url))

    // Start transaction (will be rolled back)
    let assert Ok(_) = postgres.begin_transaction(pool)

    // Run test
    let result = test_fn(pool)

    // Rollback to clean up
    let _ = postgres.rollback_transaction(pool)

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

## Test Helpers Module

cquill provides a `cquill/testing` module with utilities for common testing tasks.

### Using the Testing Module

```gleam
import cquill/testing
import cquill/adapter/memory
import gleam/dynamic
import gleeunit/should

pub fn memory_store_test() {
  // Get a fresh memory store using the callback pattern
  use store <- testing.with_memory_store()

  // Each test gets an isolated, fresh store
  let store = memory.create_table(store, "users", "id")
  let assert Ok(store) = memory.insert_row(store, "users", "1", [
    dynamic.from(1), dynamic.from("test@example.com"),
  ])

  let assert Ok(rows) = memory.get_all_rows(store, "users")
  list.length(rows) |> should.equal(1)
}

pub fn context_with_unique_data_test() {
  // Use a full test context for unique ID generation
  use ctx <- testing.with_context()

  // Generate unique IDs
  let #(ctx, id) = testing.next_id(ctx)

  // Generate unique strings
  let #(ctx, email) = testing.unique_email(ctx)
  // email = "test_1@example.com"

  // Access the store from the context
  let store = testing.get_store(ctx)
  let store = memory.create_table(store, "users", "id")

  let assert Ok(store) = memory.insert_row(store, "users", int.to_string(id), [
    dynamic.from(id), dynamic.from(email),
  ])

  // Update the context with the new store
  let ctx = testing.set_store(ctx, store)

  let assert Ok(rows) = memory.get_all_rows(testing.get_store(ctx), "users")
  list.length(rows) |> should.equal(1)
}
```

### Test Context

The test context tracks state across test operations:

```gleam
import cquill/testing.{type TestContext}
import gleeunit/should

pub fn context_tracks_state_test() {
  use ctx <- testing.with_context()

  // Generate unique IDs (counter increments)
  let #(ctx, id1) = testing.next_id(ctx)
  let #(ctx, id2) = testing.next_id(ctx)

  id1 |> should.equal(1)
  id2 |> should.equal(2)

  // Generate multiple IDs at once
  let #(ctx, ids) = testing.next_ids(ctx, 3)
  ids |> should.equal([3, 4, 5])

  // Track inserted entity keys for verification
  let ctx = testing.track_insert(ctx, "users", "1")
  let ctx = testing.track_insert(ctx, "users", "2")

  // Query tracked entities
  testing.tracked_count(ctx, "users") |> should.equal(2)
  testing.get_tracked(ctx, "users") |> should.equal(["2", "1"])
}
```

### Creating Multiple Tables

```gleam
import cquill/testing
import cquill/adapter/memory

pub fn setup_multiple_tables_test() {
  use store <- testing.with_memory_store()

  // Create multiple tables at once
  let store = testing.create_tables(store, [
    #("users", "id"),
    #("posts", "id"),
    #("comments", "id"),
  ])

  // All tables are now ready for testing
  let assert Ok(user_rows) = memory.get_all_rows(store, "users")
  let assert Ok(post_rows) = memory.get_all_rows(store, "posts")
  let assert Ok(comment_rows) = memory.get_all_rows(store, "comments")

  list.length(user_rows) |> should.equal(0)
  list.length(post_rows) |> should.equal(0)
  list.length(comment_rows) |> should.equal(0)
}
```

### Unique String Generation

```gleam
import cquill/testing

pub fn unique_strings_test() {
  use ctx <- testing.with_context()

  // Generate unique emails
  let #(ctx, email1) = testing.unique_email(ctx)
  let #(ctx, email2) = testing.unique_email(ctx)
  email1 |> should.equal("test_1@example.com")
  email2 |> should.equal("test_2@example.com")

  // Generate unique usernames
  let #(ctx, username) = testing.unique_username(ctx)
  username |> should.equal("user_3")

  // Generate custom unique strings
  let #(_ctx, custom) = testing.unique_string(ctx, "item", "_code")
  custom |> should.equal("item_4_code")
}
```

## Factory Pattern

The factory pattern creates test data with sensible defaults, making tests readable and maintainable.

### Basic Factory Module

```gleam
// test/support/factory.gleam

import gleam/option.{type Option, None, Some}

/// Data for creating a new user
pub type NewUser {
  NewUser(email: String, name: Option(String), password: String)
}

/// Create a NewUser with default values
pub fn new_user() -> NewUser {
  NewUser(
    email: "testuser@example.com",
    name: Some("Test User"),
    password: "password123",
  )
}

/// Create a NewUser with custom email
pub fn new_user_with_email(email: String) -> NewUser {
  NewUser(..new_user(), email: email)
}

/// Create a NewUser with no name
pub fn new_user_anonymous(email: String) -> NewUser {
  NewUser(email: email, name: None, password: "password123")
}
```

### Factory with Unique Values

For tests that need unique values, combine factories with the test context:

```gleam
// test/support/factory.gleam

import cquill/testing.{type TestContext}

/// Create a NewUser with a unique email
pub fn new_unique_user(ctx: TestContext) -> #(TestContext, NewUser) {
  let #(ctx, email) = testing.unique_email(ctx)
  let user = NewUser(
    email: email,
    name: Some("Test User"),
    password: "password123",
  )
  #(ctx, user)
}

/// Create multiple unique users
pub fn new_unique_users(ctx: TestContext, count: Int) -> #(TestContext, List(NewUser)) {
  list.range(1, count)
  |> list.fold(#(ctx, []), fn(acc, _i) {
    let #(ctx, users) = acc
    let #(ctx, user) = new_unique_user(ctx)
    #(ctx, [user, ..users])
  })
}
```

### Factory for Related Entities

```gleam
/// Data for creating a new post
pub type NewPost {
  NewPost(user_id: Int, title: String, body: String, status: PostStatus)
}

pub type PostStatus {
  Draft
  Published
}

/// Create a NewPost with default values (draft)
pub fn new_post(user_id: Int) -> NewPost {
  NewPost(
    user_id: user_id,
    title: "Test Post",
    body: "This is the body of the test post.",
    status: Draft,
  )
}

/// Create a NewPost that's published
pub fn new_published_post(user_id: Int) -> NewPost {
  NewPost(..new_post(user_id), status: Published)
}

/// Create a NewPost with custom title
pub fn new_post_with_title(user_id: Int, title: String) -> NewPost {
  NewPost(..new_post(user_id), title: title)
}
```

### Using Factories in Tests

```gleam
import support/factory
import support/setup

pub fn create_user_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let result = accounts.create_user(ctx.user_store, new_user)

  let assert Ok(#(_, user)) = result
  user.email |> should.equal("testuser@example.com")
}

pub fn multiple_users_with_unique_emails_test() {
  use ctx <- setup.with_context()

  let user1 = factory.new_user_with_email("user1@example.com")
  let user2 = factory.new_user_with_email("user2@example.com")

  let assert Ok(#(store, _)) = accounts.create_user(ctx.user_store, user1)
  let assert Ok(#(store, _)) = accounts.create_user(store, user2)

  let users = accounts.list_users(store)
  list.length(users) |> should.equal(2)
}
```

## Test Context Pattern

The test context pattern provides a structured way to manage test state across related operations.

### Defining a Test Context

```gleam
// test/support/setup.gleam

import your_app/services/accounts
import your_app/services/posts
import your_app/services/comments

/// Test context containing all stores
pub type TestContext {
  TestContext(
    user_store: accounts.UserStore,
    post_store: posts.PostStore,
    comment_store: comments.CommentStore,
  )
}

/// Create a fresh test context
pub fn new_context() -> TestContext {
  TestContext(
    user_store: accounts.new_store(),
    post_store: posts.new_store(),
    comment_store: comments.new_store(),
  )
}

/// Execute a test function with a fresh context
pub fn with_context(test_fn: fn(TestContext) -> a) -> a {
  let ctx = new_context()
  test_fn(ctx)
}
```

### Using the Test Context

```gleam
import support/setup
import support/factory

pub fn full_workflow_test() {
  use ctx <- setup.with_context()

  // Create a user
  let new_user = factory.new_user()
  let assert Ok(#(user_store, user)) = accounts.create_user(ctx.user_store, new_user)

  // Create a post for the user
  let new_post = factory.new_post(user.id)
  let assert Ok(#(post_store, post)) = posts.create_post(ctx.post_store, new_post)

  // Create a comment on the post
  let new_comment = factory.new_comment(post.id)
  let assert Ok(#(comment_store, comment)) = comments.create_comment(ctx.comment_store, new_comment)

  // Assertions
  comment.post_id |> should.equal(post.id)
  post.user_id |> should.equal(user.id)
}
```

## PostgreSQL Sandbox Testing

For integration tests with PostgreSQL, use the sandbox pattern to ensure test isolation.

### Sandbox Pattern

The sandbox pattern wraps each test in a transaction that gets rolled back:

```gleam
// test/support/sandbox.gleam

import cquill/adapter/postgres
import gleam/erlang/process

/// Run a test function within a sandboxed database transaction.
/// The transaction is automatically rolled back after the test completes.
pub fn with_sandbox(
  pool: postgres.Pool,
  test_fn: fn(postgres.Connection) -> a,
) -> a {
  // Check out a connection from the pool
  let assert Ok(conn) = postgres.checkout(pool)

  // Begin transaction
  let assert Ok(_) = postgres.query(conn, "BEGIN", [])

  // Run the test
  let result = test_fn(conn)

  // Rollback transaction (cleanup)
  let _ = postgres.query(conn, "ROLLBACK", [])

  // Return connection to pool
  postgres.checkin(pool, conn)

  result
}

/// Run a test with a sandboxed connection that supports nested savepoints.
pub fn with_savepoint_sandbox(
  pool: postgres.Pool,
  test_fn: fn(postgres.Connection, fn(String) -> Nil) -> a,
) -> a {
  use conn <- with_sandbox(pool)

  let savepoint_counter = process.new_subject()
  process.send(savepoint_counter, 0)

  let create_savepoint = fn(name: String) {
    let _ = postgres.query(conn, "SAVEPOINT " <> name, [])
    Nil
  }

  test_fn(conn, create_savepoint)
}
```

### Using the Sandbox in Tests

```gleam
import support/sandbox
import gleam/os

fn get_test_pool() -> Result(postgres.Pool, String) {
  case os.get_env("DATABASE_URL") {
    Error(_) -> Error("DATABASE_URL not set")
    Ok(url) -> postgres.connect(postgres.parse_url(url))
  }
}

pub fn postgres_crud_test() {
  case get_test_pool() {
    Error(msg) -> io.println("Skipping: " <> msg)
    Ok(pool) -> {
      use conn <- sandbox.with_sandbox(pool)

      // Insert a user (will be rolled back)
      let assert Ok(_) = postgres.query(
        conn,
        "INSERT INTO users (email) VALUES ($1)",
        [postgres.string("test@example.com")],
      )

      // Verify it exists
      let assert Ok(rows) = postgres.query(
        conn,
        "SELECT email FROM users WHERE email = $1",
        [postgres.string("test@example.com")],
      )
      list.length(rows) |> should.equal(1)

      // After test ends, transaction is rolled back automatically
    }
  }
}
```

### CI Configuration for PostgreSQL Tests

```yaml
# .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:16
        env:
          POSTGRES_USER: postgres
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: test_db
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v4

      - name: Install Gleam
        uses: gleam-lang/setup-gleam@v1

      - name: Run tests
        env:
          DATABASE_URL: postgres://postgres:postgres@localhost:5432/test_db
        run: gleam test
```

## Property-Based Testing

Property-based testing helps find edge cases by generating random test inputs.

### Basic Property Tests with qcheck

```gleam
import gleam/qcheck

/// Property: Query composition should be associative for independent conditions
pub fn query_composition_associative_test() {
  use <- qcheck.given(qcheck.int())
  fn(id) {
    let q1 = query.from_table("users")
      |> query.where(query.eq_int("id", id))
      |> query.where(query.eq_bool("active", True))

    let q2 = query.from_table("users")
      |> query.where(query.eq_bool("active", True))
      |> query.where(query.eq_int("id", id))

    // Both should have the same number of conditions
    list.length(q1.wheres) == list.length(q2.wheres)
  }
}
```

### Testing Invariants

```gleam
import gleam/qcheck
import cquill/adapter/memory
import cquill/testing
import gleam/dynamic
import gleam/int

/// Property: Inserting then retrieving should return matching data
pub fn insert_retrieve_roundtrip_test() {
  use <- qcheck.given(qcheck.string())
  fn(email) {
    use store <- testing.with_memory_store()

    let store = memory.create_table(store, "users", "id")
    let row = [dynamic.from(1), dynamic.from(email)]

    case memory.insert_row(store, "users", "1", row) {
      Ok(store) -> {
        case memory.get_row(store, "users", "1") {
          Ok(found_row) -> {
            // Row should have 2 elements
            list.length(found_row) == 2
          }
          Error(_) -> False
        }
      }
      Error(_) -> True  // Insertion errors are acceptable for property tests
    }
  }
}

/// Property: Count should never be negative
pub fn count_never_negative_test() {
  use <- qcheck.given(qcheck.list(qcheck.int()))
  fn(ids) {
    use store <- testing.with_memory_store()

    let store = memory.create_table(store, "items", "id")

    // Try to insert each ID (some may fail as duplicates)
    let store = list.fold(ids, store, fn(acc, id) {
      let key = int.to_string(id)
      case memory.insert_row(acc, "items", key, [dynamic.from(id)]) {
        Ok(new_store) -> new_store
        Error(_) -> acc  // Keep existing store on duplicate key
      }
    })

    case memory.get_all_rows(store, "items") {
      Ok(rows) -> list.length(rows) >= 0
      Error(_) -> True
    }
  }
}
```

### Custom Generators

```gleam
import gleam/qcheck

/// Generate a valid email address
fn email_generator() -> qcheck.Generator(String) {
  qcheck.map2(
    qcheck.string_non_empty(),
    qcheck.string_non_empty(),
    fn(local, domain) { local <> "@" <> domain <> ".com" },
  )
}

/// Generate a user input
fn user_input_generator() -> qcheck.Generator(UserInput) {
  qcheck.map3(
    email_generator(),
    qcheck.string(),
    qcheck.int_uniform(0, 120),
    fn(email, name, age) { UserInput(email, name, age) },
  )
}

pub fn validation_property_test() {
  use <- qcheck.given(user_input_generator())
  fn(input) {
    case validate_user_input(input) {
      Ok(_) -> {
        // If validation passes, all constraints must be met
        string.contains(input.email, "@") &&
        string.length(input.name) >= 2 &&
        input.age >= 18
      }
      Error(errors) -> {
        // If validation fails, at least one constraint is violated
        !string.contains(input.email, "@") ||
        string.length(input.name) < 2 ||
        input.age < 18
      }
    }
  }
}
```

## Best Practices

### 1. Use the Memory Adapter for Most Tests

The memory adapter is fast and deterministic. Reserve PostgreSQL tests for integration scenarios.

```gleam
// Good: Fast, isolated test
pub fn user_creation_test() {
  use store <- testing.with_memory_store()
  // ...
}

// Only when needed: Slow, requires setup
pub fn postgres_specific_behavior_test() {
  use conn <- sandbox.with_sandbox(get_pool())
  // ...
}
```

### 2. Test Error Paths

Don't just test happy paths. Test error handling too.

```gleam
pub fn handles_not_found_error_test() {
  use store <- testing.with_memory_store()

  let store = memory.create_table(store, "users", "id")

  let result = accounts.get_user(store, 999)

  result |> should.equal(Error(accounts.UserNotFound))
}

pub fn handles_duplicate_email_test() {
  use store <- testing.with_memory_store()

  let new_user = factory.new_user()
  let assert Ok(#(store, _)) = accounts.create_user(store, new_user)

  // Try to create again with same email
  let result = accounts.create_user(store, new_user)

  result |> should.equal(Error(accounts.EmailAlreadyExists))
}
```

### 3. Use Factories for Readable Tests

Factories make tests more readable by hiding irrelevant details.

```gleam
// Hard to read: lots of irrelevant detail
pub fn bad_test() {
  let user = NewUser(
    email: "test@example.com",
    name: Some("Test User"),
    password: "password123",
    role: Author,
    created_at: "2024-01-01",
    updated_at: "2024-01-01",
  )
  // ...
}

// Easy to read: only shows what matters
pub fn good_test() {
  let user = factory.new_user()
  // ...
}
```

### 4. Keep Tests Independent

Each test should set up its own state and not depend on other tests.

```gleam
// Bad: Tests depend on shared state
let shared_store = memory.new_store()

pub fn test_1() {
  // Modifies shared_store
}

pub fn test_2() {
  // Depends on state from test_1
}

// Good: Each test is isolated
pub fn test_1() {
  use store <- testing.with_memory_store()
  // ...
}

pub fn test_2() {
  use store <- testing.with_memory_store()
  // ...
}
```

### 5. Use Descriptive Test Names

Name tests to describe the scenario and expected behavior.

```gleam
// Bad: Vague name
pub fn user_test() { ... }

// Good: Describes scenario and expectation
pub fn create_user_with_valid_email_succeeds_test() { ... }
pub fn create_user_with_duplicate_email_returns_error_test() { ... }
pub fn authenticate_with_wrong_password_fails_test() { ... }
```

### 6. Clean Up After Integration Tests

Always ensure integration tests clean up, even if they fail.

```gleam
// Good: Transaction rollback ensures cleanup
pub fn integration_test() {
  use conn <- sandbox.with_sandbox(pool)
  // Test runs in transaction that's always rolled back
}

// Also good: Explicit cleanup in finally block
pub fn integration_test_manual() {
  let conn = get_connection()
  try {
    // Test code
  } finally {
    cleanup(conn)
  }
}
```

## Running Tests

```bash
# Run all tests
gleam test

# Run with verbose output
gleam test -- --verbose

# Run specific test file (gleeunit)
gleam test test/my_module_test.gleam
```

## Next Steps

- See [CRUD Operations](./crud.md) for operations to test
- Read about [Transactions](./transactions.md) for testing atomic operations
- Check the [Blog Example](../../examples/blog/) for a complete testing setup
- Review [Adapter Contract](../ADAPTER_CONTRACT.md) for adapter test requirements
