/// Test utilities for cquill applications.
///
/// This module provides helpers for testing applications that use cquill,
/// including test context management and factory utilities for generating
/// unique test data.
///
/// ## Example Usage
///
/// ```gleam
/// import cquill/testing.{with_memory_store, with_context}
/// import cquill/adapter/memory
/// import gleeunit/should
///
/// pub fn my_test() {
///   use store <- with_memory_store()
///
///   // Test with a fresh memory store
///   let assert Ok(store) = memory.create_table(store, "users", [])
///   // ... perform test operations
/// }
///
/// pub fn test_with_unique_data() {
///   use ctx <- with_context()
///
///   let #(ctx, user_id) = next_id(ctx)
///   let #(ctx, email) = unique_email(ctx)
///   // user_id = 1, email = "test_1@example.com"
/// }
/// ```
import cquill/adapter/memory.{type MemoryStore}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list

// ============================================================================
// TEST CONTEXT TYPES
// ============================================================================

/// A test context containing a memory store and tracking information.
///
/// The test context provides:
/// - A fresh memory store for database operations
/// - An auto-incrementing ID counter for generating unique IDs
/// - Tracking of inserted entities for verification
pub type TestContext {
  TestContext(
    /// The memory store for database operations
    store: MemoryStore,
    /// Counter for generating unique IDs in factories
    id_counter: Int,
    /// Tracks inserted entity IDs by table name for verification
    inserted: Dict(String, List(String)),
  )
}

// ============================================================================
// TEST CONTEXT MANAGEMENT
// ============================================================================

/// Create a new test context with a fresh memory store.
///
/// ## Example
///
/// ```gleam
/// let ctx = new_context()
/// // ctx.id_counter starts at 1
/// ```
pub fn new_context() -> TestContext {
  TestContext(store: memory.new_store(), id_counter: 1, inserted: dict.new())
}

/// Run a test function with a fresh memory store.
///
/// This is the recommended way to write tests that need a memory store.
/// Each test gets a completely fresh store, ensuring test isolation.
///
/// ## Example
///
/// ```gleam
/// pub fn insert_user_test() {
///   use store <- with_memory_store()
///
///   let assert Ok(store) = memory.create_table(store, "users", [])
///   // ... test code
/// }
/// ```
pub fn with_memory_store(test_fn: fn(MemoryStore) -> a) -> a {
  let store = memory.new_store()
  test_fn(store)
}

/// Run a test function with a fresh test context.
///
/// The context provides additional utilities like ID generation and
/// entity tracking beyond what the raw memory store provides.
///
/// ## Example
///
/// ```gleam
/// pub fn multi_entity_test() {
///   use ctx <- with_context()
///
///   let #(ctx, user_id) = next_id(ctx)
///   let #(ctx, email) = unique_email(ctx)
///   // ... test code using the context
/// }
/// ```
pub fn with_context(test_fn: fn(TestContext) -> a) -> a {
  let ctx = new_context()
  test_fn(ctx)
}

/// Get the memory store from a test context.
///
/// ## Example
///
/// ```gleam
/// let store = get_store(ctx)
/// let result = memory.get_row(store, "users", "1")
/// ```
pub fn get_store(ctx: TestContext) -> MemoryStore {
  ctx.store
}

/// Update the memory store in a test context.
///
/// Use this after performing operations that return a new store.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(new_store) = memory.create_table(ctx.store, "users", [])
/// let ctx = set_store(ctx, new_store)
/// ```
pub fn set_store(ctx: TestContext, store: MemoryStore) -> TestContext {
  TestContext(..ctx, store: store)
}

/// Reset a test context, clearing all data but preserving table structure.
///
/// Useful for running multiple scenarios within the same test.
///
/// ## Example
///
/// ```gleam
/// // After first scenario
/// let ctx = reset(ctx)
/// // Tables still exist but are empty, ID counter resets to 1
/// ```
pub fn reset(ctx: TestContext) -> TestContext {
  TestContext(
    store: memory.reset(ctx.store),
    id_counter: 1,
    inserted: dict.new(),
  )
}

// ============================================================================
// ID GENERATION
// ============================================================================

/// Generate the next unique ID for test data.
/// Returns the updated context and the new ID.
///
/// IDs start at 1 and increment with each call.
///
/// ## Example
///
/// ```gleam
/// let #(ctx, id1) = next_id(ctx)
/// let #(ctx, id2) = next_id(ctx)
/// // id1 = 1, id2 = 2
/// ```
pub fn next_id(ctx: TestContext) -> #(TestContext, Int) {
  let id = ctx.id_counter
  let new_ctx = TestContext(..ctx, id_counter: id + 1)
  #(new_ctx, id)
}

/// Generate a unique string for test data (useful for emails, usernames, etc.)
///
/// Combines prefix, unique ID, and suffix to create unique values.
///
/// ## Example
///
/// ```gleam
/// let #(ctx, email) = unique_string(ctx, "user", "@example.com")
/// // email = "user_1@example.com"
///
/// let #(ctx, username) = unique_string(ctx, "testuser", "")
/// // username = "testuser_2"
/// ```
pub fn unique_string(
  ctx: TestContext,
  prefix: String,
  suffix: String,
) -> #(TestContext, String) {
  let #(new_ctx, id) = next_id(ctx)
  let value = prefix <> "_" <> int.to_string(id) <> suffix
  #(new_ctx, value)
}

/// Generate a unique email for test data.
///
/// Convenience function that generates emails like "test_1@example.com".
///
/// ## Example
///
/// ```gleam
/// let #(ctx, email1) = unique_email(ctx)
/// let #(ctx, email2) = unique_email(ctx)
/// // email1 = "test_1@example.com"
/// // email2 = "test_2@example.com"
/// ```
pub fn unique_email(ctx: TestContext) -> #(TestContext, String) {
  unique_string(ctx, "test", "@example.com")
}

/// Generate a unique username for test data.
///
/// ## Example
///
/// ```gleam
/// let #(ctx, username) = unique_username(ctx)
/// // username = "user_1"
/// ```
pub fn unique_username(ctx: TestContext) -> #(TestContext, String) {
  unique_string(ctx, "user", "")
}

// ============================================================================
// ENTITY TRACKING
// ============================================================================

/// Track an inserted entity key for later verification or cleanup.
///
/// This is useful for tracking what entities were created during a test
/// so you can verify they exist or clean them up.
///
/// ## Example
///
/// ```gleam
/// let ctx = track_insert(ctx, "users", "1")
/// let ctx = track_insert(ctx, "users", "2")
/// tracked_count(ctx, "users")  // Returns 2
/// ```
pub fn track_insert(ctx: TestContext, table: String, key: String) -> TestContext {
  let existing = case dict.get(ctx.inserted, table) {
    Ok(keys) -> keys
    Error(_) -> []
  }
  let updated = dict.insert(ctx.inserted, table, [key, ..existing])
  TestContext(..ctx, inserted: updated)
}

/// Get all tracked entity keys for a table.
///
/// ## Example
///
/// ```gleam
/// let keys = get_tracked(ctx, "users")
/// // Returns ["2", "1"] (most recent first)
/// ```
pub fn get_tracked(ctx: TestContext, table: String) -> List(String) {
  case dict.get(ctx.inserted, table) {
    Ok(keys) -> keys
    Error(_) -> []
  }
}

/// Get the count of tracked entities for a table.
///
/// ## Example
///
/// ```gleam
/// let count = tracked_count(ctx, "users")
/// // Returns number of entities tracked for "users" table
/// ```
pub fn tracked_count(ctx: TestContext, table: String) -> Int {
  list.length(get_tracked(ctx, table))
}

// ============================================================================
// TEST SETUP HELPERS
// ============================================================================

/// Create multiple tables in the memory store.
///
/// Each table is specified as a tuple of (table_name, primary_key_column).
/// This is a convenience function for setting up test fixtures.
///
/// ## Example
///
/// ```gleam
/// let store = create_tables(store, [
///   #("users", "id"),
///   #("posts", "id"),
///   #("comments", "id"),
/// ])
/// ```
pub fn create_tables(
  store: MemoryStore,
  tables: List(#(String, String)),
) -> MemoryStore {
  list.fold(tables, store, fn(acc_store, table_def) {
    let #(name, primary_key) = table_def
    memory.create_table(acc_store, name, primary_key)
  })
}

/// Batch generate unique IDs.
///
/// ## Example
///
/// ```gleam
/// let #(ctx, ids) = next_ids(ctx, 3)
/// // ids = [1, 2, 3]
/// ```
pub fn next_ids(ctx: TestContext, count: Int) -> #(TestContext, List(Int)) {
  list.range(1, count)
  |> list.fold(#(ctx, []), fn(acc, _) {
    let #(current_ctx, ids) = acc
    let #(new_ctx, id) = next_id(current_ctx)
    #(new_ctx, list.append(ids, [id]))
  })
}
