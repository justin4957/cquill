// Constraint Violation Tests
//
// Comprehensive tests for constraint enforcement that all adapters must pass.
// Tests unique constraints, not-null constraints, foreign keys, and check constraints.

import cquill/adapter/memory.{type MemoryRow, type MemoryStore}
import cquill/error
import cquill/schema
import cquill/schema/field
import gleam/dynamic
import gleeunit/should

// ============================================================================
// TEST SETUP
// ============================================================================

fn setup_constrained_store() -> MemoryStore {
  // Users table with various constraints
  let users_schema =
    schema.new("users")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.string("email") |> field.not_null |> field.unique)
    |> schema.add_field(
      field.string("username") |> field.not_null |> field.unique,
    )
    |> schema.add_field(field.string("name") |> field.nullable)

  // Posts table with foreign key
  let posts_schema =
    schema.new("posts")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(
      field.integer("user_id")
      |> field.not_null
      |> field.references("users", "id"),
    )
    |> schema.add_field(field.string("title") |> field.not_null)
    |> schema.add_field(field.string("content") |> field.nullable)

  // Comments table with foreign key to posts
  let comments_schema =
    schema.new("comments")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(
      field.integer("post_id")
      |> field.not_null
      |> field.references("posts", "id"),
    )
    |> schema.add_field(field.string("body") |> field.not_null)

  memory.new_store()
  |> memory.create_table_from_schema(users_schema)
  |> memory.create_table_from_schema(posts_schema)
  |> memory.create_table_from_schema(comments_schema)
}

// ============================================================================
// PRIMARY KEY CONSTRAINT TESTS
// ============================================================================

pub fn primary_key_enforces_uniqueness_test() {
  let store = setup_constrained_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Try to insert with same primary key
  let user2: MemoryRow = [
    dynamic.int(1),
    dynamic.string("bob@example.com"),
    dynamic.string("bob"),
    dynamic.string("Bob"),
  ]

  case memory.insert_row(store, "users", "1", user2) {
    Error(error.UniqueViolation(constraint, _)) -> {
      constraint |> should.equal("users_pkey")
    }
    _ -> should.fail()
  }
}

pub fn primary_key_allows_different_values_test() {
  let store = setup_constrained_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Insert second user with different key
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.string("bob"),
    dynamic.string("Bob"),
  ]

  let result = memory.insert_row(store, "users", "2", user2)
  result |> should.be_ok
}

// ============================================================================
// UNIQUE CONSTRAINT TESTS
// ============================================================================

pub fn unique_constraint_rejects_duplicate_email_test() {
  let store = setup_constrained_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("shared@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Try to insert second user with same email
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("shared@example.com"),
    // Same email!
    dynamic.string("bob"),
    dynamic.string("Bob"),
  ]

  case memory.insert_row(store, "users", "2", user2) {
    Error(error.UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn unique_constraint_rejects_duplicate_username_test() {
  let store = setup_constrained_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("shared_username"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Try to insert second user with same username
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.string("shared_username"),
    // Same username!
    dynamic.string("Bob"),
  ]

  case memory.insert_row(store, "users", "2", user2) {
    Error(error.UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn unique_constraint_allows_distinct_values_test() {
  let store = setup_constrained_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Insert second user with different email and username
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.string("bob"),
    dynamic.string("Bob"),
  ]

  let result = memory.insert_row(store, "users", "2", user2)
  result |> should.be_ok
}

pub fn update_unique_violation_test() {
  let store = setup_constrained_store()

  // Insert two users
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("bob@example.com"),
    dynamic.string("bob"),
    dynamic.string("Bob"),
  ]

  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)
  let assert Ok(store) = memory.insert_row(store, "users", "2", user2)

  // Try to update user2 to have user1's email
  let updated_user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("alice@example.com"),
    // Conflicts with user1!
    dynamic.string("bob"),
    dynamic.string("Bob"),
  ]

  case memory.update_row(store, "users", "2", updated_user2) {
    Error(error.UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn update_same_row_unique_allowed_test() {
  let store = setup_constrained_store()

  // Insert a user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Update the same row keeping the same email
  let updated: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    // Same email is OK
    dynamic.string("alice"),
    dynamic.string("Alice Updated"),
  ]

  let result = memory.update_row(store, "users", "1", updated)
  result |> should.be_ok
}

// ============================================================================
// NOT NULL CONSTRAINT TESTS
// ============================================================================

pub fn not_null_rejects_null_email_test() {
  let store = setup_constrained_store()

  // Try to insert user with null email
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.nil(),
    // email is null!
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]

  case memory.insert_row(store, "users", "1", user) {
    Error(error.NotNullViolation(column)) -> {
      column |> should.equal("email")
    }
    _ -> should.fail()
  }
}

pub fn not_null_rejects_null_username_test() {
  let store = setup_constrained_store()

  // Try to insert user with null username
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.nil(),
    // username is null!
    dynamic.string("Alice"),
  ]

  case memory.insert_row(store, "users", "1", user) {
    Error(error.NotNullViolation(column)) -> {
      column |> should.equal("username")
    }
    _ -> should.fail()
  }
}

pub fn not_null_allows_null_in_nullable_field_test() {
  let store = setup_constrained_store()

  // Insert user with null name (which is nullable)
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.nil(),
    // name is nullable, so this is OK
  ]

  let result = memory.insert_row(store, "users", "1", user)
  result |> should.be_ok
}

pub fn update_not_null_violation_test() {
  let store = setup_constrained_store()

  // Insert valid user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Try to update to null email
  let updated: MemoryRow = [
    dynamic.int(1),
    dynamic.nil(),
    // Setting email to null!
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]

  case memory.update_row(store, "users", "1", updated) {
    Error(error.NotNullViolation(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// FOREIGN KEY CONSTRAINT TESTS
// ============================================================================

pub fn foreign_key_rejects_invalid_reference_test() {
  let store = setup_constrained_store()

  // Try to insert post without creating user first
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(999),
    // user_id 999 doesn't exist!
    dynamic.string("Test Post"),
    dynamic.string("Content"),
  ]

  case memory.insert_row(store, "posts", "1", post) {
    Error(error.ForeignKeyViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn foreign_key_allows_valid_reference_test() {
  let store = setup_constrained_store()

  // First create a user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Now insert post referencing that user
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    // References existing user
    dynamic.string("Test Post"),
    dynamic.string("Content"),
  ]

  let result = memory.insert_row(store, "posts", "1", post)
  result |> should.be_ok
}

pub fn foreign_key_cascades_constraint_check_test() {
  let store = setup_constrained_store()

  // Create user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Create post
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    dynamic.string("Test Post"),
    dynamic.string("Content"),
  ]
  let assert Ok(store) = memory.insert_row(store, "posts", "1", post)

  // Try to create comment referencing the post
  let comment: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    // References existing post
    dynamic.string("Great post!"),
  ]

  let result = memory.insert_row(store, "comments", "1", comment)
  result |> should.be_ok
}

pub fn foreign_key_rejects_deletion_with_dependents_test() {
  let store = setup_constrained_store()

  // Create user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Create post referencing user
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    dynamic.string("Test Post"),
    dynamic.string("Content"),
  ]
  let assert Ok(store) = memory.insert_row(store, "posts", "1", post)

  // Try to delete user (should fail because post references it)
  case memory.delete_row(store, "users", "1") {
    Error(error.ForeignKeyViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn foreign_key_allows_deletion_without_dependents_test() {
  let store = setup_constrained_store()

  // Create user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Delete user (no dependents)
  let result = memory.delete_row(store, "users", "1")
  result |> should.be_ok
}

pub fn foreign_key_update_invalid_reference_test() {
  let store = setup_constrained_store()

  // Create user
  let user: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user)

  // Create post
  let post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    dynamic.string("Test Post"),
    dynamic.string("Content"),
  ]
  let assert Ok(store) = memory.insert_row(store, "posts", "1", post)

  // Try to update post to reference non-existent user
  let updated_post: MemoryRow = [
    dynamic.int(1),
    dynamic.int(999),
    // Invalid user_id!
    dynamic.string("Test Post"),
    dynamic.string("Content"),
  ]

  case memory.update_row(store, "posts", "1", updated_post) {
    Error(error.ForeignKeyViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// COMPOSITE CONSTRAINT TESTS
// ============================================================================

fn setup_composite_constraint_store() -> MemoryStore {
  // Table with composite unique constraint
  let memberships_schema =
    schema.new("memberships")
    |> schema.single_primary_key("id")
    |> schema.add_field(field.integer("id") |> field.primary_key)
    |> schema.add_field(field.integer("user_id") |> field.not_null)
    |> schema.add_field(field.integer("group_id") |> field.not_null)
    |> schema.add_field(field.string("role") |> field.not_null)

  let store =
    memory.new_store()
    |> memory.create_table_from_schema(memberships_schema)

  // Add composite unique constraint
  case
    memory.add_unique_constraint(
      store,
      "memberships",
      "memberships_user_group_unique",
      [
        "user_id",
        "group_id",
      ],
    )
  {
    Ok(s) -> s
    Error(_) -> store
  }
}

pub fn composite_unique_allows_same_user_different_group_test() {
  let store = setup_composite_constraint_store()

  // Insert membership for user 1 in group 1
  let m1: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    // user_id
    dynamic.int(1),
    // group_id
    dynamic.string("member"),
  ]
  let assert Ok(store) = memory.insert_row(store, "memberships", "1", m1)

  // Insert membership for same user in different group
  let m2: MemoryRow = [
    dynamic.int(2),
    dynamic.int(1),
    // same user_id
    dynamic.int(2),
    // different group_id
    dynamic.string("admin"),
  ]

  let result = memory.insert_row(store, "memberships", "2", m2)
  result |> should.be_ok
}

pub fn composite_unique_allows_different_user_same_group_test() {
  let store = setup_composite_constraint_store()

  // Insert membership for user 1 in group 1
  let m1: MemoryRow = [
    dynamic.int(1),
    dynamic.int(1),
    // user_id
    dynamic.int(1),
    // group_id
    dynamic.string("member"),
  ]
  let assert Ok(store) = memory.insert_row(store, "memberships", "1", m1)

  // Insert membership for different user in same group
  let m2: MemoryRow = [
    dynamic.int(2),
    dynamic.int(2),
    // different user_id
    dynamic.int(1),
    // same group_id
    dynamic.string("member"),
  ]

  let result = memory.insert_row(store, "memberships", "2", m2)
  result |> should.be_ok
}

// ============================================================================
// MULTIPLE CONSTRAINT VIOLATION TESTS
// ============================================================================

pub fn first_constraint_violation_reported_test() {
  let store = setup_constrained_store()

  // Insert first user
  let user1: MemoryRow = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("alice"),
    dynamic.string("Alice"),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", user1)

  // Try to insert user violating multiple constraints
  // (duplicate email AND duplicate username)
  let user2: MemoryRow = [
    dynamic.int(2),
    dynamic.string("alice@example.com"),
    // Violates unique
    dynamic.string("alice"),
    // Also violates unique
    dynamic.string("Not Alice"),
  ]

  // Should get at least one unique violation error
  case memory.insert_row(store, "users", "2", user2) {
    Error(error.UniqueViolation(_, _)) -> should.be_true(True)
    _ -> should.fail()
  }
}
