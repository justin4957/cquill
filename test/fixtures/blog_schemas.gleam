// Blog Domain Test Fixtures
//
// This module provides schema definitions for a blog domain used throughout
// cquill's test suite. These fixtures demonstrate real-world usage patterns
// and serve as the standard example domain for all tests and documentation.
//
// Domain model:
// - User: authors of posts and commenters
// - Post: blog posts with title, body, published status
// - Comment: comments on posts with author info
// - Tag: categorization for posts (future)
// - PostTag: many-to-many relationship (future)

import cquill/schema.{type Schema}
import cquill/schema/field
import gleam/dynamic

// ============================================================================
// USER SCHEMAS
// ============================================================================

/// Full user schema with all fields
/// Used for: reading users, admin operations
pub fn user_schema() -> Schema {
  schema.new("users")
  |> schema.in_schema("public")
  |> schema.field(
    field.integer("id")
    |> field.primary_key
    |> field.auto_increment,
  )
  |> schema.field(
    field.string("email")
    |> field.not_null
    |> field.unique
    |> field.max_length(255),
  )
  |> schema.field(
    field.string("password_hash")
    |> field.not_null,
  )
  |> schema.field(
    field.string("name")
    |> field.nullable
    |> field.max_length(100),
  )
  |> schema.field(
    field.string("bio")
    |> field.nullable,
  )
  |> schema.field(
    field.boolean("is_admin")
    |> field.not_null
    |> field.default_value(dynamic.bool(False)),
  )
  |> schema.field(
    field.datetime("inserted_at")
    |> field.not_null
    |> field.default_function("now()"),
  )
  |> schema.field(
    field.datetime("updated_at")
    |> field.not_null
    |> field.default_function("now()"),
  )
  |> schema.single_primary_key("id")
  |> schema.unique_constraint("users_email_unique", ["email"])
  |> schema.with_comment("User accounts for the blog platform")
}

/// User schema for inserts (no auto-generated fields)
/// Used for: creating new users
pub fn user_insert_schema() -> Schema {
  user_schema()
  |> schema.insertable_fields
  |> schema.exclude_fields(["inserted_at", "updated_at"])
}

/// Public user schema (no sensitive fields)
/// Used for: API responses, public profiles
pub fn user_public_schema() -> Schema {
  user_schema()
  |> schema.exclude_fields(["password_hash", "is_admin"])
}

/// User author schema (minimal fields for attribution)
/// Used for: displaying post/comment authors
pub fn user_author_schema() -> Schema {
  user_schema()
  |> schema.select_fields(["id", "name", "email"])
}

// ============================================================================
// POST SCHEMAS
// ============================================================================

/// Full post schema with all fields
/// Used for: reading posts, admin operations
pub fn post_schema() -> Schema {
  schema.new("posts")
  |> schema.in_schema("public")
  |> schema.field(
    field.integer("id")
    |> field.primary_key
    |> field.auto_increment,
  )
  |> schema.field(
    field.integer("user_id")
    |> field.not_null
    |> field.references("users", "id"),
  )
  |> schema.field(
    field.string("title")
    |> field.not_null
    |> field.max_length(255)
    |> field.min_length(1),
  )
  |> schema.field(
    field.string("slug")
    |> field.not_null
    |> field.unique
    |> field.max_length(255),
  )
  |> schema.field(
    field.string("body")
    |> field.not_null,
  )
  |> schema.field(
    field.string("excerpt")
    |> field.nullable
    |> field.max_length(500),
  )
  |> schema.field(
    field.boolean("published")
    |> field.not_null
    |> field.default_value(dynamic.bool(False)),
  )
  |> schema.field(
    field.datetime("published_at")
    |> field.nullable,
  )
  |> schema.field(
    field.integer("view_count")
    |> field.not_null
    |> field.default_value(dynamic.int(0))
    |> field.min_value(0),
  )
  |> schema.field(
    field.array("tags", field.String)
    |> field.nullable,
  )
  |> schema.field(
    field.datetime("inserted_at")
    |> field.not_null
    |> field.default_function("now()"),
  )
  |> schema.field(
    field.datetime("updated_at")
    |> field.not_null
    |> field.default_function("now()"),
  )
  |> schema.single_primary_key("id")
  |> schema.unique_constraint("posts_slug_unique", ["slug"])
  |> schema.index("posts_user_id_idx", ["user_id"], False)
  |> schema.index("posts_published_at_idx", ["published_at"], False)
  |> schema.with_comment("Blog posts")
}

/// Post schema for inserts
/// Used for: creating new posts
pub fn post_insert_schema() -> Schema {
  post_schema()
  |> schema.insertable_fields
  |> schema.exclude_fields(["inserted_at", "updated_at", "view_count"])
}

/// Post schema for public listing (summary view)
/// Used for: blog index, search results
pub fn post_listing_schema() -> Schema {
  post_schema()
  |> schema.select_fields([
    "id", "user_id", "title", "slug", "excerpt", "published", "published_at",
    "view_count", "tags",
  ])
}

/// Post schema for updates
/// Used for: editing posts
pub fn post_update_schema() -> Schema {
  post_schema()
  |> schema.select_fields([
    "title",
    "slug",
    "body",
    "excerpt",
    "published",
    "published_at",
    "tags",
  ])
}

// ============================================================================
// COMMENT SCHEMAS
// ============================================================================

/// Full comment schema
/// Used for: reading comments, moderation
pub fn comment_schema() -> Schema {
  schema.new("comments")
  |> schema.in_schema("public")
  |> schema.field(
    field.integer("id")
    |> field.primary_key
    |> field.auto_increment,
  )
  |> schema.field(
    field.integer("post_id")
    |> field.not_null
    |> field.references_with_action("posts", "id", field.Cascade),
  )
  |> schema.field(
    field.integer("user_id")
    |> field.nullable
    |> field.references_with_action("users", "id", field.SetNull),
  )
  |> schema.field(
    field.string("author_name")
    |> field.not_null
    |> field.max_length(100)
    |> field.comment("Display name for the comment author"),
  )
  |> schema.field(
    field.string("author_email")
    |> field.nullable
    |> field.max_length(255),
  )
  |> schema.field(
    field.string("content")
    |> field.not_null
    |> field.min_length(1),
  )
  |> schema.field(
    field.boolean("approved")
    |> field.not_null
    |> field.default_value(dynamic.bool(False)),
  )
  |> schema.field(
    field.integer("parent_id")
    |> field.nullable
    |> field.references("comments", "id")
    |> field.comment("For threaded comments - references parent comment"),
  )
  |> schema.field(
    field.datetime("inserted_at")
    |> field.not_null
    |> field.default_function("now()"),
  )
  |> schema.single_primary_key("id")
  |> schema.index("comments_post_id_idx", ["post_id"], False)
  |> schema.index("comments_user_id_idx", ["user_id"], False)
  |> schema.with_comment("Comments on blog posts")
}

/// Comment schema for inserts (public submission)
/// Used for: creating new comments via public form
pub fn comment_insert_schema() -> Schema {
  comment_schema()
  |> schema.select_fields([
    "post_id",
    "user_id",
    "author_name",
    "author_email",
    "content",
    "parent_id",
  ])
}

/// Comment schema for public display
/// Used for: showing comments on posts
pub fn comment_public_schema() -> Schema {
  comment_schema()
  |> schema.exclude_fields(["author_email", "approved"])
}

// ============================================================================
// TAG SCHEMAS (for future many-to-many example)
// ============================================================================

/// Tag schema
/// Used for: categorizing posts
pub fn tag_schema() -> Schema {
  schema.new("tags")
  |> schema.in_schema("public")
  |> schema.field(
    field.integer("id")
    |> field.primary_key
    |> field.auto_increment,
  )
  |> schema.field(
    field.string("name")
    |> field.not_null
    |> field.unique
    |> field.max_length(50),
  )
  |> schema.field(
    field.string("slug")
    |> field.not_null
    |> field.unique
    |> field.max_length(50),
  )
  |> schema.field(
    field.string("description")
    |> field.nullable,
  )
  |> schema.single_primary_key("id")
  |> schema.with_comment("Tags for categorizing posts")
}

/// Post-Tag junction table for many-to-many
pub fn post_tag_schema() -> Schema {
  schema.new("post_tags")
  |> schema.in_schema("public")
  |> schema.field(
    field.integer("post_id")
    |> field.not_null
    |> field.references_with_action("posts", "id", field.Cascade),
  )
  |> schema.field(
    field.integer("tag_id")
    |> field.not_null
    |> field.references_with_action("tags", "id", field.Cascade),
  )
  |> schema.primary_key(["post_id", "tag_id"])
  |> schema.with_comment("Junction table for post-tag relationships")
}

// ============================================================================
// SCHEMA COLLECTIONS
// ============================================================================

/// Get all blog domain schemas
pub fn all_schemas() -> List(Schema) {
  [
    user_schema(),
    post_schema(),
    comment_schema(),
    tag_schema(),
    post_tag_schema(),
  ]
}

/// Get all full schemas (for migrations)
pub fn full_schemas() -> List(Schema) {
  [
    user_schema(),
    post_schema(),
    comment_schema(),
    tag_schema(),
    post_tag_schema(),
  ]
}

/// Get all insert schemas
pub fn insert_schemas() -> List(Schema) {
  [
    user_insert_schema(),
    post_insert_schema(),
    comment_insert_schema(),
  ]
}

/// Get all public schemas (for API responses)
pub fn public_schemas() -> List(Schema) {
  [
    user_public_schema(),
    post_listing_schema(),
    comment_public_schema(),
  ]
}
