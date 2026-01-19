# Blog Example Application

A complete example application demonstrating cquill best practices for building a data access layer in Gleam.

## Overview

This example implements a blog with users, posts, comments, and tags. It demonstrates:

- **Multiple schema types per domain** - Different types for create, read, update operations
- **Composable query builders** - Reusable query functions that can be combined
- **Service modules** - Business logic separated from data access
- **Test patterns** - Factory functions and test context for fast, isolated tests

> **Note:** This example uses simplified in-memory Dict-based storage to focus on demonstrating
> patterns. In a production application, you would integrate these patterns with cquill's
> full adapter/repo layer.

## Project Structure

```
examples/blog/
├── src/
│   ├── blog.gleam              # Main entry point
│   └── blog/
│       ├── config.gleam        # Database configuration
│       ├── schemas/            # Schema definitions
│       │   ├── user.gleam      # User types and encoding/decoding
│       │   ├── post.gleam      # Post types and encoding/decoding
│       │   ├── comment.gleam   # Comment types
│       │   └── tag.gleam       # Tag types and post-tag join
│       ├── queries/            # Reusable query builders
│       │   ├── user_queries.gleam
│       │   ├── post_queries.gleam
│       │   └── comment_queries.gleam
│       └── services/           # Business logic
│           ├── accounts.gleam  # User management
│           ├── posts.gleam     # Post management
│           └── comments.gleam  # Comment moderation
└── test/
    ├── support/                # Test helpers
    │   ├── setup.gleam         # Test context setup
    │   └── factory.gleam       # Test data factories
    ├── unit/                   # Pure function tests
    │   ├── schemas_test.gleam
    │   └── queries_test.gleam
    └── integration/            # Tests with memory adapter
        ├── accounts_test.gleam
        ├── posts_test.gleam
        └── comments_test.gleam
```

## Getting Started

### Prerequisites

- Gleam >= 1.0.0
- Erlang/OTP >= 26

### Running the Example

```bash
cd examples/blog
gleam build
gleam run
```

### Running Tests

```bash
cd examples/blog
gleam test
```

## Key Patterns Demonstrated

### Multiple Schema Types Per Domain

Instead of one type per table, define multiple types for different operations:

```gleam
// Full user from database
pub type User {
  User(id: Int, email: String, name: Option(String), role: UserRole, ...)
}

// For creating users (no id, plain password)
pub type NewUser {
  NewUser(email: String, name: Option(String), password: String)
}

// For public display (no sensitive data)
pub type PublicUser {
  PublicUser(id: Int, email: String, name: Option(String), role: UserRole)
}

// For profile updates
pub type UserUpdate {
  UserUpdate(name: Option(String), email: Option(String), role: Option(UserRole))
}
```

### Composable Query Builders

Build queries from small, reusable functions:

```gleam
// Base query
pub fn base() -> query.Query(Nil) {
  query.from("posts")
}

// Reusable filters
pub fn published(q: query.Query(a)) -> query.Query(a) {
  q |> query.where(query.eq("status", ast.StringValue("published")))
}

pub fn by_author(q: query.Query(a), user_id: Int) -> query.Query(a) {
  q |> query.where(query.eq("user_id", ast.IntValue(user_id)))
}

pub fn newest_first(q: query.Query(a)) -> query.Query(a) {
  q |> query.order_by_desc("published_at")
}

// Compose them!
pub fn recent_published_by_author(user_id: Int, limit: Int) {
  base()
  |> published()
  |> by_author(user_id)
  |> newest_first()
  |> query.limit(limit)
}
```

### Service Modules

Keep business logic in service modules, returning updated state:

```gleam
pub fn create_user(
  store: UserStore,
  new_user: NewUser,
) -> Result(#(UserStore, User), AccountError) {
  // Validate email uniqueness
  case get_user_by_email(store, new_user.email) {
    Ok(_) -> Error(EmailAlreadyExists)
    Error(UserNotFound) -> {
      // Create user with hashed password
      let user = User(
        id: store.next_id,
        email: new_user.email,
        name: new_user.name,
        password_hash: hash_password(new_user.password),
        // ...
      )
      // Return updated store and created user
      Ok(#(updated_store, user))
    }
    Error(e) -> Error(e)
  }
}
```

### Testing with Test Context

Use the test context pattern for fast, isolated tests:

```gleam
pub fn create_user_test() {
  use ctx <- setup.with_context()  // Fresh stores for each test

  let new_user = factory.new_user()
  let result = accounts.create_user(ctx.user_store, new_user)

  let assert Ok(#(_, user)) = result
  user.email |> should.equal(new_user.email)
}
```

### Test Factories

Generate test data with sensible defaults:

```gleam
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
  NewUser(email: email, name: Some("Test User"), password: "password123")
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
```

## Domain Model

### User

| Field | Type | Description |
|-------|------|-------------|
| id | Int | Primary key |
| email | String | Unique email address |
| name | Option(String) | Display name |
| role | UserRole | admin, author, reader |
| password_hash | String | Hashed password |
| inserted_at | String | Creation timestamp |
| updated_at | String | Last update timestamp |

### Post

| Field | Type | Description |
|-------|------|-------------|
| id | Int | Primary key |
| user_id | Int | Author (FK → users) |
| title | String | Post title |
| slug | String | URL-friendly slug |
| body | String | Post content |
| status | PostStatus | draft, published, archived |
| published_at | Option(String) | Publication timestamp |
| inserted_at | String | Creation timestamp |
| updated_at | String | Last update timestamp |

### Comment

| Field | Type | Description |
|-------|------|-------------|
| id | Int | Primary key |
| post_id | Int | FK → posts |
| author_name | String | Commenter's name |
| author_email | String | Commenter's email |
| body | String | Comment content |
| approved | Bool | Moderation status |
| inserted_at | String | Creation timestamp |
| updated_at | String | Last update timestamp |

### Tag

| Field | Type | Description |
|-------|------|-------------|
| id | Int | Primary key |
| name | String | Tag display name |
| slug | String | URL-friendly slug |

## Learn More

- [cquill Documentation](../../docs/README.md)
- [Getting Started Guide](../../docs/getting-started.md)
- [Testing Guide](../../docs/guides/testing.md)
