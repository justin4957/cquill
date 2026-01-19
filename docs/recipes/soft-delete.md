# Soft Delete Recipe

Implement soft delete pattern where records are marked as deleted instead of being removed from the database.

## Why Soft Delete?

- **Audit trail**: Keep history of deleted records
- **Recovery**: Easily restore accidentally deleted data
- **Data integrity**: Maintain referential integrity
- **Compliance**: Meet data retention requirements

## Basic Implementation

### Schema

Add a `deleted_at` column to track deletion time:

```sql
-- Migration
ALTER TABLE users ADD COLUMN deleted_at TIMESTAMP;
CREATE INDEX idx_users_deleted_at ON users(deleted_at);
```

### Query Helpers

```gleam
import cquill/query
import cquill/query/ast

/// Base query that excludes soft-deleted records
pub fn active(base_query: query.Query(a)) -> query.Query(a) {
  base_query
  |> query.where(query.is_null("deleted_at"))
}

/// Query that only includes soft-deleted records
pub fn deleted(base_query: query.Query(a)) -> query.Query(a) {
  base_query
  |> query.where(query.is_not_null("deleted_at"))
}

/// Query that includes all records (active and deleted)
pub fn with_deleted(base_query: query.Query(a)) -> query.Query(a) {
  // Just return the base query unchanged
  base_query
}
```

### Soft Delete Operation

```gleam
import cquill/repo

/// Soft delete a record by ID
pub fn soft_delete(adapter, table: String, id: Int) -> Result(Int, error.AdapterError) {
  let now = get_current_timestamp()

  let update =
    ast.new_update(table)
    |> ast.set("deleted_at", ast.StringValue(now))
    |> ast.update_where(query.and_where([
      query.eq("id", ast.IntValue(id)),
      query.is_null("deleted_at"),  // Don't re-delete
    ]))

  case repo.update(adapter, update) {
    Ok(rows) -> Ok(list.length(rows))
    Error(e) -> Error(e)
  }
}

/// Soft delete records matching a condition
pub fn soft_delete_where(
  adapter,
  table: String,
  condition: query.Condition,
) -> Result(Int, error.AdapterError) {
  let now = get_current_timestamp()

  let update =
    ast.new_update(table)
    |> ast.set("deleted_at", ast.StringValue(now))
    |> ast.update_where(query.and_where([
      condition,
      query.is_null("deleted_at"),
    ]))

  case repo.update(adapter, update) {
    Ok(rows) -> Ok(list.length(rows))
    Error(e) -> Error(e)
  }
}
```

### Restore Operation

```gleam
/// Restore a soft-deleted record
pub fn restore(adapter, table: String, id: Int) -> Result(Int, error.AdapterError) {
  let update =
    ast.new_update(table)
    |> ast.set("deleted_at", ast.NullValue)
    |> ast.update_where(query.and_where([
      query.eq("id", ast.IntValue(id)),
      query.is_not_null("deleted_at"),  // Only restore deleted
    ]))

  case repo.update(adapter, update) {
    Ok(rows) -> Ok(list.length(rows))
    Error(e) -> Error(e)
  }
}

/// Restore all soft-deleted records matching condition
pub fn restore_where(
  adapter,
  table: String,
  condition: query.Condition,
) -> Result(Int, error.AdapterError) {
  let update =
    ast.new_update(table)
    |> ast.set("deleted_at", ast.NullValue)
    |> ast.update_where(query.and_where([
      condition,
      query.is_not_null("deleted_at"),
    ]))

  case repo.update(adapter, update) {
    Ok(rows) -> Ok(list.length(rows))
    Error(e) -> Error(e)
  }
}
```

### Permanent Delete

```gleam
/// Permanently delete a soft-deleted record
pub fn hard_delete(adapter, table: String, id: Int) -> Result(Int, error.AdapterError) {
  let delete =
    ast.new_delete(table)
    |> ast.delete_where(query.and_where([
      query.eq("id", ast.IntValue(id)),
      query.is_not_null("deleted_at"),  // Only hard-delete already soft-deleted
    ]))

  repo.delete(adapter, delete)
}

/// Purge all soft-deleted records older than a date
pub fn purge_deleted_before(
  adapter,
  table: String,
  before_date: String,
) -> Result(Int, error.AdapterError) {
  let delete =
    ast.new_delete(table)
    |> ast.delete_where(query.and_where([
      query.is_not_null("deleted_at"),
      query.lt("deleted_at", ast.StringValue(before_date)),
    ]))

  repo.delete(adapter, delete)
}
```

## Usage Example

```gleam
// Create query for users
let users = query.from("users")

// Get all active users (excludes deleted)
let active_users = users |> active()
case repo.all(adapter, active_users) {
  Ok(rows) -> display_users(rows)
  Error(e) -> handle_error(e)
}

// Get deleted users for admin view
let deleted_users = users |> deleted()
case repo.all(adapter, deleted_users) {
  Ok(rows) -> display_deleted_users(rows)
  Error(e) -> handle_error(e)
}

// Soft delete a user
case soft_delete(adapter, "users", user_id) {
  Ok(1) -> io.println("User deleted")
  Ok(0) -> io.println("User not found or already deleted")
  Ok(_) -> io.println("Unexpected result")
  Error(e) -> handle_error(e)
}

// Restore a user
case restore(adapter, "users", user_id) {
  Ok(1) -> io.println("User restored")
  Ok(0) -> io.println("User not found or not deleted")
  Ok(_) -> io.println("Unexpected result")
  Error(e) -> handle_error(e)
}
```

## Advanced: Soft Delete Module

Create a reusable module:

```gleam
// soft_delete.gleam

import cquill/query
import cquill/query/ast
import cquill/repo
import cquill/error

pub type SoftDeletable {
  SoftDeletable(
    table: String,
    deleted_at_column: String,
  )
}

pub fn new(table: String) -> SoftDeletable {
  SoftDeletable(table: table, deleted_at_column: "deleted_at")
}

pub fn with_column(sd: SoftDeletable, column: String) -> SoftDeletable {
  SoftDeletable(..sd, deleted_at_column: column)
}

pub fn active_query(sd: SoftDeletable) -> query.Query(Nil) {
  query.from(sd.table)
  |> query.where(query.is_null(sd.deleted_at_column))
}

pub fn deleted_query(sd: SoftDeletable) -> query.Query(Nil) {
  query.from(sd.table)
  |> query.where(query.is_not_null(sd.deleted_at_column))
}

pub fn all_query(sd: SoftDeletable) -> query.Query(Nil) {
  query.from(sd.table)
}

pub fn delete(
  adapter,
  sd: SoftDeletable,
  id: Int,
) -> Result(Bool, error.AdapterError) {
  let now = get_current_timestamp()

  let update =
    ast.new_update(sd.table)
    |> ast.set(sd.deleted_at_column, ast.StringValue(now))
    |> ast.update_where(query.and_where([
      query.eq("id", ast.IntValue(id)),
      query.is_null(sd.deleted_at_column),
    ]))

  case repo.update(adapter, update) {
    Ok(rows) -> Ok(list.length(rows) > 0)
    Error(e) -> Error(e)
  }
}

pub fn restore(
  adapter,
  sd: SoftDeletable,
  id: Int,
) -> Result(Bool, error.AdapterError) {
  let update =
    ast.new_update(sd.table)
    |> ast.set(sd.deleted_at_column, ast.NullValue)
    |> ast.update_where(query.and_where([
      query.eq("id", ast.IntValue(id)),
      query.is_not_null(sd.deleted_at_column),
    ]))

  case repo.update(adapter, update) {
    Ok(rows) -> Ok(list.length(rows) > 0)
    Error(e) -> Error(e)
  }
}
```

### Usage

```gleam
let users = soft_delete.new("users")

// Query active users
let active_users = soft_delete.active_query(users)
  |> query.order_by_asc("name")
repo.all(adapter, active_users)

// Soft delete
soft_delete.delete(adapter, users, 123)

// Restore
soft_delete.restore(adapter, users, 123)
```

## Cascading Soft Delete

Handle related records when soft deleting:

```gleam
pub fn soft_delete_user_with_posts(adapter, user_id: Int) {
  repo.transaction(adapter, fn(tx) {
    let now = get_current_timestamp()

    // Soft delete user
    let user_update =
      ast.new_update("users")
      |> ast.set("deleted_at", ast.StringValue(now))
      |> ast.update_where(query.eq("id", ast.IntValue(user_id)))

    use _ <- result.try(repo.update(tx, user_update))

    // Soft delete user's posts
    let posts_update =
      ast.new_update("posts")
      |> ast.set("deleted_at", ast.StringValue(now))
      |> ast.update_where(query.and_where([
        query.eq("user_id", ast.IntValue(user_id)),
        query.is_null("deleted_at"),
      ]))

    use _ <- result.try(repo.update(tx, posts_update))

    // Soft delete comments on those posts
    let comments_update =
      ast.new_update("comments")
      |> ast.set("deleted_at", ast.StringValue(now))
      |> ast.update_where(query.raw(
        "post_id IN (SELECT id FROM posts WHERE user_id = $1)",
        [ast.IntValue(user_id)],
      ))

    use _ <- result.try(repo.update(tx, comments_update))

    Ok(Nil)
  })
}
```

## Testing Soft Delete

```gleam
pub fn soft_delete_marks_record_test() {
  let store = setup_store_with_user(1, "alice@example.com")

  // Soft delete
  case soft_delete(store, "users", 1) {
    Ok(1) -> {
      // Verify deleted_at is set
      let query = query.from("users")
        |> query.where(query.eq("id", ast.IntValue(1)))

      case repo.one(store, query) {
        Ok(row) -> {
          dict.get(row, "deleted_at")
          |> result.is_ok()
          |> should.be_true()
        }
        Error(_) -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn active_query_excludes_deleted_test() {
  let store = setup_store()
    |> insert_user(1, "active@example.com", None)
    |> insert_user(2, "deleted@example.com", Some("2024-01-01"))

  let query = query.from("users") |> active()

  case repo.all(store, query) {
    Ok(rows) -> {
      list.length(rows) |> should.equal(1)
      case list.first(rows) {
        Ok(row) -> {
          dict.get(row, "id") |> should.equal(Ok(ast.IntValue(1)))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn restore_clears_deleted_at_test() {
  let store = setup_store()
    |> insert_user(1, "test@example.com", Some("2024-01-01"))

  case restore(store, "users", 1) {
    Ok(1) -> {
      let query = query.from("users")
        |> query.where(query.eq("id", ast.IntValue(1)))

      case repo.one(store, query) {
        Ok(row) -> {
          dict.get(row, "deleted_at")
          |> should.equal(Ok(ast.NullValue))
        }
        Error(_) -> should.fail()
      }
    }
    _ -> should.fail()
  }
}
```
