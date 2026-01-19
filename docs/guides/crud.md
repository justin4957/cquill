# CRUD Operations

This guide covers the fundamental Create, Read, Update, and Delete operations in cquill.

## Create (INSERT)

### Single Record Insert

```gleam
import cquill/query/ast
import cquill/repo

let insert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name", "active"])
  |> ast.insert_values([[
    ast.StringValue("alice@example.com"),
    ast.StringValue("Alice"),
    ast.BoolValue(True),
  ]])
  |> ast.returning(["id", "created_at"])

case repo.insert(adapter, insert) {
  Ok(rows) -> {
    // rows contains the RETURNING data
    case list.first(rows) {
      Ok(row) -> {
        let id = dict.get(row, "id")
        io.println("Created user with ID: " <> string.inspect(id))
      }
      Error(_) -> io.println("Insert succeeded")
    }
  }
  Error(error.UniqueViolation(constraint, _)) -> {
    io.println("Duplicate value: " <> constraint)
  }
  Error(error.NotNullViolation(column)) -> {
    io.println("Missing required field: " <> column)
  }
  Error(e) -> {
    io.println("Insert failed: " <> error.format(e))
  }
}
```

### Bulk Insert

Insert multiple records in a single operation:

```gleam
let users = [
  [ast.StringValue("alice@example.com"), ast.StringValue("Alice")],
  [ast.StringValue("bob@example.com"), ast.StringValue("Bob")],
  [ast.StringValue("carol@example.com"), ast.StringValue("Carol")],
]

let insert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values(users)

case repo.insert_all(adapter, insert) {
  Ok(count) -> io.println("Inserted " <> int.to_string(count) <> " users")
  Error(e) -> io.println("Bulk insert failed: " <> error.format(e))
}
```

### Insert with ON CONFLICT (Upsert)

Handle conflicts gracefully:

```gleam
// ON CONFLICT DO NOTHING
let insert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values([[
    ast.StringValue("alice@example.com"),
    ast.StringValue("Alice"),
  ]])
  |> ast.on_conflict(ast.DoNothing)

// ON CONFLICT DO UPDATE
let upsert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values([[
    ast.StringValue("alice@example.com"),
    ast.StringValue("Alice Updated"),
  ]])
  |> ast.on_conflict(ast.DoUpdate(
    conflict_columns: ["email"],
    update_columns: ["name"],
  ))
```

## Read (SELECT)

### Fetch All Records

```gleam
import cquill/query
import cquill/repo

let users_query = query.from("users")

case repo.all(adapter, users_query) {
  Ok(rows) -> {
    list.each(rows, fn(row) {
      let email = dict.get(row, "email")
      io.println("User: " <> string.inspect(email))
    })
  }
  Error(e) -> io.println("Query failed: " <> error.format(e))
}
```

### Fetch Single Record

```gleam
let user_query =
  query.from("users")
  |> query.where(query.eq("id", ast.IntValue(1)))

case repo.one(adapter, user_query) {
  Ok(row) -> {
    let name = dict.get(row, "name")
    io.println("Found: " <> string.inspect(name))
  }
  Error(error.NotFound) -> {
    io.println("User not found")
  }
  Error(error.TooManyRows(expected, got)) -> {
    io.println("Expected 1, found " <> int.to_string(got))
  }
  Error(e) -> io.println("Query failed: " <> error.format(e))
}
```

### Filtered Queries

```gleam
// Active users only
let active_users =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))

// Users by role
let admins =
  query.from("users")
  |> query.where(query.eq("role", ast.StringValue("admin")))

// Combined filters
let active_admins =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.where(query.eq("role", ast.StringValue("admin")))

// OR conditions
let special_users =
  query.from("users")
  |> query.where(query.or_where([
    query.eq("role", ast.StringValue("admin")),
    query.eq("role", ast.StringValue("moderator")),
  ]))
```

### Sorted and Limited Queries

```gleam
// Newest first, limit 10
let recent_users =
  query.from("users")
  |> query.order_by_desc("created_at")
  |> query.limit(10)

// Paginated
let page_2 =
  query.from("users")
  |> query.order_by_asc("id")
  |> query.limit(20)
  |> query.offset(20)  // Skip first 20
```

### Select Specific Columns

```gleam
// Only fetch needed columns
let user_names =
  query.from("users")
  |> query.select(["id", "name"])
  |> query.where(query.eq("active", ast.BoolValue(True)))
```

## Update

### Update Single Record

```gleam
import cquill/query/ast

let update =
  ast.new_update("users")
  |> ast.set("name", ast.StringValue("Alice Smith"))
  |> ast.set("updated_at", ast.StringValue("2024-01-15T10:00:00Z"))
  |> ast.update_where(query.eq("id", ast.IntValue(1)))
  |> ast.returning(["id", "name", "updated_at"])

case repo.update(adapter, update) {
  Ok(rows) -> {
    case list.first(rows) {
      Ok(row) -> io.println("Updated: " <> string.inspect(row))
      Error(_) -> io.println("Update succeeded, no return data")
    }
  }
  Error(error.NotFound) -> {
    io.println("No matching record to update")
  }
  Error(e) -> io.println("Update failed: " <> error.format(e))
}
```

### Update Multiple Records

```gleam
// Deactivate all inactive users older than 90 days
let update =
  ast.new_update("users")
  |> ast.set("active", ast.BoolValue(False))
  |> ast.update_where(query.and_where([
    query.eq("active", ast.BoolValue(True)),
    query.lt("last_login", ast.StringValue("2024-01-01")),
  ]))

case repo.update(adapter, update) {
  Ok(rows) -> {
    io.println("Deactivated " <> int.to_string(list.length(rows)) <> " users")
  }
  Error(e) -> io.println("Update failed: " <> error.format(e))
}
```

### Conditional Updates

```gleam
// Only update if version matches (optimistic locking)
let update =
  ast.new_update("posts")
  |> ast.set("title", ast.StringValue("New Title"))
  |> ast.set("version", ast.IntValue(2))
  |> ast.update_where(query.and_where([
    query.eq("id", ast.IntValue(post_id)),
    query.eq("version", ast.IntValue(1)),  // Expected version
  ]))
  |> ast.returning(["id", "version"])

case repo.update(adapter, update) {
  Ok([]) -> {
    // No rows updated - version mismatch
    Error(error.StaleData("1", "unknown"))
  }
  Ok(rows) -> Ok(rows)
  Error(e) -> Error(e)
}
```

## Delete

### Delete Single Record

```gleam
let delete =
  ast.new_delete("users")
  |> ast.delete_where(query.eq("id", ast.IntValue(1)))
  |> ast.returning(["id", "email"])

case repo.delete(adapter, delete) {
  Ok(count) when count > 0 -> {
    io.println("Deleted user")
  }
  Ok(_) -> {
    io.println("No user found to delete")
  }
  Error(error.ForeignKeyViolation(constraint, _)) -> {
    io.println("Cannot delete: referenced by other records")
  }
  Error(e) -> io.println("Delete failed: " <> error.format(e))
}
```

### Delete Multiple Records

```gleam
// Delete old sessions
let delete =
  ast.new_delete("sessions")
  |> ast.delete_where(query.lt("expires_at", ast.StringValue("2024-01-01")))

case repo.delete(adapter, delete) {
  Ok(count) -> {
    io.println("Deleted " <> int.to_string(count) <> " expired sessions")
  }
  Error(e) -> io.println("Delete failed: " <> error.format(e))
}
```

### Delete All Records (Truncate)

```gleam
// WARNING: Deletes all records in table
let delete_all =
  ast.new_delete("temp_data")
  // No where clause = delete all

case repo.delete(adapter, delete_all) {
  Ok(count) -> io.println("Cleared " <> int.to_string(count) <> " records")
  Error(e) -> io.println("Delete failed: " <> error.format(e))
}
```

## Transactions

Wrap related operations in a transaction:

```gleam
repo.transaction(adapter, fn(tx) {
  // Create user
  use user_rows <- result.try(repo.insert(tx, create_user))
  let user_id = get_id(user_rows)

  // Create profile with user_id
  let profile_insert = create_profile_insert(user_id)
  use _ <- result.try(repo.insert(tx, profile_insert))

  // Return success
  Ok(user_id)
})
```

## Helper Functions

Create reusable CRUD functions:

```gleam
// Generic insert helper
pub fn insert_one(adapter, table: String, data: Dict(String, ast.Value)) {
  let columns = dict.keys(data)
  let values = dict.values(data) |> list.map(fn(v) { [v] })

  let insert =
    ast.new_insert(table)
    |> ast.insert_columns(columns)
    |> ast.insert_values(values)
    |> ast.returning(["id"])

  repo.insert(adapter, insert)
  |> result.map(fn(rows) {
    case list.first(rows) {
      Ok(row) -> dict.get(row, "id")
      Error(_) -> Error(Nil)
    }
  })
}

// Generic find by ID
pub fn find_by_id(adapter, table: String, id: Int) {
  let query =
    query.from(table)
    |> query.where(query.eq("id", ast.IntValue(id)))

  repo.one(adapter, query)
}

// Generic update by ID
pub fn update_by_id(
  adapter,
  table: String,
  id: Int,
  updates: List(#(String, ast.Value)),
) {
  let update =
    list.fold(updates, ast.new_update(table), fn(q, pair) {
      let #(column, value) = pair
      ast.set(q, column, value)
    })
    |> ast.update_where(query.eq("id", ast.IntValue(id)))

  repo.update(adapter, update)
}

// Generic delete by ID
pub fn delete_by_id(adapter, table: String, id: Int) {
  let delete =
    ast.new_delete(table)
    |> ast.delete_where(query.eq("id", ast.IntValue(id)))

  repo.delete(adapter, delete)
}
```

## Next Steps

- Learn about [Advanced Queries](./queries.md) for complex operations
- See [Transactions](./transactions.md) for atomic operations
- Read about [Error Handling](../reference/errors.md)
