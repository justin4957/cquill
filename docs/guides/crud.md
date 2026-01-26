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
import cquill/schema
import cquill/schema/field

// Define your schema
let user_schema = schema.new("users")
  |> schema.add_field(field.integer("id") |> field.primary_key())
  |> schema.add_field(field.string("email") |> field.not_null())
  |> schema.add_field(field.string("name"))
  |> schema.add_field(field.boolean("active") |> field.not_null())

// Build the query
let users_query = query.from(user_schema)

// Execute using the adapter
case adapter.query(postgres_adapter, pool, compiled_query) {
  Ok(rows) -> {
    list.each(rows, fn(row) {
      io.println("User: " <> string.inspect(row))
    })
  }
  Error(e) -> io.println("Query failed: " <> error.format_error(e))
}
```

### Fetch Single Record

```gleam
let user_query =
  query.from(user_schema)
  |> query.where(query.eq("id", 1))

case adapter.query_one(postgres_adapter, pool, compiled_query) {
  Ok(row) -> {
    io.println("Found: " <> string.inspect(row))
  }
  Error(error.NotFound) -> {
    io.println("User not found")
  }
  Error(error.TooManyRows(expected, got)) -> {
    io.println("Expected 1, found " <> int.to_string(got))
  }
  Error(e) -> io.println("Query failed: " <> error.format_error(e))
}
```

### Filtered Queries

```gleam
// Active users only
let active_users =
  query.from(user_schema)
  |> query.where(query.eq("active", True))

// Users by role
let admins =
  query.from(user_schema)
  |> query.where(query.eq("role", "admin"))

// Combined filters (multiple where = AND)
let active_admins =
  query.from(user_schema)
  |> query.where(query.eq("active", True))
  |> query.where(query.eq("role", "admin"))

// OR conditions
let special_users =
  query.from(user_schema)
  |> query.where(query.or([
    query.eq("role", "admin"),
    query.eq("role", "moderator"),
  ]))
```

### Sorted and Limited Queries

```gleam
// Newest first, limit 10
let recent_users =
  query.from(user_schema)
  |> query.order_by_desc("created_at")
  |> query.limit(10)

// Paginated using limit/offset
let page_2 =
  query.from(user_schema)
  |> query.order_by_asc("id")
  |> query.limit(20)
  |> query.offset(20)  // Skip first 20

// Paginated using the helper
let page_3 =
  query.from(user_schema)
  |> query.order_by_asc("id")
  |> query.paginate(page: 3, per_page: 20)
```

### Select Specific Columns

```gleam
// Only fetch needed columns
let user_names =
  query.from(user_schema)
  |> query.select(["id", "name"])
  |> query.where(query.eq("active", True))
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
import cquill/query
import cquill/query/ast

// Generic find by ID using table name
pub fn find_by_id(table: String, id: Int) {
  query.from_table(table)
  |> query.where(query.eq("id", id))
}

// Find by ID using schema (preferred for type safety)
pub fn find_by_id_schema(schema: schema.Schema, id: Int) {
  query.from(schema)
  |> query.where(query.eq("id", id))
}

// Build an update query by ID
pub fn update_by_id(table: String, id: Int) {
  ast.new_update(table)
  |> ast.update_where(query.eq("id", id))
}

// Build a delete query by ID
pub fn delete_by_id(table: String, id: Int) {
  ast.new_delete(table)
  |> ast.delete_where(query.eq("id", id))
}

// Reusable filter for active records
pub fn active_only(q) {
  q |> query.where(query.eq("active", True))
}

// Reusable pagination
pub fn with_pagination(q, page: Int, per_page: Int) {
  q |> query.paginate(page: page, per_page: per_page)
}
```

## Next Steps

- Learn about [Advanced Queries](./queries.md) for complex operations
- See [Transactions](./transactions.md) for atomic operations
- Read about [Error Handling](../reference/errors.md)
