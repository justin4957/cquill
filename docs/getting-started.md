# Getting Started with cquill

This guide will help you get cquill installed and running your first database operations.

## Installation

Add cquill to your Gleam project:

```sh
gleam add cquill
```

For PostgreSQL support, you'll also need the pgo driver:

```sh
gleam add pgo
```

## Your First Operations

Here's a complete example showing how to use the in-memory adapter for testing:

```gleam
import cquill/adapter/memory
import cquill/schema
import cquill/schema/field
import gleam/dynamic
import gleam/int
import gleam/io

pub fn main() {
  // Create an in-memory store
  let store = memory.new_store()

  // Create a schema for users
  let user_schema = schema.new("users")
    |> schema.add_field(field.integer("id") |> field.primary_key())
    |> schema.add_field(field.string("email") |> field.not_null() |> field.unique())
    |> schema.add_field(field.string("name"))
    |> schema.add_field(field.boolean("active") |> field.not_null())

  // Create a table from the schema
  let store = memory.create_table_from_schema(store, user_schema)

  // Insert a user (key is the primary key value, row is a list of dynamic values)
  let assert Ok(store) = memory.insert_row(
    store,
    "users",
    "1",  // Primary key as string
    [dynamic.int(1), dynamic.string("alice@example.com"), dynamic.string("Alice"), dynamic.bool(True)]
  )

  // Insert another user
  let assert Ok(store) = memory.insert_row(
    store,
    "users",
    "2",
    [dynamic.int(2), dynamic.string("bob@example.com"), dynamic.string("Bob"), dynamic.bool(False)]
  )

  // Query all rows from the table
  case memory.get_all_rows(store, "users") {
    Ok(rows) -> {
      io.println("Found " <> int.to_string(list.length(rows)) <> " users")
    }
    Error(e) -> {
      io.println("Query failed")
    }
  }

  // Get a specific row by primary key
  case memory.get_row(store, "users", "1") {
    Ok(row) -> io.println("Found Alice!")
    Error(_) -> io.println("User not found")
  }
}
```

## Building Queries with the Query Module

For more complex queries, use the query builder:

```gleam
import cquill/query
import cquill/schema
import cquill/schema/field

pub fn example() {
  // Define your schema
  let user_schema = schema.new("users")
    |> schema.add_field(field.integer("id") |> field.primary_key())
    |> schema.add_field(field.string("email") |> field.not_null())
    |> schema.add_field(field.string("name"))
    |> schema.add_field(field.boolean("active") |> field.not_null())

  // Build a query from the schema
  let active_users_query = query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.select(["id", "email", "name"])
    |> query.order_by_asc("email")
    |> query.limit(10)

  // Queries can be composed
  let base_query = query.from(user_schema)

  let active_query = base_query
    |> query.where(query.eq("active", True))

  let admin_query = base_query
    |> query.where(query.eq("role", "admin"))
}
```

## Key Concepts

### 1. Queries are Data

In cquill, queries are represented as data structures (ASTs), not strings. This enables:

- **Type safety** - Invalid queries fail at compile time
- **Composition** - Build complex queries from simple parts
- **Inspection** - Debug and log queries easily

### 2. Adapters Abstract the Backend

The same query patterns work with different backends:

```gleam
// In-memory adapter for testing
let store = memory.new_store()
let assert Ok(rows) = memory.get_all_rows(store, "users")

// PostgreSQL adapter for production
// let assert Ok(rows) = adapter.query(postgres.adapter(), pool, compiled_query)
```

### 3. Explicit Error Handling

All operations return `Result` types with specific error information:

```gleam
import cquill/error

case memory.insert_row(store, "users", "1", row) {
  Ok(new_store) -> handle_success(new_store)
  Error(error.UniqueViolation(constraint, detail)) -> {
    io.println("Duplicate value: " <> constraint)
  }
  Error(error.NotNullViolation(column)) -> {
    io.println("Missing required field: " <> column)
  }
  Error(e) -> handle_other_error(e)
}
```

## Next Steps

- Read about [Schemas](./concepts/schemas.md) to understand how cquill describes your data
- Learn [Query Building](./concepts/queries.md) for more complex queries
- See [CRUD Operations](./guides/crud.md) for common data operations
- Check out [Testing](./guides/testing.md) for testing strategies

## Quick Reference

### Schema Building

```gleam
import cquill/schema
import cquill/schema/field

schema.new("users")
|> schema.add_field(field.integer("id") |> field.primary_key())
|> schema.add_field(field.string("email") |> field.not_null() |> field.unique())
|> schema.add_field(field.string("name") |> field.nullable())
|> schema.add_field(field.boolean("active") |> field.not_null())
|> schema.add_field(field.datetime("created_at") |> field.not_null())
```

### Query Building

```gleam
import cquill/query

// Start from a schema
query.from(user_schema)

// Or from a table name directly
query.from_table("users")

// SELECT specific columns
|> query.select(["id", "email"])

// SELECT *
|> query.select_all()

// WHERE conditions
|> query.where(query.eq("id", 1))
|> query.where(query.gt("age", 18))
|> query.where(query.like("name", "%smith%"))
|> query.where(query.is_null("deleted_at"))

// ORDER BY
|> query.order_by_asc("created_at")
|> query.order_by_desc("updated_at")

// LIMIT / OFFSET
|> query.limit(10)
|> query.offset(20)

// DISTINCT
|> query.distinct()
```

### Conditions

```gleam
// Equality (type-inferred)
query.eq("field", value)
query.not_eq("field", value)

// Typed equality helpers
query.eq("id", 1)
query.eq("name", "Alice")
query.eq("active", True)

// Comparison
query.gt("field", value)   // >
query.gte("field", value)  // >=
query.lt("field", value)   // <
query.lte("field", value)  // <=

// Pattern matching
query.like("field", "%pattern%")

// NULL checks
query.is_null("field")
query.is_not_null("field")

// IN
query.is_in("field", [value1, value2])

// BETWEEN
query.between("field", low, high)

// Logical operators
query.and([cond1, cond2])
query.or([cond1, cond2])
query.not(condition)
```

### Field Types

```gleam
import cquill/schema/field

field.integer("id")           // INT
field.big_integer("count")    // BIGINT
field.float("price")          // FLOAT
field.decimal("amount", 10, 2) // DECIMAL(10,2)
field.string("name")          // VARCHAR/TEXT
field.boolean("active")       // BOOLEAN
field.datetime("created_at")  // TIMESTAMP
field.date("birth_date")      // DATE
field.time("start_time")      // TIME
field.uuid("uuid")            // UUID
field.json("metadata")        // JSON/JSONB
```
