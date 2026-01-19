# Getting Started with cquill

This guide will help you get cquill installed and running your first database query.

## Installation

Add cquill to your Gleam project:

```sh
gleam add cquill
```

For PostgreSQL support, you'll also need the pgo driver:

```sh
gleam add pgo
```

## Your First Query

Here's a complete example showing how to query a PostgreSQL database:

```gleam
import cquill/adapter/memory
import cquill/query
import cquill/query/ast
import cquill/repo
import gleam/dict
import gleam/io
import gleam/option.{None, Some}

pub fn main() {
  // Create an in-memory store for testing
  let store = memory.new_store()

  // Create a table
  let store = memory.create_table(store, "users", ["id", "email", "name", "active"])

  // Insert some data
  let store = memory.insert_row(store, "users", dict.from_list([
    #("id", ast.IntValue(1)),
    #("email", ast.StringValue("alice@example.com")),
    #("name", ast.StringValue("Alice")),
    #("active", ast.BoolValue(True)),
  ]))

  let store = memory.insert_row(store, "users", dict.from_list([
    #("id", ast.IntValue(2)),
    #("email", ast.StringValue("bob@example.com")),
    #("name", ast.StringValue("Bob")),
    #("active", ast.BoolValue(False)),
  ]))

  // Build a query for active users
  let active_users_query =
    query.from("users")
    |> query.where(query.eq("active", ast.BoolValue(True)))
    |> query.select(["id", "email", "name"])
    |> query.order_by_asc("email")

  // Execute the query
  case memory.execute_select(store, active_users_query) {
    Ok(rows) -> {
      io.println("Found " <> int.to_string(list.length(rows)) <> " active users:")
      list.each(rows, fn(row) {
        io.println("  - " <> string.inspect(row))
      })
    }
    Error(e) -> {
      io.println_error("Query failed: " <> string.inspect(e))
    }
  }
}
```

## Key Concepts

### 1. Queries are Data

In cquill, queries are represented as data structures (ASTs), not strings. This enables:

- **Type safety** - Invalid queries fail at compile time
- **Composition** - Build complex queries from simple parts
- **Inspection** - Debug and log queries easily

```gleam
// Queries can be composed
let base_query = query.from("users")

let active_query = base_query
  |> query.where(query.eq("active", ast.BoolValue(True)))

let admin_query = base_query
  |> query.where(query.eq("role", ast.StringValue("admin")))
```

### 2. Adapters Abstract the Backend

The same query works with different backends:

```gleam
// In-memory adapter for testing
let result = memory.execute_select(store, my_query)

// PostgreSQL adapter for production (when implemented)
// let result = postgres.execute_select(pool, my_query)
```

### 3. Explicit Error Handling

All operations return `Result` types with specific error information:

```gleam
case memory.execute_select(store, query) {
  Ok(rows) -> handle_success(rows)
  Error(error.NotFound) -> handle_not_found()
  Error(error.QueryFailed(msg, code)) -> handle_query_error(msg)
  Error(e) -> handle_other_error(e)
}
```

## Next Steps

- Read about [Schemas](./concepts/schemas.md) to understand how cquill describes your data
- Learn [Query Building](./concepts/queries.md) for more complex queries
- See [CRUD Operations](./guides/crud.md) for common data operations
- Check out [Testing](./guides/testing.md) for testing strategies

## Quick Reference

### Query Building

```gleam
import cquill/query
import cquill/query/ast

// SELECT
query.from("users")
|> query.select(["id", "email"])
|> query.select_all()  // SELECT *

// WHERE
|> query.where(query.eq("id", ast.IntValue(1)))
|> query.where(query.gt("age", ast.IntValue(18)))
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
// Equality
query.eq("field", value)
query.not_eq("field", value)

// Comparison
query.gt("field", value)   // >
query.gte("field", value)  // >=
query.lt("field", value)   // <
query.lte("field", value)  // <=

// Pattern matching
query.like("field", "%pattern%")
query.ilike("field", "%pattern%")  // Case insensitive

// NULL checks
query.is_null("field")
query.is_not_null("field")

// IN
query.in_list("field", [value1, value2])

// BETWEEN
query.between("field", low, high)

// Logical operators
query.and_where([cond1, cond2])
query.or_where([cond1, cond2])
query.not_where(condition)
```

### Value Types

```gleam
import cquill/query/ast

ast.IntValue(42)
ast.FloatValue(3.14)
ast.StringValue("hello")
ast.BoolValue(True)
ast.NullValue
ast.ListValue([ast.IntValue(1), ast.IntValue(2)])
```
