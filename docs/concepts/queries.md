# Queries

Queries in cquill are data structures, not strings. This fundamental design choice enables type safety, composition, and inspection that string-based queries cannot provide.

## Queries as Data

Every query in cquill is represented as an Abstract Syntax Tree (AST):

```gleam
import cquill/query
import cquill/query/ast

// This creates a Query data structure, not a SQL string
let my_query =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.order_by_asc("created_at")
  |> query.limit(10)
```

The query above produces a data structure like:

```gleam
Query(
  source: TableSource(table: "users", schema_name: None),
  select: SelectAll,
  wheres: [Where(Eq("active", BoolValue(True)))],
  order_bys: [OrderBy("created_at", Asc, NullsDefault)],
  limit: Some(10),
  offset: None,
  joins: [],
  distinct: False,
  group_bys: [],
  havings: [],
)
```

## Building Queries

### FROM Clause

Start every query by specifying the source table:

```gleam
// Simple table reference
query.from("users")

// With schema qualification
query.from_qualified("public", "users")
```

### SELECT Clause

Specify which columns to retrieve:

```gleam
// Select all columns (default)
query.from("users")
|> query.select_all()

// Select specific columns
query.from("users")
|> query.select(["id", "email", "name"])

// Select with expressions
query.from("users")
|> query.select_expr([
  ast.ColumnExpr("id"),
  ast.AggregateExpr(ast.Count, "id"),
])
```

### WHERE Clause

Filter results with conditions:

```gleam
import cquill/query/ast

// Single condition
query.from("users")
|> query.where(query.eq("active", ast.BoolValue(True)))

// Multiple conditions (AND)
query.from("users")
|> query.where(query.eq("active", ast.BoolValue(True)))
|> query.where(query.gt("age", ast.IntValue(18)))

// OR conditions
query.from("users")
|> query.where(query.or_where([
  query.eq("role", ast.StringValue("admin")),
  query.eq("role", ast.StringValue("moderator")),
]))

// Complex conditions
query.from("users")
|> query.where(query.and_where([
  query.eq("active", ast.BoolValue(True)),
  query.or_where([
    query.gt("age", ast.IntValue(21)),
    query.eq("verified", ast.BoolValue(True)),
  ]),
]))
```

### Condition Types

```gleam
// Equality
query.eq("field", value)
query.not_eq("field", value)

// Comparison
query.gt("field", value)   // Greater than
query.gte("field", value)  // Greater than or equal
query.lt("field", value)   // Less than
query.lte("field", value)  // Less than or equal

// Pattern matching
query.like("name", "%smith%")
query.not_like("name", "%test%")
query.ilike("name", "%SMITH%")  // Case insensitive

// NULL checks
query.is_null("deleted_at")
query.is_not_null("email")

// IN clause
query.in_list("status", [
  ast.StringValue("pending"),
  ast.StringValue("active"),
])
query.not_in_list("status", [ast.StringValue("deleted")])

// BETWEEN
query.between("age", ast.IntValue(18), ast.IntValue(65))

// Negation
query.not_where(query.eq("role", ast.StringValue("guest")))
```

### ORDER BY Clause

Sort results:

```gleam
// Ascending order
query.from("users")
|> query.order_by_asc("created_at")

// Descending order
query.from("users")
|> query.order_by_desc("score")

// Multiple sort columns
query.from("users")
|> query.order_by_desc("score")
|> query.order_by_asc("name")

// With NULL handling
query.from("users")
|> query.order_by("score", ast.Desc, ast.NullsLast)
```

### LIMIT and OFFSET

Paginate results:

```gleam
// Limit results
query.from("users")
|> query.limit(10)

// With offset (for pagination)
query.from("users")
|> query.limit(10)
|> query.offset(20)  // Skip first 20 rows
```

### DISTINCT

Remove duplicate rows:

```gleam
query.from("users")
|> query.select(["country"])
|> query.distinct()
```

### GROUP BY and HAVING

Aggregate data:

```gleam
query.from("orders")
|> query.select_expr([
  ast.ColumnExpr("customer_id"),
  ast.AggregateExpr(ast.Sum, "total"),
])
|> query.group_by(["customer_id"])
|> query.having(query.gt("sum_total", ast.IntValue(1000)))
```

### JOIN Clause

Join multiple tables:

```gleam
query.from("orders")
|> query.inner_join("customers", query.eq("orders.customer_id", "customers.id"))
|> query.left_join("products", query.eq("orders.product_id", "products.id"))
|> query.select(["orders.id", "customers.name", "products.title"])
```

Join types:
- `query.inner_join()` - Only matching rows
- `query.left_join()` - All left rows, matching right rows
- `query.right_join()` - All right rows, matching left rows
- `query.full_join()` - All rows from both tables

## Query Composition

Queries are immutable and composable:

```gleam
// Base query
let users = query.from("users")

// Add conditions to create new queries
let active_users = users
  |> query.where(query.eq("active", ast.BoolValue(True)))

let admin_users = users
  |> query.where(query.eq("role", ast.StringValue("admin")))

// Combine for reusable query parts
fn paginated(q, page, per_page) {
  q
  |> query.limit(per_page)
  |> query.offset((page - 1) * per_page)
}

let page_1 = active_users |> paginated(1, 20)
let page_2 = active_users |> paginated(2, 20)
```

### Query Functions

Create reusable query builders:

```gleam
// Filter by date range
fn created_between(q, start_date, end_date) {
  q
  |> query.where(query.gte("created_at", ast.StringValue(start_date)))
  |> query.where(query.lt("created_at", ast.StringValue(end_date)))
}

// Filter by search term
fn search_name(q, term) {
  q
  |> query.where(query.ilike("name", "%" <> term <> "%"))
}

// Combine multiple filters
let results =
  query.from("users")
  |> created_between("2024-01-01", "2024-12-31")
  |> search_name("john")
  |> query.order_by_asc("name")
```

## INSERT, UPDATE, DELETE Queries

### INSERT

```gleam
import cquill/query/ast

// Single row insert
let insert_query =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values([[
    ast.StringValue("alice@example.com"),
    ast.StringValue("Alice"),
  ]])
  |> ast.returning(["id"])

// Multiple row insert
let bulk_insert =
  ast.new_insert("users")
  |> ast.insert_columns(["email", "name"])
  |> ast.insert_values([
    [ast.StringValue("alice@example.com"), ast.StringValue("Alice")],
    [ast.StringValue("bob@example.com"), ast.StringValue("Bob")],
  ])
```

### UPDATE

```gleam
let update_query =
  ast.new_update("users")
  |> ast.set("name", ast.StringValue("Alice Smith"))
  |> ast.set("updated_at", ast.StringValue("2024-01-15T10:00:00Z"))
  |> ast.update_where(query.eq("id", ast.IntValue(1)))
  |> ast.returning(["id", "name"])
```

### DELETE

```gleam
let delete_query =
  ast.new_delete("users")
  |> ast.delete_where(query.eq("id", ast.IntValue(1)))
  |> ast.returning(["id"])
```

## Raw SQL

For complex queries that can't be expressed with the builder:

```gleam
// Raw condition
query.from("users")
|> query.where(query.raw("age > ? AND verified = ?", [
  ast.IntValue(18),
  ast.BoolValue(True),
]))
```

## Query Inspection

Since queries are data, you can inspect them:

```gleam
// Get the table name
let table = case my_query.source {
  ast.TableSource(table, _) -> table
  _ -> "unknown"
}

// Count WHERE conditions
let condition_count = list.length(my_query.wheres)

// Check if query has pagination
let is_paginated = option.is_some(my_query.limit)
```

## Next Steps

- Learn about the [Repo](./repo.md) for executing queries
- See [Advanced Queries](../guides/queries.md) for complex examples
- Read about [Transactions](../guides/transactions.md)
