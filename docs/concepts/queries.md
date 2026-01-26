# Queries

Queries in cquill are data structures, not strings. This fundamental design choice enables type safety, composition, and inspection that string-based queries cannot provide.

## Queries as Data

Every query in cquill is represented as an Abstract Syntax Tree (AST):

```gleam
import cquill/query
import cquill/schema
import cquill/schema/field

// Define a schema first
let user_schema = schema.new("users")
  |> schema.add_field(field.integer("id") |> field.primary_key())
  |> schema.add_field(field.boolean("active"))
  |> schema.add_field(field.datetime("created_at"))

// This creates a Query data structure, not a SQL string
let my_query =
  query.from(user_schema)
  |> query.where(query.eq("active", True))
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

Start every query by specifying the source:

```gleam
// From a schema (preferred - provides type safety)
query.from(user_schema)

// From a table name directly (for simple queries)
query.from_table("users")

// With schema qualification
query.from_qualified("public", "users")
```

### SELECT Clause

Specify which columns to retrieve:

```gleam
// Select all columns (default)
query.from_table("users")
|> query.select_all()

// Select specific columns
query.from_table("users")
|> query.select(["id", "email", "name"])

// Select with expressions (for computed columns, aggregates)
import cquill/query/ast
query.from_table("users")
|> query.select_expr([
  ast.SelectExpression(ast.ColumnExpr("id"), option.None),
  ast.SelectExpression(ast.AggregateExpr(ast.Count, "id"), option.Some("user_count")),
])
```

### WHERE Clause

Filter results with conditions:

```gleam
// Single condition (type-inferred values)
query.from_table("users")
|> query.where(query.eq("active", True))

// Multiple conditions (AND)
query.from_table("users")
|> query.where(query.eq("active", True))
|> query.where(query.gt("age", 18))

// Using typed helpers for explicit types
query.from_table("users")
|> query.where(query.eq("active", True))
|> query.where(query.gt("age", 18))

// OR conditions
query.from_table("users")
|> query.where(query.or([
  query.eq("role", "admin"),
  query.eq("role", "moderator"),
]))

// Complex conditions with AND/OR
query.from_table("users")
|> query.where(query.and([
  query.eq("active", True),
  query.or([
    query.gt("age", 21),
    query.eq("verified", True),
  ]),
]))
```

### Condition Types

```gleam
// Equality (type-inferred)
query.eq("field", value)
query.not_eq("field", value)

// Typed equality helpers
query.eq("id", 1)
query.eq("name", "Alice")
query.eq("active", True)

// Comparison (type-inferred)
query.gt("field", value)   // Greater than
query.gte("field", value)  // Greater than or equal
query.lt("field", value)   // Less than
query.lte("field", value)  // Less than or equal

// Typed comparison helpers
query.gt("age", 18)
query.lt("score", 100)

// Pattern matching
query.like("name", "%smith%")
query.not_like("name", "%test%")

// NULL checks
query.is_null("deleted_at")
query.is_not_null("email")

// IN clause
query.is_in("status", ["pending", "active"])
query.is_not_in("status", ["deleted"])

// Typed IN helpers
query.is_in("id", [1, 2, 3])
query.is_in("role", ["admin", "moderator"])

// BETWEEN
query.between("age", 18, 65)

// Negation
query.not(query.eq("role", "guest"))
```

### ORDER BY Clause

Sort results:

```gleam
// Ascending order
query.from_table("users")
|> query.order_by_asc("created_at")

// Descending order
query.from_table("users")
|> query.order_by_desc("score")

// Multiple sort columns
query.from_table("users")
|> query.order_by_desc("score")
|> query.order_by_asc("name")

// With direction parameter
query.from_table("users")
|> query.order_by("score", query.desc)

// With NULL handling
query.from_table("users")
|> query.order_by_with_nulls("score", query.desc, query.nulls_last)
```

### LIMIT and OFFSET

Paginate results:

```gleam
// Limit results
query.from_table("users")
|> query.limit(10)

// With offset (for pagination)
query.from_table("users")
|> query.limit(10)
|> query.offset(20)  // Skip first 20 rows

// Using the paginate helper
query.from_table("users")
|> query.paginate(page: 3, per_page: 10)  // Page 3 with 10 items per page
```

### DISTINCT

Remove duplicate rows:

```gleam
query.from_table("users")
|> query.select(["country"])
|> query.distinct()
```

### GROUP BY and HAVING

Aggregate data:

```gleam
import cquill/query/ast

query.from_table("orders")
|> query.select_expr([
  ast.SelectExpression(ast.ColumnExpr("customer_id"), option.None),
  ast.SelectExpression(ast.AggregateExpr(ast.Sum, "total"), option.Some("total_sum")),
])
|> query.group_by("customer_id")
|> query.having(query.gt("total_sum", 1000))
```

### JOIN Clause

Join multiple tables:

```gleam
query.from_table("orders")
|> query.join("customers", on: query.eq("orders.customer_id", "customers.id"))
|> query.left_join("products", on: query.eq("orders.product_id", "products.id"))
|> query.select(["orders.id", "customers.name", "products.title"])
```

Join types:
- `query.join()` - Inner join (only matching rows)
- `query.left_join()` - All left rows, matching right rows
- `query.right_join()` - All right rows, matching left rows
- `query.full_join()` - All rows from both tables
- `query.cross_join()` - Cartesian product

## Query Composition

Queries are immutable and composable:

```gleam
// Base query from a schema
let users = query.from(user_schema)

// Add conditions to create new queries
let active_users = users
  |> query.where(query.eq("active", True))

let admin_users = users
  |> query.where(query.eq("role", "admin"))

// Create reusable query parts
fn paginated(q, page, per_page) {
  q
  |> query.paginate(page: page, per_page: per_page)
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
  |> query.where(query.gte("created_at", start_date))
  |> query.where(query.lt("created_at", end_date))
}

// Filter by search term
fn search_name(q, term) {
  q
  |> query.where(query.like("name", "%" <> term <> "%"))
}

// Combine multiple filters
let results =
  query.from(user_schema)
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

For complex queries that can't be expressed with the builder, use raw SQL conditions via the AST:

```gleam
import cquill/query/ast

// Create a raw condition
let raw_cond = ast.Raw("age > $1 AND verified = $2", [ast.IntValue(18), ast.BoolValue(True)])

query.from_table("users")
|> query.where(raw_cond)
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
