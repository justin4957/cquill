# Advanced Queries

This guide covers advanced query patterns in cquill including joins, aggregations, subqueries, and complex conditions.

## Complex WHERE Conditions

### Combining AND and OR

```gleam
import cquill/query
import cquill/query/ast

// (active = true) AND (role = 'admin' OR role = 'moderator')
let query =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.where(query.or_where([
    query.eq("role", ast.StringValue("admin")),
    query.eq("role", ast.StringValue("moderator")),
  ]))

// (status = 'pending') AND ((priority = 'high') OR (created_at < '2024-01-01'))
let urgent_tasks =
  query.from("tasks")
  |> query.where(query.eq("status", ast.StringValue("pending")))
  |> query.where(query.or_where([
    query.eq("priority", ast.StringValue("high")),
    query.lt("created_at", ast.StringValue("2024-01-01")),
  ]))
```

### Negation

```gleam
// NOT (status = 'deleted')
let active_items =
  query.from("items")
  |> query.where(query.not_where(query.eq("status", ast.StringValue("deleted"))))

// NOT (role = 'guest' AND verified = false)
let trusted_users =
  query.from("users")
  |> query.where(query.not_where(query.and_where([
    query.eq("role", ast.StringValue("guest")),
    query.eq("verified", ast.BoolValue(False)),
  ])))
```

### IN and NOT IN

```gleam
// status IN ('active', 'pending', 'review')
let query =
  query.from("orders")
  |> query.where(query.in_list("status", [
    ast.StringValue("active"),
    ast.StringValue("pending"),
    ast.StringValue("review"),
  ]))

// category_id NOT IN (1, 2, 3)
let query =
  query.from("products")
  |> query.where(query.not_in_list("category_id", [
    ast.IntValue(1),
    ast.IntValue(2),
    ast.IntValue(3),
  ]))
```

### BETWEEN

```gleam
// price BETWEEN 10 AND 100
let query =
  query.from("products")
  |> query.where(query.between(
    "price",
    ast.FloatValue(10.0),
    ast.FloatValue(100.0),
  ))

// created_at BETWEEN '2024-01-01' AND '2024-12-31'
let query =
  query.from("orders")
  |> query.where(query.between(
    "created_at",
    ast.StringValue("2024-01-01"),
    ast.StringValue("2024-12-31"),
  ))
```

### Pattern Matching

```gleam
// LIKE patterns
let query =
  query.from("users")
  |> query.where(query.like("email", "%@gmail.com"))  // Ends with
  |> query.where(query.like("name", "John%"))         // Starts with
  |> query.where(query.like("bio", "%developer%"))    // Contains

// Case-insensitive ILIKE
let query =
  query.from("products")
  |> query.where(query.ilike("name", "%phone%"))

// NOT LIKE
let query =
  query.from("users")
  |> query.where(query.not_like("email", "%@test.com"))
```

## JOINs

### Inner Join

```gleam
// Get orders with customer information
let query =
  query.from("orders")
  |> query.inner_join("customers",
    query.raw_condition("orders.customer_id = customers.id"))
  |> query.select([
    "orders.id",
    "orders.total",
    "customers.name",
    "customers.email",
  ])
```

### Left Join

```gleam
// Get all users with their optional profile
let query =
  query.from("users")
  |> query.left_join("profiles",
    query.raw_condition("users.id = profiles.user_id"))
  |> query.select([
    "users.id",
    "users.email",
    "profiles.bio",
    "profiles.avatar_url",
  ])
```

### Multiple Joins

```gleam
// Orders with customer and product details
let query =
  query.from("orders")
  |> query.inner_join("customers",
    query.raw_condition("orders.customer_id = customers.id"))
  |> query.inner_join("order_items",
    query.raw_condition("orders.id = order_items.order_id"))
  |> query.inner_join("products",
    query.raw_condition("order_items.product_id = products.id"))
  |> query.select([
    "orders.id",
    "customers.name",
    "products.title",
    "order_items.quantity",
  ])
```

### Self Join

```gleam
// Employees with their managers
let query =
  query.from("employees")
  |> query.left_join_as("employees", "managers",
    query.raw_condition("employees.manager_id = managers.id"))
  |> query.select([
    "employees.name AS employee_name",
    "managers.name AS manager_name",
  ])
```

## Aggregations

### COUNT

```gleam
// Count all users
let query =
  query.from("users")
  |> query.select_expr([ast.AggregateExpr(ast.Count, "*")])

// Count active users
let query =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.select_expr([ast.AggregateExpr(ast.Count, "id")])
```

### SUM, AVG, MIN, MAX

```gleam
// Total revenue
let query =
  query.from("orders")
  |> query.select_expr([ast.AggregateExpr(ast.Sum, "total")])

// Average order value
let query =
  query.from("orders")
  |> query.select_expr([ast.AggregateExpr(ast.Avg, "total")])

// Price range
let query =
  query.from("products")
  |> query.select_expr([
    ast.AggregateExpr(ast.Min, "price"),
    ast.AggregateExpr(ast.Max, "price"),
  ])
```

### GROUP BY

```gleam
// Orders per customer
let query =
  query.from("orders")
  |> query.select_expr([
    ast.ColumnExpr("customer_id"),
    ast.AggregateExpr(ast.Count, "id"),
    ast.AggregateExpr(ast.Sum, "total"),
  ])
  |> query.group_by(["customer_id"])

// Sales by month
let query =
  query.from("orders")
  |> query.select_expr([
    ast.FunctionExpr("date_trunc", [
      ast.LiteralExpr(ast.StringValue("month")),
      ast.ColumnExpr("created_at"),
    ]),
    ast.AggregateExpr(ast.Sum, "total"),
  ])
  |> query.group_by(["date_trunc('month', created_at)"])
```

### HAVING

```gleam
// Customers with more than 5 orders
let query =
  query.from("orders")
  |> query.select_expr([
    ast.ColumnExpr("customer_id"),
    ast.AggregateExpr(ast.Count, "id"),
  ])
  |> query.group_by(["customer_id"])
  |> query.having(query.gt("count", ast.IntValue(5)))

// Categories with high average price
let query =
  query.from("products")
  |> query.select_expr([
    ast.ColumnExpr("category_id"),
    ast.AggregateExpr(ast.Avg, "price"),
  ])
  |> query.group_by(["category_id"])
  |> query.having(query.gt("avg", ast.FloatValue(100.0)))
```

## Subqueries

### IN Subquery

```gleam
// Users who have placed orders
let orders_subquery =
  query.from("orders")
  |> query.select(["customer_id"])
  |> query.distinct()

let query =
  query.from("users")
  |> query.where(query.in_subquery("id", orders_subquery))
```

### Correlated Subquery

```gleam
// Products with above-average price in their category
let query =
  query.from("products")
  |> query.where(query.raw(
    "price > (SELECT AVG(price) FROM products p2 WHERE p2.category_id = products.category_id)",
    [],
  ))
```

## Raw SQL

For queries that can't be expressed with the builder:

```gleam
// Raw condition
let query =
  query.from("users")
  |> query.where(query.raw("age > $1 AND verified = $2", [
    ast.IntValue(18),
    ast.BoolValue(True),
  ]))

// Complex expression
let query =
  query.from("products")
  |> query.where(query.raw(
    "price * quantity > $1",
    [ast.FloatValue(1000.0)],
  ))
```

## Query Composition

### Reusable Query Parts

```gleam
// Base query builder
fn base_products_query() {
  query.from("products")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.where(query.is_null("deleted_at"))
}

// Add category filter
fn in_category(q, category_id: Int) {
  q |> query.where(query.eq("category_id", ast.IntValue(category_id)))
}

// Add price range
fn price_range(q, min: Float, max: Float) {
  q
  |> query.where(query.gte("price", ast.FloatValue(min)))
  |> query.where(query.lte("price", ast.FloatValue(max)))
}

// Add search
fn search_name(q, term: String) {
  q |> query.where(query.ilike("name", "%" <> term <> "%"))
}

// Combine them
let affordable_electronics =
  base_products_query()
  |> in_category(1)
  |> price_range(0.0, 100.0)
  |> search_name("phone")
```

### Conditional Query Building

```gleam
pub type ProductFilters {
  ProductFilters(
    category_id: Option(Int),
    min_price: Option(Float),
    max_price: Option(Float),
    search: Option(String),
    in_stock: Option(Bool),
  )
}

pub fn build_product_query(filters: ProductFilters) {
  query.from("products")
  |> apply_if(filters.category_id, fn(q, cat_id) {
    query.where(q, query.eq("category_id", ast.IntValue(cat_id)))
  })
  |> apply_if(filters.min_price, fn(q, min) {
    query.where(q, query.gte("price", ast.FloatValue(min)))
  })
  |> apply_if(filters.max_price, fn(q, max) {
    query.where(q, query.lte("price", ast.FloatValue(max)))
  })
  |> apply_if(filters.search, fn(q, term) {
    query.where(q, query.ilike("name", "%" <> term <> "%"))
  })
  |> apply_if(filters.in_stock, fn(q, in_stock) {
    query.where(q, query.gt("stock_count", ast.IntValue(0)))
  })
}

fn apply_if(q, option: Option(a), f: fn(Query, a) -> Query) -> Query {
  case option {
    Some(value) -> f(q, value)
    None -> q
  }
}
```

## Performance Considerations

### Select Only Needed Columns

```gleam
// Good: Select only what you need
let query =
  query.from("users")
  |> query.select(["id", "name", "email"])

// Avoid: Fetching all columns when you don't need them
let query =
  query.from("users")
  |> query.select_all()  // Fetches all columns including blobs
```

### Use Indexes

```gleam
// Good: Filter on indexed columns first
let query =
  query.from("orders")
  |> query.where(query.eq("customer_id", ast.IntValue(1)))  // Indexed
  |> query.where(query.gte("total", ast.FloatValue(100.0)))

// Consider: Add index for frequently filtered columns
// CREATE INDEX idx_orders_customer_id ON orders(customer_id);
```

### Limit Results

```gleam
// Good: Always limit when you don't need all results
let query =
  query.from("logs")
  |> query.order_by_desc("created_at")
  |> query.limit(100)

// Avoid: Fetching unlimited results
let query =
  query.from("logs")  // Could be millions of rows
```

## Next Steps

- See [Transactions](./transactions.md) for atomic operations
- Read about [Testing](./testing.md) your queries
- Check [Pagination recipes](../recipes/pagination.md)
