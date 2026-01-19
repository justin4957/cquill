# API Reference

Complete reference for all public cquill modules and functions.

## cquill/query

Query builder module for constructing SELECT queries.

### `from(table: String) -> Query(Nil)`

Create a new query for a table.

```gleam
let q = query.from("users")
```

### `from_qualified(schema: String, table: String) -> Query(Nil)`

Create a query with schema qualification.

```gleam
let q = query.from_qualified("public", "users")
```

### `select(q: Query(a), fields: List(String)) -> Query(a)`

Select specific columns.

```gleam
query.from("users")
|> query.select(["id", "email", "name"])
```

### `select_all(q: Query(a)) -> Query(a)`

Select all columns (SELECT *).

```gleam
query.from("users")
|> query.select_all()
```

### `where(q: Query(a), condition: Condition) -> Query(a)`

Add a WHERE condition. Multiple calls are AND'd together.

```gleam
query.from("users")
|> query.where(query.eq("active", ast.BoolValue(True)))
|> query.where(query.gt("age", ast.IntValue(18)))
```

### `order_by_asc(q: Query(a), field: String) -> Query(a)`

Add ascending ORDER BY.

```gleam
query.from("users")
|> query.order_by_asc("created_at")
```

### `order_by_desc(q: Query(a), field: String) -> Query(a)`

Add descending ORDER BY.

```gleam
query.from("users")
|> query.order_by_desc("score")
```

### `limit(q: Query(a), n: Int) -> Query(a)`

Limit result count.

```gleam
query.from("users")
|> query.limit(10)
```

### `offset(q: Query(a), n: Int) -> Query(a)`

Skip first n rows.

```gleam
query.from("users")
|> query.limit(10)
|> query.offset(20)
```

### `distinct(q: Query(a)) -> Query(a)`

Select only distinct rows.

```gleam
query.from("orders")
|> query.select(["customer_id"])
|> query.distinct()
```

### `group_by(q: Query(a), fields: List(String)) -> Query(a)`

Group results by fields.

```gleam
query.from("orders")
|> query.group_by(["customer_id"])
```

### `having(q: Query(a), condition: Condition) -> Query(a)`

Filter grouped results.

```gleam
query.from("orders")
|> query.group_by(["customer_id"])
|> query.having(query.gt("count", ast.IntValue(5)))
```

## Condition Functions

### `eq(field: String, value: Value) -> Condition`

Equality condition (field = value).

```gleam
query.eq("id", ast.IntValue(1))
```

### `not_eq(field: String, value: Value) -> Condition`

Inequality condition (field != value).

```gleam
query.not_eq("status", ast.StringValue("deleted"))
```

### `gt(field: String, value: Value) -> Condition`

Greater than (field > value).

```gleam
query.gt("age", ast.IntValue(18))
```

### `gte(field: String, value: Value) -> Condition`

Greater than or equal (field >= value).

```gleam
query.gte("score", ast.IntValue(100))
```

### `lt(field: String, value: Value) -> Condition`

Less than (field < value).

```gleam
query.lt("price", ast.FloatValue(50.0))
```

### `lte(field: String, value: Value) -> Condition`

Less than or equal (field <= value).

```gleam
query.lte("quantity", ast.IntValue(10))
```

### `like(field: String, pattern: String) -> Condition`

Pattern match (field LIKE pattern).

```gleam
query.like("email", "%@gmail.com")
```

### `ilike(field: String, pattern: String) -> Condition`

Case-insensitive pattern match.

```gleam
query.ilike("name", "%john%")
```

### `is_null(field: String) -> Condition`

NULL check (field IS NULL).

```gleam
query.is_null("deleted_at")
```

### `is_not_null(field: String) -> Condition`

NOT NULL check (field IS NOT NULL).

```gleam
query.is_not_null("email")
```

### `in_list(field: String, values: List(Value)) -> Condition`

IN clause (field IN (values)).

```gleam
query.in_list("status", [
  ast.StringValue("active"),
  ast.StringValue("pending"),
])
```

### `between(field: String, low: Value, high: Value) -> Condition`

BETWEEN clause (field BETWEEN low AND high).

```gleam
query.between("age", ast.IntValue(18), ast.IntValue(65))
```

### `and_where(conditions: List(Condition)) -> Condition`

Combine conditions with AND.

```gleam
query.and_where([
  query.eq("active", ast.BoolValue(True)),
  query.gt("age", ast.IntValue(18)),
])
```

### `or_where(conditions: List(Condition)) -> Condition`

Combine conditions with OR.

```gleam
query.or_where([
  query.eq("role", ast.StringValue("admin")),
  query.eq("role", ast.StringValue("moderator")),
])
```

### `not_where(condition: Condition) -> Condition`

Negate a condition.

```gleam
query.not_where(query.eq("status", ast.StringValue("deleted")))
```

## cquill/query/ast

Query AST types and constructors.

### Value Types

```gleam
pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  BoolValue(Bool)
  NullValue
  ParamValue(position: Int)
  ListValue(List(Value))
}
```

### `new_insert(table: String) -> InsertQuery(Nil)`

Create a new INSERT query.

```gleam
let insert = ast.new_insert("users")
```

### `insert_columns(q: InsertQuery(a), columns: List(String)) -> InsertQuery(a)`

Set columns to insert.

```gleam
ast.new_insert("users")
|> ast.insert_columns(["email", "name"])
```

### `insert_values(q: InsertQuery(a), values: List(List(Value))) -> InsertQuery(a)`

Set values to insert.

```gleam
ast.new_insert("users")
|> ast.insert_columns(["email", "name"])
|> ast.insert_values([[
  ast.StringValue("alice@example.com"),
  ast.StringValue("Alice"),
]])
```

### `returning(q: InsertQuery(a), columns: List(String)) -> InsertQuery(a)`

Specify RETURNING columns.

```gleam
ast.new_insert("users")
|> ast.insert_columns(["email"])
|> ast.insert_values([[ast.StringValue("test@example.com")]])
|> ast.returning(["id", "created_at"])
```

### `new_update(table: String) -> UpdateQuery(Nil)`

Create a new UPDATE query.

```gleam
let update = ast.new_update("users")
```

### `set(q: UpdateQuery(a), column: String, value: Value) -> UpdateQuery(a)`

Set a column value.

```gleam
ast.new_update("users")
|> ast.set("name", ast.StringValue("Alice"))
|> ast.set("updated_at", ast.StringValue("2024-01-15"))
```

### `update_where(q: UpdateQuery(a), condition: Condition) -> UpdateQuery(a)`

Add WHERE condition to UPDATE.

```gleam
ast.new_update("users")
|> ast.set("active", ast.BoolValue(False))
|> ast.update_where(query.eq("id", ast.IntValue(1)))
```

### `new_delete(table: String) -> DeleteQuery(Nil)`

Create a new DELETE query.

```gleam
let delete = ast.new_delete("users")
```

### `delete_where(q: DeleteQuery(a), condition: Condition) -> DeleteQuery(a)`

Add WHERE condition to DELETE.

```gleam
ast.new_delete("users")
|> ast.delete_where(query.eq("id", ast.IntValue(1)))
```

## cquill/repo

Repository module for executing queries.

### `all(adapter, query: Query(a)) -> Result(List(Row), AdapterError)`

Execute query and return all matching rows.

```gleam
case repo.all(adapter, query) {
  Ok(rows) -> handle_rows(rows)
  Error(e) -> handle_error(e)
}
```

### `one(adapter, query: Query(a)) -> Result(Row, AdapterError)`

Execute query and return exactly one row.

Returns `Error(NotFound)` if no rows, `Error(TooManyRows(1, n))` if more than one.

```gleam
case repo.one(adapter, query) {
  Ok(row) -> handle_row(row)
  Error(error.NotFound) -> handle_not_found()
  Error(e) -> handle_error(e)
}
```

### `insert(adapter, query: InsertQuery(a)) -> Result(List(Row), AdapterError)`

Execute INSERT and return RETURNING rows.

```gleam
case repo.insert(adapter, insert) {
  Ok(rows) -> handle_inserted(rows)
  Error(error.UniqueViolation(_, _)) -> handle_duplicate()
  Error(e) -> handle_error(e)
}
```

### `update(adapter, query: UpdateQuery(a)) -> Result(List(Row), AdapterError)`

Execute UPDATE and return affected/RETURNING rows.

```gleam
case repo.update(adapter, update) {
  Ok(rows) -> handle_updated(rows)
  Error(e) -> handle_error(e)
}
```

### `delete(adapter, query: DeleteQuery(a)) -> Result(Int, AdapterError)`

Execute DELETE and return count of deleted rows.

```gleam
case repo.delete(adapter, delete) {
  Ok(count) -> io.println("Deleted " <> int.to_string(count))
  Error(e) -> handle_error(e)
}
```

### `transaction(adapter, fn: fn(Tx) -> Result(a, e)) -> Result(a, e)`

Execute operations in a transaction.

```gleam
repo.transaction(adapter, fn(tx) {
  use user <- result.try(repo.insert(tx, user_insert))
  use profile <- result.try(repo.insert(tx, profile_insert))
  Ok(#(user, profile))
})
```

## cquill/adapter/memory

In-memory adapter for testing.

### `new_store() -> MemoryStore`

Create a new empty store.

```gleam
let store = memory.new_store()
```

### `create_table(store, name: String, columns: List(String)) -> MemoryStore`

Create a table in the store.

```gleam
let store = memory.create_table(store, "users", ["id", "email", "name"])
```

### `insert_row(store, table: String, row: Dict(String, Value)) -> MemoryStore`

Insert a row into a table.

```gleam
let store = memory.insert_row(store, "users", dict.from_list([
  #("id", ast.IntValue(1)),
  #("email", ast.StringValue("test@example.com")),
]))
```

### `execute_select(store, query: Query(a)) -> Result(List(Row), AdapterError)`

Execute a SELECT query.

```gleam
case memory.execute_select(store, query) {
  Ok(rows) -> handle_rows(rows)
  Error(e) -> handle_error(e)
}
```

### `begin_transaction(store) -> #(MemoryStore, Transaction)`

Start a transaction.

```gleam
let #(store, tx) = memory.begin_transaction(store)
```

### `commit_transaction(store, tx: Transaction) -> MemoryStore`

Commit a transaction.

```gleam
let store = memory.commit_transaction(store, tx)
```

### `rollback_transaction(store, tx: Transaction) -> MemoryStore`

Rollback a transaction.

```gleam
let store = memory.rollback_transaction(store, tx)
```

## cquill/telemetry

Telemetry and observability module.

### `start() -> Result(Nil, actor.StartError)`

Start the telemetry server.

```gleam
let assert Ok(_) = telemetry.start()
```

### `attach(handler_id: String, events: List(EventType), handler: Handler) -> Result(Nil, AttachError)`

Attach a telemetry handler.

```gleam
telemetry.attach(
  "my-logger",
  [telemetry.QueryStopType, telemetry.QueryExceptionType],
  telemetry.logger_handler(),
)
```

### `detach(handler_id: String) -> Result(Nil, DetachError)`

Detach a handler.

```gleam
telemetry.detach("my-logger")
```

### `emit(event: Event, metadata: Metadata) -> Nil`

Emit a telemetry event.

```gleam
telemetry.emit(
  telemetry.QueryStop(telemetry.QueryStopEvent(...)),
  telemetry.empty_metadata(),
)
```

### `logger_handler() -> Handler`

Built-in handler that logs queries.

```gleam
telemetry.attach("logger", [telemetry.QueryStopType], telemetry.logger_handler())
```

### `slow_query_handler(threshold_ms: Int) -> Handler`

Built-in handler that logs slow queries.

```gleam
telemetry.attach("slow", [telemetry.QueryStopType], telemetry.slow_query_handler(100))
```

## cquill/dev

Development mode module.

### `enable() -> Result(Nil, DevError)`

Enable dev mode with default configuration.

```gleam
dev.enable()
```

### `enable_with_config(config: DevConfig) -> Result(Nil, DevError)`

Enable dev mode with custom configuration.

```gleam
dev.enable_with_config(dev.verbose_config())
```

### `disable() -> Result(Nil, DevError)`

Disable dev mode.

```gleam
dev.disable()
```

### `is_enabled() -> Bool`

Check if dev mode is enabled.

```gleam
case dev.is_enabled() {
  True -> io.println("Dev mode active")
  False -> io.println("Dev mode inactive")
}
```

### `default_config() -> DevConfig`

Get default dev configuration.

### `minimal_config() -> DevConfig`

Get minimal dev configuration (queries only).

### `verbose_config() -> DevConfig`

Get verbose dev configuration (all debugging).
