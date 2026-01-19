# Types Reference

Complete reference for all public types in cquill.

## Query Types

### Query

Represents a SELECT query AST.

```gleam
pub type Query(schema) {
  Query(
    source: Source,
    select: Select,
    wheres: List(Where),
    order_bys: List(OrderBy),
    limit: Option(Int),
    offset: Option(Int),
    joins: List(Join),
    distinct: Bool,
    group_bys: List(String),
    havings: List(Where),
  )
}
```

### InsertQuery

Represents an INSERT query AST.

```gleam
pub type InsertQuery(schema) {
  InsertQuery(
    table: String,
    schema_name: Option(String),
    columns: List(String),
    values: List(List(Value)),
    on_conflict: Option(OnConflict),
    returning: List(String),
  )
}
```

### UpdateQuery

Represents an UPDATE query AST.

```gleam
pub type UpdateQuery(schema) {
  UpdateQuery(
    table: String,
    schema_name: Option(String),
    sets: List(SetClause),
    wheres: List(Where),
    returning: List(String),
  )
}
```

### DeleteQuery

Represents a DELETE query AST.

```gleam
pub type DeleteQuery(schema) {
  DeleteQuery(
    table: String,
    schema_name: Option(String),
    wheres: List(Where),
    returning: List(String),
  )
}
```

## Value Types

### Value

Represents a value in a query.

```gleam
pub type Value {
  /// Integer value
  IntValue(Int)
  /// Float value
  FloatValue(Float)
  /// String value
  StringValue(String)
  /// Boolean value
  BoolValue(Bool)
  /// NULL value
  NullValue
  /// Positional parameter ($1, $2, etc.)
  ParamValue(position: Int)
  /// List of values (for IN clauses)
  ListValue(List(Value))
}
```

## Condition Types

### Condition

Represents a WHERE/HAVING condition.

```gleam
pub type Condition {
  /// field = value
  Eq(field: String, value: Value)
  /// field != value
  NotEq(field: String, value: Value)
  /// field > value
  Gt(field: String, value: Value)
  /// field >= value
  Gte(field: String, value: Value)
  /// field < value
  Lt(field: String, value: Value)
  /// field <= value
  Lte(field: String, value: Value)
  /// field IN (values...)
  In(field: String, values: List(Value))
  /// field NOT IN (values...)
  NotIn(field: String, values: List(Value))
  /// field LIKE pattern
  Like(field: String, pattern: String)
  /// field NOT LIKE pattern
  NotLike(field: String, pattern: String)
  /// field ILIKE pattern
  ILike(field: String, pattern: String)
  /// field IS NULL
  IsNull(field: String)
  /// field IS NOT NULL
  IsNotNull(field: String)
  /// field BETWEEN low AND high
  Between(field: String, low: Value, high: Value)
  /// Combined conditions (AND)
  And(List(Condition))
  /// Alternative conditions (OR)
  Or(List(Condition))
  /// Negated condition
  Not(Condition)
  /// Raw SQL condition
  Raw(sql: String, params: List(Value))
}
```

## Source Types

### Source

Represents the FROM source of a query.

```gleam
pub type Source {
  /// Query from a table
  TableSource(table: String, schema_name: Option(String))
  /// Query from a subquery
  SubquerySource(query: Query(Nil))
}
```

## Select Types

### Select

Represents what to SELECT.

```gleam
pub type Select {
  /// SELECT *
  SelectAll
  /// SELECT specific fields
  SelectFields(List(String))
  /// SELECT expressions
  SelectExpr(List(SelectExpression))
}
```

### SelectExpression

A select expression with optional alias.

```gleam
pub type SelectExpression {
  SelectExpression(expr: Expr, alias: Option(String))
}
```

### Expr

Expression types for SELECT.

```gleam
pub type Expr {
  /// Column reference
  ColumnExpr(field: String)
  /// Qualified column (table.field)
  QualifiedColumnExpr(table: String, field: String)
  /// Literal value
  LiteralExpr(value: Value)
  /// Aggregate function
  AggregateExpr(func: AggregateFunc, field: String)
  /// Function call
  FunctionExpr(name: String, args: List(Expr))
  /// Binary operation
  BinaryExpr(left: Expr, op: BinaryOp, right: Expr)
}
```

### AggregateFunc

Aggregate functions.

```gleam
pub type AggregateFunc {
  Count
  Sum
  Avg
  Min
  Max
  CountDistinct
}
```

## Order Types

### OrderBy

ORDER BY clause.

```gleam
pub type OrderBy {
  OrderBy(field: String, direction: Direction, nulls: NullsOrder)
}
```

### Direction

Sort direction.

```gleam
pub type Direction {
  Asc
  Desc
}
```

### NullsOrder

NULL ordering preference.

```gleam
pub type NullsOrder {
  NullsDefault
  NullsFirst
  NullsLast
}
```

## Join Types

### Join

JOIN clause.

```gleam
pub type Join {
  Join(
    join_type: JoinType,
    table: String,
    table_alias: Option(String),
    on: Condition,
  )
}
```

### JoinType

Types of JOIN.

```gleam
pub type JoinType {
  InnerJoin
  LeftJoin
  RightJoin
  FullJoin
  CrossJoin
}
```

## Conflict Types

### OnConflict

Conflict handling for INSERT.

```gleam
pub type OnConflict {
  /// ON CONFLICT DO NOTHING
  DoNothing
  /// ON CONFLICT (columns) DO UPDATE SET ...
  DoUpdate(conflict_columns: List(String), update_columns: List(String))
  /// ON CONFLICT ON CONSTRAINT name DO NOTHING
  DoNothingOnConstraint(constraint_name: String)
  /// ON CONFLICT ON CONSTRAINT name DO UPDATE SET ...
  DoUpdateOnConstraint(constraint_name: String, update_columns: List(String))
}
```

## Schema Types

### Schema

Schema metadata.

```gleam
pub type Schema {
  Schema(
    table_name: String,
    schema_name: Option(String),
    fields: List(Field),
  )
}
```

### Field

Field metadata.

```gleam
pub type Field {
  Field(
    name: String,
    field_type: FieldType,
    constraints: List(Constraint),
    default: Option(String),
  )
}
```

### FieldType

Field data types.

```gleam
pub type FieldType {
  Integer
  BigInt
  Float
  Decimal(precision: Int, scale: Int)
  String
  Text
  Boolean
  Timestamp
  TimestampTz
  Date
  Time
  Uuid
  Json
  Jsonb
  Binary
  Array(element_type: FieldType)
}
```

### Constraint

Field constraints.

```gleam
pub type Constraint {
  NotNull
  Unique
  PrimaryKey
  AutoIncrement
  Check(expression: String)
  References(table: String, column: String)
  Default(value: String)
}
```

## Telemetry Types

### Event

Telemetry events.

```gleam
pub type Event {
  QueryStart(QueryStartEvent)
  QueryStop(QueryStopEvent)
  QueryException(QueryExceptionEvent)
  PoolCheckout(PoolCheckoutEvent)
  PoolCheckin(PoolCheckinEvent)
  PoolTimeout(PoolTimeoutEvent)
  TransactionStart(TransactionStartEvent)
  TransactionCommit(TransactionCommitEvent)
  TransactionRollback(TransactionRollbackEvent)
  SavepointCreate(SavepointCreateEvent)
  SavepointRollback(SavepointRollbackEvent)
  SavepointRelease(SavepointReleaseEvent)
  BatchStart(BatchStartEvent)
  BatchStop(BatchStopEvent)
}
```

### EventType

Event type enumeration.

```gleam
pub type EventType {
  QueryStartType
  QueryStopType
  QueryExceptionType
  PoolCheckoutType
  PoolCheckinType
  PoolTimeoutType
  TransactionStartType
  TransactionCommitType
  TransactionRollbackType
  SavepointCreateType
  SavepointRollbackType
  SavepointReleaseType
  BatchStartType
  BatchStopType
}
```

### Handler

Telemetry handler function type.

```gleam
pub type Handler =
  fn(Event, Metadata) -> Nil
```

### Metadata

Event metadata.

```gleam
pub type Metadata =
  Dict(String, Dynamic)
```

## Dev Mode Types

### DevConfig

Development mode configuration.

```gleam
pub type DevConfig {
  DevConfig(
    enabled: Bool,
    log_queries: Bool,
    log_params: Bool,
    slow_query_threshold_ms: Int,
    log_pool_stats: Bool,
    pool_stats_interval_ms: Int,
    mask_fields: List(String),
    mask_pattern: String,
    explain_queries: Bool,
  )
}
```

### PoolStats

Pool statistics.

```gleam
pub type PoolStats {
  PoolStats(
    pool_name: String,
    size: Int,
    in_use: Int,
    available: Int,
    waiting: Int,
    total_checkouts: Int,
    total_timeouts: Int,
  )
}
```

## Row Type

Query results are returned as dictionaries:

```gleam
pub type Row =
  Dict(String, Value)
```

Access values:

```gleam
case dict.get(row, "email") {
  Ok(ast.StringValue(email)) -> email
  Ok(_) -> "unexpected type"
  Error(_) -> "field not found"
}
```
