// cquill Query AST
//
// This module defines the abstract syntax tree (AST) types for queries.
// Queries are represented as data structures, not SQL strings.
// This enables:
// - Inspection and debugging
// - Composition and transformation
// - Type-safe query building
// - Adapter-agnostic query representation
//
// SQL generation happens in a separate compiler module, not here.

import gleam/option.{type Option}

// ============================================================================
// CORE QUERY TYPE
// ============================================================================

/// The main query type representing a complete query AST.
/// The schema parameter provides type safety by tracking which schema
/// the query is built against.
pub type Query(schema) {
  Query(
    /// The source table/schema being queried
    source: Source,
    /// What fields to select
    select: Select,
    /// WHERE conditions (all must match - implicit AND)
    wheres: List(Where),
    /// ORDER BY clauses
    order_bys: List(OrderBy),
    /// LIMIT clause
    limit: Option(Int),
    /// OFFSET clause
    offset: Option(Int),
    /// JOIN clauses
    joins: List(Join),
    /// DISTINCT flag
    distinct: Bool,
    /// GROUP BY fields
    group_bys: List(String),
    /// HAVING conditions (for aggregate queries)
    havings: List(Where),
  )
}

// ============================================================================
// SOURCE TYPES
// ============================================================================

/// Represents the source of a query (table, subquery, etc.)
pub type Source {
  /// Query from a table by name
  TableSource(table: String, schema_name: Option(String))
  /// Query from a subquery (for future use)
  SubquerySource(query: Query(Nil))
}

// ============================================================================
// SELECT TYPES
// ============================================================================

/// What columns to select
pub type Select {
  /// SELECT * - all columns
  SelectAll
  /// SELECT specific fields by name
  SelectFields(List(String))
  /// SELECT expressions (for computed columns, aggregates)
  SelectExpr(List(SelectExpression))
}

/// A select expression with optional alias
pub type SelectExpression {
  SelectExpression(expr: Expr, alias: Option(String))
}

/// Expression types for SELECT clauses
pub type Expr {
  /// A column reference
  ColumnExpr(field: String)
  /// A qualified column (table.field)
  QualifiedColumnExpr(table: String, field: String)
  /// A literal value
  LiteralExpr(value: Value)
  /// An aggregate function
  AggregateExpr(func: AggregateFunc, field: String)
  /// A function call
  FunctionExpr(name: String, args: List(Expr))
  /// Binary operation
  BinaryExpr(left: Expr, op: BinaryOp, right: Expr)
}

/// Aggregate functions
pub type AggregateFunc {
  Count
  Sum
  Avg
  Min
  Max
  CountDistinct
}

/// Binary operators for expressions
pub type BinaryOp {
  Add
  Subtract
  Multiply
  Divide
  Concat
}

// ============================================================================
// WHERE/CONDITION TYPES
// ============================================================================

/// A WHERE clause wrapper
pub type Where {
  Where(condition: Condition)
}

/// Condition types for WHERE and HAVING clauses
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
  /// field ILIKE pattern (case-insensitive LIKE)
  ILike(field: String, pattern: String)
  /// field NOT ILIKE pattern
  NotILike(field: String, pattern: String)
  /// field IS NULL
  IsNull(field: String)
  /// field IS NOT NULL
  IsNotNull(field: String)
  /// field BETWEEN low AND high
  Between(field: String, low: Value, high: Value)
  /// Combined conditions (all must be true)
  And(List(Condition))
  /// Alternative conditions (any can be true)
  Or(List(Condition))
  /// Negated condition
  Not(Condition)
  /// Raw condition for advanced use cases
  Raw(sql: String, params: List(Value))
}

// ============================================================================
// VALUE TYPES
// ============================================================================

/// Represents a value in a query (for parameters, literals)
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
  /// Positional parameter placeholder ($1, $2, etc.)
  ParamValue(position: Int)
  /// List of values (for IN clauses)
  ListValue(List(Value))
}

// ============================================================================
// ORDER BY TYPES
// ============================================================================

/// ORDER BY clause
pub type OrderBy {
  OrderBy(field: String, direction: Direction, nulls: NullsOrder)
}

/// Sort direction
pub type Direction {
  Asc
  Desc
}

/// NULL ordering preference
pub type NullsOrder {
  /// Use database default
  NullsDefault
  /// NULLs come first
  NullsFirst
  /// NULLs come last
  NullsLast
}

// ============================================================================
// JOIN TYPES
// ============================================================================

/// JOIN clause
pub type Join {
  Join(
    join_type: JoinType,
    table: String,
    table_alias: Option(String),
    on: Condition,
  )
}

/// Types of JOIN
pub type JoinType {
  InnerJoin
  LeftJoin
  RightJoin
  FullJoin
  CrossJoin
}

// ============================================================================
// QUERY OPERATIONS ENUM (for mutation tracking)
// ============================================================================

/// Type of query operation (useful for adapter routing)
pub type QueryOperation {
  SelectOp
  InsertOp
  UpdateOp
  DeleteOp
}

// ============================================================================
// DEFAULT CONSTRUCTORS
// ============================================================================

/// Create a new empty Query AST for a table source
pub fn new(table: String) -> Query(Nil) {
  Query(
    source: TableSource(table:, schema_name: option.None),
    select: SelectAll,
    wheres: [],
    order_bys: [],
    limit: option.None,
    offset: option.None,
    joins: [],
    distinct: False,
    group_bys: [],
    havings: [],
  )
}

/// Create a new empty Query AST with schema qualification
pub fn new_qualified(schema_name: String, table: String) -> Query(Nil) {
  Query(
    source: TableSource(table:, schema_name: option.Some(schema_name)),
    select: SelectAll,
    wheres: [],
    order_bys: [],
    limit: option.None,
    offset: option.None,
    joins: [],
    distinct: False,
    group_bys: [],
    havings: [],
  )
}

/// Create a default OrderBy with ascending order
pub fn order_by_asc(field: String) -> OrderBy {
  OrderBy(field:, direction: Asc, nulls: NullsDefault)
}

/// Create a default OrderBy with descending order
pub fn order_by_desc(field: String) -> OrderBy {
  OrderBy(field:, direction: Desc, nulls: NullsDefault)
}

// ============================================================================
// INSERT QUERY TYPE
// ============================================================================

/// Represents an INSERT query AST.
/// The schema parameter provides type safety.
pub type InsertQuery(schema) {
  InsertQuery(
    /// The target table
    table: String,
    /// Optional schema name (e.g., "public")
    schema_name: Option(String),
    /// Columns to insert into
    columns: List(String),
    /// Values to insert (list of rows, each row is a list of values)
    values: List(List(Value)),
    /// Conflict handling strategy
    on_conflict: Option(OnConflict),
    /// Columns to return after insert
    returning: List(String),
  )
}

/// Conflict handling strategies for INSERT
pub type OnConflict {
  /// ON CONFLICT DO NOTHING
  DoNothing
  /// ON CONFLICT (columns) DO UPDATE SET ...
  DoUpdate(conflict_columns: List(String), update_columns: List(String))
  /// ON CONFLICT ON CONSTRAINT constraint_name DO NOTHING
  DoNothingOnConstraint(constraint_name: String)
  /// ON CONFLICT ON CONSTRAINT constraint_name DO UPDATE SET ...
  DoUpdateOnConstraint(constraint_name: String, update_columns: List(String))
}

/// Create a new empty InsertQuery AST
pub fn new_insert(table: String) -> InsertQuery(Nil) {
  InsertQuery(
    table: table,
    schema_name: option.None,
    columns: [],
    values: [],
    on_conflict: option.None,
    returning: [],
  )
}

/// Create a new InsertQuery AST with schema qualification
pub fn new_insert_qualified(
  schema_name: String,
  table: String,
) -> InsertQuery(Nil) {
  InsertQuery(
    table: table,
    schema_name: option.Some(schema_name),
    columns: [],
    values: [],
    on_conflict: option.None,
    returning: [],
  )
}

// ============================================================================
// UPDATE QUERY TYPE
// ============================================================================

/// Represents an UPDATE query AST.
/// The schema parameter provides type safety.
pub type UpdateQuery(schema) {
  UpdateQuery(
    /// The target table
    table: String,
    /// Optional schema name
    schema_name: Option(String),
    /// SET clauses: list of (column, value) pairs
    sets: List(SetClause),
    /// WHERE conditions
    wheres: List(Where),
    /// Columns to return after update
    returning: List(String),
  )
}

/// A SET clause in an UPDATE query
pub type SetClause {
  SetClause(column: String, value: Value)
}

/// Create a new empty UpdateQuery AST
pub fn new_update(table: String) -> UpdateQuery(Nil) {
  UpdateQuery(
    table: table,
    schema_name: option.None,
    sets: [],
    wheres: [],
    returning: [],
  )
}

/// Create a new UpdateQuery AST with schema qualification
pub fn new_update_qualified(
  schema_name: String,
  table: String,
) -> UpdateQuery(Nil) {
  UpdateQuery(
    table: table,
    schema_name: option.Some(schema_name),
    sets: [],
    wheres: [],
    returning: [],
  )
}

// ============================================================================
// DELETE QUERY TYPE
// ============================================================================

/// Represents a DELETE query AST.
/// The schema parameter provides type safety.
pub type DeleteQuery(schema) {
  DeleteQuery(
    /// The target table
    table: String,
    /// Optional schema name
    schema_name: Option(String),
    /// WHERE conditions
    wheres: List(Where),
    /// Columns to return after delete
    returning: List(String),
  )
}

/// Create a new empty DeleteQuery AST
pub fn new_delete(table: String) -> DeleteQuery(Nil) {
  DeleteQuery(table: table, schema_name: option.None, wheres: [], returning: [])
}

/// Create a new DeleteQuery AST with schema qualification
pub fn new_delete_qualified(
  schema_name: String,
  table: String,
) -> DeleteQuery(Nil) {
  DeleteQuery(
    table: table,
    schema_name: option.Some(schema_name),
    wheres: [],
    returning: [],
  )
}
