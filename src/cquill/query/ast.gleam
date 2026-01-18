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
