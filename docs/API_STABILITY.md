# API Stability Guide

This document defines the public API surface of cquill and the stability guarantees for v1.0.0 and beyond.

## Stability Commitment

Starting with v1.0.0, cquill follows [Semantic Versioning](https://semver.org/):

- **Patch releases (1.0.x)**: Bug fixes only, no API changes
- **Minor releases (1.x.0)**: New features, backwards compatible
- **Major releases (x.0.0)**: May include breaking changes

## Public API Definition

The **public API** consists of all exported types and functions from the modules listed below. Any changes to these will follow the versioning policy above.

### Tier 1: Stable Core (Highest Stability)

These modules form the core of cquill and have the strongest stability guarantees:

| Module | Purpose |
|--------|---------|
| `cquill/schema` | Schema definitions and metadata |
| `cquill/schema/field` | Field types, constraints, and modifiers |
| `cquill/query` | Query builder DSL |
| `cquill/repo` | Repository operations |
| `cquill/error` | Error types and classification |

### Tier 2: Stable Adapters

These modules are stable but may have adapter-specific behaviors:

| Module | Purpose |
|--------|---------|
| `cquill/adapter` | Adapter protocol and base types |
| `cquill/adapter/postgres` | PostgreSQL adapter |
| `cquill/adapter/memory` | In-memory reference adapter |

### Tier 3: Supporting Modules

These modules are public but may evolve more frequently:

| Module | Purpose |
|--------|---------|
| `cquill/query/ast` | Query AST types (prefer builder functions) |
| `cquill/query/builder` | Advanced query composition |
| `cquill/changeset` | Validation (minimal in v1.0) |
| `cquill/testing` | Test utilities |

### Not Part of Public API

The following are **not** part of the public API and may change without notice:

- Internal modules (paths containing `/internal/`)
- Functions starting with `internal_`
- CLI modules (`cquill/cli/*`, `cquill/codegen/*`)
- Development modules (`cquill/dev`, `cquill/telemetry`)
- AST structure internals (use builder functions instead)

## Module Reference

### Schema Layer

#### `cquill/schema`

Core schema definition module. A Schema describes a database table's structure.

**Types:**
- `Schema` - Complete table definition
- `TableConstraint` - Table-level constraints (UniqueConstraint, Index, TableCheck, CompositeForeignKey)
- `SchemaError` - Validation errors
- `SchemaDiff` - Change tracking between schemas

**Key Functions:**
```gleam
// Construction
new(table_name: String) -> Schema
new_with_schema(schema_name: String, table_name: String) -> Schema

// Building (pipeline-friendly)
field(schema: Schema, field: Field) -> Schema
primary_key(schema: Schema, fields: List(String)) -> Schema
unique_constraint(schema: Schema, name: String, columns: List(String)) -> Schema

// Accessors
get_field(schema: Schema, name: String) -> Option(Field)
get_fields(schema: Schema) -> List(Field)
get_primary_key(schema: Schema) -> List(String)

// Validation
validate(schema: Schema) -> List(SchemaError)
is_valid(schema: Schema) -> Bool
```

#### `cquill/schema/field`

Field definitions with types and constraints.

**Types:**
- `Field` - Complete field definition
- `FieldType` - SQL data types (Integer, String, Boolean, DateTime, etc.)
- `Constraint` - Field constraints (NotNull, Unique, ForeignKey, Check, etc.)
- `Default` - Default value specifications
- `ForeignKeyAction` - Referential actions (Cascade, Restrict, SetNull, etc.)

**Key Functions:**
```gleam
// Constructors (prefer these over new())
integer(name: String) -> Field
string(name: String) -> Field
boolean(name: String) -> Field
datetime(name: String) -> Field
uuid(name: String) -> Field

// Modifiers (pipeline-friendly)
nullable(field: Field) -> Field
not_null(field: Field) -> Field
unique(field: Field) -> Field
auto_increment(field: Field) -> Field
references(field: Field, table: String, column: String) -> Field

// Accessors
get_name(field: Field) -> String
get_type(field: Field) -> FieldType
is_nullable(field: Field) -> Bool
```

### Query Layer

#### `cquill/query`

Main query builder module. Queries are built using pipelines and are composable.

**Types:**
- Re-exports from `query/ast`: `Query`, `Condition`, `Direction`, `Value`, etc.

**Key Functions:**
```gleam
// Query construction
from(schema: Schema) -> Query(Schema)
from_table(table: String) -> Query(Nil)

// Selection
select(query: Query(s), fields: List(String)) -> Query(s)
select_all(query: Query(s)) -> Query(s)
distinct(query: Query(s)) -> Query(s)

// Conditions
where(query: Query(s), condition: Condition) -> Query(s)
or_where(query: Query(s), condition: Condition) -> Query(s)

// Condition builders
eq(field: String, value: a) -> Condition
gt(field: String, value: a) -> Condition
lt(field: String, value: a) -> Condition
like(field: String, pattern: String) -> Condition
is_null(field: String) -> Condition
and(conditions: List(Condition)) -> Condition
or(conditions: List(Condition)) -> Condition

// Ordering
order_by(query: Query(s), field: String, direction: Direction) -> Query(s)
order_by_asc(query: Query(s), field: String) -> Query(s)
order_by_desc(query: Query(s), field: String) -> Query(s)

// Pagination
limit(query: Query(s), n: Int) -> Query(s)
offset(query: Query(s), n: Int) -> Query(s)
paginate(query: Query(s), page page: Int, per_page per_page: Int) -> Query(s)

// Joins
join(query: Query(s), table: String, on on: Condition) -> Query(s)
left_join(query: Query(s), table: String, on on: Condition) -> Query(s)

// Grouping
group_by(query: Query(s), field: String) -> Query(s)
having(query: Query(s), condition: Condition) -> Query(s)
```

#### `cquill/query/builder`

Advanced query composition utilities.

**Types:**
- `QueryModifier(s)` - Reusable query transformation function
- `Scope(s)` - Named query modifier

**Key Functions:**
```gleam
// Composition
compose(modifiers: List(QueryModifier(s))) -> QueryModifier(s)
when(condition: Bool, modifier: QueryModifier(s)) -> QueryModifier(s)

// Common patterns
not_deleted() -> QueryModifier(s)
published() -> QueryModifier(s)
active() -> QueryModifier(s)
recent_first() -> QueryModifier(s)

// Scopes
scope(name: String, modifier: QueryModifier(s)) -> Scope(s)
apply_scope(query: Query(s), scope: Scope(s)) -> Query(s)
```

### Repository Layer

#### `cquill/repo`

Unified database operations interface.

**Types:**
- `RepoError` - High-level error types (NotFound, ConstraintError, ConnectionError, etc.)
- `ConstraintKind` - Constraint violation types
- `ValidationError` - Validation failure details
- `RepoTransactionError(e)` - Transaction-specific errors
- `RepoSavepointError(e)` - Savepoint-specific errors
- `BatchConfig` - Batch operation configuration

**Key Functions:**
```gleam
// Query operations
all(adapter, conn, query, decoder) -> Result(List(a), RepoError)
one(adapter, conn, query, decoder) -> Result(Option(a), RepoError)
get(adapter, conn, query, decoder) -> Result(a, RepoError)
exists(adapter, conn, query) -> Result(Bool, RepoError)
count(adapter, conn, query, decoder) -> Result(Int, RepoError)

// Mutations
insert(adapter, conn, query, decoder) -> Result(a, RepoError)
update(adapter, conn, query, decoder) -> Result(a, RepoError)
delete(adapter, conn, query) -> Result(Int, RepoError)

// Batch operations
batch_insert(adapter, conn, queries) -> Result(Int, RepoError)
batch_update(adapter, conn, queries) -> Result(Int, RepoError)

// Transactions
transaction(adapter, conn, fn(conn) -> Result(a, e)) -> Result(a, RepoTransactionError(e))

// Error handling
is_not_found(error: RepoError) -> Bool
is_constraint_error(error: RepoError) -> Bool
is_recoverable(error: RepoError) -> Bool
format_error(error: RepoError) -> String
```

### Adapter Layer

#### `cquill/adapter`

Adapter protocol and base types.

**Types:**
- `Adapter(conn, row)` - Generic adapter type
- `CompiledQuery` - SQL with parameters
- `QueryParam` - Parameter types (ParamInt, ParamString, etc.)
- `AdapterCapabilities` - Feature support flags
- `AdapterError` - Adapter-level errors
- `TransactionError(e)` - Transaction errors

**Key Functions:**
```gleam
// Adapter construction
new(name:, capabilities:, execute_query:, ...) -> Adapter(conn, row)

// Capabilities
supports_transactions(adapter) -> Bool
supports_returning(adapter) -> Bool
capabilities(adapter) -> AdapterCapabilities

// Operations
query(adapter, conn, compiled_query) -> Result(List(row), AdapterError)
mutate(adapter, conn, compiled_query) -> Result(Int, AdapterError)
transaction(adapter, conn, fn) -> Result(a, TransactionError(e))

// Query params
param_int(value: Int) -> QueryParam
param_string(value: String) -> QueryParam
param_bool(value: Bool) -> QueryParam
param_null() -> QueryParam
```

#### `cquill/adapter/postgres`

PostgreSQL adapter using pog library.

**Types:**
- `PostgresConfig` - Connection pool configuration
- `PostgresConnection` - Connection handle
- `BatchConfig` - Batch operation settings

**Key Functions:**
```gleam
// Configuration
default_config(name: atom) -> PostgresConfig
config_from_url(name: atom, url: String) -> Result(PostgresConfig, Nil)
host(config, value) -> PostgresConfig
port(config, value) -> PostgresConfig
database(config, value) -> PostgresConfig
user(config, value) -> PostgresConfig
password(config, value) -> PostgresConfig
pool_size(config, value) -> PostgresConfig

// Connection
start(config: PostgresConfig) -> Result(PostgresConnection, StartError)
postgres_adapter() -> Adapter(PostgresConnection, List(Dynamic))

// Transactions
execute_transaction(conn, fn) -> Result(a, TransactionError(Nil))

// Savepoints
create_savepoint(conn, name) -> Result(Nil, SavepointError(Nil))
rollback_to_savepoint(conn, name) -> Result(Nil, SavepointError(Nil))
release_savepoint(conn, name) -> Result(Nil, SavepointError(Nil))
execute_savepoint(conn, name, fn) -> Result(a, SavepointError(Nil))
```

#### `cquill/adapter/memory`

In-memory reference adapter for testing.

**Types:**
- `MemoryStore` - In-memory database state
- `MemoryTable` - Table structure and data
- `ForeignKeyConstraint` - FK constraint definition
- `Snapshot` - Transaction snapshot

**Key Functions:**
```gleam
// Store management
new() -> MemoryStore
new_store() -> MemoryStore
reset(store) -> MemoryStore
create_table_from_schema(store, schema) -> MemoryStore

// Operations
insert_row(store, table, id, row) -> Result(MemoryStore, AdapterError)
update_row(store, table, id, row) -> Result(MemoryStore, AdapterError)
delete_row(store, table, id) -> Result(MemoryStore, AdapterError)
get_all_rows(store, table) -> Result(List(row), AdapterError)

// Adapter
memory_adapter() -> Adapter(MemoryStore, MemoryRow)

// Transactions
execute_transaction(store, fn) -> Result(#(MemoryStore, a), TransactionError(Nil))

// Savepoints
create_savepoint(store, name) -> Result(MemoryStore, SavepointError(Nil))
rollback_to_savepoint(store, name) -> Result(MemoryStore, SavepointError(Nil))
execute_savepoint(store, name, fn) -> Result(#(MemoryStore, a), SavepointError(Nil))
```

### Error Handling

#### `cquill/error`

Unified error types with classification.

**Types:**
- `AdapterError` - All adapter-level errors (18 variants)
- `TransactionError(e)` - Transaction errors (8 variants)
- `SavepointError(e)` - Savepoint errors (6 variants)

**Key Functions:**
```gleam
// Classification
is_not_found(error: AdapterError) -> Bool
is_constraint_violation(error: AdapterError) -> Bool
is_unique_violation(error: AdapterError) -> Bool
is_foreign_key_violation(error: AdapterError) -> Bool
is_connection_error(error: AdapterError) -> Bool
is_recoverable(error: AdapterError) -> Bool

// Formatting
format_error(error: AdapterError) -> String
format_transaction_error(error: TransactionError(e)) -> String
format_savepoint_error(error: SavepointError(e)) -> String

// Construction
not_found() -> AdapterError
query_failed(message: String) -> AdapterError
unique_violation(table: String, constraint: String) -> AdapterError
```

### Testing Utilities

#### `cquill/testing`

Test helpers for memory adapter testing.

**Types:**
- `TestContext` - Test state with store and counters

**Key Functions:**
```gleam
// Context
new_context() -> TestContext
with_context(fn(TestContext) -> a) -> a
get_store(ctx: TestContext) -> MemoryStore
reset(ctx: TestContext) -> TestContext

// ID generation
next_id(ctx: TestContext) -> #(TestContext, Int)

// Unique data
unique_email(ctx: TestContext) -> #(TestContext, String)
unique_username(ctx: TestContext) -> #(TestContext, String)

// Entity tracking
track_insert(ctx, table, id) -> TestContext
get_tracked(ctx, table) -> List(String)
```

## API Design Principles

### Pipeline-Friendly Design

All builder functions take the primary data structure as the first argument:

```gleam
// Good - supports piping
schema.new("users")
|> schema.field(field.string("name"))
|> schema.field(field.string("email") |> field.unique())
|> schema.primary_key(["id"])

query.from(user_schema)
|> query.where(query.eq("active", True))
|> query.order_by_desc("created_at")
|> query.limit(10)
```

### Result Types for All I/O

Every function that performs I/O returns a `Result` type:

```gleam
// All database operations return Result
repo.all(adapter, conn, query, decoder)
// -> Result(List(User), RepoError)

repo.insert(adapter, conn, query, decoder)
// -> Result(User, RepoError)
```

### Option for Missing Values

Functions that may not find data return `Option`:

```gleam
schema.get_field(schema, "email")
// -> Option(Field)

repo.one(adapter, conn, query, decoder)
// -> Result(Option(User), RepoError)
```

### Specific Error Types

Errors are specific variants, not generic strings:

```gleam
// Good - specific errors
case error {
  repo.NotFound -> handle_not_found()
  repo.ConstraintError(UniqueConstraint, name) -> handle_duplicate()
  repo.ConnectionError(message) -> handle_connection()
  _ -> handle_other()
}
```

## Breaking Change Policy

Before v1.0:
- Breaking changes may occur in any release
- Document all changes in CHANGELOG.md

After v1.0:
- Breaking changes only in major versions
- Deprecate before removing (minimum one minor version)
- Provide migration guides for all breaking changes

### What Constitutes a Breaking Change

**Breaking:**
- Removing a public function or type
- Changing function signatures (parameters, return types)
- Changing type definitions
- Renaming public modules

**Not Breaking:**
- Adding new functions or types
- Adding optional parameters (with defaults)
- Bug fixes that change incorrect behavior
- Performance improvements
- Documentation changes

## Deprecation Process

1. Add `@deprecated` comment with version and alternative
2. Emit warning when deprecated function is used (where possible)
3. Document in CHANGELOG.md
4. Keep for at least one minor version
5. Remove in next major version

Example:
```gleam
/// @deprecated since 1.2.0 - Use `query.where_eq` instead
pub fn where_equals(query, field, value) {
  query.where(query, query.eq(field, value))
}
```
