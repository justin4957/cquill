# cquill Adapter Contract

This document specifies the contract that all cquill adapters must follow. It serves as both documentation for users and a specification for implementers.

## Overview

cquill uses an adapter pattern to abstract database operations. Each adapter:
- Implements a minimal set of operations
- Declares its capabilities upfront
- Uses consistent error types
- Can optionally support advanced features

## Core Types

### Adapter Type

```gleam
pub opaque type Adapter(conn, row)
```

Type parameters:
- `conn`: Connection/pool type specific to this adapter (e.g., `pgo.Pool` for Postgres)
- `row`: Raw row type returned by the adapter (typically `List(Dynamic)`)

### Required Operations

Every adapter must implement these functions:

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `execute_query` | `fn(conn, CompiledQuery) -> Result(List(row), AdapterError)` | Execute a SELECT query |
| `execute_mutation` | `fn(conn, CompiledQuery) -> Result(Int, AdapterError)` | Execute INSERT/UPDATE/DELETE, return affected count |
| `execute_returning` | `fn(conn, CompiledQuery) -> Result(Option(row), AdapterError)` | Execute INSERT with RETURNING clause |
| `begin_transaction` | `fn(conn) -> Result(conn, AdapterError)` | Start a transaction |
| `commit_transaction` | `fn(conn) -> Result(Nil, AdapterError)` | Commit current transaction |
| `rollback_transaction` | `fn(conn) -> Result(Nil, AdapterError)` | Rollback current transaction |

### Capabilities

Adapters declare their capabilities via `AdapterCapabilities`:

```gleam
pub type AdapterCapabilities {
  AdapterCapabilities(
    transactions: Bool,      // Multi-statement transactions
    returning: Bool,         // RETURNING clause support
    batch_insert: Bool,      // Batch insert operations
    upsert: Bool,            // INSERT ON CONFLICT/upsert
    max_params: Option(Int), // Max query parameters
    json_operations: Bool,   // JSON operators
    array_types: Bool,       // Array column types
  )
}
```

## Error Types

All adapters must map their internal errors to `AdapterError`:

| Error | When to Use | Recoverable |
|-------|-------------|-------------|
| `NotFound` | Record not found when one was expected | No |
| `ConstraintViolation(constraint, detail)` | Unique, FK, check constraint violated | No |
| `ConnectionError(reason)` | Connection failed or lost | Yes |
| `QueryError(message)` | Syntax error, invalid query | No |
| `Timeout` | Operation timed out | Yes |
| `StaleData(expected, actual)` | Optimistic locking conflict | Yes |
| `NotSupported(operation)` | Adapter doesn't support this | No |
| `AdapterSpecific(code, message)` | Adapter-specific errors | Depends |

### Error Mapping Guidelines

1. **Be specific**: Use `ConstraintViolation` over generic `QueryError` when possible
2. **Include context**: Error messages should include relevant details (table name, constraint name)
3. **Preserve codes**: When mapping from driver errors, preserve the original error code in `AdapterSpecific`

## Behavioral Contracts

### Query Execution (`execute_query`)

**Input**: A `CompiledQuery` with SQL and parameters

**Expected behavior**:
1. Execute the query against the database
2. Return all matching rows as `List(row)`
3. Return empty list `[]` if no rows match (not an error)
4. Return `Error(QueryError(_))` for invalid SQL
5. Return `Error(Timeout)` if query exceeds timeout

**Example**:
```gleam
let query = CompiledQuery(
  sql: "SELECT * FROM users WHERE id = $1",
  params: [ParamInt(1)],
  expected_columns: 3,
)
adapter.query(adp, conn, query)
// -> Ok([[dynamic.int(1), dynamic.string("alice@example.com"), ...]])
```

### Mutation Execution (`execute_mutation`)

**Input**: A `CompiledQuery` with INSERT/UPDATE/DELETE SQL

**Expected behavior**:
1. Execute the mutation
2. Return the number of affected rows
3. Return `Error(ConstraintViolation(_, _))` for constraint violations
4. **Do not** return `NotFound` if no rows affected (return `Ok(0)` instead)

**Example**:
```gleam
let query = CompiledQuery(
  sql: "UPDATE users SET email = $1 WHERE id = $2",
  params: [ParamString("new@example.com"), ParamInt(1)],
  expected_columns: 0,
)
adapter.mutate(adp, conn, query)
// -> Ok(1)
```

### Insert with Returning (`execute_returning`)

**Input**: A `CompiledQuery` with INSERT ... RETURNING SQL

**Expected behavior**:
1. If `capabilities.returning == False`, return `Error(NotSupported("RETURNING"))`
2. Execute the insert
3. Return `Some(row)` with the inserted row
4. Return `None` if insert succeeded but no row returned (shouldn't happen normally)

### Transactions

**Expected behavior for `transaction/3`**:
1. If `capabilities.transactions == False`:
   - Execute the function directly without transaction
   - Wrap user errors in `UserError(e)`
2. If `capabilities.transactions == True`:
   - Call `begin_transaction`
   - Execute user function
   - On success: call `commit_transaction`
   - On error: call `rollback_transaction`, return `UserError(e)`

**Transaction isolation**: Adapters should use their database's default isolation level. Document any deviations.

**Nested transactions**: Not supported. Calling `begin_transaction` while already in a transaction should return an error.

## Implementing a New Adapter

### Step 1: Define Connection Type

```gleam
pub type MyConnection {
  MyConnection(/* connection details */)
}
```

### Step 2: Implement Required Functions

```gleam
fn my_execute_query(
  conn: MyConnection,
  query: CompiledQuery,
) -> Result(List(List(Dynamic)), AdapterError) {
  // Implementation
}

// ... other functions
```

### Step 3: Create the Adapter

```gleam
pub fn my_adapter() -> Adapter(MyConnection, List(Dynamic)) {
  adapter.new(
    name: "my_adapter",
    capabilities: my_capabilities(),
    execute_query: my_execute_query,
    execute_mutation: my_execute_mutation,
    execute_returning: my_execute_returning,
    begin_transaction: my_begin_transaction,
    commit_transaction: my_commit_transaction,
    rollback_transaction: my_rollback_transaction,
  )
}
```

### Step 4: Pass Contract Tests

All adapters must pass the shared contract test suite in `test/adapter/contract_test.gleam`.

## Contract Test Suite

The contract test suite verifies:

### Basic Operations
- [ ] `query` returns empty list for no matches
- [ ] `query` returns all matching rows
- [ ] `mutate` returns affected row count
- [ ] `mutate` returns 0 for no matches (not error)
- [ ] `insert_returning` returns inserted row

### Error Handling
- [ ] Invalid SQL returns `QueryError`
- [ ] Constraint violation returns `ConstraintViolation`
- [ ] Missing table returns appropriate error
- [ ] Timeout behavior (if testable)

### Transactions (if supported)
- [ ] Successful transaction commits
- [ ] Failed transaction rolls back
- [ ] Nested transaction attempt fails
- [ ] Connection loss during transaction handled

### Capability Enforcement
- [ ] `NotSupported` returned for unsupported operations
- [ ] Declared capabilities match actual behavior

## Compatibility Matrix

| Feature | Memory | Postgres | SQLite (future) |
|---------|--------|----------|-----------------|
| Basic CRUD | ✓ | ✓ | ✓ |
| Transactions | ✓ | ✓ | ✓ |
| RETURNING | ✓ | ✓ | Partial |
| Batch Insert | ✓ | ✓ | ✓ |
| Upsert | ✓ | ✓ | ✓ |
| JSON ops | ✗ | ✓ | ✓ |
| Array types | ✓ | ✓ | ✗ |

## Performance Expectations

Adapters should:
1. **Pool connections**: Use connection pooling for production adapters
2. **Prepare statements**: Cache prepared statements where possible
3. **Batch efficiently**: `insert_all` should use batch operations, not N inserts
4. **Timeout properly**: Respect query timeouts, don't hang indefinitely

## Version Compatibility

When the adapter protocol changes:
1. Minor additions (new optional capabilities) maintain backwards compatibility
2. Breaking changes require major version bump
3. Deprecation warnings for 1 minor version before removal

## References

- [Ecto.Adapter](https://hexdocs.pm/ecto/Ecto.Adapter.html) - Inspiration for adapter design
- [Repository Pattern](https://martinfowler.com/eaaCatalog/repository.html) - Domain-focused abstraction
- [Creating Ecto Adapters](https://michal.muskala.eu/post/creating-ecto-adapters/) - Implementation guide
