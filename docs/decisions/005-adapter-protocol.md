# ADR-005: Adapter Protocol Interface Design

**Status:** Accepted
**Date:** 2026-01-17
**Issue:** #12
**Supersedes:** None
**Informed by:** ADR-001 (Ecosystem Audit), ADR-002 (Driver Selection)

## Decision

**Define a minimal adapter protocol using a record-of-functions pattern with explicit capability declarations.**

The adapter interface includes:
1. Six core operations (query, mutate, returning, begin/commit/rollback transaction)
2. Declared capabilities for optional features
3. Unified error types across all adapters
4. Reference implementation via in-memory adapter

## Context

cquill needs an abstraction layer between the repository API and database drivers. This adapter layer must:
- Support multiple backends (Postgres, Memory, future SQLite)
- Provide consistent error handling
- Allow testing without real databases
- Be minimal but complete

We evaluated patterns from:
- **Ecto.Adapter** - Elixir's behaviour-based adapter system
- **Repository pattern** - Domain-focused persistence abstraction
- **Payload CMS** - Database adapter with capability detection

## Options Considered

### Option A: Behaviour-like Interface (Ecto-style)

Define operations as a protocol/interface that adapters implement.

**Pros:**
- Familiar pattern from Ecto
- Clear contract

**Cons:**
- Gleam doesn't have Elixir-style behaviours
- Would require runtime dispatch

### Option B: Record of Functions

Store adapter operations as functions in a record.

```gleam
pub opaque type Adapter(conn, row) {
  Adapter(
    name: String,
    capabilities: AdapterCapabilities,
    execute_query: fn(conn, CompiledQuery) -> Result(List(row), AdapterError),
    // ... other operations
  )
}
```

**Pros:**
- First-class in Gleam
- Type-safe at compile time
- Easy to compose and test
- No runtime reflection

**Cons:**
- More verbose than behaviours
- Must pass adapter explicitly

### Option C: Module-based (like pog)

Each adapter is a module with specific function names.

**Pros:**
- Simple to understand
- No indirection

**Cons:**
- No polymorphism
- Can't swap adapters at runtime
- Harder to test

## Decision Rationale

We choose **Option B: Record of Functions** because:

### 1. Type Safety

The record approach provides compile-time type checking:
```gleam
pub fn query(
  adapter: Adapter(conn, row),
  connection: conn,
  compiled: CompiledQuery,
) -> Result(List(row), AdapterError)
```

The types `conn` and `row` are enforced by the compiler.

### 2. Testability

Mock adapters are trivial to create:
```gleam
let mock_adapter = adapter.new(
  name: "mock",
  capabilities: adapter.default_capabilities(),
  execute_query: fn(_, _) { Ok([]) },
  // ... stub other operations
)
```

### 3. Explicit Capabilities

Rather than runtime feature detection, adapters declare capabilities upfront:
```gleam
pub type AdapterCapabilities {
  AdapterCapabilities(
    transactions: Bool,
    returning: Bool,
    batch_insert: Bool,
    // ...
  )
}
```

This follows Ecto's philosophy: "adapters should not emulate behaviours that are not natural to the database."

### 4. Minimal Surface Area

Six operations cover all persistence needs:
- `execute_query` - SELECT operations
- `execute_mutation` - INSERT/UPDATE/DELETE with affected count
- `execute_returning` - INSERT with RETURNING clause
- `begin_transaction` / `commit_transaction` / `rollback_transaction` - Transaction control

Batch operations are intentionally excluded from the core interface - they can be built on top using `execute_mutation` with batched SQL.

## Implementation Details

### Error Types

Unified error type covering common failure modes:

```gleam
pub type AdapterError {
  NotFound
  ConstraintViolation(constraint: String, detail: String)
  ConnectionError(reason: String)
  QueryError(message: String)
  Timeout
  StaleData(expected_version: String, actual_version: String)
  NotSupported(operation: String)
  AdapterSpecific(code: String, message: String)
}
```

### Transaction Handling

For adapters without transaction support, `transaction/3` executes the function directly:

```gleam
pub fn transaction(adapter, conn, operation) {
  case adapter.capabilities.transactions {
    False ->
      case operation(conn) {
        Ok(result) -> Ok(result)
        Error(e) -> Error(UserError(e))
      }
    True -> // Begin, execute, commit/rollback
  }
}
```

This avoids emulating transactions where they don't exist naturally.

### Memory Adapter

The in-memory adapter serves as:
1. Reference implementation of the protocol
2. Testing adapter for fast, isolated tests
3. Example for implementing new adapters

Located at `src/cquill/adapter/memory.gleam`.

## Consequences

### Positive

- **Type safety**: Compile-time verification of adapter usage
- **Testability**: Easy to mock and stub
- **Explicit**: Capabilities declared upfront, no runtime surprises
- **Minimal**: Six operations cover all needs
- **Consistent**: Unified error types across adapters

### Negative

- **Verbose**: Must pass adapter explicitly to all operations
- **No late binding**: Adapter choice is compile-time, not config-time
- **Learning curve**: Different from Ecto's behaviour pattern

### Trade-offs Accepted

| Giving Up | In Exchange For |
|-----------|-----------------|
| Behaviour-based dispatch | Compile-time type safety |
| Implicit adapter | Explicit, testable interface |
| Feature emulation | Honest capability reporting |

## Validation

This decision is validated by:

1. **Implementation**:
   - Adapter protocol at `src/cquill/adapter.gleam`
   - Memory adapter at `src/cquill/adapter/memory.gleam`

2. **Tests**: 27 tests passing covering:
   - Error type utilities
   - Capability declarations
   - Memory store operations
   - Adapter protocol operations
   - Transaction handling

3. **Documentation**:
   - Contract specification at `docs/ADAPTER_CONTRACT.md`

## Future Considerations

1. **Connection pools**: Add pool management to adapter interface
2. **Streaming**: Add cursor-based streaming for large result sets
3. **Migrations**: Separate adapter interface for schema migrations
4. **Observability**: Add telemetry hooks for metrics/tracing

## References

- [Ecto.Adapter](https://hexdocs.pm/ecto/Ecto.Adapter.html) - Elixir adapter behaviours
- [Creating Ecto Adapters](https://michal.muskala.eu/post/creating-ecto-adapters/) - Implementation guide
- [Repository Pattern](https://martinfowler.com/eaaCatalog/repository.html) - Martin Fowler
- [The Repository Pattern Done Right](https://blog.mnavarro.dev/the-repository-pattern-done-right)
- [Writing Extensible Elixir with Behaviours](https://www.djm.org.uk/posts/writing-extensible-elixir-with-behaviours-adapters-pluggable-backends/)

Sources:
- [Ecto.Adapter documentation](https://hexdocs.pm/ecto/Ecto.Adapter.html)
- [Creating Ecto Adapters](https://michal.muskala.eu/post/creating-ecto-adapters/)
- [Payload CMS Database Adapters](https://deepwiki.com/payloadcms/payload/3.1-database-adapters)
- [Repository Pattern - Cosmic Python](https://www.cosmicpython.com/book/chapter_02_repository.html)
