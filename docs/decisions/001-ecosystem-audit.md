# ADR-001: Gleam Database Ecosystem Audit

**Status:** Complete
**Date:** 2026-01-17
**Issue:** #7

## Context

Before building cquill, we need to understand the existing Gleam database ecosystem, available Erlang infrastructure, and patterns we can adopt or improve upon.

## Libraries Audited

### Gleam Libraries

#### 1. pog (formerly gleam_pgo) - PostgreSQL Client

**Repository:** github.com/lpil/pog
**Version:** 4.1.0
**Stats:** 232 stars, 28 forks, actively maintained

**Architecture:**
- Wraps the Erlang `pgo` library
- Named process pools for connection management
- Builder pattern for configuration
- Fluent API with pipe operators for queries

**Key API Patterns:**

```gleam
// Configuration
let config = pog.default_config()
  |> pog.host("localhost")
  |> pog.database("mydb")
  |> pog.pool_size(10)

// Query execution
pog.query("SELECT id, name FROM users WHERE active = $1")
|> pog.parameter(pog.bool(True))
|> pog.returning(user_decoder)
|> pog.execute(pool)
```

**Strengths:**
- Clean, idiomatic Gleam API
- Built-in connection pooling
- Type-safe parameter encoding (`pog.int()`, `pog.text()`, etc.)
- OpenTelemetry tracing support
- Comprehensive error types with PostgreSQL error code mapping
- Transaction support

**Weaknesses:**
- No query builder (raw SQL only)
- No schema abstraction
- Decoder composition requires manual work
- Coupled to PostgreSQL (no adapter abstraction)

**Lessons for cquill:**
- The builder pattern for configuration works well
- Parameter encoding via typed constructors is ergonomic
- Error code mapping to semantic types is valuable
- Default 5-second query timeout is sensible

---

#### 2. sqlight - SQLite Client

**Repository:** github.com/lpil/sqlight
**Version:** 1.0.3
**Stats:** 138 stars, dual-target (Erlang + Deno)

**Architecture:**
- Wraps `esqlite` (NIF) on Erlang, `x/sqlite` (WASM) on Deno
- Single-connection model (no pooling needed for SQLite)
- Uses `gleam/dynamic/decode` for result decoding

**Key API Patterns:**

```gleam
// Connection with automatic cleanup
use conn <- sqlight.with_connection(":memory:")

// Query with decoder
sqlight.query(
  "SELECT id, name FROM users WHERE active = ?",
  conn,
  [sqlight.bool(True)],
  user_decoder,
)
```

**Strengths:**
- Cross-platform (BEAM + JS/Deno)
- Clean resource management pattern (`with_connection`)
- Comprehensive SQLite error code mapping
- Simple, focused API

**Weaknesses:**
- No pooling (by design for SQLite)
- Boolean handling quirk (SQLite uses integers)
- Limited to single statement per `query()` call

**Lessons for cquill:**
- `with_connection` pattern is excellent for resource cleanup
- Cross-platform considerations affect design
- Custom decoders (like `decode_bool`) may be needed for type quirks

---

#### 3. gleam/dynamic/decode - Decoder Combinators

**Package:** gleam_stdlib
**Module:** gleam/dynamic/decode

**Architecture:**
- Functional combinator pattern
- Composable decoders via `field`, `at`, `then`, `map`
- Path tracking for error messages

**Key Patterns:**

```gleam
// Decoder composition using `use`
let decoder = {
  use name <- decode.field("name", decode.string)
  use age <- decode.field("age", decode.int)
  use email <- decode.optional_field("email", decode.string, None)
  decode.success(User(name:, age:, email:))
}

// Run decoder
decode.run(dynamic_data, decoder)
```

**Combinators Available:**
- Primitives: `string`, `int`, `float`, `bool`, `bit_array`
- Collections: `list`, `dict`, `optional`
- Navigation: `field`, `at`, `subfield`
- Composition: `then`, `map`, `one_of`
- Error handling: `collapse_errors`, `failure`

**Strengths:**
- Elegant composition with `use` expressions
- Detailed error paths for debugging
- `one_of` for union type decoding
- `optional_field` with default values

**Lessons for cquill:**
- Adopt this pattern for row decoding
- Path tracking helps users debug decode failures
- The `use` expression pattern is very ergonomic
- Consider specialized row decoders that work positionally

---

### Erlang Libraries

#### 4. pgo - Erlang PostgreSQL Driver

**Repository:** github.com/erleans/pgo
**Requirements:** OTP 26+

**Architecture:**
- Direct socket checkout (no message passing overhead)
- Binary protocol with cached OIDs
- Built-in connection pooling
- OpenTelemetry instrumentation

**Key Design Decisions:**
- "No message passing. Clients checkout the socket and use it directly."
- Binary protocol for efficiency
- Pool parameters: `pool_size`, `queue_target`, `queue_interval`, `idle_interval`

**Strengths:**
- High performance (direct socket access)
- Modern OTP design
- Good observability support
- Transaction support

**Weaknesses:**
- OTP 26+ only
- Less mature than epgsql

**Recommendation:** **Primary choice for cquill's Postgres adapter**

---

#### 5. epgsql - Erlang PostgreSQL Driver

**Repository:** github.com/epgsql/epgsql
**Maturity:** Long-established, community maintained

**Architecture:**
- Three API styles: sync (`epgsql`), async (`epgsqla`), incremental (`epgsqli`)
- Request pipelining capability
- Pluggable codec system for custom types
- No built-in pooling (use with pgapp or poolboy)

**Key Features:**
- Prepared statement caching
- Streaming results (`epgsqli`)
- LISTEN/NOTIFY support
- Binary protocol with automatic type conversion

**Strengths:**
- Very mature and battle-tested
- Flexible API options
- Streaming for large result sets
- Extensible type system

**Weaknesses:**
- Requires separate pooling solution
- More complex API surface
- Older design patterns

**Recommendation:** Consider for advanced features (streaming), but pgo is simpler

---

#### 6. poolboy - Generic Worker Pool

**Repository:** github.com/devinus/poolboy
**Status:** Stable but inactive maintenance

**Architecture:**
- Generic worker process pool
- LIFO/FIFO worker selection
- Overflow capacity for burst handling
- Supervisor-based worker management

**API Pattern:**

```erlang
Worker = poolboy:checkout(PoolName),
try
    gen_server:call(Worker, Request)
after
    poolboy:checkin(PoolName, Worker)
end

% Or using transaction wrapper
poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, Request)
end)
```

**Strengths:**
- Simple, focused design
- Rock-solid stability
- Widely used and understood

**Weaknesses:**
- Inactive maintenance
- Erlang-only (no Elixir niceties)
- No built-in metrics

**Recommendation:** Not needed if using pgo (has built-in pooling)

---

#### 7. poolex - Elixir Worker Pool

**Repository:** github.com/general-CbIC/poolex
**Status:** Actively maintained

**Architecture:**
- Native Elixir implementation
- Dynamic scaling based on load
- Built-in metrics
- Pluggable worker/caller strategies

**Strengths:**
- Modern Elixir patterns
- Active maintenance
- Better observability than poolboy

**Weaknesses:**
- Elixir-focused (additional dependency for Gleam)
- Not needed if using pgo

**Recommendation:** Not needed if using pgo

---

### Supporting Libraries

#### 8. Decimal (Elixir)

**Package:** decimal
**Purpose:** Arbitrary precision decimal arithmetic

**Representation:** `sign * coefficient * 10^exponent`

**Strengths:**
- Exact representation (no floating-point errors)
- IEEE 854-1987 compliant
- Rich arithmetic operations

**Recommendation:** Use for `NUMERIC`/`DECIMAL` PostgreSQL types

---

#### 9. birl - Gleam Time Library

**Package:** birl
**Version:** 1.8.0

**Types:**
- `Time` - opaque moment in time
- `Day` - year/month/date record
- `TimeOfDay` - hour/minute/second/millisecond

**Strengths:**
- ISO8601 parsing
- Unix timestamp conversion
- Timezone support (basic)
- Idiomatic Gleam API

**Recommendation:** Use for timestamp types, integrate with pog's timestamp decoder

---

## Comparative Analysis

### Query Execution Patterns

| Library | Pattern | Pros | Cons |
|---------|---------|------|------|
| pog | Fluent builder | Composable, readable | Verbose for simple queries |
| sqlight | Function call | Simple, direct | Less composable |
| epgsql | Multiple APIs | Flexible | Complex choice |

### Row Decoding Approaches

| Approach | Example | Pros | Cons |
|----------|---------|------|------|
| Dynamic decode | pog, sqlight | Type-safe, composable | Manual decoder writing |
| Positional tuple | epgsql | Fast, simple | Brittle, no field names |
| Dict/Map | pgo option | Flexible | Runtime key lookup |

### Connection Pooling

| Library | Pooling | Notes |
|---------|---------|-------|
| pgo | Built-in | Queue-based, configurable |
| epgsql | External (poolboy) | Requires separate setup |
| sqlight | None | SQLite doesn't need pooling |

---

## Recommendations for cquill

### 1. PostgreSQL Driver: Use pgo via pog

**Rationale:**
- pog already provides an excellent Gleam wrapper
- pgo has built-in pooling (no need for poolboy)
- Direct socket access provides good performance
- OpenTelemetry support aligns with our telemetry goals

**Action:** Don't wrap pgo directly; consider depending on pog or forking/adapting its FFI layer.

### 2. Decoder Pattern: Adopt gleam/dynamic/decode Style

**Rationale:**
- Established pattern in Gleam ecosystem
- Composable with `use` expressions
- Good error messages with path tracking
- Users already familiar with it

**Adaptation for cquill:**
```gleam
// Row decoder (positional by index)
let user_decoder = {
  use id <- decode.index(0, decode.int)
  use email <- decode.index(1, decode.string)
  use name <- decode.index(2, decode.optional(decode.string))
  decode.success(User(id:, email:, name:))
}
```

### 3. Connection Pool: Use pgo's Built-in Pooling

**Rationale:**
- Well-designed queue-based pool
- Configurable parameters
- No additional dependency

### 4. Type Mappings

| PostgreSQL | Gleam | Library |
|------------|-------|---------|
| `integer`, `serial` | `Int` | stdlib |
| `bigint` | `Int` | stdlib |
| `real`, `double` | `Float` | stdlib |
| `numeric`, `decimal` | `Decimal` | decimal package |
| `varchar`, `text` | `String` | stdlib |
| `boolean` | `Bool` | stdlib |
| `bytea` | `BitArray` | stdlib |
| `timestamp[tz]` | `birl.Time` | birl |
| `date` | `birl.Day` | birl |
| `time` | `birl.TimeOfDay` | birl |
| `uuid` | `String` or newtype | custom |
| `json[b]` | `Dynamic` or `Json` | gleam_json |
| `ARRAY` | `List(a)` | stdlib |
| `ENUM` | Generated type | codegen |

### 5. Error Handling

Adopt pog's error type structure but extend for cquill's needs:

```gleam
pub type AdapterError {
  // Constraint violations (from pog pattern)
  ConstraintViolated(message: String, constraint: String, detail: String)

  // Extended constraint types
  UniqueViolation(constraint: String, column: String, value: String)
  ForeignKeyViolation(constraint: String, detail: String)
  NotNullViolation(column: String)

  // Query errors
  PostgresqlError(code: String, name: String, message: String)
  UnexpectedArgumentCount(expected: Int, got: Int)
  UnexpectedResultType(List(DecodeError))

  // Connection errors
  QueryTimeout
  ConnectionUnavailable
  PoolExhausted
}
```

### 6. What cquill Adds

Based on ecosystem gaps, cquill should provide:

1. **Schema Layer** (missing everywhere)
   - Type-safe table/column definitions
   - No runtime "magic" like Ecto macros

2. **Query Builder** (missing everywhere)
   - Composable, type-safe query construction
   - Phantom types for compile-time validation

3. **Adapter Abstraction** (missing everywhere)
   - Same API for Postgres, Memory, future DBs
   - Testability without real database

4. **Code Generation** (missing everywhere)
   - Generate schemas from database introspection
   - Generate decoders automatically

5. **Changeset/Validation** (missing everywhere)
   - Pure validation functions
   - Structured error types

---

## Conclusion

The Gleam database ecosystem has solid foundations (pog, sqlight) but lacks higher-level abstractions. cquill fills this gap by providing:

- **Schema definitions** without ORM magic
- **Composable queries** with type safety
- **Adapter pattern** for testability
- **Code generation** for reduced boilerplate

We should build on pog's excellent FFI work rather than reinventing PostgreSQL connectivity.

---

## Next Steps

1. ✅ Complete ecosystem audit (this document)
2. Create decision document for driver choice (#8)
3. Create decision document for pooling strategy (#9)
4. Spike row decoding ergonomics (#10)
5. Spike schema introspection codegen (#11)
