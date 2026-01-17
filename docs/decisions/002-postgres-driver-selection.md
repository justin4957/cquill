# ADR-002: PostgreSQL Driver Selection

**Status:** Accepted
**Date:** 2026-01-17
**Issue:** #8
**Supersedes:** None
**Informed by:** ADR-001 (Ecosystem Audit)

## Decision

**Use pgo (via the pog Gleam wrapper) as the PostgreSQL driver for cquill.**

## Context

cquill needs a PostgreSQL driver to execute queries. The Erlang ecosystem offers two primary options:

1. **pgo** - Modern driver with built-in pooling
2. **epgsql** - Mature driver requiring external pooling

We evaluated both against cquill's requirements.

## Options Considered

### Option A: pgo (via pog)

**pgo** is a modern Erlang PostgreSQL driver designed for OTP 26+. **pog** is the official Gleam wrapper maintained by the Gleam core team.

**Pros:**
- Built-in connection pooling (queue-based, configurable)
- Direct socket checkout (no message passing overhead)
- Binary protocol with cached OIDs for efficiency
- OpenTelemetry instrumentation built-in
- Clean, idiomatic Gleam API via pog
- Actively maintained by Gleam ecosystem
- Transaction support
- PostgreSQL error code mapping

**Cons:**
- Requires OTP 26+ (may limit older deployments)
- Less mature than epgsql
- No streaming/incremental result support

### Option B: epgsql (with poolboy)

**epgsql** is a long-established Erlang PostgreSQL driver with multiple API styles.

**Pros:**
- Very mature and battle-tested
- Streaming results via `epgsqli` API
- Pluggable codec system for custom types
- Request pipelining capability
- Works with older OTP versions

**Cons:**
- Requires separate pooling solution (poolboy, poolex)
- More complex API surface (three different APIs)
- No official Gleam wrapper (would need to write FFI)
- Older design patterns
- More setup complexity

### Option C: Direct FFI to libpq

Write our own Gleam FFI directly to PostgreSQL's C library.

**Pros:**
- Maximum control
- No dependency on Erlang libraries

**Cons:**
- Significant implementation effort
- Reinventing well-solved problems
- Security and stability risks
- Maintenance burden

## Decision Rationale

We choose **Option A: pgo via pog** for the following reasons:

### 1. Reduced Complexity

pgo's built-in pooling eliminates the need for a separate pooling dependency. This means:
- Fewer moving parts
- Single configuration point
- Consistent behavior

### 2. Gleam Ecosystem Alignment

pog is maintained by the Gleam core team and represents the idiomatic way to access PostgreSQL from Gleam. Using it means:
- Familiar API for Gleam developers
- Consistent with ecosystem conventions
- Benefit from upstream improvements

### 3. Performance Characteristics

pgo's design philosophy ("No message passing. Clients checkout the socket and use it directly.") provides:
- Lower latency per query
- Better resource utilization
- Predictable performance

### 4. Modern Features

pgo provides features we need:
- OpenTelemetry support (aligns with Phase 5 telemetry goals)
- Comprehensive error types
- Transaction support

### 5. Sufficient for MVP

While epgsql offers streaming, pgo covers all requirements for cquill's MVP:
- Connection pooling ✓
- Parameterized queries ✓
- Transactions ✓
- Error handling ✓

Streaming can be added later if needed via a separate adapter.

## Implementation Approach

### Dependency Strategy

Rather than depending directly on pog, cquill will:

1. **Use pog as a reference** for FFI patterns
2. **Depend on pgo directly** from Erlang
3. **Write a thin Gleam FFI layer** tailored to cquill's adapter interface

This approach provides:
- Control over the API surface
- Ability to optimize for cquill's patterns
- No coupling to pog's release cycle

### FFI Structure

```
src/cquill/adapter/postgres/
├── ffi.gleam         # Low-level pgo FFI bindings
├── connection.gleam  # Connection type and lifecycle
├── pool.gleam        # Pool configuration and management
├── query.gleam       # Query execution
└── types.gleam       # Type encoding/decoding
```

### Configuration Mapping

pgo configuration maps to cquill's PostgresConfig:

| cquill Config | pgo Config | Default |
|---------------|------------|---------|
| `host` | `host` | "127.0.0.1" |
| `port` | `port` | 5432 |
| `database` | `database` | required |
| `user` | `user` | required |
| `password` | `password` | None |
| `pool_size` | `pool_size` | 10 |
| `ssl_mode` | `ssl` | Disable |
| `connect_timeout_ms` | `connect_timeout` | 5000 |
| `query_timeout_ms` | (per-query) | 5000 |

## Consequences

### Positive

- Simpler architecture (pooling included)
- Gleam-idiomatic patterns to follow
- Good performance characteristics
- Built-in observability

### Negative

- OTP 26+ requirement
- No streaming for large result sets (initially)
- Dependent on pgo's maintenance

### Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| pgo maintenance slows | Low | Medium | Could fork or switch to epgsql adapter |
| Need streaming results | Medium | Medium | Add epgsql-based adapter later |
| OTP 26 too restrictive | Low | High | Document requirement clearly |

## Validation

This decision will be validated by:

1. **Spike implementation** (#11) demonstrating:
   - Connection establishment
   - Query execution
   - Transaction handling
   - Error mapping

2. **Adapter contract tests** passing for:
   - All CRUD operations
   - Transaction semantics
   - Constraint violation handling

## References

- [pgo GitHub](https://github.com/erleans/pgo)
- [pog GitHub](https://github.com/lpil/pog)
- [pog HexDocs](https://hexdocs.pm/pog)
- [epgsql GitHub](https://github.com/epgsql/epgsql)
- ADR-001: Ecosystem Audit
