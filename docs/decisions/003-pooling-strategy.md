# ADR-003: Connection Pooling Strategy

**Status:** Accepted
**Date:** 2026-01-17
**Issue:** #9
**Supersedes:** None
**Informed by:** ADR-001 (Ecosystem Audit), ADR-002 (Driver Selection)

## Decision

**Use pgo's built-in connection pooling for the PostgreSQL adapter.**

This decision follows directly from ADR-002 (choosing pgo as the driver), as pgo includes an integrated connection pool.

## Context

Database connection pooling is essential for production applications:
- Creating connections is expensive (TCP handshake, authentication, TLS)
- Databases have connection limits
- Pooling amortizes connection cost across many queries

Given our decision to use pgo (ADR-002), we must decide whether to:
1. Use pgo's built-in pooling
2. Disable pgo's pooling and use an external solution (poolboy, poolex)
3. Build custom pooling on top of pgo

## Options Considered

### Option A: pgo's Built-in Pooling

Use the connection pool that ships with pgo.

**Pros:**
- Zero additional dependencies
- Single configuration point
- Designed specifically for PostgreSQL connections
- Queue-based with configurable parameters
- Direct socket checkout (no message passing)
- Well-integrated with pgo's connection lifecycle

**Cons:**
- Less flexibility than generic poolers
- Can't easily swap pooling implementations
- pgo-specific configuration parameters

### Option B: External Pooling (poolboy)

Disable pgo's pooling and wrap with poolboy.

**Pros:**
- Well-known, battle-tested
- Generic (same pool for multiple resource types)
- LIFO/FIFO selection strategies

**Cons:**
- Additional dependency
- Configuration in two places
- Message passing overhead (poolboy uses gen_server)
- Poolboy is not actively maintained
- Requires custom worker module

### Option C: External Pooling (poolex)

Disable pgo's pooling and wrap with poolex.

**Pros:**
- Actively maintained
- Dynamic scaling
- Built-in metrics

**Cons:**
- Elixir-focused library
- Additional dependency
- Configuration complexity
- Not commonly used with Gleam

### Option D: Custom Pooling

Build a Gleam-native connection pool.

**Pros:**
- Full control
- Gleam-idiomatic API
- No external dependencies

**Cons:**
- Significant implementation effort
- Connection pooling is a solved problem
- Risk of bugs in critical infrastructure
- Maintenance burden

## Decision Rationale

We choose **Option A: pgo's built-in pooling** for the following reasons:

### 1. Simplicity

Using the integrated pool means:
- One dependency instead of two
- One configuration structure
- No impedance mismatch between pool and driver

### 2. Performance

pgo's pool design provides performance benefits:
- Direct socket checkout (no gen_server call for checkout)
- No message passing overhead
- Connection and pool lifecycle are unified

### 3. Sufficient Flexibility

pgo's pool configuration covers our needs:

| Parameter | Purpose | Default |
|-----------|---------|---------|
| `pool_size` | Number of connections | 1 |
| `queue_target` | Target queue wait time (ms) | 50 |
| `queue_interval` | Queue check interval (ms) | 1000 |
| `idle_interval` | Idle connection ping (ms) | 1000 |

### 4. No Compelling Alternative

The main reasons to use external pooling don't apply:
- **poolboy**: Not actively maintained
- **poolex**: Elixir-focused, overkill for our needs
- **Custom**: Reinventing solved problems

## Implementation Details

### Pool Configuration

cquill's PostgresConfig maps to pgo pool settings:

```gleam
pub type PostgresConfig {
  PostgresConfig(
    // Connection settings
    host: String,
    port: Int,
    database: String,
    user: String,
    password: Option(String),
    ssl_mode: SslMode,

    // Pool settings (maps to pgo)
    pool_size: Int,           // pgo: pool_size
    queue_target_ms: Int,     // pgo: queue_target
    queue_interval_ms: Int,   // pgo: queue_interval
    idle_interval_ms: Int,    // pgo: idle_interval

    // Timeouts
    connect_timeout_ms: Int,
    query_timeout_ms: Int,
  )
}

pub fn default_config() -> PostgresConfig {
  PostgresConfig(
    host: "127.0.0.1",
    port: 5432,
    database: "",
    user: "",
    password: None,
    ssl_mode: SslDisable,
    pool_size: 10,
    queue_target_ms: 50,
    queue_interval_ms: 1000,
    idle_interval_ms: 1000,
    connect_timeout_ms: 5000,
    query_timeout_ms: 5000,
  )
}
```

### Pool Lifecycle

```gleam
/// Start a connection pool
pub fn connect(config: PostgresConfig) -> Result(Pool, AdapterError)

/// Stop a connection pool
pub fn disconnect(pool: Pool) -> Nil

/// Execute with automatic checkout/checkin
pub fn with_connection(
  pool: Pool,
  f: fn(Connection) -> Result(a, e),
) -> Result(a, AdapterError)
```

### Pool Health

pgo handles connection health automatically:
- Connections are pinged during idle periods
- Failed connections are removed and replaced
- Queue backpressure prevents overload

### Observability

pgo supports OpenTelemetry tracing, which we'll expose:

```gleam
pub type PostgresConfig {
  // ...
  trace: Bool,  // Enable OpenTelemetry tracing
}
```

## Consequences

### Positive

- Simpler architecture
- Better performance (no message passing)
- Unified configuration
- No additional dependencies

### Negative

- Tied to pgo's pool implementation
- Less flexibility for exotic pooling needs
- Pool configuration options limited to pgo's parameters

### Trade-offs Accepted

| Giving Up | In Exchange For |
|-----------|-----------------|
| Custom pool strategies | Simplicity |
| External pool metrics | Integrated tracing |
| Pool implementation swapping | Fewer dependencies |

## Validation

This decision will be validated by:

1. **Load testing** demonstrating:
   - Pool handles concurrent requests
   - Queue backpressure works correctly
   - No connection leaks

2. **Integration tests** verifying:
   - Pool starts and stops cleanly
   - Connections are reused
   - Failed connections are recovered

## Future Considerations

If we later need features pgo doesn't provide:

1. **Dynamic scaling**: Could implement at cquill level by managing multiple pools
2. **Advanced metrics**: Add telemetry hooks around pool operations
3. **Different strategies**: Create alternative adapter using epgsql + poolboy

## References

- [pgo Pool Configuration](https://github.com/erleans/pgo#pool-configuration)
- [pog Documentation](https://hexdocs.pm/pog)
- ADR-001: Ecosystem Audit
- ADR-002: PostgreSQL Driver Selection
