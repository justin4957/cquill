# Configuration Reference

Configuration options for cquill and its components.

## PostgreSQL Adapter Configuration

### Connection Configuration

```gleam
import cquill/adapter/postgres

let config = postgres.Config(
  /// Database host
  host: "localhost",

  /// Database port
  port: 5432,

  /// Database name
  database: "myapp_development",

  /// Database user
  user: "postgres",

  /// Database password
  password: "secret",

  /// Connection pool size
  pool_size: 10,

  /// Enable SSL
  ssl: False,

  /// Connection timeout in milliseconds
  connection_timeout: 5000,

  /// Query timeout in milliseconds
  query_timeout: 30000,
)
```

### Configuration from URL

```gleam
// Parse DATABASE_URL format
let config = postgres.parse_url("postgres://user:pass@host:5432/dbname")

// With options
let config = postgres.parse_url("postgres://user:pass@host:5432/dbname?pool_size=20&ssl=true")
```

### Configuration from Environment

```gleam
fn get_database_config() -> postgres.Config {
  postgres.Config(
    host: env.get("DB_HOST") |> result.unwrap("localhost"),
    port: env.get("DB_PORT") |> result.try(int.parse) |> result.unwrap(5432),
    database: env.get("DB_NAME") |> result.unwrap("myapp_dev"),
    user: env.get("DB_USER") |> result.unwrap("postgres"),
    password: env.get("DB_PASSWORD") |> result.unwrap(""),
    pool_size: env.get("DB_POOL_SIZE") |> result.try(int.parse) |> result.unwrap(10),
    ssl: env.get("DB_SSL") |> result.map(fn(s) { s == "true" }) |> result.unwrap(False),
    connection_timeout: 5000,
    query_timeout: 30000,
  )
}
```

### Pool Configuration

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `pool_size` | Int | 10 | Maximum number of connections |
| `pool_overflow` | Int | 0 | Additional connections allowed under load |
| `pool_timeout` | Int | 5000 | Time to wait for connection (ms) |
| `idle_timeout` | Int | 300000 | Close idle connections after (ms) |

### Timeout Configuration

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `connection_timeout` | Int | 5000 | Connection attempt timeout (ms) |
| `query_timeout` | Int | 30000 | Query execution timeout (ms) |
| `statement_timeout` | Int | None | PostgreSQL statement_timeout |

### SSL Configuration

```gleam
let config = postgres.Config(
  ...base_config,
  ssl: True,
  ssl_opts: Some(postgres.SslOpts(
    verify: postgres.VerifyPeer,
    cacertfile: "/path/to/ca.crt",
    certfile: Some("/path/to/client.crt"),
    keyfile: Some("/path/to/client.key"),
  )),
)
```

## Development Mode Configuration

### DevConfig

```gleam
import cquill/dev

let config = dev.DevConfig(
  /// Enable/disable dev mode
  enabled: True,

  /// Log all queries
  log_queries: True,

  /// Include parameters in logs
  log_params: True,

  /// Threshold for slow query warnings (ms)
  slow_query_threshold_ms: 100,

  /// Enable pool statistics logging
  log_pool_stats: False,

  /// Pool stats logging interval (ms)
  pool_stats_interval_ms: 10000,

  /// Fields to mask in logs
  mask_fields: ["password", "token", "secret", "key", "credential", "api_key"],

  /// Pattern to replace masked values
  mask_pattern: "[REDACTED]",

  /// Enable EXPLAIN output
  explain_queries: False,
)
```

### Preset Configurations

#### Default Configuration

Balanced settings for development:

```gleam
let config = dev.default_config()
// DevConfig(
//   enabled: True,
//   log_queries: True,
//   log_params: True,
//   slow_query_threshold_ms: 100,
//   log_pool_stats: False,
//   pool_stats_interval_ms: 10000,
//   mask_fields: ["password", "token", "secret", "key", "credential", "api_key"],
//   mask_pattern: "[REDACTED]",
//   explain_queries: False,
// )
```

#### Minimal Configuration

Query logging only, no parameters:

```gleam
let config = dev.minimal_config()
// DevConfig(
//   enabled: True,
//   log_queries: True,
//   log_params: False,
//   slow_query_threshold_ms: 500,
//   log_pool_stats: False,
//   ...
// )
```

#### Verbose Configuration

All debugging enabled:

```gleam
let config = dev.verbose_config()
// DevConfig(
//   enabled: True,
//   log_queries: True,
//   log_params: True,
//   slow_query_threshold_ms: 50,
//   log_pool_stats: True,
//   pool_stats_interval_ms: 5000,
//   explain_queries: True,
//   ...
// )
```

### Environment Variable Activation

Enable dev mode via environment variable:

```bash
# Enable dev mode
CQUILL_DEV=1 gleam run

# Accepted values: 1, true, yes, on
```

```gleam
// At application startup
dev.enable_from_env()
```

## Telemetry Configuration

### Attaching Handlers

```gleam
import cquill/telemetry

// Start telemetry server
telemetry.start()

// Attach handlers
telemetry.attach(
  "my-logger",        // Handler ID
  [                   // Event types to subscribe
    telemetry.QueryStopType,
    telemetry.QueryExceptionType,
  ],
  my_handler_fn,      // Handler function
)
```

### Event Types

| Event Type | Description |
|------------|-------------|
| `QueryStartType` | Query execution started |
| `QueryStopType` | Query execution completed |
| `QueryExceptionType` | Query execution failed |
| `PoolCheckoutType` | Connection checked out from pool |
| `PoolCheckinType` | Connection returned to pool |
| `PoolTimeoutType` | Pool checkout timed out |
| `TransactionStartType` | Transaction started |
| `TransactionCommitType` | Transaction committed |
| `TransactionRollbackType` | Transaction rolled back |
| `SavepointCreateType` | Savepoint created |
| `SavepointRollbackType` | Rolled back to savepoint |
| `SavepointReleaseType` | Savepoint released |
| `BatchStartType` | Batch operation started |
| `BatchStopType` | Batch operation completed |

### Built-in Handlers

```gleam
// Logger handler - logs queries
telemetry.attach("logger", [telemetry.QueryStopType], telemetry.logger_handler())

// Slow query handler - logs queries exceeding threshold
telemetry.attach("slow", [telemetry.QueryStopType], telemetry.slow_query_handler(100))

// Debug handler - logs all events
telemetry.attach("debug", telemetry.all_event_types(), telemetry.debug_handler())

// Metrics handler - custom callbacks
telemetry.attach("metrics", [telemetry.QueryStopType], telemetry.metrics_handler(
  on_query_complete: fn(query, duration_us, row_count) { ... },
  on_query_error: fn(error_type) { ... },
  on_pool_timeout: fn(pool_name) { ... },
))
```

## Memory Adapter Configuration

The memory adapter requires no configuration:

```gleam
import cquill/adapter/memory

// Create a new store
let store = memory.new_store()

// Create tables as needed
let store = store
  |> memory.create_table("users", ["id", "email", "name"])
  |> memory.create_table("posts", ["id", "user_id", "title", "body"])
```

## Code Generation Configuration

### Generator Config

```gleam
import cquill/codegen/generator

let config = generator.GeneratorConfig(
  /// Output directory for generated files
  output_dir: "src/generated",

  /// Module prefix for generated modules
  module_prefix: "myapp/schema",

  /// Tables to include (empty = all)
  include_tables: [],

  /// Tables to exclude
  exclude_tables: ["schema_migrations", "_prisma_migrations"],

  /// Generate decoders
  generate_decoders: True,

  /// Generate encoders
  generate_encoders: True,

  /// Generate query helpers
  generate_queries: True,
)
```

### CLI Configuration

Via command line:

```bash
gleam run -m cquill_cli generate \
  --output-dir src/generated \
  --module-prefix myapp/schema \
  --exclude schema_migrations
```

Via gleam.toml:

```toml
[cquill]
output_dir = "src/generated"
module_prefix = "myapp/schema"
exclude_tables = ["schema_migrations"]
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `DATABASE_URL` | PostgreSQL connection URL | - |
| `DB_HOST` | Database host | localhost |
| `DB_PORT` | Database port | 5432 |
| `DB_NAME` | Database name | - |
| `DB_USER` | Database user | postgres |
| `DB_PASSWORD` | Database password | - |
| `DB_POOL_SIZE` | Connection pool size | 10 |
| `DB_SSL` | Enable SSL | false |
| `CQUILL_DEV` | Enable dev mode | - |

## Production Recommendations

### Connection Pool

```gleam
let config = postgres.Config(
  ...base_config,
  // Size based on: (2 * CPU cores) + spindle count
  pool_size: 20,

  // Allow some overflow for burst traffic
  pool_overflow: 5,

  // Don't wait too long for connections
  pool_timeout: 5000,

  // Clean up idle connections
  idle_timeout: 300000,  // 5 minutes
)
```

### Timeouts

```gleam
let config = postgres.Config(
  ...base_config,
  // Fail fast on connection issues
  connection_timeout: 5000,

  // Reasonable query timeout
  query_timeout: 30000,

  // PostgreSQL-level timeout
  statement_timeout: 25000,
)
```

### SSL

```gleam
let config = postgres.Config(
  ...base_config,
  ssl: True,
  ssl_opts: Some(postgres.SslOpts(
    verify: postgres.VerifyPeer,
    cacertfile: "/etc/ssl/certs/ca-certificates.crt",
  )),
)
```

### Disable Dev Mode

```gleam
// Don't enable dev mode in production
case env.get("GLEAM_ENV") {
  Ok("production") -> Nil
  _ -> dev.enable_from_env()
}
```
