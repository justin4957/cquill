# ADR-004: Row Decoding and Code Generation Strategy

**Status:** Accepted
**Date:** 2026-01-17
**Issue:** #10, #11
**Supersedes:** None
**Informed by:** ADR-002 (Driver Selection), Spikes in `spikes/row_decoding/` and `spikes/schema_codegen/`

## Decision

**Adopt a hybrid approach combining manual decoder combinators with CLI-based code generation:**

1. **Core row decoding**: Use continuation-passing style combinators (`column`, `succeed`) for composable, type-safe decoders
2. **Code generation**: CLI tool that introspects PostgreSQL schema and generates Gleam modules with types and decoders
3. **Type mapping**: Comprehensive PostgreSQL-to-Gleam type mapping supporting nullability, arrays, and common types

## Context

cquill needs to decode database rows (lists of Dynamic values) into typed Gleam records. This involves:

1. **Row decoding**: Converting `List(Dynamic)` rows from pgo into typed Gleam values
2. **Type mapping**: Translating PostgreSQL types to appropriate Gleam types
3. **Code generation**: Optionally generating boilerplate decoders from database schema

We evaluated patterns from:
- **gleam/dynamic/decode**: Standard library decoder combinators
- **squirrel**: Gleam code generation from SQL queries using pg_type introspection
- **parrot**: Multi-database codegen using sqlc
- **sqlx (Rust)**: Compile-time query validation via database connection

## Options Considered

### Option A: Manual Decoders Only

Hand-write all decoders using combinators.

```gleam
pub fn user_decoder() -> RowDecoder(User) {
  use id <- column(0, decode.int)
  use email <- column(1, decode.string)
  use name <- optional_column(2, decode.string)
  succeed(User(id:, email:, name:))
}
```

**Pros:**
- Full control and flexibility
- No build-time dependencies
- Easy to customize per-query

**Cons:**
- Boilerplate for large schemas
- Manual synchronization with database schema
- Error-prone

### Option B: Positional Tuple Decoders

Use `decode2`, `decode3`, etc. functions that return tuples.

```gleam
pub fn decode_user(row: Row) -> DecodeResult(User) {
  use #(id, email, name) <- result.try(decode3(
    row, decode.int, decode.string, decode.optional(decode.string)
  ))
  Ok(User(id:, email:, name:))
}
```

**Pros:**
- Simpler API for fixed-column queries
- Pattern matching on tuples is idiomatic

**Cons:**
- Limited to fixed arity (decode2 through decode10)
- Less flexible than column-based approach
- No column skipping

### Option C: Schema-Driven Runtime Decoders

Load schema metadata at runtime and build decoders dynamically.

**Pros:**
- Always in sync with database
- No code generation step

**Cons:**
- Runtime overhead
- Errors discovered at runtime only
- Less type safety

### Option D: CLI Code Generation (squirrel-style)

Generate Gleam modules from database schema introspection.

```bash
cquill generate --schema public --output src/generated/
```

**Pros:**
- Generated code is inspectable and auditable
- No runtime overhead
- Works with version control
- Explicit workflow

**Cons:**
- Extra step in development
- Must regenerate on schema changes

### Option E: Compile-Time Validation (sqlx-style)

Connect to database at compile time to validate queries.

**Pros:**
- Catch errors at compile time
- No schema drift

**Cons:**
- Complex build setup
- Requires database during CI
- Not idiomatic for Gleam ecosystem

## Decision Rationale

We choose **Option A + Option D: Manual combinators with CLI code generation** because:

### 1. Manual Combinators as Foundation

The `column`/`succeed` pattern provides:
- **Composability**: Decoders can be combined and reused
- **Flexibility**: Custom logic can be inserted anywhere in the pipeline
- **Type safety**: Compile-time guarantees on decoder types
- **Familiarity**: Similar to `gleam/dynamic/decode` patterns

### 2. CLI Code Generation for Productivity

CLI-based generation (like squirrel) fits cquill because:
- **Gleam philosophy**: Explicit over implicit - developers see all generated code
- **Version control**: Generated code is committed and reviewed
- **No build complexity**: No database connection required during compilation
- **Inspectable**: Easy to audit and understand what's happening

### 3. Not Compile-Time Validation

We reject sqlx-style compile-time validation because:
- Gleam doesn't have procedural macros like Rust
- Build-time database connections add complexity
- Schema drift between build and runtime remains possible

## Implementation Details

### Core Decoder API

Located in `src/cquill/row.gleam`:

```gleam
pub opaque type RowDecoder(a) {
  RowDecoder(run: fn(Row) -> DecodeResult(a))
}

pub fn column(
  index: Int,
  inner: decode.Decoder(a),
  next: fn(a) -> RowDecoder(b),
) -> RowDecoder(b)

pub fn optional_column(
  index: Int,
  inner: decode.Decoder(a),
  next: fn(Option(a)) -> RowDecoder(b),
) -> RowDecoder(b)

pub fn succeed(value: a) -> RowDecoder(a)

pub fn run_decoder(row: Row, decoder: RowDecoder(a)) -> DecodeResult(a)
```

### Type Mapping

| PostgreSQL Type | Gleam Type |
|-----------------|------------|
| `int2`, `int4`, `int8`, `serial` | `Int` |
| `float4`, `float8` | `Float` |
| `text`, `varchar`, `char` | `String` |
| `bool`, `boolean` | `Bool` |
| `bytea` | `BitArray` |
| `uuid` | `youid/uuid.Uuid` |
| `json`, `jsonb` | `gleam/json.Json` |
| `timestamp`, `timestamptz` | `gleam/time/timestamp.Timestamp` |
| `date` | `gleam/time/calendar.Date` |
| `<type>[]` | `List(<type>)` |
| Nullable column | `Option(<type>)` |

### Schema Introspection Queries

```sql
-- Column information
SELECT column_name, data_type, is_nullable, udt_name, ...
FROM information_schema.columns
WHERE table_schema = $1 AND table_name = $2

-- Primary key detection
SELECT a.attname
FROM pg_index i
JOIN pg_attribute a ON ...
WHERE i.indisprimary

-- Type resolution (for custom types)
SELECT typname, oid, typtype
FROM pg_type WHERE oid = $1
```

### CLI Commands

```bash
# Generate all tables in schema
cquill generate --schema public

# Generate specific tables
cquill generate --tables users,posts

# Output to specific directory
cquill generate --output src/db/

# Check if generated code matches schema (CI)
cquill generate --check
```

### Generated Code Structure

```gleam
// Generated by cquill - DO NOT EDIT
// Source: public.users

import gleam/option.{type Option}
import gleam/dynamic/decode
import cquill/row.{type RowDecoder, column, optional_column, succeed}

pub type Users {
  Users(
    id: Int,
    email: String,
    name: Option(String),
    created_at: Timestamp,
  )
}

pub fn users_decoder() -> RowDecoder(Users) {
  use id <- column(0, decode.int)
  use email <- column(1, decode.string)
  use name <- optional_column(2, decode.string)
  use created_at <- column(3, timestamp_decoder())
  succeed(Users(id:, email:, name:, created_at:))
}
```

## Consequences

### Positive

- **Type safety**: Compile-time guarantees on decoder types
- **Flexibility**: Mix generated and custom decoders freely
- **Transparency**: All code is visible and auditable
- **Performance**: No runtime reflection or schema loading
- **Ecosystem fit**: Follows Gleam conventions

### Negative

- **Manual sync**: Must regenerate when schema changes
- **CLI dependency**: Requires additional tool installation
- **Learning curve**: Two patterns to understand (manual + generated)

### Trade-offs Accepted

| Giving Up | In Exchange For |
|-----------|-----------------|
| Compile-time DB validation | Simpler build process |
| Automatic sync | Explicit, auditable code |
| Single pattern | Flexibility for custom cases |

## Validation

This decision is validated by:

1. **Spike implementations** demonstrating:
   - Manual decoders work with `use` syntax
   - Positional decoders compose correctly
   - Code generation produces valid Gleam
   - Type mapping covers common PostgreSQL types

2. **Test coverage** in spikes:
   - 16 tests for row decoding patterns
   - 22 tests for code generation

## Future Considerations

1. **Watch mode**: CLI could watch for schema changes and regenerate
2. **Query-specific decoders**: Generate decoders for specific SELECT queries (like squirrel)
3. **Migration integration**: Regenerate after migrations run
4. **Custom type support**: Plugin system for user-defined PostgreSQL types

## References

- [squirrel](https://github.com/giacomocavalieri/squirrel) - Gleam SQL code generation
- [parrot](https://github.com/daniellionel01/parrot) - Multi-DB code generation for Gleam
- [sqlx](https://github.com/launchbadge/sqlx) - Rust compile-time SQL validation
- [gleam/dynamic/decode](https://hexdocs.pm/gleam_stdlib/gleam/dynamic/decode.html) - Gleam decoder patterns
- Spike: `spikes/row_decoding/`
- Spike: `spikes/schema_codegen/`

Sources:
- [squirrel DeepWiki](https://deepwiki.com/giacomocavalieri/squirrel)
- [parrot GitHub](https://github.com/daniellionel01/parrot)
- [sqlx documentation](https://docs.rs/sqlx/latest/sqlx/macro.query.html)
- [Unraveling sqlx Macros](https://leapcell.io/blog/unraveling-sqlx-macros-compile-time-sql-verification-and-database-connectivity-in-rust)
