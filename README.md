# cquill

A compile-time safe data access library for Gleam.

[![Package Version](https://img.shields.io/hexpm/v/cquill)](https://hex.pm/packages/cquill)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/cquill/)
[![CI](https://github.com/justin4957/cquill/actions/workflows/ci.yml/badge.svg)](https://github.com/justin4957/cquill/actions/workflows/ci.yml)
[![License](https://img.shields.io/hexpm/l/cquill)](https://github.com/justin4957/cquill/blob/main/LICENSE)

**"Ecto, but scaled down and typed, for Gleam"** — Schema-like types, composable queries, and adapter-based persistence without locking into any particular database.

## Design Philosophy

- **Compile-time safety over runtime convenience** — Invalid queries should fail at compile time
- **Explicit over implicit** — No magic; transformations are visible and traceable
- **Gleam-idiomatic** — Leverage Result types, pipelines, and the module system naturally
- **Adapter-first** — Define persistence boundaries early; real DBs are just one adapter
- **Small, composable modules** — Each module has one responsibility

## Architecture

cquill follows a layered architecture with clear boundaries:

```
┌─────────────────────────────────────────┐
│           Your Application              │
├─────────────────────────────────────────┤
│  Schema    │  Changeset  │    Query     │  ← Pure, no I/O
│  (types)   │ (validation)│   (builder)  │
├─────────────────────────────────────────┤
│                  Repo                   │  ← Public API
├─────────────────────────────────────────┤
│  Memory    │  Postgres   │   (Future)   │  ← Adapters
│  Adapter   │  Adapter    │   Adapters   │
└─────────────────────────────────────────┘
```

## Installation

```sh
gleam add cquill
```

## Quick Start

```gleam
import cquill/schema
import cquill/schema/field
import cquill/query
import cquill/adapter/postgres

pub fn main() {
  // Define your schema
  let user_schema = schema.new("users")
    |> schema.add_field(field.integer("id") |> field.primary_key())
    |> schema.add_field(field.string("email") |> field.not_null())
    |> schema.add_field(field.string("name") |> field.nullable())
    |> schema.add_field(field.boolean("active") |> field.not_null())
    |> schema.add_field(field.datetime("created_at") |> field.not_null())

  // Build a query
  let active_users = query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.order_by_desc("created_at")
    |> query.limit(10)

  // Execute via adapter
  use pool <- result.try(postgres.connect(config))
  adapter.query(postgres.adapter(), pool, compiled_query)
}
```

## Key Features

### Core
- **Schemas as data** — Define table structure with fields, types, and constraints
- **Composable queries** — Type-safe AST-based query builder with compile-time validation
- **Changesets** — Validate and transform data with pure functions before persistence
- **Adapter abstraction** — Same API works with Postgres, in-memory, or custom backends

### Query Builder
- Full SQL support: `SELECT`, `WHERE`, `JOIN`, `ORDER BY`, `GROUP BY`, `HAVING`
- Conditions: `eq`, `gt`, `gte`, `lt`, `lte`, `like`, `ilike`, `in_list`, `between`, `is_null`
- Joins: `INNER`, `LEFT`, `RIGHT`, `FULL`, `CROSS`
- Subqueries and raw SQL escape hatch

### Adapters
- **Memory Adapter** — Fast, isolated testing with full transaction and savepoint support
- **PostgreSQL Adapter** — Production-ready with connection pooling and RETURNING clause

### Developer Experience
- Code generation from database schemas (`gleam run -m cquill generate`)
- Rich error messages with actionable hints
- Telemetry and logging hooks for observability
- Watch mode for development workflow

## Database Migrations

cquill focuses on runtime data access, not schema evolution. We recommend using dedicated migration tools:

| Tool | Best For | Installation |
|------|----------|--------------|
| [dbmate](https://github.com/amacneil/dbmate) | Simple SQL migrations | `brew install dbmate` |
| [sqitch](https://sqitch.org/) | Complex dependency chains | `brew install sqitch` |
| [flyway](https://flywaydb.org/) | Enterprise environments | `brew install flyway` |

### Quick Start with dbmate

```bash
# Create a migration
dbmate new add_users_table

# Apply migrations
dbmate up

# Regenerate cquill types
gleam run -m cquill_cli generate
```

See [docs/MIGRATIONS.md](docs/MIGRATIONS.md) for the complete migration guide, including:
- CI/CD integration examples
- Schema drift detection
- Makefile templates
- Best practices

## Status

**Stable** — cquill v0.1.1 is production-ready with comprehensive test coverage including:
- 1300+ unit and integration tests
- Property-based testing for query builder
- Concurrent access and race condition testing
- PostgreSQL integration tests
- Security audit validation

See the [CHANGELOG](CHANGELOG.md) for release history.

## Requirements

| Dependency | Minimum Version |
|------------|-----------------|
| Gleam | 1.14.0+ |
| Erlang/OTP | 26+ |
| PostgreSQL | 14+ (for postgres adapter) |

## Development

```sh
gleam test           # Run all tests
gleam format         # Format code
gleam docs build     # Build documentation
```

Further documentation can be found at <https://hexdocs.pm/cquill>.

## License

Apache-2.0
