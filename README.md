# cquill

A compile-time safe data access library for Gleam.

[![Package Version](https://img.shields.io/hexpm/v/cquill)](https://hex.pm/packages/cquill)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/cquill/)

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
import cquill/query
import cquill/repo
import cquill/adapter/postgres

pub fn main() {
  // Define your schema
  let user_schema = schema.new("users")
    |> schema.field("id", schema.int())
    |> schema.field("email", schema.string())
    |> schema.field("name", schema.optional(schema.string()))

  // Build a query
  let active_users = query.from(user_schema)
    |> query.where(query.eq("active", True))
    |> query.order_by("created_at", query.Desc)
    |> query.limit(10)

  // Execute via repo
  use pool <- result.try(postgres.connect(config))
  repo.all(pool, active_users)
}
```

## Key Features

- **Schemas as data** — Define structure without coupling to persistence
- **Composable queries** — Build complex queries from simple, reusable parts
- **Changesets** — Validate and transform data before persistence
- **Adapter abstraction** — Same API works with Postgres, in-memory, or custom backends
- **Testable by design** — Use in-memory adapter for fast, isolated tests

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

This library is currently in early development. See the [GitHub Issues](https://github.com/justin4957/cquill/issues) for the development roadmap.

### Roadmap

- **Phase 0**: Foundation & Research
- **Phase 1**: Core Query Execution
- **Phase 2**: Code Generation (MVP)
- **Phase 3**: Type-Safe Query Builder
- **Phase 4**: Transactions & Advanced Features
- **Phase 5**: Developer Experience

## Development

```sh
gleam test           # Run all tests
gleam format         # Format code
gleam docs build     # Build documentation
```

Further documentation can be found at <https://hexdocs.pm/cquill>.

## License

Apache-2.0
