# cquill

A composable data access library for Gleam.

[![Package Version](https://img.shields.io/hexpm/v/cquill)](https://hex.pm/packages/cquill)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/cquill/)

**Ecto-inspired** — Schema definitions, composable queries, and adapter-based persistence without locking into any particular database.

> **Note on Type Safety**: This library uses string-based field names and runtime value conversion. It does not currently provide compile-time verification that field names exist in your schema or that value types match field types. See the [Roadmap](#roadmap) for planned type safety improvements.

## Design Philosophy

- **Explicit over implicit** — No magic; transformations are visible and traceable
- **Gleam-idiomatic** — Leverage Result types, pipelines, and the module system naturally
- **Adapter-first** — Define persistence boundaries early; real DBs are just one adapter
- **Small, composable modules** — Each module has one responsibility
- **Queries as data** — Queries are inspectable AST structures, not opaque strings

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

### Defining Schemas

```gleam
import cquill/schema
import cquill/schema/field

// Define your schema - this describes the table structure
let user_schema = schema.new("users")
  |> schema.add_field(field.integer("id") |> field.primary_key)
  |> schema.add_field(field.string("email") |> field.not_null)
  |> schema.add_field(field.string("name") |> field.nullable)
  |> schema.add_field(field.boolean("active") |> field.not_null)
  |> schema.add_field(field.integer("age") |> field.nullable)
```

### Building Queries

```gleam
import cquill/query

// Build queries using composable pipelines
// Note: Field names are strings - typos won't be caught at compile time
let active_users = query.from(user_schema)
  |> query.where(query.eq("active", True))
  |> query.order_by_desc("created_at")
  |> query.limit(10)

// Queries are just data - inspect them for debugging
let debug_str = query.to_debug_string(active_users)
```

### Executing Queries (Memory Adapter)

The memory adapter is perfect for testing and development:

```gleam
import cquill/adapter
import cquill/adapter/memory
import gleam/dynamic

pub fn example() {
  // Create an in-memory store with a table and column metadata
  // Column names enable WHERE clause filtering beyond just the primary key
  let store = memory.new_store()
    |> memory.create_table_with_columns("users", "id", [
      "id", "email", "name", "active", "age",
    ])

  // Insert data (column order must match the columns list above)
  let row = [
    dynamic.int(1),
    dynamic.string("alice@example.com"),
    dynamic.string("Alice"),
    dynamic.bool(True),
    dynamic.int(30),
  ]
  let assert Ok(store) = memory.insert_row(store, "users", "1", row)

  // Query using the adapter - WHERE clauses filter by any column
  let adp = memory.memory_adapter()
  let compiled = adapter.CompiledQuery(
    sql: "SELECT * FROM users WHERE active = $1",
    params: [adapter.ParamBool(True)],
    expected_columns: 5,
  )

  case adapter.query(adp, store, compiled) {
    Ok(rows) -> // rows is List(List(Dynamic)) - filtered by active = True
    Error(err) -> // handle error
  }
}
```

### Validating Data with Changesets

```gleam
import cquill/changeset
import gleam/dict
import gleam/dynamic
import gleam/option.{Some}

pub fn validate_user(data: Dict(String, Dynamic)) {
  changeset.new(data)
    |> changeset.validate_required(["email", "name"])
    |> changeset.validate_format("email", "^[^@]+@[^@]+$")
    |> changeset.validate_length("name", min: 2, max: 100)
    |> changeset.validate_number_range("age", min: Some(0), max: Some(150))
    |> changeset.apply()
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

- [x] **Phase 1**: Core Query Execution — Composable query AST, adapter abstraction
- [x] **Phase 2**: Code Generation (MVP) — Generate Gleam types from database schemas
- [ ] **Phase 3**: Type-Safe Query Builder — Compile-time field name validation, typed columns
- [ ] **Phase 4**: Transactions & Advanced Features
- [ ] **Phase 5**: Developer Experience

#### Type Safety Improvements (Planned)

The following improvements are planned to provide compile-time guarantees:

1. **Typed column references** — Replace string field names with generated column types
2. **Schema-validated queries** — Compile-time errors for non-existent fields
3. **Type-checked conditions** — Ensure comparison values match field types

## Development

```sh
gleam test           # Run all tests
gleam format         # Format code
gleam docs build     # Build documentation
```

Further documentation can be found at <https://hexdocs.pm/cquill>.

## License

Apache-2.0
