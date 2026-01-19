# cquill Documentation

Welcome to the cquill documentation. cquill is a compile-time safe data access library for Gleam, inspired by Ecto but designed specifically for Gleam's type system and idioms.

## Quick Links

- [Getting Started](./getting-started.md) - Installation and first query
- [API Reference](./reference/api.md) - Complete API documentation
- [Examples](./examples/) - Working code examples

## Documentation Structure

### Concepts

Understand the core ideas behind cquill:

- [Schemas](./concepts/schemas.md) - How schemas describe your data
- [Queries](./concepts/queries.md) - Building type-safe queries
- [Repo](./concepts/repo.md) - The repository pattern for data access
- [Adapters](./concepts/adapters.md) - Abstraction over different backends

### Guides

Step-by-step tutorials for common tasks:

- [CRUD Operations](./guides/crud.md) - Create, read, update, delete
- [Advanced Queries](./guides/queries.md) - Joins, aggregations, subqueries
- [Transactions](./guides/transactions.md) - Transaction patterns and savepoints
- [Testing](./guides/testing.md) - Test strategies with cquill
- [Migrations](./MIGRATIONS.md) - Database migration integration

### Reference

Detailed technical reference:

- [API Reference](./reference/api.md) - All public functions
- [Types Reference](./reference/types.md) - All public types
- [Errors Reference](./reference/errors.md) - Error types and handling
- [Configuration](./reference/config.md) - Configuration options

### Recipes

Real-world patterns and solutions:

- [Pagination](./recipes/pagination.md) - Offset and cursor pagination
- [Soft Delete](./recipes/soft-delete.md) - Soft delete pattern
- [Audit Logging](./recipes/audit-log.md) - Track changes
- [Multi-tenancy](./recipes/multi-tenant.md) - Multi-tenant queries

## Design Philosophy

cquill is built on these principles:

1. **Compile-time safety over runtime convenience** - Invalid queries fail at compile time
2. **Explicit over implicit** - No magic; transformations are visible
3. **Gleam-idiomatic** - Leverage Result types, pipelines, and the module system
4. **Adapter-first** - Same API works with any backend
5. **Small, composable modules** - Each module has one responsibility

## Architecture Overview

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

## Getting Help

- [GitHub Issues](https://github.com/justin4957/cquill/issues) - Report bugs or request features
- [GitHub Discussions](https://github.com/justin4957/cquill/discussions) - Ask questions

## Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines on contributing to cquill.
