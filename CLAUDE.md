# cquill Development Guide

## Project Vision

**"Ecto, but scaled down and typed, for Gleam"** — A data access library providing schema-like types, composable queries, and adapter-based persistence, without locking into any particular DB or transport.

## Core Design Principles

1. **Compile-time safety over runtime convenience** — Invalid queries should fail at compile time
2. **Explicit over implicit** — No magic; transformations are visible and traceable
3. **Gleam-idiomatic** — Leverage Result types, pipelines, and the module system naturally
4. **Adapter-first** — Define persistence boundaries early; real DBs are just one adapter
5. **Small, composable modules** — Each module has one responsibility and is independently testable

## Layered Architecture

All code must belong to exactly one of these layers:

### 1. Schema Layer (`cquill/schema`)
- Gleam records + metadata describing fields, types, relationships
- **No I/O in this layer**
- Defines structure, not behavior
- Multiple schemas per domain encouraged (read model, write model, form model)

### 2. Changeset/Validation Layer (`cquill/changeset`)
- Pure functions: domain values → valid typed structs OR rich error types
- No side effects
- Composable validation pipelines

### 3. Query Layer (`cquill/query`)
- Typed DSL building abstract query structures (AST-like), **not strings**
- Queries are data: inspectable, serializable, composable
- Small functions that combine via pipelines

### 4. Repo/Adapter Layer (`cquill/repo`, `cquill/adapter`)
- Single public API surface for all I/O
- Adapters implement a behavior-like interface
- This is the **only** place DB/IO happens

## Adapter Protocol

Define minimal adapter interface before implementing any specific backend:

```gleam
// Every adapter must implement these operations
pub type AdapterBehaviour {
  insert: fn(Schema, Record) -> Result(Record, AdapterError)
  update: fn(Schema, Record, Changeset) -> Result(Record, AdapterError)
  delete: fn(Schema, Record) -> Result(Nil, AdapterError)
  all: fn(Schema, Query) -> Result(List(Record), AdapterError)
  one: fn(Schema, Query) -> Result(Option(Record), AdapterError)
  transaction: fn(fn(Tx) -> Result(a, e)) -> Result(a, TransactionError(e))
}
```

Required adapters:
- **In-memory adapter** (for testing, always implemented first)
- **Postgres adapter** (primary production target)

## Module Structure

```
src/cquill/
├── schema.gleam          # Schema definitions and metadata
├── schema/
│   ├── field.gleam       # Field types and metadata
│   └── association.gleam # Relationship definitions
├── changeset.gleam       # Changeset creation and validation
├── changeset/
│   └── validation.gleam  # Validation functions
├── query.gleam           # Query builder entry point
├── query/
│   ├── builder.gleam     # Query construction
│   ├── ast.gleam         # Query AST types
│   └── compiler.gleam    # AST to adapter-specific format
├── repo.gleam            # Public repo API
├── adapter.gleam         # Adapter behaviour definition
├── adapter/
│   ├── memory.gleam      # In-memory adapter (testing)
│   └── postgres.gleam    # Postgres adapter
└── error.gleam           # Unified error types
```

## Testing Strategy

### Three Concentric Rings

#### Ring 1: Unit Tests (Pure Core)
Location: `test/unit/`

**Schemas:**
- Schema metadata correctly describes Gleam types
- Field names, types, nullability are accurate
- Association metadata tested separately

**Changesets:**
- Valid input → valid struct, no errors
- Invalid input → structured errors (field, constraint, message)
- Property tests for invariants

**Queries:**
- Building query from filters yields expected AST
- Composition operations are associative/idempotent where expected
- Property tests for query invariants

#### Ring 2: Adapter Contract Tests
Location: `test/adapter/`

Shared test suite that ANY adapter must pass:
- `insert` persists or returns constraint violations predictably
- `update` reflects current state, reports stale data
- `delete` / `delete_all` semantics
- `all` / `one` / pagination semantics
- Transaction isolation

**In-memory adapter is the reference implementation.**

#### Ring 3: System/Usage Tests
Location: `test/system/`

Example domain (e.g., "blog" with users, posts, comments):
- Full CRUD flows
- Multi-step transactions
- Error scenarios

### Test Commands

```bash
# Run all tests
gleam test

# Run specific ring
gleam test test/unit
gleam test test/adapter
gleam test test/system

# Run with coverage (when available)
gleam test --coverage
```

## Verification Methods

### 1. Adapter Contract Specification
Maintain `docs/ADAPTER_CONTRACT.md` documenting:
- Required operations and their semantics
- Edge cases and expected behaviors
- Error forms and when they occur
- Performance expectations

### 2. Compatibility Matrix
Maintain `docs/COMPATIBILITY.md`:
| Feature | Memory | Postgres | (Future) |
|---------|--------|----------|----------|
| Basic CRUD | ✓ | ✓ | |
| Transactions | ✓ | ✓ | |
| Associations | ✓ | ✓ | |
| Migrations | N/A | ✓ | |

### 3. Query Compilation Tests
For each query feature:
- Test AST generation (pure)
- Test compilation to adapter format
- Snapshot tests for regression detection

### 4. Schema Evolution Tests
- Schema metadata is versioned
- Older serialized metadata can be loaded
- Schema diffs can be computed

## Development Workflow

### Adding a New Feature

1. **Identify the layer** — Which layer does this belong to?
2. **Write unit tests first** — Pure functions, no I/O
3. **Implement in isolation** — No dependencies on other layers if possible
4. **Add adapter contract tests** — If it affects persistence
5. **Add system test** — Demonstrate usage in example domain
6. **Add doc tests** — Every public function needs an example

### Code Review Checklist

- [ ] Feature belongs to exactly one layer
- [ ] No I/O outside repo/adapter layer
- [ ] Unit tests for pure logic
- [ ] Adapter tests if persistence-related
- [ ] Doc tests for public API
- [ ] No backwards-incompatible changes without deprecation

## CI Requirements

- All tests pass
- Code formatted (`gleam format --check`)
- No compiler warnings
- Coverage thresholds met (when tooling available):
  - Schema layer: 90%+
  - Changeset layer: 90%+
  - Query layer: 85%+
  - Repo/Adapter: 80%+

## Public API Definition

The public API is fully documented in [docs/API_STABILITY.md](docs/API_STABILITY.md).

### API Stability Tiers

**Tier 1: Stable Core** (Highest stability guarantees)
| Module | Purpose | Function Count |
|--------|---------|----------------|
| `cquill/schema` | Schema definitions and metadata | ~58 functions |
| `cquill/schema/field` | Field types, constraints, modifiers | ~52 functions |
| `cquill/query` | Query builder DSL | ~50 functions |
| `cquill/repo` | Repository operations | ~80 functions |
| `cquill/error` | Error types and classification | ~30 functions |

**Tier 2: Stable Adapters**
| Module | Purpose | Function Count |
|--------|---------|----------------|
| `cquill/adapter` | Adapter protocol and base types | ~30 functions |
| `cquill/adapter/postgres` | PostgreSQL adapter | ~50 functions |
| `cquill/adapter/memory` | In-memory reference adapter | ~70 functions |

**Tier 3: Supporting Modules**
| Module | Purpose | Function Count |
|--------|---------|----------------|
| `cquill/query/ast` | Query AST types | ~20 functions |
| `cquill/query/builder` | Advanced query composition | ~30 functions |
| `cquill/changeset` | Validation (minimal) | ~10 functions |
| `cquill/testing` | Test utilities | ~20 functions |

**Not Public API** (may change without notice):
- `cquill/cli/*` - CLI implementation
- `cquill/codegen/*` - Code generation internals
- `cquill/dev` - Development mode
- `cquill/telemetry` - Telemetry internals
- `cquill/introspection` - Database introspection
- Functions prefixed with `internal_`

### API Design Principles

1. **Pipeline-friendly**: Primary data structure is always the first parameter
2. **Result types for I/O**: All database operations return `Result`
3. **Option for missing values**: `get_field()`, `one()` return `Option`
4. **Specific error types**: Errors are typed variants, not strings
5. **Concise and focused**: Idiomatic Gleam packages have a small API with clear entry-points
6. **Leverage type inference**: Use generic functions over type-specific variants

### API Design Anti-Patterns to Avoid

The following patterns lead to bloated, unfocused APIs:

1. **Type-specific helper functions**: Avoid creating `eq_int()`, `eq_string()`, `eq_bool()` when a generic `eq()` works via type inference. Gleam's type system handles this automatically.

2. **Generated helper functions**: Don't auto-generate convenience functions for every combination of types/operations. This creates API surface that is hard to understand and maintain.

3. **Internal value constructors as public API**: Functions like `int_value()`, `string_value()` should be internal unless they provide user-facing value.

4. **Dev/bench code in src/**: Keep benchmarks, development utilities, and internal tooling outside the published `src/` directory. They bloat the published package and confuse users about the public API.

**Prefer this:**
```gleam
// One generic function using type inference
query.eq("active", True)
query.eq("age", 25)
query.eq("name", "Alice")
```

**Over this (anti-pattern):**
```gleam
// Multiple redundant type-specific functions
query.eq_bool("active", True)
query.eq_int("age", 25)
query.eq_string("name", "Alice")
```

### Known API Considerations

1. **Memory adapter returns tuples**: Because `MemoryStore` is immutable, operations return `#(MemoryStore, result)`. This is the correct Gleam pattern but differs from traditional mutable adapters.

2. **Savepoint API**: `repo.savepoint()` requires adapter-specific calls because savepoint semantics vary between adapters. Use `postgres.execute_savepoint()` or `memory.execute_savepoint()` directly.

3. **Query type parameter**: `Query(schema)` carries schema type for compile-time safety. The schema is not required at runtime - `Query(Nil)` works for untyped queries.

4. **Common pattern functions**: `builder.not_deleted()`, `builder.published()`, etc. assume specific field names (`deleted_at`, `published`, `active`). Document or customize for your domain.

## Backwards Compatibility

- Public API changes require deprecation period
- Schema metadata format changes require migration helpers
- Query AST changes require changelog entries
- Every breaking change needs upgrade documentation

## Example Domain for Testing

Use a "blog" domain throughout tests and docs:

```gleam
// User schema
pub type User {
  User(id: Int, email: String, name: Option(String), inserted_at: Time)
}

// Post schema
pub type Post {
  Post(id: Int, user_id: Int, title: String, body: String, published: Bool)
}

// Comment schema
pub type Comment {
  Comment(id: Int, post_id: Int, author: String, content: String)
}
```

This domain is used in:
- All doc examples
- System tests
- README snippets

## Key Ecto Insights to Emulate

1. **Queries are data** — Composable, inspectable, not just strings
2. **Multiple schemas per domain** — Read/write/form models
3. **Explicit changesets** — Validation separate from persistence
4. **Sandbox testing** — Isolated, parallelizable tests
5. **Adapter abstraction** — Same API, different backends

## Open Source Library Standards

### Semantic Versioning

cquill follows [Semantic Versioning 2.0.0](https://semver.org/):

- **MAJOR (X.0.0)**: Breaking API changes
- **MINOR (0.X.0)**: New features, backwards compatible
- **PATCH (0.0.X)**: Bug fixes, backwards compatible

**Pre-1.0 Policy:**
- `0.x.y` releases may have breaking changes in minor versions
- Document all breaking changes in CHANGELOG.md
- Provide migration guides for breaking changes

**Post-1.0 Policy:**
- Breaking changes require major version bump
- Deprecate before removing (minimum one minor version)
- Security fixes may bypass deprecation cycle

### Release Process

See [docs/API_STABILITY.md](docs/API_STABILITY.md) for the complete release process.

#### Pre-Release Checklist
1. All tests pass (`gleam test`)
2. Code formatted (`gleam format --check`)
3. No compiler warnings
4. CHANGELOG.md updated with all changes
5. Documentation updated (`gleam docs build`)
6. Version bumped in `gleam.toml`
7. README.md examples verified working

### Issue and PR Guidelines

#### Issue Labels
- `bug` - Something isn't working
- `enhancement` - New feature or improvement
- `documentation` - Documentation improvements
- `breaking-change` - Will require major version bump
- `good-first-issue` - Good for newcomers
- `v1.0` - Issues targeted for v1.0.0 release

#### PR Requirements
1. Linked to an issue (use "Closes #123" or "Fixes #123")
2. All CI checks pass
3. Tests added for new functionality
4. Documentation updated if needed
5. CHANGELOG.md entry added

#### Commit Message Format
```
type(scope): short description

Longer description if needed.

Closes #123
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

### Deprecation Policy

When deprecating functionality:

1. Add `@deprecated` annotation with version and alternative
2. Log warning when deprecated function is called
3. Document in CHANGELOG.md
4. Keep deprecated code for at least one minor version
5. Remove in next major version with migration guide

## References

- [Ecto documentation](https://hexdocs.pm/ecto/)
- [How Ecto promotes well-designed applications](https://www.mojotech.com/blog/how-elixir-ecto-promotes-well-designed-applications/)
- [Understanding Ecto associations](https://blog.appsignal.com/2020/11/10/understanding-associations-in-elixir-ecto.html)
- [Semantic Versioning](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [API Stability Guide](docs/API_STABILITY.md)
