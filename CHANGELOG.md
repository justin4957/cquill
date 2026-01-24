# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Nothing yet

### Changed
- Nothing yet

### Fixed
- Nothing yet

## [0.1.1] - 2026-01-23

### Added

#### Testing Infrastructure
- Comprehensive concurrent access and race condition tests for memory adapter
- Property-based tests for query builder using randomized inputs
- Complete adapter contract test coverage ensuring all adapters behave consistently
- Security audit tests and documentation for vulnerability assessment
- Performance benchmarks for query builder, schema operations, and memory adapter
- PostgreSQL integration tests in CI pipeline

#### Developer Experience
- Improved error messages with actionable hints and context
- Multi-platform CLI builds (macOS, Linux) for easier installation
- Positive assertion test helpers to improve test readability

### Changed

#### Code Quality
- Replaced custom helper functions with Gleam stdlib equivalents (`list.length`, `int.to_string`)
- Flattened nested case expressions for improved readability
- Used `int.to_string()` instead of `string.inspect()` for integers

#### Documentation
- Fixed code examples in documentation to match actual API
- Documented intentional result discarding patterns
- Comprehensive API stability review for v1.0 preparation

### Fixed
- Removed all compiler warnings (138 warnings eliminated)

## [0.1.0] - 2026-01-19

### Added

#### Core Features
- **Schema Layer** - Define table structure with fields, types, and constraints
  - Primary key support (single and composite)
  - Constraints: unique, check, foreign key, not null
  - Multiple schemas per domain pattern
  - Schema metadata for code generation

- **Query Builder** - Type-safe AST-based query construction
  - `SELECT`, `WHERE`, `ORDER BY`, `LIMIT`, `OFFSET`
  - Conditions: `eq`, `gt`, `gte`, `lt`, `lte`, `like`, `ilike`, `is_null`, `is_not_null`, `between`, `in_list`
  - Logical operators: `and`, `or`, `not`
  - Phantom-typed queries for compile-time column validation
  - Joins: INNER, LEFT, RIGHT, FULL, CROSS
  - Distinct, group by, having
  - Subqueries
  - Raw SQL escape hatch for complex queries

- **Changeset/Validation** - Pure validation functions for data transformation

- **Repo API** - Unified interface for database operations
  - Adapter-agnostic error types
  - CRUD convenience functions
  - Query compilation and execution

#### Adapters
- **Memory Adapter** - Reference in-memory implementation
  - Full constraint support (unique, not-null, foreign key)
  - Transaction support with rollback
  - Savepoints for nested transactions
  - Batch operations
  - Auto-increment IDs

- **PostgreSQL Adapter** - Production-ready Postgres support
  - Full integration with pog library
  - Connection pooling
  - Transaction support
  - RETURNING clause support
  - Error mapping to cquill error types

#### Code Generation
- CLI tool: `gleam run -m cquill generate`
- Watch mode for development
- Generates Gleam types from database schemas
- Typed column generation with phantom types
- Database introspection

#### Developer Experience
- Rich error messages with actionable hints
- Development mode with enhanced debugging
- Telemetry and logging hooks
- Slow query warnings
- Testing utilities module

### Documentation
- Comprehensive getting started guide
- Concept guides (schemas, queries, adapters, repo)
- How-to guides (CRUD, transactions, testing)
- Recipes (pagination, soft delete, multi-tenancy, audit logging)
- API reference documentation
- Migration tool integration guides (dbmate, sqitch, flyway)
- Architecture decision records

### Infrastructure
- GitHub Actions CI/CD pipeline
- Multi-version compatibility testing (Gleam 1.14+, OTP 26-28)
- Automated documentation generation
- Release automation with Hex.pm publishing

[Unreleased]: https://github.com/justin4957/cquill/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/justin4957/cquill/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/justin4957/cquill/releases/tag/v0.1.0
