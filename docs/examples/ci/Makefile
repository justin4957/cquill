# Example Makefile for cquill projects with database migrations
# Copy this to your project root and customize as needed

# Default database URL for local development
# Override with: make db-migrate DATABASE_URL=postgres://...
DATABASE_URL ?= postgres://localhost:5432/myapp_dev?sslmode=disable

# Test database URL
TEST_DATABASE_URL ?= postgres://localhost:5432/myapp_test?sslmode=disable

.PHONY: help deps build test fmt check
.PHONY: db-create db-drop db-migrate db-rollback db-reset db-status db-generate db-new
.PHONY: ci-test ci-lint

# =============================================================================
# Help
# =============================================================================

help:
	@echo "cquill Project Makefile"
	@echo ""
	@echo "Development:"
	@echo "  deps          Download dependencies"
	@echo "  build         Build the project"
	@echo "  test          Run tests"
	@echo "  fmt           Format code"
	@echo "  check         Run all checks (format, build, test)"
	@echo ""
	@echo "Database:"
	@echo "  db-create     Create database"
	@echo "  db-drop       Drop database"
	@echo "  db-migrate    Run pending migrations and regenerate code"
	@echo "  db-rollback   Rollback last migration and regenerate code"
	@echo "  db-reset      Drop, create, migrate, and regenerate"
	@echo "  db-status     Show migration status"
	@echo "  db-generate   Regenerate cquill code from database"
	@echo "  db-new        Create a new migration (prompts for name)"
	@echo ""
	@echo "CI:"
	@echo "  ci-test       Run tests in CI mode"
	@echo "  ci-lint       Run linting checks"
	@echo ""
	@echo "Options:"
	@echo "  DATABASE_URL  Database connection URL (default: local dev)"

# =============================================================================
# Development Commands
# =============================================================================

deps:
	gleam deps download

build: deps
	gleam build

test: deps
	DATABASE_URL=$(TEST_DATABASE_URL) gleam test

fmt:
	gleam format

check: fmt build test
	@echo "All checks passed!"

# =============================================================================
# Database Commands
# =============================================================================

db-create:
	DATABASE_URL=$(DATABASE_URL) dbmate create

db-drop:
	DATABASE_URL=$(DATABASE_URL) dbmate drop

db-migrate:
	DATABASE_URL=$(DATABASE_URL) dbmate up
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate
	@echo ""
	@echo "Migration complete! Generated code updated."
	@echo "Don't forget to commit both the migration and generated code."

db-rollback:
	DATABASE_URL=$(DATABASE_URL) dbmate down
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate
	@echo ""
	@echo "Rollback complete! Generated code updated."

db-reset:
	DATABASE_URL=$(DATABASE_URL) dbmate drop || true
	DATABASE_URL=$(DATABASE_URL) dbmate create
	DATABASE_URL=$(DATABASE_URL) dbmate up
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate
	@echo ""
	@echo "Database reset complete!"

db-status:
	DATABASE_URL=$(DATABASE_URL) dbmate status

db-generate:
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate
	@echo "Generated code updated from database schema."

db-new:
	@read -p "Migration name (e.g., add_users_table): " name; \
	DATABASE_URL=$(DATABASE_URL) dbmate new $$name; \
	echo ""; \
	echo "Migration created! Next steps:"; \
	echo "1. Edit the new migration file in db/migrations/"; \
	echo "2. Run: make db-migrate"

# =============================================================================
# Test Database Commands
# =============================================================================

test-db-create:
	DATABASE_URL=$(TEST_DATABASE_URL) dbmate create

test-db-migrate:
	DATABASE_URL=$(TEST_DATABASE_URL) dbmate up

test-db-reset:
	DATABASE_URL=$(TEST_DATABASE_URL) dbmate drop || true
	DATABASE_URL=$(TEST_DATABASE_URL) dbmate create
	DATABASE_URL=$(TEST_DATABASE_URL) dbmate up

# =============================================================================
# CI Commands
# =============================================================================

ci-test:
	gleam format --check
	gleam build
	gleam test

ci-lint:
	gleam format --check

# Check for schema drift in CI
ci-schema-check:
	gleam run -m cquill_cli generate
	@if ! git diff --exit-code src/db/; then \
		echo ""; \
		echo "ERROR: Generated code is out of sync!"; \
		echo "Run 'make db-generate' and commit the changes."; \
		exit 1; \
	fi
	@echo "Schema check passed!"

# =============================================================================
# Docker Commands (optional)
# =============================================================================

docker-db-start:
	docker run -d --name myapp-postgres \
		-e POSTGRES_USER=postgres \
		-e POSTGRES_PASSWORD=postgres \
		-e POSTGRES_DB=myapp_dev \
		-p 5432:5432 \
		postgres:15

docker-db-stop:
	docker stop myapp-postgres
	docker rm myapp-postgres

# Start fresh database in Docker
docker-db-reset: docker-db-stop docker-db-start
	@echo "Waiting for PostgreSQL to be ready..."
	@sleep 3
	$(MAKE) db-migrate
