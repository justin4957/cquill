# Database Migrations with cquill

This guide explains how to use cquill with external migration tools for schema management.

## Philosophy

cquill focuses on **runtime data access**, not schema evolution. Database migrations are better handled by dedicated tools for several reasons:

- **Separation of concerns**: Schema evolution is a deployment concern, not a runtime concern
- **Language agnostic**: SQL migrations work regardless of application language
- **Proven tools**: dbmate, sqitch, flyway are battle-tested with large communities
- **Simplicity**: Don't reinvent what others do well
- **Team familiarity**: DBAs and ops teams often prefer standard SQL migration tools

## Recommended Tools

### dbmate (Recommended)

[dbmate](https://github.com/amacneil/dbmate) is a lightweight, database-agnostic migration tool that uses plain SQL files.

**Why we recommend dbmate:**
- Simple SQL-based migrations
- Supports PostgreSQL, MySQL, SQLite, and more
- No runtime dependencies
- Easy to install and use
- Great for teams new to migrations

#### Installation

```bash
# macOS
brew install dbmate

# Linux
curl -fsSL -o /usr/local/bin/dbmate \
  https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64
chmod +x /usr/local/bin/dbmate

# Using Go
go install github.com/amacneil/dbmate/v2@latest
```

#### Basic Usage

```bash
# Set database URL
export DATABASE_URL="postgres://user:pass@localhost:5432/myapp?sslmode=disable"

# Create a new migration
dbmate new add_users_table
# Creates: db/migrations/20240115120000_add_users_table.sql

# Apply all pending migrations
dbmate up

# Rollback the last migration
dbmate down

# Check migration status
dbmate status

# Create database (if it doesn't exist)
dbmate create

# Drop database
dbmate drop
```

#### Migration File Format

```sql
-- db/migrations/20240115120000_add_users_table.sql

-- migrate:up
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  name VARCHAR(255),
  inserted_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_users_email ON users(email);

-- migrate:down
DROP INDEX IF EXISTS idx_users_email;
DROP TABLE IF EXISTS users;
```

### sqitch

[sqitch](https://sqitch.org/) is a sophisticated, dependency-aware database change management tool.

**Best for:**
- Complex dependency chains between migrations
- Need for detailed change tracking
- Teams with advanced database needs

#### Installation

```bash
# macOS
brew install sqitch --with-postgres-support

# Linux (Debian/Ubuntu)
apt-get install sqitch libdbd-pg-perl
```

#### Basic Usage

```bash
# Initialize project
sqitch init myapp --engine pg --top-dir db

# Add a change
sqitch add users -n "Add users table"
# Creates: db/deploy/users.sql, db/revert/users.sql, db/verify/users.sql

# Deploy changes
sqitch deploy

# Revert last change
sqitch revert --to @HEAD^

# Verify changes
sqitch verify

# Show status
sqitch status
```

#### Migration Files (sqitch uses separate files)

```sql
-- db/deploy/users.sql
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  name VARCHAR(255),
  inserted_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- db/revert/users.sql
DROP TABLE IF EXISTS users;

-- db/verify/users.sql
SELECT id, email, name, inserted_at FROM users WHERE FALSE;
```

### flyway

[Flyway](https://flywaydb.org/) is an enterprise-grade migration tool, popular in Java ecosystems.

**Best for:**
- Enterprise environments
- Java/JVM-based teams
- Need for commercial support

#### Installation

```bash
# macOS
brew install flyway

# Using Docker
docker run -v $(pwd)/sql:/flyway/sql flyway/flyway migrate
```

#### Basic Usage

```bash
# Run migrations
flyway -url=jdbc:postgresql://localhost/myapp migrate

# Get info
flyway info

# Validate migrations
flyway validate

# Repair metadata
flyway repair
```

#### Migration File Format

```sql
-- sql/V1__Add_users_table.sql
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  name VARCHAR(255)
);

-- sql/V2__Add_posts_table.sql
CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  user_id INTEGER REFERENCES users(id),
  title VARCHAR(255) NOT NULL,
  body TEXT
);
```

## Integration with cquill

### Development Workflow

The recommended workflow integrates migrations with cquill's code generation:

```bash
#!/bin/bash
# scripts/add-migration.sh

set -e

# 1. Create the migration
dbmate new "$1"

echo "Migration created! Next steps:"
echo "1. Edit the migration file in db/migrations/"
echo "2. Run: make db-migrate"
echo "3. Commit both the migration and generated code"
```

```bash
#!/bin/bash
# scripts/migrate.sh

set -e

# Apply migrations
dbmate up

# Regenerate cquill types
gleam run -m cquill_cli generate --database-url "$DATABASE_URL"

echo "Migration complete! Generated code updated."
```

### Makefile Integration

```makefile
# Makefile

.PHONY: db-create db-migrate db-rollback db-generate db-reset db-status

# Database URL for development
DATABASE_URL ?= postgres://localhost:5432/myapp_dev?sslmode=disable

# Create database
db-create:
	DATABASE_URL=$(DATABASE_URL) dbmate create

# Apply migrations and regenerate code
db-migrate:
	DATABASE_URL=$(DATABASE_URL) dbmate up
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate

# Rollback last migration and regenerate code
db-rollback:
	DATABASE_URL=$(DATABASE_URL) dbmate down
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate

# Just regenerate code (no migrations)
db-generate:
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate

# Reset database (drop, create, migrate)
db-reset:
	DATABASE_URL=$(DATABASE_URL) dbmate drop || true
	DATABASE_URL=$(DATABASE_URL) dbmate create
	DATABASE_URL=$(DATABASE_URL) dbmate up
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate

# Check migration status
db-status:
	DATABASE_URL=$(DATABASE_URL) dbmate status

# Create a new migration
db-new:
	@read -p "Migration name: " name; \
	DATABASE_URL=$(DATABASE_URL) dbmate new $$name
```

### CI/CD Integration

#### GitHub Actions

```yaml
# .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_USER: postgres
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: myapp_test
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v4

      - name: Install dbmate
        run: |
          curl -fsSL -o /usr/local/bin/dbmate \
            https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64
          chmod +x /usr/local/bin/dbmate

      - name: Run migrations
        run: dbmate up
        env:
          DATABASE_URL: postgres://postgres:postgres@localhost:5432/myapp_test?sslmode=disable

      - name: Setup Gleam
        uses: erlef/setup-beam@v1
        with:
          otp-version: "26"
          gleam-version: "1.4"

      - name: Verify generated code is up to date
        run: |
          gleam run -m cquill_cli generate
          git diff --exit-code src/db/
        env:
          DATABASE_URL: postgres://postgres:postgres@localhost:5432/myapp_test?sslmode=disable

      - name: Run tests
        run: gleam test
        env:
          DATABASE_URL: postgres://postgres:postgres@localhost:5432/myapp_test?sslmode=disable
```

#### Production Deployment

```yaml
# .github/workflows/deploy.yml
name: Deploy

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install dbmate
        run: |
          curl -fsSL -o /usr/local/bin/dbmate \
            https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64
          chmod +x /usr/local/bin/dbmate

      - name: Run migrations
        run: dbmate up
        env:
          DATABASE_URL: ${{ secrets.DATABASE_URL }}

      # Deploy your application...
      - name: Deploy application
        run: ./scripts/deploy.sh
```

## Schema Drift Detection

Ensure your generated code always matches your database schema:

### CI Check

Add this step to your CI workflow:

```yaml
- name: Check for schema drift
  run: |
    # Regenerate code from current database
    gleam run -m cquill_cli generate

    # Fail if there are any differences
    if ! git diff --exit-code src/db/; then
      echo "ERROR: Generated code is out of sync with database schema!"
      echo "Run 'make db-generate' locally and commit the changes."
      exit 1
    fi
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Check if any migration files are staged
if git diff --cached --name-only | grep -q "db/migrations/"; then
  echo "Migration files detected. Checking generated code..."

  # Regenerate code
  gleam run -m cquill_cli generate 2>/dev/null

  # Check for unstaged changes in generated code
  if ! git diff --exit-code src/db/; then
    echo ""
    echo "ERROR: Generated code needs to be updated!"
    echo "Run 'make db-migrate' and stage the changes."
    exit 1
  fi
fi
```

## Project Structure

Recommended project layout:

```
myapp/
├── db/
│   ├── migrations/                    # Migration files
│   │   ├── 20240115120000_add_users_table.sql
│   │   ├── 20240115130000_add_posts_table.sql
│   │   └── 20240116100000_add_comments_table.sql
│   └── seeds/                         # Seed data (optional)
│       ├── development.sql
│       └── test.sql
├── src/
│   ├── db/                            # Generated cquill code
│   │   ├── schema.gleam
│   │   ├── users.gleam
│   │   ├── posts.gleam
│   │   └── comments.gleam
│   └── myapp/
│       └── ...
├── test/
│   └── ...
├── .env                               # Local DATABASE_URL (gitignored)
├── .env.example                       # Example environment file
├── Makefile
└── gleam.toml
```

## Environment Configuration

### Development

```bash
# .env (gitignored)
DATABASE_URL=postgres://localhost:5432/myapp_dev?sslmode=disable
```

```bash
# .env.example (committed)
DATABASE_URL=postgres://localhost:5432/myapp_dev?sslmode=disable
```

### Multiple Environments

```bash
# Load environment-specific config
source .env.${ENV:-development}
dbmate up
```

Or use separate database URLs:

```bash
# Development
DATABASE_URL=postgres://localhost:5432/myapp_dev

# Test
DATABASE_URL=postgres://localhost:5432/myapp_test

# Production
DATABASE_URL=postgres://user:pass@prod-db.example.com:5432/myapp
```

## Best Practices

### Migration Guidelines

1. **Always include down migrations**: Every `up` should have a corresponding `down`
2. **Make migrations idempotent**: Use `IF EXISTS` / `IF NOT EXISTS`
3. **Keep migrations small**: One logical change per migration
4. **Never modify existing migrations**: Create new migrations for changes
5. **Test rollbacks**: Always test that `down` migrations work

### Example: Safe Column Addition

```sql
-- migrate:up
ALTER TABLE users ADD COLUMN IF NOT EXISTS avatar_url VARCHAR(500);

-- migrate:down
ALTER TABLE users DROP COLUMN IF EXISTS avatar_url;
```

### Example: Safe Index Creation

```sql
-- migrate:up
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_posts_user_id ON posts(user_id);

-- migrate:down
DROP INDEX CONCURRENTLY IF EXISTS idx_posts_user_id;
```

### Example: Data Migration

```sql
-- migrate:up
-- Add new column
ALTER TABLE users ADD COLUMN full_name VARCHAR(255);

-- Migrate data
UPDATE users SET full_name = CONCAT(first_name, ' ', last_name);

-- Add constraint after data is migrated
ALTER TABLE users ALTER COLUMN full_name SET NOT NULL;

-- migrate:down
ALTER TABLE users DROP COLUMN full_name;
```

## Troubleshooting

### Common Issues

**Migration fails with "relation already exists"**
- Check if migration was partially applied
- Use `IF NOT EXISTS` in your DDL statements
- Consider using `dbmate status` to check state

**Generated code doesn't match schema**
- Run `make db-generate` to regenerate
- Ensure DATABASE_URL points to the correct database
- Check that all migrations have been applied

**CI fails with schema drift**
- Run migrations and regenerate code locally
- Commit both migration and generated code together
- Ensure CI uses the same database version as development

### Recovery Commands

```bash
# Force mark a migration as applied (use carefully!)
dbmate rollback  # if stuck
dbmate up        # reapply

# Check which migrations have been applied
dbmate status

# Reset everything (DESTROYS DATA)
dbmate drop
dbmate create
dbmate up
```

## Further Reading

- [dbmate documentation](https://github.com/amacneil/dbmate)
- [sqitch documentation](https://sqitch.org/docs/)
- [flyway documentation](https://flywaydb.org/documentation/)
- [PostgreSQL ALTER TABLE](https://www.postgresql.org/docs/current/sql-altertable.html)
- [Zero-downtime migrations](https://blog.2ndquadrant.com/schema-changes-in-production/)
