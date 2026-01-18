# dbmate Example Setup

This example shows how to use [dbmate](https://github.com/amacneil/dbmate) with cquill for database migrations.

## Prerequisites

1. Install dbmate:
   ```bash
   # macOS
   brew install dbmate

   # Linux
   curl -fsSL -o /usr/local/bin/dbmate \
     https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64
   chmod +x /usr/local/bin/dbmate
   ```

2. Set up your database URL:
   ```bash
   export DATABASE_URL="postgres://localhost:5432/myapp_dev?sslmode=disable"
   ```

## Directory Structure

```
myapp/
├── db/
│   └── migrations/
│       ├── 20240115120000_create_users.sql
│       ├── 20240115130000_create_posts.sql
│       └── 20240116100000_create_comments.sql
├── src/
│   └── db/           # Generated cquill code
├── Makefile
└── .env
```

## Quick Start

```bash
# Create database
dbmate create

# Run all migrations
dbmate up

# Regenerate cquill code
gleam run -m cquill_cli generate

# Check status
dbmate status
```

## Example Migrations

See the `migrations/` directory for example SQL files:

- `20240115120000_create_users.sql` - Users table with email and profile
- `20240115130000_create_posts.sql` - Posts table with foreign key to users
- `20240116100000_create_comments.sql` - Comments with foreign keys

## Makefile Commands

Copy this to your project's Makefile:

```makefile
DATABASE_URL ?= postgres://localhost:5432/myapp_dev?sslmode=disable

db-create:
	DATABASE_URL=$(DATABASE_URL) dbmate create

db-migrate:
	DATABASE_URL=$(DATABASE_URL) dbmate up
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate

db-rollback:
	DATABASE_URL=$(DATABASE_URL) dbmate down
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate

db-status:
	DATABASE_URL=$(DATABASE_URL) dbmate status

db-reset:
	DATABASE_URL=$(DATABASE_URL) dbmate drop || true
	DATABASE_URL=$(DATABASE_URL) dbmate create
	DATABASE_URL=$(DATABASE_URL) dbmate up
	DATABASE_URL=$(DATABASE_URL) gleam run -m cquill_cli generate
```

## Workflow

1. Create a new migration:
   ```bash
   dbmate new add_user_roles
   ```

2. Edit the generated file:
   ```sql
   -- migrate:up
   ALTER TABLE users ADD COLUMN role VARCHAR(50) DEFAULT 'user';

   -- migrate:down
   ALTER TABLE users DROP COLUMN role;
   ```

3. Apply migration:
   ```bash
   make db-migrate
   ```

4. Commit both migration and generated code:
   ```bash
   git add db/migrations/ src/db/
   git commit -m "Add user roles"
   ```

## Tips

- Always include `-- migrate:down` sections
- Use `IF EXISTS` / `IF NOT EXISTS` for idempotency
- Keep migrations small and focused
- Never modify existing migrations after they're deployed
