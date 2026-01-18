# sqitch Example Setup

This example shows how to use [sqitch](https://sqitch.org/) with cquill for database migrations.

## Prerequisites

1. Install sqitch:
   ```bash
   # macOS
   brew install sqitch --with-postgres-support

   # Linux (Debian/Ubuntu)
   apt-get install sqitch libdbd-pg-perl

   # Docker
   docker pull sqitch/sqitch
   ```

2. Set up your database:
   ```bash
   createdb myapp_dev
   ```

## Directory Structure

sqitch uses a more structured layout with separate directories for deploy, revert, and verify scripts:

```
myapp/
├── db/
│   ├── deploy/          # Forward migrations
│   │   ├── users.sql
│   │   ├── posts.sql
│   │   └── comments.sql
│   ├── revert/          # Rollback scripts
│   │   ├── users.sql
│   │   ├── posts.sql
│   │   └── comments.sql
│   ├── verify/          # Verification queries
│   │   ├── users.sql
│   │   ├── posts.sql
│   │   └── comments.sql
│   └── sqitch.plan      # Migration plan
├── src/
│   └── db/              # Generated cquill code
├── sqitch.conf
└── Makefile
```

## Quick Start

```bash
# Initialize sqitch in your project
sqitch init myapp --engine pg --top-dir db

# Add a change
sqitch add users -n "Add users table"

# Edit the generated files in db/deploy/, db/revert/, db/verify/

# Deploy to database
sqitch deploy db:pg:myapp_dev

# Check status
sqitch status db:pg:myapp_dev

# Regenerate cquill code
gleam run -m cquill_cli generate
```

## Example Files

### sqitch.conf

```ini
[core]
    engine = pg
    top_dir = db

[engine "pg"]
    target = db:pg:myapp_dev

[deploy]
    verify = true

[rebase]
    verify = true
```

### sqitch.plan

```
%syntax-version=1.0.0
%project=myapp

users 2024-01-15T12:00:00Z Your Name <you@example.com> # Add users table
posts [users] 2024-01-15T13:00:00Z Your Name <you@example.com> # Add posts table
comments [posts users] 2024-01-16T10:00:00Z Your Name <you@example.com> # Add comments table
```

### Deploy Scripts

See the example files in this directory:
- `deploy/users.sql`
- `deploy/posts.sql`
- `deploy/comments.sql`

### Revert Scripts

- `revert/users.sql`
- `revert/posts.sql`
- `revert/comments.sql`

### Verify Scripts

- `verify/users.sql`
- `verify/posts.sql`
- `verify/comments.sql`

## Makefile Commands

```makefile
DATABASE_URL ?= db:pg:myapp_dev

db-deploy:
	sqitch deploy $(DATABASE_URL)
	gleam run -m cquill_cli generate

db-revert:
	sqitch revert $(DATABASE_URL) --to @HEAD^
	gleam run -m cquill_cli generate

db-verify:
	sqitch verify $(DATABASE_URL)

db-status:
	sqitch status $(DATABASE_URL)

db-log:
	sqitch log $(DATABASE_URL)
```

## Key Differences from dbmate

| Feature | dbmate | sqitch |
|---------|--------|--------|
| Migration format | Single file with up/down | Separate deploy/revert/verify files |
| Dependencies | Timestamp-based | Explicit in plan file |
| Verification | Manual | Built-in verify step |
| Complexity | Simple | More sophisticated |
| Best for | Simple projects | Complex dependency chains |

## When to Use sqitch

Choose sqitch when you need:
- Explicit dependencies between migrations
- Built-in verification of changes
- Detailed change history and logging
- Complex database schemas with interdependencies

## Workflow

1. Add a new change:
   ```bash
   sqitch add user_roles --requires users -n "Add role column to users"
   ```

2. Edit the generated files:
   - `db/deploy/user_roles.sql` - The forward migration
   - `db/revert/user_roles.sql` - The rollback
   - `db/verify/user_roles.sql` - Verification query

3. Deploy:
   ```bash
   sqitch deploy
   ```

4. Verify:
   ```bash
   sqitch verify
   ```

5. Regenerate cquill code and commit:
   ```bash
   gleam run -m cquill_cli generate
   git add db/ src/db/
   git commit -m "Add user roles"
   ```

## Tips

- Use `--requires` to specify dependencies when adding changes
- Always write verification queries that will fail if the change wasn't applied
- Use `sqitch log` to see full history of deployed changes
- Run `sqitch verify` after every deploy to ensure consistency
