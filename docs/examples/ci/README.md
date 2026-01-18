# CI Workflow Examples

This directory contains example CI/CD workflow configurations for using cquill with database migrations.

## GitHub Actions

### test.yml

Complete test workflow with:
- PostgreSQL service container
- dbmate migration runner
- Schema drift detection
- Gleam test runner

### deploy.yml

Production deployment workflow with:
- Migration execution before deploy
- Safe deployment practices

## Usage

Copy the relevant workflow files to your `.github/workflows/` directory and customize:

1. Update database credentials
2. Adjust Gleam/OTP versions
3. Modify deployment commands
4. Add your specific test commands

## Key Features

### Schema Drift Detection

The test workflow includes a check that regenerates cquill code and fails if there are differences:

```yaml
- name: Check for schema drift
  run: |
    gleam run -m cquill_cli generate
    git diff --exit-code src/db/
```

This ensures generated code is always committed alongside migrations.

### Database Service

Uses PostgreSQL service container for isolated testing:

```yaml
services:
  postgres:
    image: postgres:15
    env:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: myapp_test
```

### Migration Caching

For faster CI runs, consider caching dbmate:

```yaml
- name: Cache dbmate
  uses: actions/cache@v3
  with:
    path: /usr/local/bin/dbmate
    key: dbmate-2.0.0
```
