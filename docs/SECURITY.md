# Security Documentation

This document describes cquill's security architecture, best practices, and guidelines for secure database operations.

## Overview

cquill is designed with security as a core principle. The library provides multiple layers of protection against common database security vulnerabilities:

1. **Parameterized queries** - All user values are passed as parameters, never interpolated into SQL
2. **AST-based query building** - Queries are represented as AST structures, not strings
3. **Identifier escaping** - Table and column names are properly escaped
4. **Credential isolation** - Database credentials are never logged or exposed in errors

> **Note**: The query builder uses string-based field names and does not provide compile-time validation that field names exist in your schema. Runtime errors will occur for invalid field names.

## SQL Injection Prevention

### Parameterized Queries

cquill uses parameterized queries for all database operations. User-supplied values are never directly interpolated into SQL strings. Instead, values are:

1. Wrapped in typed `Value` variants (`StringValue`, `IntValue`, etc.)
2. Passed to the database driver as separate parameters
3. Escaped and quoted by the database driver itself

```gleam
// Safe - value is parameterized
let query =
  query.from(user_schema)
  |> query.where(query.eq("email", user_input))  // user_input becomes $1

// The generated SQL is:
// SELECT * FROM "users" WHERE "email" = $1
// Parameters: [user_input]
```

### Query AST Architecture

Queries are represented as Abstract Syntax Trees (AST), not SQL strings. This design:

- Prevents SQL injection at the structural level
- Makes queries inspectable and composable
- Separates query logic from SQL generation

```gleam
// Query is data, not a string
let query =
  query.from(user_schema)
  |> query.where(query.eq("name", "O'Brien"))  // Quotes handled safely
  |> query.where(query.like("email", "%@example.com"))

// Can inspect the query structure
let conditions = query.get_conditions(query)
```

### Identifier Escaping

Table names, column names, and other identifiers are escaped using PostgreSQL's double-quote escaping:

```gleam
// Internal function (not exposed publicly)
fn escape_identifier(name: String) -> String {
  "\"" <> string.replace(name, "\"", "\"\"") <> "\""
}

// "users" -> "\"users\""
// "user\"table" -> "\"user\"\"table\""  (quotes are doubled)
```

### Raw SQL Warning

The `Raw` condition type allows raw SQL for advanced use cases. **Use with extreme caution:**

```gleam
// DANGEROUS if sql contains user input
query.where(query.raw("custom_function(field) = value", []))

// SAFE - use parameters for user values
query.where(query.raw("custom_function(field) = $1", [user_value]))
```

**Best Practice:** Never construct raw SQL from user input. Always use the typed query builder functions.

## Credential Handling

### Configuration Security

Database credentials should be managed securely:

```gleam
// Load credentials from environment variables
let password = case os.get_env("DATABASE_PASSWORD") {
  Ok(p) -> Some(p)
  Error(_) -> None
}

let config = PostgresConfig(
  host: os.get_env("DATABASE_HOST") |> result.unwrap("localhost"),
  port: 5432,
  database: "myapp",
  user: os.get_env("DATABASE_USER") |> result.unwrap("postgres"),
  password: password,
  // ... other settings
)
```

### What Is NOT Logged

cquill is designed to never log or expose:

- Database passwords
- Connection strings containing credentials
- Full connection URLs

### Error Messages

Error messages are designed to be helpful for debugging without exposing sensitive information:

```gleam
// Error messages include:
// - Constraint names (e.g., "users_email_key")
// - Column names involved in errors
// - PostgreSQL error codes
// - General error descriptions

// Error messages do NOT include:
// - Database passwords
// - Full connection strings
// - Authentication tokens
```

## Connection Security

### SSL/TLS Configuration

Always use SSL in production:

```gleam
let config = PostgresConfig(
  // ... other settings
  ssl: pog.SslRequired,  // Require SSL connection
  // Or use pog.SslPrefer for compatibility
)
```

SSL modes:
- `SslDisabled` - No SSL (development only)
- `SslPrefer` - Use SSL if available
- `SslRequired` - Require SSL (recommended for production)

### Connection Timeouts

Configure appropriate timeouts to prevent resource exhaustion:

```gleam
let config = PostgresConfig(
  // ... other settings
  default_timeout: 30_000,  // 30 second query timeout
  idle_interval: 60_000,    // 60 second idle check
)
```

### Connection Pooling

Use connection pooling to manage database connections efficiently:

```gleam
let config = PostgresConfig(
  // ... other settings
  pool_size: 10,  // Adjust based on workload
)
```

## Telemetry and Logging

### What Telemetry Captures

The telemetry system captures:

- Query SQL text (with `$n` placeholders)
- Query parameters (as typed values)
- Execution timing
- Row counts
- Error information

### Security Considerations

**Query Parameters in Telemetry:**

Telemetry events include query parameters. If you implement a telemetry handler that logs to external systems, consider:

1. Filtering sensitive columns (password_hash, tokens, etc.)
2. Redacting parameter values in production logs
3. Using separate log levels for parameter data

```gleam
// Example: Filtering telemetry for sensitive data
pub fn secure_logger_handler() -> telemetry.Handler {
  fn(event, _metadata) {
    case event {
      telemetry.QueryStop(e) -> {
        // Log query but not parameters
        io.println("[query] " <> e.query <> " [" <> int.to_string(e.duration_us) <> "us]")
        // Don't log: e.params (may contain sensitive data)
      }
      _ -> Nil
    }
  }
}
```

### Built-in Handlers

The built-in `logger_handler()` and `debug_handler()` log query text but you should review them for your security requirements before use in production.

## Error Handling

### Information Leakage Prevention

Error messages are designed to help debugging without leaking sensitive information:

| Error Type | Includes | Excludes |
|------------|----------|----------|
| `ConnectionFailed` | Generic reason | Connection string, password |
| `QueryFailed` | Error message, code | Parameter values |
| `UniqueViolation` | Constraint name, detail | Connection info |
| `DecodeFailed` | Column, expected type | Actual values |

### Error Response Best Practices

When returning errors to API clients:

```gleam
// DON'T expose internal error details to clients
case result {
  Error(error.QueryFailed(msg, code)) -> {
    // Log full error for debugging
    logger.error("Query failed: " <> msg <> " [" <> option.unwrap(code, "unknown") <> "]")
    // Return generic error to client
    json.object([#("error", json.string("Database error"))])
  }
}

// DO use appropriate error mapping
case result {
  Error(error.NotFound) ->
    response.status(404) |> response.json(#("error", "Not found"))
  Error(error.UniqueViolation(..)) ->
    response.status(409) |> response.json(#("error", "Resource already exists"))
  Error(_) ->
    response.status(500) |> response.json(#("error", "Internal error"))
}
```

## Security Testing

cquill includes security-focused tests in `test/security/`:

- `sql_injection_test.gleam` - SQL injection prevention tests
- `credential_handling_test.gleam` - Credential and error handling tests

Run security tests:

```bash
gleam test test/security/
```

### Test Coverage

The security tests verify:

1. **SQL Injection Prevention**
   - Malicious string values are parameterized
   - Special characters (quotes, backslashes) are handled safely
   - LIKE patterns with injection attempts are safe
   - IN clause values are parameterized
   - Numeric values maintain type safety

2. **Identifier Escaping**
   - Table names with special characters are escaped
   - Column names with quotes are doubled

3. **Error Messages**
   - Connection errors don't contain credentials
   - Query errors don't expose sensitive data
   - Constraint violations show constraint names only

4. **Telemetry Events**
   - Events use parameterized query format
   - No credentials in event data

## Security Checklist

Before deploying to production:

- [ ] Use SSL for database connections (`ssl: pog.SslRequired`)
- [ ] Load credentials from environment variables, not code
- [ ] Configure appropriate connection and query timeouts
- [ ] Review telemetry handlers for sensitive data logging
- [ ] Map internal errors to user-safe responses
- [ ] Never use `query.raw()` with user input
- [ ] Run security tests as part of CI/CD

## Reporting Security Issues

If you discover a security vulnerability in cquill:

1. **Do not** open a public issue
2. Email security concerns to the maintainers directly
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact assessment

We will respond within 48 hours and work with you to address the issue.

## Security Updates

Security fixes are prioritized and released as patch versions. Subscribe to repository releases to stay informed about security updates.

## References

- [OWASP SQL Injection Prevention Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/SQL_Injection_Prevention_Cheat_Sheet.html)
- [PostgreSQL Security Documentation](https://www.postgresql.org/docs/current/security.html)
- [CWE-89: SQL Injection](https://cwe.mitre.org/data/definitions/89.html)
