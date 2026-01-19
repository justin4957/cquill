# Schemas

Schemas in cquill describe the structure of your data. Unlike traditional ORMs where entities are tightly coupled to database tables, cquill schemas are metadata that describe fields, types, and relationships without performing any I/O.

## What is a Schema?

A schema is a description of a database table's structure:

```gleam
import cquill/schema
import cquill/schema/field

// Define a schema for the users table
let user_schema =
  schema.new("users")
  |> schema.add_field(field.new("id", field.Integer) |> field.primary_key())
  |> schema.add_field(field.new("email", field.String) |> field.not_null())
  |> schema.add_field(field.new("name", field.String) |> field.nullable())
  |> schema.add_field(field.new("active", field.Boolean) |> field.default("true"))
  |> schema.add_field(field.new("created_at", field.Timestamp) |> field.not_null())
```

## Schema Components

### Table Name

Every schema has a table name that identifies the database table:

```gleam
let schema = schema.new("users")

// With schema qualification
let schema = schema.new_qualified("public", "users")
```

### Fields

Fields describe columns in the table:

```gleam
import cquill/schema/field

// Basic field
field.new("email", field.String)

// With constraints
field.new("email", field.String)
|> field.not_null()
|> field.unique()

// With default value
field.new("active", field.Boolean)
|> field.default("true")

// Primary key
field.new("id", field.Integer)
|> field.primary_key()
|> field.auto_increment()
```

### Field Types

cquill supports common database field types:

| Type | Gleam Type | Description |
|------|------------|-------------|
| `field.Integer` | `Int` | Integer values |
| `field.BigInt` | `Int` | Large integer values |
| `field.Float` | `Float` | Floating point numbers |
| `field.Decimal(precision, scale)` | `Float` | Exact decimal numbers |
| `field.String` | `String` | Text values |
| `field.Text` | `String` | Long text values |
| `field.Boolean` | `Bool` | True/false values |
| `field.Timestamp` | Custom | Date and time |
| `field.Date` | Custom | Date only |
| `field.Time` | Custom | Time only |
| `field.Uuid` | `String` | UUID values |
| `field.Json` | `Dynamic` | JSON data |
| `field.Jsonb` | `Dynamic` | Binary JSON (PostgreSQL) |

### Constraints

Apply constraints to fields:

```gleam
field.new("email", field.String)
|> field.not_null()        // NOT NULL
|> field.unique()          // UNIQUE
|> field.primary_key()     // PRIMARY KEY
|> field.check("length(email) > 5")  // CHECK constraint
|> field.references("users", "id")   // FOREIGN KEY
```

## Multiple Schemas per Domain

A key insight from Ecto is that you can have multiple schemas for the same data, each serving a different purpose:

```gleam
// Full user schema for reading
let user_read_schema =
  schema.new("users")
  |> schema.add_field(field.new("id", field.Integer))
  |> schema.add_field(field.new("email", field.String))
  |> schema.add_field(field.new("name", field.String))
  |> schema.add_field(field.new("password_hash", field.String))
  |> schema.add_field(field.new("created_at", field.Timestamp))
  |> schema.add_field(field.new("updated_at", field.Timestamp))

// Minimal schema for inserting new users
let user_insert_schema =
  schema.new("users")
  |> schema.add_field(field.new("email", field.String))
  |> schema.add_field(field.new("name", field.String))
  |> schema.add_field(field.new("password_hash", field.String))

// Schema for public display (no sensitive fields)
let user_public_schema =
  schema.new("users")
  |> schema.add_field(field.new("id", field.Integer))
  |> schema.add_field(field.new("name", field.String))
```

## Schema Introspection

You can inspect schemas at runtime:

```gleam
// Get table name
schema.table_name(user_schema)  // "users"

// Get all fields
schema.fields(user_schema)  // List of Field records

// Get a specific field
schema.get_field(user_schema, "email")  // Option(Field)

// Get primary key field(s)
schema.primary_key_fields(user_schema)  // List of Field
```

## Generated Schemas

For real-world usage, cquill can generate schemas from your database:

```sh
# Generate schemas from database introspection
gleam run -m cquill_cli generate
```

This creates typed schema modules based on your actual database structure. See [Code Generation](../guides/codegen.md) for details.

## Best Practices

### 1. Keep Schemas Focused

Create schemas that match specific use cases rather than one monolithic schema:

```gleam
// Good: Focused schemas
let user_list_schema = ...  // For listing users
let user_detail_schema = ...  // For user detail view
let user_create_schema = ...  // For creating users

// Avoid: One schema for everything
let user_schema = ...  // Contains every field
```

### 2. Separate Read and Write Schemas

Read operations often need different fields than write operations:

```gleam
// Read schema includes computed/joined fields
let order_read_schema =
  schema.new("orders")
  |> schema.add_field(field.new("id", field.Integer))
  |> schema.add_field(field.new("total", field.Decimal(10, 2)))
  |> schema.add_field(field.new("customer_name", field.String))  // Joined

// Write schema only includes writable fields
let order_write_schema =
  schema.new("orders")
  |> schema.add_field(field.new("customer_id", field.Integer))
  |> schema.add_field(field.new("total", field.Decimal(10, 2)))
```

### 3. Use Schema for Validation Context

Schemas provide context for validation:

```gleam
// Validate that all required fields are present
fn validate_for_insert(data, schema) {
  schema.fields(schema)
  |> list.filter(fn(f) { field.is_required(f) })
  |> list.all(fn(f) { dict.has_key(data, field.name(f)) })
}
```

## Next Steps

- Learn about [Queries](./queries.md) to query your schemas
- See [Repo](./repo.md) for executing queries
- Read about [Adapters](./adapters.md) for backend abstraction
