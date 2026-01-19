# Multi-Tenant Recipe

Patterns for implementing multi-tenant applications with cquill.

## Multi-Tenancy Strategies

### 1. Row-Level (Shared Database, Shared Schema)

All tenants share tables with a `tenant_id` column.

**Pros:**
- Simplest to implement
- Efficient resource usage
- Easy cross-tenant queries (admin)

**Cons:**
- Risk of data leaks if filters forgotten
- Harder to customize per tenant

### 2. Schema-Level (Shared Database, Separate Schemas)

Each tenant has their own PostgreSQL schema.

**Pros:**
- Strong data isolation
- Easy per-tenant customization
- Simple backup/restore per tenant

**Cons:**
- More complex connection management
- Schema migrations for each tenant

### 3. Database-Level (Separate Databases)

Each tenant has their own database.

**Pros:**
- Complete isolation
- Independent scaling
- Maximum customization

**Cons:**
- Highest resource overhead
- Complex management

## Row-Level Multi-Tenancy

### Schema Design

```sql
-- Add tenant_id to all tenant-scoped tables
ALTER TABLE users ADD COLUMN tenant_id INTEGER NOT NULL REFERENCES tenants(id);
ALTER TABLE orders ADD COLUMN tenant_id INTEGER NOT NULL REFERENCES tenants(id);
ALTER TABLE products ADD COLUMN tenant_id INTEGER NOT NULL REFERENCES tenants(id);

-- Create indexes for tenant queries
CREATE INDEX idx_users_tenant ON users(tenant_id);
CREATE INDEX idx_orders_tenant ON orders(tenant_id);
CREATE INDEX idx_products_tenant ON products(tenant_id);

-- Composite indexes for common queries
CREATE INDEX idx_orders_tenant_created ON orders(tenant_id, created_at DESC);
```

### Tenant Context

```gleam
import cquill/query
import cquill/query/ast

/// Current tenant context
pub type TenantContext {
  TenantContext(
    tenant_id: Int,
    tenant_name: String,
    is_admin: Bool,
  )
}

/// Global tenant context - thread-local in real apps
pub opaque type TenantState {
  TenantState(context: Option(TenantContext))
}

/// Set current tenant
pub fn set_tenant(state: TenantState, context: TenantContext) -> TenantState {
  TenantState(context: Some(context))
}

/// Clear tenant context
pub fn clear_tenant(state: TenantState) -> TenantState {
  TenantState(context: None)
}

/// Get current tenant (fails if not set)
pub fn require_tenant(state: TenantState) -> Result(TenantContext, String) {
  case state.context {
    Some(ctx) -> Ok(ctx)
    None -> Error("No tenant context set")
  }
}
```

### Scoped Queries

```gleam
/// Automatically scope query to current tenant
pub fn scoped(
  base_query: query.Query(a),
  tenant_id: Int,
) -> query.Query(a) {
  base_query
  |> query.where(query.eq("tenant_id", ast.IntValue(tenant_id)))
}

/// Create scoped query from table name
pub fn from_scoped(table: String, tenant_id: Int) -> query.Query(Nil) {
  query.from(table)
  |> query.where(query.eq("tenant_id", ast.IntValue(tenant_id)))
}

/// Usage
pub fn get_tenant_users(adapter, tenant_id: Int) {
  from_scoped("users", tenant_id)
  |> query.order_by_asc("name")
  |> repo.all(adapter, _)
}
```

### Scoped Repository

```gleam
import cquill/repo
import cquill/error

/// Tenant-scoped repository wrapper
pub type TenantRepo {
  TenantRepo(
    adapter: adapter.Adapter,
    tenant_id: Int,
  )
}

pub fn new(adapter, tenant_id: Int) -> TenantRepo {
  TenantRepo(adapter: adapter, tenant_id: tenant_id)
}

/// Scoped select - automatically filters by tenant
pub fn all(
  repo: TenantRepo,
  base_query: query.Query(a),
) -> Result(List(Row), error.AdapterError) {
  let scoped_query = scoped(base_query, repo.tenant_id)
  repo.all(repo.adapter, scoped_query)
}

/// Scoped one - automatically filters by tenant
pub fn one(
  repo: TenantRepo,
  base_query: query.Query(a),
) -> Result(Row, error.AdapterError) {
  let scoped_query = scoped(base_query, repo.tenant_id)
  repo.one(repo.adapter, scoped_query)
}

/// Scoped insert - automatically adds tenant_id
pub fn insert(
  trepo: TenantRepo,
  table: String,
  data: Dict(String, ast.Value),
) -> Result(List(Row), error.AdapterError) {
  // Add tenant_id to the data
  let data_with_tenant = dict.insert(
    data,
    "tenant_id",
    ast.IntValue(trepo.tenant_id),
  )

  let columns = dict.keys(data_with_tenant)
  let values = dict.values(data_with_tenant) |> list.map(fn(v) { [v] })

  let insert =
    ast.new_insert(table)
    |> ast.insert_columns(columns)
    |> ast.insert_values(values)
    |> ast.returning(["id"] |> list.append(columns))

  repo.insert(trepo.adapter, insert)
}

/// Scoped update - only updates within tenant
pub fn update(
  trepo: TenantRepo,
  table: String,
  id: Int,
  changes: Dict(String, ast.Value),
) -> Result(List(Row), error.AdapterError) {
  let update = list.fold(dict.to_list(changes), ast.new_update(table), fn(q, pair) {
    let #(column, value) = pair
    ast.set(q, column, value)
  })
  |> ast.update_where(query.and_where([
    query.eq("id", ast.IntValue(id)),
    query.eq("tenant_id", ast.IntValue(trepo.tenant_id)),
  ]))
  |> ast.returning(dict.keys(changes))

  repo.update(trepo.adapter, update)
}

/// Scoped delete - only deletes within tenant
pub fn delete(
  trepo: TenantRepo,
  table: String,
  id: Int,
) -> Result(Int, error.AdapterError) {
  let delete =
    ast.new_delete(table)
    |> ast.delete_where(query.and_where([
      query.eq("id", ast.IntValue(id)),
      query.eq("tenant_id", ast.IntValue(trepo.tenant_id)),
    ]))

  repo.delete(trepo.adapter, delete)
}
```

### Usage Example

```gleam
// Get tenant from authentication
let tenant_id = get_tenant_from_auth(request)

// Create scoped repository
let trepo = tenant_repo.new(adapter, tenant_id)

// All queries are automatically scoped
case tenant_repo.all(trepo, query.from("users")) {
  Ok(users) -> {
    // These are only users for this tenant
    display_users(users)
  }
  Error(e) -> handle_error(e)
}

// Inserts automatically include tenant_id
let user_data = dict.from_list([
  #("email", ast.StringValue("alice@example.com")),
  #("name", ast.StringValue("Alice")),
])

case tenant_repo.insert(trepo, "users", user_data) {
  Ok([user]) -> {
    // User was created with correct tenant_id
    Ok(user)
  }
  Error(e) -> Error(e)
}
```

## Schema-Level Multi-Tenancy

### Schema Management

```gleam
/// Create schema for new tenant
pub fn create_tenant_schema(
  adapter,
  tenant_slug: String,
) -> Result(Nil, error.AdapterError) {
  let schema_name = "tenant_" <> tenant_slug

  // Create the schema
  use _ <- result.try(repo.execute_raw(
    adapter,
    "CREATE SCHEMA " <> schema_name,
    [],
  ))

  // Run migrations in the new schema
  use _ <- result.try(run_migrations_in_schema(adapter, schema_name))

  Ok(Nil)
}

/// Drop tenant schema (careful!)
pub fn drop_tenant_schema(
  adapter,
  tenant_slug: String,
) -> Result(Nil, error.AdapterError) {
  let schema_name = "tenant_" <> tenant_slug

  repo.execute_raw(
    adapter,
    "DROP SCHEMA " <> schema_name <> " CASCADE",
    [],
  )
  |> result.map(fn(_) { Nil })
}
```

### Schema-Scoped Queries

```gleam
/// Set search_path for tenant
pub fn with_tenant_schema(
  adapter,
  tenant_slug: String,
  operation: fn() -> Result(a, error.AdapterError),
) -> Result(a, error.AdapterError) {
  let schema_name = "tenant_" <> tenant_slug

  // Set schema
  use _ <- result.try(repo.execute_raw(
    adapter,
    "SET search_path TO " <> schema_name <> ", public",
    [],
  ))

  // Run operation
  let result = operation()

  // Reset schema (best effort)
  let _ = repo.execute_raw(adapter, "SET search_path TO public", [])

  result
}

/// Usage
with_tenant_schema(adapter, "acme", fn() {
  // All queries now target tenant_acme schema
  query.from("users")
  |> repo.all(adapter, _)
})
```

### Connection Per Tenant

```gleam
import gleam/dict

/// Pool of connections per tenant
pub type TenantPools {
  TenantPools(pools: Dict(String, adapter.Adapter))
}

/// Get or create connection pool for tenant
pub fn get_tenant_pool(
  pools: TenantPools,
  tenant_slug: String,
  base_config: postgres.Config,
) -> Result(#(TenantPools, adapter.Adapter), error.AdapterError) {
  case dict.get(pools.pools, tenant_slug) {
    Ok(pool) -> Ok(#(pools, pool))
    Error(_) -> {
      // Create new pool with schema set
      let config = postgres.Config(
        ...base_config,
        // Set default schema in connection
        options: [#("options", "-c search_path=tenant_" <> tenant_slug)],
      )

      use pool <- result.try(postgres.connect(config))

      let new_pools = TenantPools(
        pools: dict.insert(pools.pools, tenant_slug, pool),
      )

      Ok(#(new_pools, pool))
    }
  }
}
```

## Database-Level Multi-Tenancy

### Tenant Database Management

```gleam
/// Tenant database configuration
pub type TenantDatabase {
  TenantDatabase(
    tenant_id: Int,
    tenant_slug: String,
    database_name: String,
    host: String,
    port: Int,
  )
}

/// Create database for new tenant
pub fn provision_tenant_database(
  admin_adapter,
  tenant: TenantDatabase,
) -> Result(Nil, error.AdapterError) {
  // Create database
  use _ <- result.try(repo.execute_raw(
    admin_adapter,
    "CREATE DATABASE " <> tenant.database_name,
    [],
  ))

  // Connect to new database and run migrations
  let config = postgres.Config(
    host: tenant.host,
    port: tenant.port,
    database: tenant.database_name,
    user: "app_user",
    password: get_password(),
    pool_size: 5,
    ssl: True,
    connection_timeout: 5000,
    query_timeout: 30000,
  )

  use pool <- result.try(postgres.connect(config))
  run_migrations(pool)
}
```

### Dynamic Connection Routing

```gleam
import gleam/dict

/// Connection router for multi-database tenancy
pub type ConnectionRouter {
  ConnectionRouter(
    connections: Dict(Int, adapter.Adapter),
    config_lookup: fn(Int) -> Result(postgres.Config, String),
  )
}

pub fn new_router(
  config_lookup: fn(Int) -> Result(postgres.Config, String),
) -> ConnectionRouter {
  ConnectionRouter(
    connections: dict.new(),
    config_lookup: config_lookup,
  )
}

/// Get connection for tenant
pub fn get_connection(
  router: ConnectionRouter,
  tenant_id: Int,
) -> Result(#(ConnectionRouter, adapter.Adapter), error.AdapterError) {
  case dict.get(router.connections, tenant_id) {
    Ok(conn) -> Ok(#(router, conn))
    Error(_) -> {
      // Look up config for tenant
      case router.config_lookup(tenant_id) {
        Ok(config) -> {
          use pool <- result.try(postgres.connect(config))

          let new_router = ConnectionRouter(
            ...router,
            connections: dict.insert(router.connections, tenant_id, pool),
          )

          Ok(#(new_router, pool))
        }
        Error(msg) -> Error(error.ConnectionFailed(msg))
      }
    }
  }
}

/// Execute with tenant connection
pub fn with_tenant(
  router: ConnectionRouter,
  tenant_id: Int,
  operation: fn(adapter.Adapter) -> Result(a, error.AdapterError),
) -> Result(#(ConnectionRouter, a), error.AdapterError) {
  use #(router, conn) <- result.try(get_connection(router, tenant_id))
  use result <- result.try(operation(conn))
  Ok(#(router, result))
}
```

## Middleware Pattern

For web applications, use middleware to set tenant context:

```gleam
/// Tenant middleware for web requests
pub fn tenant_middleware(
  request: Request,
  next: fn(Request, TenantContext) -> Response,
) -> Response {
  // Extract tenant from subdomain, header, or path
  case extract_tenant(request) {
    Ok(tenant_ctx) -> {
      // Validate tenant exists and is active
      case validate_tenant(tenant_ctx.tenant_id) {
        Ok(_) -> next(request, tenant_ctx)
        Error(_) -> response.not_found("Tenant not found")
      }
    }
    Error(_) -> response.bad_request("Missing tenant identifier")
  }
}

fn extract_tenant(request: Request) -> Result(TenantContext, String) {
  // Option 1: Subdomain (acme.myapp.com)
  case get_subdomain(request) {
    Ok(subdomain) -> lookup_tenant_by_subdomain(subdomain)
    Error(_) -> {
      // Option 2: Header (X-Tenant-ID)
      case request.get_header(request, "x-tenant-id") {
        Ok(tenant_id) -> lookup_tenant_by_id(tenant_id)
        Error(_) -> {
          // Option 3: Path prefix (/tenants/:id/...)
          case extract_tenant_from_path(request.path) {
            Ok(tenant_id) -> lookup_tenant_by_id(tenant_id)
            Error(_) -> Error("No tenant identifier found")
          }
        }
      }
    }
  }
}
```

## Testing Multi-Tenant Code

```gleam
pub fn scoped_query_filters_by_tenant_test() {
  let store = setup_store()
    |> insert_user(1, "tenant1@example.com", 1)  // tenant 1
    |> insert_user(2, "tenant2@example.com", 2)  // tenant 2
    |> insert_user(3, "other@example.com", 1)    // tenant 1

  let trepo = tenant_repo.new(store, 1)

  case tenant_repo.all(trepo, query.from("users")) {
    Ok(users) -> {
      // Should only return users from tenant 1
      list.length(users) |> should.equal(2)

      // Verify all users belong to tenant 1
      list.all(users, fn(user) {
        dict.get(user, "tenant_id") == Ok(ast.IntValue(1))
      })
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn scoped_insert_adds_tenant_id_test() {
  let store = setup_store()
  let trepo = tenant_repo.new(store, 42)

  let user_data = dict.from_list([
    #("email", ast.StringValue("new@example.com")),
  ])

  case tenant_repo.insert(trepo, "users", user_data) {
    Ok([user]) -> {
      // Verify tenant_id was added
      dict.get(user, "tenant_id")
      |> should.equal(Ok(ast.IntValue(42)))
    }
    _ -> should.fail()
  }
}

pub fn scoped_update_respects_tenant_boundary_test() {
  let store = setup_store()
    |> insert_user(1, "user@example.com", 1)  // tenant 1
    |> insert_user(2, "other@example.com", 2) // tenant 2

  // Try to update user from tenant 2 while scoped to tenant 1
  let trepo = tenant_repo.new(store, 1)

  let changes = dict.from_list([
    #("email", ast.StringValue("hacked@example.com")),
  ])

  case tenant_repo.update(trepo, "users", 2, changes) {
    Ok([]) -> {
      // Update should affect 0 rows (user 2 is in tenant 2)
      Nil
    }
    Ok(_) -> should.fail()  // Should not update
    Error(_) -> should.fail()
  }

  // Verify user 2 wasn't modified
  let query = query.from("users")
    |> query.where(query.eq("id", ast.IntValue(2)))

  case repo.one(store, query) {
    Ok(user) -> {
      dict.get(user, "email")
      |> should.equal(Ok(ast.StringValue("other@example.com")))
    }
    Error(_) -> should.fail()
  }
}

pub fn scoped_delete_respects_tenant_boundary_test() {
  let store = setup_store()
    |> insert_user(1, "user@example.com", 1)
    |> insert_user(2, "other@example.com", 2)

  let trepo = tenant_repo.new(store, 1)

  // Try to delete user from tenant 2
  case tenant_repo.delete(trepo, "users", 2) {
    Ok(0) -> {
      // Should delete 0 rows
      Nil
    }
    _ -> should.fail()
  }

  // Verify user 2 still exists
  let count_query = query.from("users")
  case repo.all(store, count_query) {
    Ok(users) -> list.length(users) |> should.equal(2)
    Error(_) -> should.fail()
  }
}
```

## Security Considerations

### 1. Always Validate Tenant Access

```gleam
pub fn require_tenant_access(
  user: User,
  tenant_id: Int,
) -> Result(Nil, String) {
  case user.tenant_id == tenant_id || user.is_super_admin {
    True -> Ok(Nil)
    False -> Error("Access denied to tenant")
  }
}
```

### 2. Audit Cross-Tenant Operations

```gleam
pub fn audit_cross_tenant_access(
  user: User,
  target_tenant_id: Int,
  operation: String,
) {
  case user.tenant_id != target_tenant_id {
    True -> {
      log_security_event(
        "cross_tenant_access",
        [
          #("user_id", int.to_string(user.id)),
          #("user_tenant", int.to_string(user.tenant_id)),
          #("target_tenant", int.to_string(target_tenant_id)),
          #("operation", operation),
        ],
      )
    }
    False -> Nil
  }
}
```

### 3. Prevent Tenant ID Manipulation

```gleam
/// Never trust tenant_id from user input for ownership
pub fn update_user_safe(
  trepo: TenantRepo,
  user_id: Int,
  changes: Dict(String, ast.Value),
) -> Result(List(Row), error.AdapterError) {
  // Remove tenant_id if present in changes (prevent tenant hopping)
  let safe_changes = dict.delete(changes, "tenant_id")

  tenant_repo.update(trepo, "users", user_id, safe_changes)
}
```
