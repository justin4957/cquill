# Audit Logging Recipe

Implement audit logging to track all changes to your data.

## Why Audit Logging?

- **Compliance**: Meet regulatory requirements (GDPR, HIPAA, SOX)
- **Debugging**: Understand how data changed over time
- **Security**: Detect unauthorized changes
- **Recovery**: Know what to restore after incidents

## Basic Implementation

### Audit Log Table

```sql
CREATE TABLE audit_logs (
  id BIGSERIAL PRIMARY KEY,
  table_name VARCHAR(255) NOT NULL,
  record_id INTEGER NOT NULL,
  action VARCHAR(20) NOT NULL,  -- 'INSERT', 'UPDATE', 'DELETE'
  old_data JSONB,
  new_data JSONB,
  changed_fields TEXT[],
  user_id INTEGER,
  user_ip VARCHAR(45),
  user_agent TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_audit_logs_table_record ON audit_logs(table_name, record_id);
CREATE INDEX idx_audit_logs_user ON audit_logs(user_id);
CREATE INDEX idx_audit_logs_created ON audit_logs(created_at);
```

### Audit Log Types

```gleam
pub type AuditAction {
  Insert
  Update
  Delete
}

pub type AuditEntry {
  AuditEntry(
    table_name: String,
    record_id: Int,
    action: AuditAction,
    old_data: Option(Dict(String, ast.Value)),
    new_data: Option(Dict(String, ast.Value)),
    changed_fields: List(String),
    user_id: Option(Int),
    user_ip: Option(String),
    user_agent: Option(String),
  )
}

pub type AuditContext {
  AuditContext(
    user_id: Option(Int),
    user_ip: Option(String),
    user_agent: Option(String),
  )
}
```

### Core Audit Functions

```gleam
import cquill/query/ast
import cquill/repo
import gleam/json

/// Log an audit entry
pub fn log_audit(adapter, entry: AuditEntry) -> Result(Nil, error.AdapterError) {
  let action_str = case entry.action {
    Insert -> "INSERT"
    Update -> "UPDATE"
    Delete -> "DELETE"
  }

  let insert =
    ast.new_insert("audit_logs")
    |> ast.insert_columns([
      "table_name", "record_id", "action",
      "old_data", "new_data", "changed_fields",
      "user_id", "user_ip", "user_agent",
    ])
    |> ast.insert_values([[
      ast.StringValue(entry.table_name),
      ast.IntValue(entry.record_id),
      ast.StringValue(action_str),
      encode_json_value(entry.old_data),
      encode_json_value(entry.new_data),
      encode_array(entry.changed_fields),
      option_to_value(entry.user_id, ast.IntValue),
      option_to_value(entry.user_ip, ast.StringValue),
      option_to_value(entry.user_agent, ast.StringValue),
    ]])

  case repo.insert(adapter, insert) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

fn encode_json_value(data: Option(Dict(String, ast.Value))) -> ast.Value {
  case data {
    Some(d) -> ast.StringValue(json.to_string(dict_to_json(d)))
    None -> ast.NullValue
  }
}

fn option_to_value(opt: Option(a), f: fn(a) -> ast.Value) -> ast.Value {
  case opt {
    Some(v) -> f(v)
    None -> ast.NullValue
  }
}
```

### Audited Operations

```gleam
/// Insert with audit logging
pub fn audited_insert(
  adapter,
  table: String,
  data: Dict(String, ast.Value),
  context: AuditContext,
) -> Result(Dict(String, ast.Value), error.AdapterError) {
  repo.transaction(adapter, fn(tx) {
    // Perform insert
    let columns = dict.keys(data)
    let values = dict.values(data) |> list.map(fn(v) { [v] })

    let insert =
      ast.new_insert(table)
      |> ast.insert_columns(columns)
      |> ast.insert_values(values)
      |> ast.returning(["id"] |> list.append(columns))

    use rows <- result.try(repo.insert(tx, insert))

    case list.first(rows) {
      Ok(row) -> {
        // Get the inserted ID
        let record_id = case dict.get(row, "id") {
          Ok(ast.IntValue(id)) -> id
          _ -> 0
        }

        // Log the audit entry
        let entry = AuditEntry(
          table_name: table,
          record_id: record_id,
          action: Insert,
          old_data: None,
          new_data: Some(row),
          changed_fields: columns,
          user_id: context.user_id,
          user_ip: context.user_ip,
          user_agent: context.user_agent,
        )

        use _ <- result.try(log_audit(tx, entry))

        Ok(row)
      }
      Error(_) -> Error(error.NotFound)
    }
  })
}

/// Update with audit logging
pub fn audited_update(
  adapter,
  table: String,
  id: Int,
  changes: Dict(String, ast.Value),
  context: AuditContext,
) -> Result(Dict(String, ast.Value), error.AdapterError) {
  repo.transaction(adapter, fn(tx) {
    // Fetch current record
    let select_query =
      query.from(table)
      |> query.where(query.eq("id", ast.IntValue(id)))

    use old_data <- result.try(repo.one(tx, select_query))

    // Perform update
    let update = list.fold(dict.to_list(changes), ast.new_update(table), fn(q, pair) {
      let #(column, value) = pair
      ast.set(q, column, value)
    })
    |> ast.update_where(query.eq("id", ast.IntValue(id)))
    |> ast.returning(dict.keys(old_data) |> list.append(["id"]))

    use rows <- result.try(repo.update(tx, update))

    case list.first(rows) {
      Ok(new_data) -> {
        // Calculate changed fields
        let changed_fields = dict.keys(changes)
          |> list.filter(fn(key) {
            dict.get(old_data, key) != dict.get(new_data, key)
          })

        // Log the audit entry
        let entry = AuditEntry(
          table_name: table,
          record_id: id,
          action: Update,
          old_data: Some(old_data),
          new_data: Some(new_data),
          changed_fields: changed_fields,
          user_id: context.user_id,
          user_ip: context.user_ip,
          user_agent: context.user_agent,
        )

        use _ <- result.try(log_audit(tx, entry))

        Ok(new_data)
      }
      Error(_) -> Error(error.NotFound)
    }
  })
}

/// Delete with audit logging
pub fn audited_delete(
  adapter,
  table: String,
  id: Int,
  context: AuditContext,
) -> Result(Nil, error.AdapterError) {
  repo.transaction(adapter, fn(tx) {
    // Fetch current record before deletion
    let select_query =
      query.from(table)
      |> query.where(query.eq("id", ast.IntValue(id)))

    use old_data <- result.try(repo.one(tx, select_query))

    // Perform delete
    let delete =
      ast.new_delete(table)
      |> ast.delete_where(query.eq("id", ast.IntValue(id)))

    use count <- result.try(repo.delete(tx, delete))

    case count {
      0 -> Error(error.NotFound)
      _ -> {
        // Log the audit entry
        let entry = AuditEntry(
          table_name: table,
          record_id: id,
          action: Delete,
          old_data: Some(old_data),
          new_data: None,
          changed_fields: [],
          user_id: context.user_id,
          user_ip: context.user_ip,
          user_agent: context.user_agent,
        )

        use _ <- result.try(log_audit(tx, entry))

        Ok(Nil)
      }
    }
  })
}
```

## Querying Audit Logs

```gleam
/// Get audit history for a record
pub fn get_record_history(
  adapter,
  table: String,
  record_id: Int,
) -> Result(List(Dict(String, ast.Value)), error.AdapterError) {
  let query =
    query.from("audit_logs")
    |> query.where(query.eq("table_name", ast.StringValue(table)))
    |> query.where(query.eq("record_id", ast.IntValue(record_id)))
    |> query.order_by_desc("created_at")

  repo.all(adapter, query)
}

/// Get recent audit logs for a user
pub fn get_user_activity(
  adapter,
  user_id: Int,
  limit: Int,
) -> Result(List(Dict(String, ast.Value)), error.AdapterError) {
  let query =
    query.from("audit_logs")
    |> query.where(query.eq("user_id", ast.IntValue(user_id)))
    |> query.order_by_desc("created_at")
    |> query.limit(limit)

  repo.all(adapter, query)
}

/// Get changes to a specific field
pub fn get_field_changes(
  adapter,
  table: String,
  record_id: Int,
  field: String,
) -> Result(List(Dict(String, ast.Value)), error.AdapterError) {
  let query =
    query.from("audit_logs")
    |> query.where(query.eq("table_name", ast.StringValue(table)))
    |> query.where(query.eq("record_id", ast.IntValue(record_id)))
    |> query.where(query.raw(
      "$1 = ANY(changed_fields)",
      [ast.StringValue(field)],
    ))
    |> query.order_by_desc("created_at")

  repo.all(adapter, query)
}
```

## Usage Example

```gleam
// Create audit context from request
let context = AuditContext(
  user_id: Some(current_user.id),
  user_ip: Some(request.client_ip),
  user_agent: Some(request.headers.user_agent),
)

// Create user with audit
let user_data = dict.from_list([
  #("email", ast.StringValue("alice@example.com")),
  #("name", ast.StringValue("Alice")),
])

case audited_insert(adapter, "users", user_data, context) {
  Ok(user) -> {
    io.println("Created user: " <> string.inspect(user))
  }
  Error(e) -> handle_error(e)
}

// Update user with audit
let changes = dict.from_list([
  #("name", ast.StringValue("Alice Smith")),
])

case audited_update(adapter, "users", user_id, changes, context) {
  Ok(user) -> {
    io.println("Updated user: " <> string.inspect(user))
  }
  Error(e) -> handle_error(e)
}

// View history
case get_record_history(adapter, "users", user_id) {
  Ok(history) -> {
    list.each(history, fn(entry) {
      io.println("Action: " <> string.inspect(dict.get(entry, "action")))
      io.println("At: " <> string.inspect(dict.get(entry, "created_at")))
      io.println("Changed: " <> string.inspect(dict.get(entry, "changed_fields")))
      io.println("---")
    })
  }
  Error(e) -> handle_error(e)
}
```

## Advanced: Automatic Audit with Telemetry

Use telemetry to automatically audit all operations:

```gleam
import cquill/telemetry

pub fn setup_audit_telemetry(adapter, get_context: fn() -> AuditContext) {
  // This is conceptual - actual implementation depends on your telemetry setup
  telemetry.attach(
    "audit-logger",
    [
      telemetry.QueryStopType,
    ],
    fn(event, metadata) {
      case event {
        telemetry.QueryStop(e) -> {
          // Parse the query to determine if it's INSERT/UPDATE/DELETE
          // and log accordingly
          case parse_mutation_type(e.query) {
            Some(Insert) -> {
              // Extract table and data, log audit
            }
            Some(Update) -> {
              // Extract table, id, changes, log audit
            }
            Some(Delete) -> {
              // Extract table and id, log audit
            }
            None -> Nil  // SELECT, no audit needed
          }
        }
        _ -> Nil
      }
    }
  )
}
```

## Retention and Cleanup

```gleam
/// Delete audit logs older than specified days
pub fn cleanup_old_audit_logs(
  adapter,
  retention_days: Int,
) -> Result(Int, error.AdapterError) {
  let cutoff_date = calculate_cutoff_date(retention_days)

  let delete =
    ast.new_delete("audit_logs")
    |> ast.delete_where(query.lt("created_at", ast.StringValue(cutoff_date)))

  repo.delete(adapter, delete)
}

/// Archive audit logs to separate table
pub fn archive_audit_logs(
  adapter,
  before_date: String,
) -> Result(Int, error.AdapterError) {
  repo.transaction(adapter, fn(tx) {
    // Copy to archive table
    let copy_result = repo.execute_raw(tx,
      "INSERT INTO audit_logs_archive SELECT * FROM audit_logs WHERE created_at < $1",
      [ast.StringValue(before_date)],
    )

    use _ <- result.try(copy_result)

    // Delete from main table
    let delete =
      ast.new_delete("audit_logs")
      |> ast.delete_where(query.lt("created_at", ast.StringValue(before_date)))

    repo.delete(tx, delete)
  })
}
```

## Testing Audit Logging

```gleam
pub fn audited_insert_creates_audit_log_test() {
  let store = setup_test_store()
  let context = AuditContext(user_id: Some(1), user_ip: None, user_agent: None)

  let data = dict.from_list([
    #("email", ast.StringValue("test@example.com")),
  ])

  case audited_insert(store, "users", data, context) {
    Ok(user) -> {
      // Check audit log was created
      let audit_query =
        query.from("audit_logs")
        |> query.where(query.eq("table_name", ast.StringValue("users")))
        |> query.where(query.eq("action", ast.StringValue("INSERT")))

      case repo.all(store, audit_query) {
        Ok([log]) -> {
          dict.get(log, "user_id") |> should.equal(Ok(ast.IntValue(1)))
          dict.get(log, "new_data") |> result.is_ok() |> should.be_true()
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn audited_update_records_changes_test() {
  let store = setup_store_with_user(1)
  let context = AuditContext(user_id: Some(1), user_ip: None, user_agent: None)

  let changes = dict.from_list([
    #("name", ast.StringValue("Updated Name")),
  ])

  case audited_update(store, "users", 1, changes, context) {
    Ok(_) -> {
      let audit_query =
        query.from("audit_logs")
        |> query.where(query.eq("action", ast.StringValue("UPDATE")))

      case repo.one(store, audit_query) {
        Ok(log) -> {
          dict.get(log, "changed_fields")
          |> should.equal(Ok(ast.ListValue([ast.StringValue("name")])))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
```
