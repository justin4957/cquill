# Transactions

Transactions ensure that multiple database operations either all succeed or all fail together. This guide covers transaction patterns in cquill.

## Basic Transactions

### Simple Transaction

```gleam
import cquill/repo
import cquill/query/ast

repo.transaction(adapter, fn(tx) {
  // Create order
  let order_insert = ast.new_insert("orders")
    |> ast.insert_columns(["customer_id", "total"])
    |> ast.insert_values([[ast.IntValue(1), ast.FloatValue(99.99)]])
    |> ast.returning(["id"])

  use order_rows <- result.try(repo.insert(tx, order_insert))
  let order_id = get_id(order_rows)

  // Create order items
  let items_insert = ast.new_insert("order_items")
    |> ast.insert_columns(["order_id", "product_id", "quantity"])
    |> ast.insert_values([[
      ast.IntValue(order_id),
      ast.IntValue(42),
      ast.IntValue(2),
    ]])

  use _ <- result.try(repo.insert(tx, items_insert))

  // Update inventory
  let update = ast.new_update("products")
    |> ast.set("stock", ast.IntValue(98))  // Decrement
    |> ast.update_where(query.eq("id", ast.IntValue(42)))

  use _ <- result.try(repo.update(tx, update))

  Ok(order_id)
})
```

### Transaction Result Handling

```gleam
case repo.transaction(adapter, my_transaction_fn) {
  Ok(result) -> {
    io.println("Transaction succeeded: " <> string.inspect(result))
  }
  Error(error.TransactionFailed(reason)) -> {
    io.println("Transaction failed: " <> reason)
    // All changes have been rolled back
  }
  Error(e) -> {
    io.println("Error: " <> error.format(e))
  }
}
```

## Savepoints

Savepoints allow partial rollback within a transaction.

### Creating Savepoints

```gleam
repo.transaction(adapter, fn(tx) {
  // Create the main order
  use order <- result.try(create_order(tx))

  // Create a savepoint before processing items
  use sp <- repo.savepoint(tx, "process_items")

  // Try to process items
  case process_order_items(tx, order.id) {
    Ok(items) -> {
      // Success - release the savepoint
      repo.release_savepoint(tx, sp)
      Ok(#(order, items))
    }
    Error(_) -> {
      // Failed - rollback to savepoint
      repo.rollback_to_savepoint(tx, sp)
      // Order still exists, but items are rolled back
      Ok(#(order, []))
    }
  }
})
```

### Nested Savepoints

```gleam
repo.transaction(adapter, fn(tx) {
  use user <- result.try(create_user(tx))

  // First savepoint for profile
  use sp1 <- repo.savepoint(tx, "create_profile")

  case create_profile(tx, user.id) {
    Ok(profile) -> {
      // Nested savepoint for preferences
      use sp2 <- repo.savepoint(tx, "create_preferences")

      case create_preferences(tx, user.id) {
        Ok(prefs) -> {
          repo.release_savepoint(tx, sp2)
          repo.release_savepoint(tx, sp1)
          Ok(#(user, Some(profile), Some(prefs)))
        }
        Error(_) -> {
          repo.rollback_to_savepoint(tx, sp2)
          repo.release_savepoint(tx, sp1)
          Ok(#(user, Some(profile), None))
        }
      }
    }
    Error(_) -> {
      repo.rollback_to_savepoint(tx, sp1)
      Ok(#(user, None, None))
    }
  }
})
```

## Transaction Patterns

### All or Nothing

The most common pattern - everything succeeds or nothing does:

```gleam
pub fn transfer_funds(adapter, from_id: Int, to_id: Int, amount: Float) {
  repo.transaction(adapter, fn(tx) {
    // Debit source account
    use _ <- result.try(debit_account(tx, from_id, amount))

    // Credit destination account
    use _ <- result.try(credit_account(tx, to_id, amount))

    // Create transfer record
    use transfer <- result.try(create_transfer_record(tx, from_id, to_id, amount))

    Ok(transfer)
  })
}

fn debit_account(tx, account_id: Int, amount: Float) {
  let update = ast.new_update("accounts")
    |> ast.set_raw("balance", "balance - $1", [ast.FloatValue(amount)])
    |> ast.update_where(query.and_where([
      query.eq("id", ast.IntValue(account_id)),
      query.gte("balance", ast.FloatValue(amount)),  // Ensure sufficient funds
    ]))
    |> ast.returning(["id", "balance"])

  case repo.update(tx, update) {
    Ok([]) -> Error(error.NotFound)  // No rows updated = insufficient funds
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}
```

### Partial Success with Savepoints

Allow some operations to fail while preserving others:

```gleam
pub fn import_products(adapter, products: List(ProductData)) {
  repo.transaction(adapter, fn(tx) {
    // Import each product, collecting results
    let results = list.map(products, fn(product) {
      use sp <- repo.savepoint(tx, "import_" <> product.sku)

      case insert_product(tx, product) {
        Ok(id) -> {
          repo.release_savepoint(tx, sp)
          Ok(#(product.sku, id))
        }
        Error(e) -> {
          repo.rollback_to_savepoint(tx, sp)
          Error(#(product.sku, e))
        }
      }
    })

    // Separate successes and failures
    let successes = list.filter_map(results, fn(r) {
      case r { Ok(x) -> Ok(x) Error(_) -> Error(Nil) }
    })
    let failures = list.filter_map(results, fn(r) {
      case r { Error(x) -> Ok(x) Ok(_) -> Error(Nil) }
    })

    Ok(#(successes, failures))
  })
}
```

### Optimistic Locking

Detect concurrent modifications:

```gleam
pub fn update_document(adapter, doc_id: Int, new_content: String, expected_version: Int) {
  repo.transaction(adapter, fn(tx) {
    // Update only if version matches
    let update = ast.new_update("documents")
      |> ast.set("content", ast.StringValue(new_content))
      |> ast.set("version", ast.IntValue(expected_version + 1))
      |> ast.set("updated_at", ast.StringValue(now()))
      |> ast.update_where(query.and_where([
        query.eq("id", ast.IntValue(doc_id)),
        query.eq("version", ast.IntValue(expected_version)),
      ]))
      |> ast.returning(["id", "version"])

    case repo.update(tx, update) {
      Ok([row]) -> Ok(row)
      Ok([]) -> {
        // Version mismatch - fetch current to report
        let current = repo.one(tx, get_document_query(doc_id))
        case current {
          Ok(doc) -> {
            let current_version = dict.get(doc, "version")
            Error(error.StaleData(
              int.to_string(expected_version),
              string.inspect(current_version),
            ))
          }
          Error(error.NotFound) -> Error(error.NotFound)
          Error(e) -> Error(e)
        }
      }
      Error(e) -> Error(e)
    }
  })
}
```

### Idempotent Operations

Make operations safe to retry:

```gleam
pub fn process_payment(adapter, payment_id: String, amount: Float) {
  repo.transaction(adapter, fn(tx) {
    // Check if already processed (idempotency key)
    let existing = repo.one(tx,
      query.from("processed_payments")
      |> query.where(query.eq("payment_id", ast.StringValue(payment_id)))
    )

    case existing {
      Ok(_) -> {
        // Already processed - return existing result
        Ok(#("already_processed", payment_id))
      }
      Error(error.NotFound) -> {
        // Not processed yet - do the work
        use _ <- result.try(charge_payment(tx, amount))
        use _ <- result.try(record_payment(tx, payment_id, amount))
        Ok(#("processed", payment_id))
      }
      Error(e) -> Error(e)
    }
  })
}
```

## Error Handling in Transactions

### Automatic Rollback

Any error returned from the transaction function triggers a rollback:

```gleam
repo.transaction(adapter, fn(tx) {
  use user <- result.try(create_user(tx))

  // If this fails, the user creation is also rolled back
  use profile <- result.try(create_profile(tx, user.id))

  Ok(#(user, profile))
})
```

### Manual Rollback

Explicitly rollback based on business logic:

```gleam
repo.transaction(adapter, fn(tx) {
  use order <- result.try(create_order(tx))

  // Business validation
  case validate_order(order) {
    Ok(_) -> {
      use _ <- result.try(finalize_order(tx, order.id))
      Ok(order)
    }
    Error(validation_error) -> {
      // Rollback by returning an error
      Error(error.AdapterSpecific("validation", validation_error))
    }
  }
})
```

### Handling Constraint Violations

```gleam
repo.transaction(adapter, fn(tx) {
  let insert = create_user_insert(email)

  case repo.insert(tx, insert) {
    Ok(rows) -> Ok(rows)
    Error(error.UniqueViolation("users_email_key", _)) -> {
      // Handle duplicate email specifically
      Error(error.AdapterSpecific("duplicate_email", email))
    }
    Error(e) -> Error(e)
  }
})
```

## Transaction Isolation

### Read Committed (Default)

The default isolation level. Each statement sees committed data:

```gleam
// This is the default behavior
repo.transaction(adapter, fn(tx) {
  // Each query sees the latest committed data
  use users <- result.try(repo.all(tx, query.from("users")))
  // Another transaction may have committed between queries
  use orders <- result.try(repo.all(tx, query.from("orders")))
  Ok(#(users, orders))
})
```

### Serializable (When Needed)

For critical sections that need complete isolation:

```gleam
// Use when you need strict isolation
repo.transaction_with(adapter, TransactionOptions(
  isolation: Serializable,
), fn(tx) {
  // Operations here are fully isolated
  use balance <- result.try(get_account_balance(tx, account_id))
  use _ <- result.try(debit_account(tx, account_id, amount))
  Ok(balance - amount)
})
```

## Best Practices

### 1. Keep Transactions Short

```gleam
// Good: Quick transaction
repo.transaction(adapter, fn(tx) {
  use order <- result.try(repo.insert(tx, order_insert))
  use _ <- result.try(repo.insert(tx, items_insert))
  Ok(order)
})

// Avoid: Long-running operations in transactions
repo.transaction(adapter, fn(tx) {
  use order <- result.try(repo.insert(tx, order_insert))
  // DON'T: Call external APIs inside transactions
  let _ = send_email_notification(order)  // This is slow!
  Ok(order)
})
```

### 2. Don't Nest Transactions

```gleam
// Avoid: Nested transactions (use savepoints instead)
repo.transaction(adapter, fn(tx) {
  repo.transaction(tx, fn(inner_tx) {  // This doesn't work as expected
    // ...
  })
})

// Good: Use savepoints for nested rollback
repo.transaction(adapter, fn(tx) {
  use sp <- repo.savepoint(tx, "nested")
  // ...
})
```

### 3. Handle All Error Cases

```gleam
// Good: Handle specific errors
case repo.transaction(adapter, my_fn) {
  Ok(result) -> handle_success(result)
  Error(error.UniqueViolation(_, _)) -> handle_duplicate()
  Error(error.ForeignKeyViolation(_, _)) -> handle_fk_error()
  Error(error.Timeout) -> handle_timeout()
  Error(e) -> handle_unexpected(e)
}
```

## Next Steps

- See [Testing](./testing.md) for testing transactions
- Read about [Savepoints](../concepts/repo.md#savepoints)
- Check [Error Reference](../reference/errors.md)
