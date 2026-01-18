// Raw SQL in Typed Query Tests
//
// Tests for using raw SQL expressions within typed queries.
// These tests verify the integration between the raw module
// and the typed query builder.

import cquill/query/ast
import cquill/typed/query.{
  to_ast, typed_from, typed_group_by_raw, typed_having_raw, typed_order_by_raw,
  typed_raw_condition, typed_select_raw, typed_select_raw_aliased, typed_where,
  typed_where_raw,
}
import cquill/typed/raw.{
  column_raw_gt, count_all, count_over_with_alias, date_trunc, interval, now,
  random, raw, subtract,
}
import cquill/typed/table.{type Column, type Table, column, table}
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// MOCK TABLE TYPES
// ============================================================================

/// Phantom type for the users table
pub type UserTable

/// Phantom type for the orders table
pub type OrderTable

// Mock table factories
fn users() -> Table(UserTable) {
  table("users")
}

fn orders() -> Table(OrderTable) {
  table("orders")
}

// Mock column factories
fn user_id() -> Column(UserTable, Int) {
  column("id")
}

fn user_active() -> Column(UserTable, Bool) {
  column("active")
}

fn user_created_at() -> Column(UserTable, String) {
  column("created_at")
}

fn order_user_id() -> Column(OrderTable, Int) {
  column("user_id")
}

fn order_created_at() -> Column(OrderTable, String) {
  column("created_at")
}

// ============================================================================
// TYPED_WHERE_RAW TESTS
// ============================================================================

pub fn typed_where_raw_adds_condition_test() {
  let query =
    typed_from(users())
    |> typed_where_raw(raw("created_at > NOW() - INTERVAL '7 days'"))

  let ast_query = to_ast(query)
  list.length(ast_query.wheres) |> should.equal(1)

  case ast_query.wheres {
    [ast.Where(condition)] -> {
      case condition {
        ast.Raw(sql, _) -> {
          sql |> should.equal("created_at > NOW() - INTERVAL '7 days'")
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn typed_where_raw_with_params_test() {
  let query =
    typed_from(users())
    |> typed_where_raw(
      raw.raw_with_params("email = $1", [ast.StringValue("test@example.com")]),
    )

  let ast_query = to_ast(query)
  case ast_query.wheres {
    [ast.Where(condition)] -> {
      case condition {
        ast.Raw(sql, params) -> {
          sql |> should.equal("email = $1")
          params |> should.equal([ast.StringValue("test@example.com")])
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn mixed_typed_and_raw_where_test() {
  let query =
    typed_from(users())
    |> typed_where(query.typed_eq(user_active(), True))
    |> typed_where_raw(raw("created_at > NOW() - INTERVAL '7 days'"))

  let ast_query = to_ast(query)
  list.length(ast_query.wheres) |> should.equal(2)
}

// ============================================================================
// TYPED_SELECT_RAW TESTS
// ============================================================================

pub fn typed_select_raw_adds_expressions_test() {
  let query =
    typed_from(users())
    |> typed_select_raw([count_all()])

  let ast_query = to_ast(query)
  case ast_query.select {
    ast.SelectExpr(exprs) -> {
      list.length(exprs) |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn typed_select_raw_multiple_expressions_test() {
  let query =
    typed_from(users())
    |> typed_select_raw([count_all(), count_over_with_alias("total")])

  let ast_query = to_ast(query)
  case ast_query.select {
    ast.SelectExpr(exprs) -> {
      list.length(exprs) |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn typed_select_raw_aliased_test() {
  let query =
    typed_from(users())
    |> typed_select_raw_aliased([
      #(count_all(), "user_count"),
      #(now(), "query_time"),
    ])

  let ast_query = to_ast(query)
  case ast_query.select {
    ast.SelectExpr(exprs) -> {
      list.length(exprs) |> should.equal(2)
      // Check that aliases are set
      case exprs {
        [first, second] -> {
          first.alias |> should.equal(Some("user_count"))
          second.alias |> should.equal(Some("query_time"))
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// ============================================================================
// TYPED_ORDER_BY_RAW TESTS
// ============================================================================

pub fn typed_order_by_raw_adds_order_test() {
  let query =
    typed_from(users())
    |> typed_order_by_raw(random(), ast.Asc)

  let ast_query = to_ast(query)
  list.length(ast_query.order_bys) |> should.equal(1)

  case ast_query.order_bys {
    [order] -> {
      order.field |> should.equal("RANDOM()")
      order.direction |> should.equal(ast.Asc)
    }
    _ -> should.fail()
  }
}

pub fn typed_order_by_raw_desc_test() {
  let query =
    typed_from(users())
    |> typed_order_by_raw(raw("COALESCE(updated_at, created_at)"), ast.Desc)

  let ast_query = to_ast(query)
  case ast_query.order_bys {
    [order] -> {
      order.field |> should.equal("COALESCE(updated_at, created_at)")
      order.direction |> should.equal(ast.Desc)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// TYPED_GROUP_BY_RAW TESTS
// ============================================================================

pub fn typed_group_by_raw_adds_group_test() {
  let query =
    typed_from(orders())
    |> typed_group_by_raw(date_trunc("month", order_created_at()))

  let ast_query = to_ast(query)
  list.length(ast_query.group_bys) |> should.equal(1)

  case ast_query.group_bys {
    [group] -> {
      group |> should.equal("DATE_TRUNC('month', created_at)")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// TYPED_HAVING_RAW TESTS
// ============================================================================

pub fn typed_having_raw_adds_having_test() {
  let query =
    typed_from(orders())
    |> typed_group_by_raw(raw("user_id"))
    |> typed_having_raw(raw("COUNT(*) > 5"))

  let ast_query = to_ast(query)
  list.length(ast_query.havings) |> should.equal(1)

  case ast_query.havings {
    [ast.Where(condition)] -> {
      case condition {
        ast.Raw(sql, _) -> {
          sql |> should.equal("COUNT(*) > 5")
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// ============================================================================
// TYPED_RAW_CONDITION TESTS
// ============================================================================

pub fn typed_raw_condition_in_where_test() {
  let query =
    typed_from(users())
    |> typed_where(typed_raw_condition(raw("status IN ('active', 'pending')")))

  let ast_query = to_ast(query)
  list.length(ast_query.wheres) |> should.equal(1)
}

pub fn typed_raw_condition_with_column_raw_test() {
  // Use column_raw_gt from the raw module
  let query =
    typed_from(users())
    |> typed_where(
      typed_raw_condition(column_raw_gt(
        user_created_at(),
        subtract(now(), interval(7, "days")),
      )),
    )

  let ast_query = to_ast(query)
  case ast_query.wheres {
    [ast.Where(condition)] -> {
      case condition {
        ast.Raw(sql, _) -> {
          sql |> should.equal("created_at > (NOW() - INTERVAL '7 days')")
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// ============================================================================
// COMPLEX QUERY TESTS
// ============================================================================

pub fn complete_query_with_raw_expressions_test() {
  // Test a complete query mixing typed and raw expressions
  let query =
    typed_from(users())
    |> typed_select_raw_aliased([
      #(raw("id"), "user_id"),
      #(raw("email"), "user_email"),
      #(count_over_with_alias("total"), "total_count"),
    ])
    |> typed_where(query.typed_eq(user_active(), True))
    |> typed_where_raw(raw("created_at > NOW() - INTERVAL '30 days'"))
    |> typed_order_by_raw(raw("COALESCE(last_login, created_at)"), ast.Desc)

  let ast_query = to_ast(query)

  // Verify SELECT
  case ast_query.select {
    ast.SelectExpr(exprs) -> list.length(exprs) |> should.equal(3)
    _ -> should.fail()
  }

  // Verify WHERE (2 conditions)
  list.length(ast_query.wheres) |> should.equal(2)

  // Verify ORDER BY (1 clause)
  list.length(ast_query.order_bys) |> should.equal(1)
}

pub fn pagination_with_total_count_test() {
  // Common pattern: Get paginated results with total count
  let query =
    typed_from(users())
    |> typed_select_raw_aliased([
      #(raw("*"), "all_columns"),
      #(count_over_with_alias("total"), "total_count"),
    ])
    |> typed_where(query.typed_eq(user_active(), True))
    |> query.typed_limit(10)
    |> query.typed_offset(0)

  let ast_query = to_ast(query)

  // Verify pagination
  ast_query.limit |> should.equal(Some(10))
  ast_query.offset |> should.equal(Some(0))

  // Verify window function in select
  case ast_query.select {
    ast.SelectExpr(exprs) -> list.length(exprs) |> should.equal(2)
    _ -> should.fail()
  }
}

pub fn aggregate_query_with_having_test() {
  // Common pattern: Aggregate with HAVING clause
  let query =
    typed_from(orders())
    |> typed_select_raw_aliased([
      #(raw("user_id"), "user_id"),
      #(count_all(), "order_count"),
      #(raw("SUM(amount)"), "total"),
    ])
    |> typed_group_by_raw(raw("user_id"))
    |> typed_having_raw(raw("COUNT(*) >= 3"))

  let ast_query = to_ast(query)

  // Verify GROUP BY
  list.length(ast_query.group_bys) |> should.equal(1)

  // Verify HAVING
  list.length(ast_query.havings) |> should.equal(1)
}

pub fn random_order_query_test() {
  // Get random user
  let query =
    typed_from(users())
    |> typed_where(query.typed_eq(user_active(), True))
    |> typed_order_by_raw(random(), ast.Asc)
    |> query.typed_limit(1)

  let ast_query = to_ast(query)

  // Verify random ordering
  case ast_query.order_bys {
    [order] -> {
      order.field |> should.equal("RANDOM()")
    }
    _ -> should.fail()
  }

  ast_query.limit |> should.equal(Some(1))
}
