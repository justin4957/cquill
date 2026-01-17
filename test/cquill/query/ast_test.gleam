import cquill/query/ast.{
  type Condition, type OrderBy, type Query, type Select, type Source, type Value,
  Asc, Desc, Eq, Gt, IntValue, NullsDefault, NullsFirst, NullsLast, OrderBy,
  Query as QueryRecord, SelectAll, SelectFields, StringValue, TableSource,
}
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// QUERY AST CONSTRUCTION TESTS
// ============================================================================

pub fn new_query_test() {
  let query = ast.new("users")

  case query.source {
    TableSource(table, None) -> table |> should.equal("users")
    _ -> should.fail()
  }

  query.select |> should.equal(SelectAll)
  query.wheres |> should.equal([])
  query.order_bys |> should.equal([])
  query.limit |> should.equal(None)
  query.offset |> should.equal(None)
  query.joins |> should.equal([])
  query.distinct |> should.be_false
}

pub fn new_qualified_query_test() {
  let query = ast.new_qualified("public", "users")

  case query.source {
    TableSource(table, Some(schema)) -> {
      table |> should.equal("users")
      schema |> should.equal("public")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// VALUE TYPES TESTS
// ============================================================================

pub fn int_value_test() {
  let val: Value = IntValue(42)
  case val {
    IntValue(n) -> n |> should.equal(42)
    _ -> should.fail()
  }
}

pub fn string_value_test() {
  let val: Value = StringValue("hello")
  case val {
    StringValue(s) -> s |> should.equal("hello")
    _ -> should.fail()
  }
}

pub fn value_types_are_distinct_test() {
  // Ensure different value types are distinguishable
  let int_val = IntValue(1)
  let str_val = StringValue("1")

  case int_val {
    IntValue(_) -> should.be_true(True)
    _ -> should.fail()
  }

  case str_val {
    StringValue(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// CONDITION TYPES TESTS
// ============================================================================

pub fn eq_condition_test() {
  let cond: Condition = Eq("id", IntValue(1))

  case cond {
    Eq(field, IntValue(val)) -> {
      field |> should.equal("id")
      val |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn gt_condition_test() {
  let cond: Condition = Gt("age", IntValue(18))

  case cond {
    Gt(field, IntValue(val)) -> {
      field |> should.equal("age")
      val |> should.equal(18)
    }
    _ -> should.fail()
  }
}

pub fn and_condition_test() {
  let cond: Condition =
    ast.And([Eq("active", ast.BoolValue(True)), Gt("age", IntValue(18))])

  case cond {
    ast.And(conditions) -> {
      conditions
      |> list.length
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn or_condition_test() {
  let cond: Condition =
    ast.Or([Eq("role", StringValue("admin")), Eq("role", StringValue("mod"))])

  case cond {
    ast.Or(conditions) -> {
      conditions
      |> list.length
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn not_condition_test() {
  let cond: Condition = ast.Not(Eq("deleted", ast.BoolValue(True)))

  case cond {
    ast.Not(inner) -> {
      case inner {
        Eq(field, _) -> field |> should.equal("deleted")
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// ============================================================================
// ORDER BY TESTS
// ============================================================================

pub fn order_by_asc_test() {
  let ob: OrderBy = ast.order_by_asc("name")

  ob.field |> should.equal("name")
  ob.direction |> should.equal(Asc)
  ob.nulls |> should.equal(NullsDefault)
}

pub fn order_by_desc_test() {
  let ob: OrderBy = ast.order_by_desc("created_at")

  ob.field |> should.equal("created_at")
  ob.direction |> should.equal(Desc)
}

pub fn order_by_with_nulls_test() {
  let ob: OrderBy = OrderBy("score", Desc, NullsLast)

  ob.field |> should.equal("score")
  ob.direction |> should.equal(Desc)
  ob.nulls |> should.equal(NullsLast)
}

pub fn nulls_order_types_test() {
  // Verify all nulls order types are distinct
  let default = NullsDefault
  let first = NullsFirst
  let last = NullsLast

  case default {
    NullsDefault -> should.be_true(True)
    _ -> should.fail()
  }

  case first {
    NullsFirst -> should.be_true(True)
    _ -> should.fail()
  }

  case last {
    NullsLast -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// SELECT TESTS
// ============================================================================

pub fn select_all_test() {
  let sel: Select = SelectAll

  case sel {
    SelectAll -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn select_fields_test() {
  let sel: Select = SelectFields(["id", "name", "email"])

  case sel {
    SelectFields(fields) -> {
      fields
      |> list.length
      |> should.equal(3)
      fields |> should.equal(["id", "name", "email"])
    }
    _ -> should.fail()
  }
}

// ============================================================================
// JOIN TESTS
// ============================================================================

pub fn inner_join_test() {
  let join: ast.Join =
    ast.Join(
      join_type: ast.InnerJoin,
      table: "posts",
      table_alias: None,
      on: Eq("users.id", ast.ParamValue(1)),
    )

  join.join_type |> should.equal(ast.InnerJoin)
  join.table |> should.equal("posts")
  join.table_alias |> should.equal(None)
}

pub fn left_join_with_alias_test() {
  let join: ast.Join =
    ast.Join(
      join_type: ast.LeftJoin,
      table: "comments",
      table_alias: Some("c"),
      on: Eq("posts.id", ast.ParamValue(1)),
    )

  join.join_type |> should.equal(ast.LeftJoin)
  join.table |> should.equal("comments")
  join.table_alias |> should.equal(Some("c"))
}

pub fn join_types_test() {
  // Verify all join types are distinct
  let inner = ast.InnerJoin
  let left = ast.LeftJoin
  let right = ast.RightJoin
  let full = ast.FullJoin
  let cross = ast.CrossJoin

  case inner {
    ast.InnerJoin -> should.be_true(True)
    _ -> should.fail()
  }

  case left {
    ast.LeftJoin -> should.be_true(True)
    _ -> should.fail()
  }

  case right {
    ast.RightJoin -> should.be_true(True)
    _ -> should.fail()
  }

  case full {
    ast.FullJoin -> should.be_true(True)
    _ -> should.fail()
  }

  case cross {
    ast.CrossJoin -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// SOURCE TESTS
// ============================================================================

pub fn table_source_test() {
  let src: Source = TableSource("users", None)

  case src {
    TableSource(table, None) -> table |> should.equal("users")
    _ -> should.fail()
  }
}

pub fn qualified_table_source_test() {
  let src: Source = TableSource("users", Some("public"))

  case src {
    TableSource(table, Some(schema)) -> {
      table |> should.equal("users")
      schema |> should.equal("public")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// IMPORTS
// ============================================================================

import gleam/list
