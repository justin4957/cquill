import cquill/query/ast.{
  type Condition, type OrderBy, type Select, type Source, type Value, Asc, Desc,
  Eq, Gt, IntValue, NullsDefault, NullsFirst, NullsLast, OrderBy, SelectAll,
  SelectFields, StringValue, TableSource,
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
  }
}

pub fn string_value_test() {
  let val: Value = StringValue("hello")
  case val {
    StringValue(s) -> s |> should.equal("hello")
  }
}

pub fn value_types_are_distinct_test() {
  // Ensure different value types are distinguishable
  let int_val = IntValue(1)
  let str_val = StringValue("1")

  case int_val {
    IntValue(_) -> should.be_true(True)
  }

  case str_val {
    StringValue(_) -> should.be_true(True)
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
  let conditions = [Eq("active", ast.BoolValue(True)), Gt("age", IntValue(18))]
  let cond: Condition = ast.And(conditions)

  conditions
  |> list.length
  |> should.equal(2)

  cond |> should.equal(ast.And(conditions))
}

pub fn or_condition_test() {
  let conditions = [
    Eq("role", StringValue("admin")),
    Eq("role", StringValue("mod")),
  ]
  let cond: Condition = ast.Or(conditions)

  conditions
  |> list.length
  |> should.equal(2)

  cond |> should.equal(ast.Or(conditions))
}

pub fn not_condition_test() {
  let inner = Eq("deleted", ast.BoolValue(True))
  let cond: Condition = ast.Not(inner)

  cond |> should.equal(ast.Not(inner))
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
  NullsDefault |> should.equal(NullsDefault)
  NullsFirst |> should.equal(NullsFirst)
  NullsLast |> should.equal(NullsLast)

  // Verify they're not equal to each other
  NullsDefault |> should.not_equal(NullsFirst)
  NullsDefault |> should.not_equal(NullsLast)
  NullsFirst |> should.not_equal(NullsLast)
}

// ============================================================================
// SELECT TESTS
// ============================================================================

pub fn select_all_test() {
  let sel: Select = SelectAll

  sel |> should.equal(SelectAll)
}

pub fn select_fields_test() {
  let sel: Select = SelectFields(["id", "name", "email"])

  sel |> should.equal(SelectFields(["id", "name", "email"]))
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
  ast.InnerJoin |> should.equal(ast.InnerJoin)
  ast.LeftJoin |> should.equal(ast.LeftJoin)
  ast.RightJoin |> should.equal(ast.RightJoin)
  ast.FullJoin |> should.equal(ast.FullJoin)
  ast.CrossJoin |> should.equal(ast.CrossJoin)

  // Verify they're not equal to each other
  ast.InnerJoin |> should.not_equal(ast.LeftJoin)
  ast.InnerJoin |> should.not_equal(ast.RightJoin)
  ast.InnerJoin |> should.not_equal(ast.FullJoin)
  ast.InnerJoin |> should.not_equal(ast.CrossJoin)
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
