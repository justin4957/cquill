import cquill/query.{
  desc, ilike, is_not_null, is_null, like, not_ilike, not_like, nulls_last,
}
import cquill/query/ast.{
  Asc, Desc, Eq, IntValue, NullsLast, Query as QueryRecord, SelectAll,
  SelectFields, StringValue, TableSource,
}
import cquill/schema
import cquill/schema/field
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// TEST HELPERS
// ============================================================================

fn user_schema() -> schema.Schema {
  schema.new("users")
  |> schema.field(field.integer("id") |> field.primary_key)
  |> schema.field(field.string("email") |> field.not_null |> field.unique)
  |> schema.field(field.string("name") |> field.nullable)
  |> schema.field(field.boolean("active"))
  |> schema.field(field.integer("age"))
  |> schema.primary_key(["id"])
}

// ============================================================================
// QUERY CONSTRUCTION TESTS
// ============================================================================

pub fn from_schema_test() {
  let q = query.from(user_schema())

  case query.get_source(q) {
    TableSource(table, None) -> table |> should.equal("users")
    _ -> should.fail()
  }

  query.get_select(q) |> should.equal(SelectAll)
  query.get_conditions(q) |> should.equal([])
  query.get_order_bys(q) |> should.equal([])
  query.get_limit(q) |> should.equal(None)
  query.get_offset(q) |> should.equal(None)
}

pub fn from_table_test() {
  let q = query.from_table("posts")

  case query.get_source(q) {
    TableSource(table, None) -> table |> should.equal("posts")
    _ -> should.fail()
  }
}

pub fn from_qualified_test() {
  let q = query.from_qualified("public", "users")

  case query.get_source(q) {
    TableSource(table, Some(schema_name)) -> {
      table |> should.equal("users")
      schema_name |> should.equal("public")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// SELECT TESTS
// ============================================================================

pub fn select_fields_test() {
  let q =
    query.from(user_schema())
    |> query.select(["id", "email", "name"])

  case query.get_select(q) {
    SelectFields(fields) -> {
      fields |> should.equal(["id", "email", "name"])
    }
    _ -> should.fail()
  }
}

pub fn select_all_test() {
  let q =
    query.from(user_schema())
    |> query.select(["id"])
    |> query.select_all

  query.get_select(q) |> should.equal(SelectAll)
}

pub fn distinct_test() {
  let q =
    query.from(user_schema())
    |> query.distinct

  query.is_distinct(q) |> should.be_true
}

// ============================================================================
// WHERE CONDITION TESTS
// ============================================================================

pub fn where_single_condition_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))

  query.has_conditions(q) |> should.be_true
  query.condition_count(q) |> should.equal(1)

  let conditions = query.get_conditions(q)
  case conditions {
    [Eq("id", IntValue(1))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn where_multiple_conditions_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("active", True))
    |> query.where(query.gt("age", 18))

  query.condition_count(q) |> should.equal(2)
}

pub fn where_eq_string_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("email", "test@example.com"))

  let conditions = query.get_conditions(q)
  case conditions {
    [Eq("email", StringValue("test@example.com"))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn where_or_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("role", "admin"))
    |> query.or_where(query.eq("role", "moderator"))

  // or_where combines existing conditions with OR
  query.condition_count(q) |> should.equal(1)

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Or(_)] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn where_clear_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.where_clear

  query.has_conditions(q) |> should.be_false
  query.condition_count(q) |> should.equal(0)
}

pub fn where_replace_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.where(query.eq("active", True))
    |> query.where_replace(query.eq("email", "new@example.com"))

  query.condition_count(q) |> should.equal(1)

  let conditions = query.get_conditions(q)
  case conditions {
    [Eq("email", StringValue("new@example.com"))] -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// CONDITION BUILDER TESTS
// ============================================================================

pub fn eq_test() {
  let cond = query.eq("age", 25)
  case cond {
    Eq("age", IntValue(25)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn not_eq_test() {
  let cond = query.not_eq("status", "deleted")
  case cond {
    ast.NotEq("status", _) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn gt_test() {
  let cond = query.gt("age", 18)
  case cond {
    ast.Gt("age", IntValue(18)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn lt_test() {
  let cond = query.lt("age", 65)
  case cond {
    ast.Lt("age", IntValue(65)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn lte_test() {
  let cond = query.lte("quantity", 100)
  case cond {
    ast.Lte("quantity", IntValue(100)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn is_null_test() {
  let cond = is_null("deleted_at")
  case cond {
    ast.IsNull("deleted_at") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn is_not_null_test() {
  let cond = is_not_null("email")
  case cond {
    ast.IsNotNull("email") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn is_in_test() {
  let cond = query.is_in("id", [1, 2, 3])
  case cond {
    ast.In("id", values) -> {
      values
      |> list.length
      |> should.equal(3)
    }
    _ -> should.fail()
  }
}

pub fn is_in_strings_test() {
  let cond = query.is_in("status", ["pending", "approved"])
  case cond {
    ast.In("status", values) -> {
      values
      |> list.length
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn like_test() {
  let cond = like("name", "%john%")
  case cond {
    ast.Like("name", "%john%") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn not_like_test() {
  let cond = not_like("name", "%admin%")
  case cond {
    ast.NotLike("name", "%admin%") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn ilike_test() {
  let cond = ilike("email", "%@EXAMPLE.COM")
  case cond {
    ast.ILike("email", "%@EXAMPLE.COM") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn not_ilike_test() {
  let cond = not_ilike("email", "%@spam.com")
  case cond {
    ast.NotILike("email", "%@spam.com") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn and_conditions_test() {
  let cond = query.and([query.eq("active", True), query.gt("age", 18)])

  case cond {
    ast.And(conditions) -> {
      conditions
      |> list.length
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn or_conditions_test() {
  let cond =
    query.or([query.eq("role", "admin"), query.eq("role", "moderator")])

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
  let cond = query.not(query.eq("deleted", True))

  case cond {
    ast.Not(inner) -> {
      case inner {
        Eq("deleted", _) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn between_test() {
  let cond = query.between("age", 18, 65)

  case cond {
    ast.Between("age", _, _) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// ORDER BY TESTS
// ============================================================================

pub fn order_by_test() {
  let q =
    query.from(user_schema())
    |> query.order_by("created_at", desc)

  query.has_order_by(q) |> should.be_true

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("created_at")
      ob.direction |> should.equal(Desc)
    }
    _ -> should.fail()
  }
}

pub fn order_by_asc_test() {
  let q =
    query.from(user_schema())
    |> query.order_by_asc("name")

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("name")
      ob.direction |> should.equal(Asc)
    }
    _ -> should.fail()
  }
}

pub fn order_by_desc_test() {
  let q =
    query.from(user_schema())
    |> query.order_by_desc("created_at")

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("created_at")
      ob.direction |> should.equal(Desc)
    }
    _ -> should.fail()
  }
}

pub fn multiple_order_bys_test() {
  let q =
    query.from(user_schema())
    |> query.order_by_desc("created_at")
    |> query.order_by_asc("name")

  let order_bys = query.get_order_bys(q)
  order_bys
  |> list.length
  |> should.equal(2)

  case order_bys {
    [first, second] -> {
      first.field |> should.equal("created_at")
      second.field |> should.equal("name")
    }
    _ -> should.fail()
  }
}

pub fn order_by_with_nulls_test() {
  let q =
    query.from(user_schema())
    |> query.order_by_with_nulls("score", desc, nulls_last)

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("score")
      ob.direction |> should.equal(Desc)
      ob.nulls |> should.equal(NullsLast)
    }
    _ -> should.fail()
  }
}

pub fn order_by_clear_test() {
  let q =
    query.from(user_schema())
    |> query.order_by_desc("created_at")
    |> query.order_by_clear

  query.has_order_by(q) |> should.be_false
}

// ============================================================================
// PAGINATION TESTS
// ============================================================================

pub fn limit_test() {
  let q =
    query.from(user_schema())
    |> query.limit(10)

  query.get_limit(q) |> should.equal(Some(10))
  query.has_pagination(q) |> should.be_true
}

pub fn offset_test() {
  let q =
    query.from(user_schema())
    |> query.offset(20)

  query.get_offset(q) |> should.equal(Some(20))
  query.has_pagination(q) |> should.be_true
}

pub fn limit_and_offset_test() {
  let q =
    query.from(user_schema())
    |> query.limit(10)
    |> query.offset(20)

  query.get_limit(q) |> should.equal(Some(10))
  query.get_offset(q) |> should.equal(Some(20))
}

pub fn paginate_test() {
  // Page 3 with 10 items per page
  let q =
    query.from(user_schema())
    |> query.paginate(page: 3, per_page: 10)

  query.get_limit(q) |> should.equal(Some(10))
  query.get_offset(q) |> should.equal(Some(20))
  // (3-1) * 10 = 20
}

pub fn paginate_first_page_test() {
  let q =
    query.from(user_schema())
    |> query.paginate(page: 1, per_page: 25)

  query.get_limit(q) |> should.equal(Some(25))
  query.get_offset(q) |> should.equal(Some(0))
}

pub fn no_pagination_test() {
  let q =
    query.from(user_schema())
    |> query.limit(10)
    |> query.offset(20)
    |> query.no_pagination

  query.get_limit(q) |> should.equal(None)
  query.get_offset(q) |> should.equal(None)
  query.has_pagination(q) |> should.be_false
}

// ============================================================================
// JOIN TESTS
// ============================================================================

pub fn inner_join_test() {
  let q =
    query.from(user_schema())
    |> query.join("posts", on: query.eq("posts.user_id", 1))

  let joins = query.get_joins(q)
  joins
  |> list.length
  |> should.equal(1)

  case joins {
    [join] -> {
      join.join_type |> should.equal(ast.InnerJoin)
      join.table |> should.equal("posts")
    }
    _ -> should.fail()
  }
}

pub fn left_join_test() {
  let q =
    query.from(user_schema())
    |> query.left_join("posts", on: query.eq("posts.user_id", 1))

  let joins = query.get_joins(q)
  case joins {
    [join] -> join.join_type |> should.equal(ast.LeftJoin)
    _ -> should.fail()
  }
}

pub fn join_with_alias_test() {
  let q =
    query.from(user_schema())
    |> query.join_as("posts", alias: "p", on: query.eq("p.user_id", 1))

  let joins = query.get_joins(q)
  case joins {
    [join] -> {
      join.table |> should.equal("posts")
      join.table_alias |> should.equal(Some("p"))
    }
    _ -> should.fail()
  }
}

pub fn multiple_joins_test() {
  let q =
    query.from(user_schema())
    |> query.join("posts", on: query.eq("posts.user_id", 1))
    |> query.left_join("comments", on: query.eq("comments.post_id", 1))

  let joins = query.get_joins(q)
  joins
  |> list.length
  |> should.equal(2)
}

// ============================================================================
// GROUP BY / HAVING TESTS
// ============================================================================

pub fn group_by_test() {
  let q =
    query.from(user_schema())
    |> query.group_by("role")

  let QueryRecord(group_bys:, ..) = q
  group_bys |> should.equal(["role"])
}

pub fn group_by_multiple_test() {
  let q =
    query.from(user_schema())
    |> query.group_by_fields(["role", "status"])

  let QueryRecord(group_bys:, ..) = q
  group_bys |> should.equal(["role", "status"])
}

pub fn having_test() {
  let q =
    query.from(user_schema())
    |> query.group_by("role")
    |> query.having(query.gt("count", 5))

  let QueryRecord(havings:, ..) = q
  havings
  |> list.length
  |> should.equal(1)
}

// ============================================================================
// COMPOSABILITY TESTS
// ============================================================================

pub fn pipeline_composition_test() {
  // Demonstrate that queries can be built with pipelines
  let q =
    query.from(user_schema())
    |> query.select(["id", "email", "name"])
    |> query.where(query.eq("active", True))
    |> query.where(query.gt("age", 18))
    |> query.order_by_desc("created_at")
    |> query.limit(10)

  // Verify all parts are set correctly
  case query.get_select(q) {
    SelectFields(fields) -> fields |> should.equal(["id", "email", "name"])
    _ -> should.fail()
  }

  query.condition_count(q) |> should.equal(2)
  query.has_order_by(q) |> should.be_true
  query.get_limit(q) |> should.equal(Some(10))
}

pub fn reusable_filter_composition_test() {
  // Define reusable filters as functions
  let active = fn(q) { query.where(q, query.eq("active", True)) }
  let adult = fn(q) { query.where(q, query.gt("age", 18)) }
  let recent_first = fn(q) { query.order_by_desc(q, "created_at") }

  // Compose them
  let q =
    query.from(user_schema())
    |> active
    |> adult
    |> recent_first
    |> query.limit(10)

  query.condition_count(q) |> should.equal(2)
  query.has_order_by(q) |> should.be_true
  query.get_limit(q) |> should.equal(Some(10))
}

// ============================================================================
// QUERY INSPECTION TESTS
// ============================================================================

pub fn has_conditions_test() {
  let q1 = query.from(user_schema())
  query.has_conditions(q1) |> should.be_false

  let q2 =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
  query.has_conditions(q2) |> should.be_true
}

pub fn has_order_by_test() {
  let q1 = query.from(user_schema())
  query.has_order_by(q1) |> should.be_false

  let q2 =
    query.from(user_schema())
    |> query.order_by_asc("name")
  query.has_order_by(q2) |> should.be_true
}

pub fn has_pagination_test() {
  let q1 = query.from(user_schema())
  query.has_pagination(q1) |> should.be_false

  let q2 =
    query.from(user_schema())
    |> query.limit(10)
  query.has_pagination(q2) |> should.be_true

  let q3 =
    query.from(user_schema())
    |> query.offset(5)
  query.has_pagination(q3) |> should.be_true
}

pub fn is_distinct_test() {
  let q1 = query.from(user_schema())
  query.is_distinct(q1) |> should.be_false

  let q2 =
    query.from(user_schema())
    |> query.distinct
  query.is_distinct(q2) |> should.be_true
}

// ============================================================================
// DEBUG STRING TESTS
// ============================================================================

pub fn to_debug_string_basic_test() {
  let q = query.from_table("users")
  let debug = query.to_debug_string(q)

  // Should contain the table name
  debug
  |> string.contains("users")
  |> should.be_true
}

pub fn to_debug_string_with_conditions_test() {
  let q =
    query.from_table("users")
    |> query.where(query.eq("id", 1))

  let debug = query.to_debug_string(q)

  debug
  |> string.contains("id = 1")
  |> should.be_true
}

pub fn to_debug_string_with_select_test() {
  let q =
    query.from_table("users")
    |> query.select(["id", "email"])

  let debug = query.to_debug_string(q)

  debug
  |> string.contains("id, email")
  |> should.be_true
}

// ============================================================================
// IMPORTS FOR TESTS
// ============================================================================

import gleam/string
