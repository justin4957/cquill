import cquill/query
import cquill/query/ast.{IntValue}
import cquill/query/builder
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
  |> schema.field(field.integer("user_id") |> field.nullable)
  |> schema.field(field.datetime("created_at"))
  |> schema.field(field.datetime("deleted_at") |> field.nullable)
  |> schema.primary_key(["id"])
}

// ============================================================================
// FILTER BUILDER TESTS
// ============================================================================

pub fn filter_test() {
  let active_filter = builder.filter(query.eq("active", True))

  let q =
    query.from(user_schema())
    |> active_filter

  query.condition_count(q) |> should.equal(1)

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Eq("active", ast.BoolValue(True))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn filter_eq_test() {
  let id_filter = builder.filter_eq("id", 42)

  let q =
    query.from(user_schema())
    |> id_filter

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Eq("id", IntValue(42))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn filter_eq_string_value_test() {
  let email_filter = builder.filter_eq("email", "test@example.com")

  let q =
    query.from(user_schema())
    |> email_filter

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Eq("email", ast.StringValue("test@example.com"))] ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn filter_not_null_test() {
  let not_null_filter = builder.filter_not_null("email")

  let q =
    query.from(user_schema())
    |> not_null_filter

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.IsNotNull("email")] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn filter_null_test() {
  let null_filter = builder.filter_null("deleted_at")

  let q =
    query.from(user_schema())
    |> null_filter

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.IsNull("deleted_at")] -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// MODIFIER COMPOSITION TESTS
// ============================================================================

pub fn compose_test() {
  let active = builder.filter(query.eq("active", True))
  let adult = builder.filter(query.gt("age", 18))
  let limited = builder.with_limit(10)

  let composed = builder.compose([active, adult, limited])

  let q =
    query.from(user_schema())
    |> composed

  query.condition_count(q) |> should.equal(2)
  query.get_limit(q) |> should.equal(Some(10))
}

pub fn and_then_test() {
  let active = builder.filter(query.eq("active", True))
  let limited = builder.with_limit(5)

  let combined = builder.and_then(active, limited)

  let q =
    query.from(user_schema())
    |> combined

  query.condition_count(q) |> should.equal(1)
  query.get_limit(q) |> should.equal(Some(5))
}

pub fn identity_test() {
  let q1 = query.from(user_schema())
  let q2 =
    query.from(user_schema())
    |> builder.identity()

  // Identity should not change the query
  query.get_conditions(q1) |> should.equal(query.get_conditions(q2))
  query.get_limit(q1) |> should.equal(query.get_limit(q2))
}

pub fn when_true_test() {
  let should_filter = True
  let conditional = builder.when(should_filter, builder.with_limit(10))

  let q =
    query.from(user_schema())
    |> conditional

  query.get_limit(q) |> should.equal(Some(10))
}

pub fn when_false_test() {
  let should_filter = False
  let conditional = builder.when(should_filter, builder.with_limit(10))

  let q =
    query.from(user_schema())
    |> conditional

  query.get_limit(q) |> should.equal(None)
}

pub fn when_some_test() {
  let maybe_limit: option.Option(Int) = Some(25)
  let conditional = builder.when_some(maybe_limit, builder.with_limit)

  let q =
    query.from(user_schema())
    |> conditional

  query.get_limit(q) |> should.equal(Some(25))
}

pub fn when_none_test() {
  let maybe_limit: option.Option(Int) = None
  let conditional = builder.when_some(maybe_limit, builder.with_limit)

  let q =
    query.from(user_schema())
    |> conditional

  query.get_limit(q) |> should.equal(None)
}

// ============================================================================
// COMMON MODIFIER TESTS
// ============================================================================

pub fn with_limit_test() {
  let q =
    query.from(user_schema())
    |> builder.with_limit(20)

  query.get_limit(q) |> should.equal(Some(20))
}

pub fn with_offset_test() {
  let q =
    query.from(user_schema())
    |> builder.with_offset(100)

  query.get_offset(q) |> should.equal(Some(100))
}

pub fn with_pagination_test() {
  let q =
    query.from(user_schema())
    |> builder.with_pagination(3, 25)

  query.get_limit(q) |> should.equal(Some(25))
  query.get_offset(q) |> should.equal(Some(50))
  // (3-1) * 25
}

pub fn order_asc_test() {
  let q =
    query.from(user_schema())
    |> builder.order_asc("name")

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("name")
      ob.direction |> should.equal(ast.Asc)
    }
    _ -> should.fail()
  }
}

pub fn order_desc_test() {
  let q =
    query.from(user_schema())
    |> builder.order_desc("created_at")

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("created_at")
      ob.direction |> should.equal(ast.Desc)
    }
    _ -> should.fail()
  }
}

pub fn with_select_test() {
  let q =
    query.from(user_schema())
    |> builder.with_select(["id", "email"])

  case query.get_select(q) {
    ast.SelectFields(fields) -> fields |> should.equal(["id", "email"])
    _ -> should.fail()
  }
}

pub fn with_distinct_test() {
  let q =
    query.from(user_schema())
    |> builder.with_distinct()

  query.is_distinct(q) |> should.be_true
}

// ============================================================================
// SCOPE TESTS
// ============================================================================

pub fn scope_test() {
  let active_scope =
    builder.scope("active", builder.filter(query.eq("active", True)))

  let q =
    query.from(user_schema())
    |> builder.apply_scope(active_scope)

  query.condition_count(q) |> should.equal(1)
}

pub fn apply_scopes_test() {
  let active_scope =
    builder.scope("active", builder.filter(query.eq("active", True)))
  let adult_scope = builder.scope("adult", builder.filter(query.gt("age", 18)))

  let q =
    query.from(user_schema())
    |> builder.apply_scopes([active_scope, adult_scope])

  query.condition_count(q) |> should.equal(2)
}

// ============================================================================
// COMMON PATTERN TESTS
// ============================================================================

pub fn not_deleted_test() {
  let q =
    query.from(user_schema())
    |> builder.not_deleted()

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.IsNull("deleted_at")] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn published_test() {
  let q =
    query.from(user_schema())
    |> builder.published()

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Eq("published", ast.BoolValue(True))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn active_test() {
  let q =
    query.from(user_schema())
    |> builder.active()

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Eq("active", ast.BoolValue(True))] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn recent_first_test() {
  let q =
    query.from(user_schema())
    |> builder.recent_first()

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("created_at")
      ob.direction |> should.equal(ast.Desc)
    }
    _ -> should.fail()
  }
}

pub fn oldest_first_test() {
  let q =
    query.from(user_schema())
    |> builder.oldest_first()

  let order_bys = query.get_order_bys(q)
  case order_bys {
    [ob] -> {
      ob.field |> should.equal("created_at")
      ob.direction |> should.equal(ast.Asc)
    }
    _ -> should.fail()
  }
}

pub fn by_user_test() {
  let q =
    query.from(user_schema())
    |> builder.by_user(42)

  let conditions = query.get_conditions(q)
  case conditions {
    [ast.Eq("user_id", IntValue(42))] -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// QUERY MERGING TESTS
// ============================================================================

pub fn merge_conditions_test() {
  let source =
    query.from_table("other")
    |> query.where(query.eq("id", 1))
    |> query.where(query.eq("active", True))

  let target = query.from(user_schema())

  let merged = builder.merge_conditions(target, source)

  query.condition_count(merged) |> should.equal(2)
}

pub fn merge_order_bys_test() {
  let source =
    query.from_table("other")
    |> query.order_by_desc("created_at")

  let target =
    query.from(user_schema())
    |> query.order_by_asc("name")

  let merged = builder.merge_order_bys(target, source)

  let order_bys = query.get_order_bys(merged)
  order_bys
  |> list.length
  |> should.equal(2)
}

pub fn merge_pagination_preserves_target_test() {
  let source =
    query.from_table("other")
    |> query.limit(100)
    |> query.offset(50)

  let target =
    query.from(user_schema())
    |> query.limit(10)

  let merged = builder.merge_pagination(target, source)

  // Target had limit, should be preserved
  query.get_limit(merged) |> should.equal(Some(10))
  // Target had no offset, should use source's
  query.get_offset(merged) |> should.equal(Some(50))
}

// ============================================================================
// QUERY CLONING TESTS
// ============================================================================

pub fn clone_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.limit(10)

  let cloned = builder.clone(q)

  query.condition_count(cloned) |> should.equal(1)
  query.get_limit(cloned) |> should.equal(Some(10))
}

pub fn clone_without_conditions_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.limit(10)

  let cloned = builder.clone_without_conditions(q)

  query.condition_count(cloned) |> should.equal(0)
  query.get_limit(cloned) |> should.equal(Some(10))
}

pub fn clone_without_order_test() {
  let q =
    query.from(user_schema())
    |> query.order_by_desc("created_at")
    |> query.limit(10)

  let cloned = builder.clone_without_order(q)

  query.has_order_by(cloned) |> should.be_false
  query.get_limit(cloned) |> should.equal(Some(10))
}

pub fn clone_without_pagination_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.limit(10)
    |> query.offset(20)

  let cloned = builder.clone_without_pagination(q)

  query.condition_count(cloned) |> should.equal(1)
  query.has_pagination(cloned) |> should.be_false
}

// ============================================================================
// QUERY ANALYSIS TESTS
// ============================================================================

pub fn conditions_equivalent_test() {
  let q1 =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.where(query.eq("active", True))

  let q2 =
    query.from_table("other")
    |> query.where(query.eq("active", True))
    |> query.where(query.eq("id", 1))

  // Same conditions, different order
  builder.conditions_equivalent(q1, q2) |> should.be_true
}

pub fn conditions_not_equivalent_test() {
  let q1 =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))

  let q2 =
    query.from_table("other")
    |> query.where(query.eq("id", 2))

  builder.conditions_equivalent(q1, q2) |> should.be_false
}

pub fn count_conditions_deep_test() {
  let q =
    query.from(user_schema())
    |> query.where(
      query.and([
        query.eq("id", 1),
        query.or([query.eq("a", True), query.eq("b", False)]),
      ]),
    )

  // 1 AND with 2 children, one of which is an OR with 2 children
  // Total: 1 (And) + 1 (Eq) + 1 (Or) + 2 (inner Eqs) = 5
  let count = builder.count_conditions_deep(q)
  count |> should.equal(5)
}

pub fn get_condition_fields_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.eq("id", 1))
    |> query.where(query.eq("email", "test@example.com"))
    |> query.where(query.gt("age", 18))

  let fields = builder.get_condition_fields(q)

  fields |> list.contains("id") |> should.be_true
  fields |> list.contains("email") |> should.be_true
  fields |> list.contains("age") |> should.be_true
  fields |> list.length |> should.equal(3)
}

pub fn get_condition_fields_nested_test() {
  let q =
    query.from(user_schema())
    |> query.where(query.and([query.eq("id", 1), query.eq("name", "test")]))

  let fields = builder.get_condition_fields(q)

  fields |> list.contains("id") |> should.be_true
  fields |> list.contains("name") |> should.be_true
}

// ============================================================================
// COMPLEX COMPOSITION TESTS
// ============================================================================

pub fn real_world_composition_test() {
  // Simulate a real-world query building scenario
  let base_query = query.from(user_schema())

  // Define reusable scopes
  let active_users =
    builder.scope("active", builder.filter(query.eq("active", True)))
  let adult_users = builder.scope("adult", builder.filter(query.gt("age", 18)))
  let recent_first_scope = builder.scope("recent", builder.recent_first())
  let paginated = builder.scope("page_1", builder.with_pagination(1, 20))

  // Compose the query
  let final_query =
    base_query
    |> builder.apply_scopes([
      active_users,
      adult_users,
      recent_first_scope,
      paginated,
    ])

  // Verify the composed query
  query.condition_count(final_query) |> should.equal(2)
  query.has_order_by(final_query) |> should.be_true
  query.get_limit(final_query) |> should.equal(Some(20))
  query.get_offset(final_query) |> should.equal(Some(0))
}

pub fn conditional_composition_test() {
  // Simulate conditional query building based on user input
  let include_inactive = False
  let min_age: option.Option(Int) = Some(21)
  let sort_by_name = True

  let q =
    query.from(user_schema())
    |> builder.when(!include_inactive, builder.active())
    |> builder.when_some(min_age, fn(age) {
      builder.filter(query.gt("age", age))
    })
    |> builder.when(sort_by_name, builder.order_asc("name"))

  // Active filter should be applied (include_inactive is False)
  // Age filter should be applied (min_age is Some(21))
  // Sort should be applied
  query.condition_count(q) |> should.equal(2)
  query.has_order_by(q) |> should.be_true
}
