// Property Test Generators for cquill
//
// This module provides generators for creating random query components
// for property-based testing. Generators create pseudo-random but
// deterministic values based on a seed, enabling reproducible tests.

import cquill/query/ast.{
  type Condition, type Direction, type JoinType, type NullsOrder, type OrderBy,
  type Query, type Select, type Value, And, Asc, Between, BoolValue, CrossJoin,
  Desc, Eq, FloatValue, FullJoin, Gt, Gte, ILike, In, InnerJoin, IntValue,
  IsNotNull, IsNull, Join, LeftJoin, Like, Lt, Lte, Not, NotEq, NotILike, NotIn,
  NotLike, NullValue, NullsDefault, NullsFirst, NullsLast, Or, OrderBy as OB,
  Query as Q, RightJoin, SelectAll, SelectFields, StringValue, TableSource,
}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

// ============================================================================
// RANDOM NUMBER GENERATOR
// ============================================================================

/// Simple linear congruential generator for deterministic pseudo-random numbers.
/// Using constants from Numerical Recipes.
pub type Seed {
  Seed(state: Int)
}

/// Create a new seed from an integer
pub fn seed(n: Int) -> Seed {
  Seed(state: int.absolute_value(n))
}

/// Generate a random integer in range [0, max) and return the new seed
pub fn random_int(s: Seed, max: Int) -> #(Int, Seed) {
  let a = 1_664_525
  let c = 1_013_904_223
  let m = 2_147_483_648
  // 2^31
  let new_state = { s.state * a + c } % m
  let value = int.absolute_value(new_state) % int.max(max, 1)
  #(value, Seed(state: new_state))
}

/// Generate a random boolean
pub fn random_bool(s: Seed) -> #(Bool, Seed) {
  let #(n, s2) = random_int(s, 2)
  #(n == 1, s2)
}

/// Pick a random element from a list
pub fn random_element(s: Seed, items: List(a)) -> #(a, Seed) {
  let len = list.length(items)
  let #(idx, s2) = random_int(s, len)
  let item = case get_at(items, idx) {
    Ok(x) -> x
    Error(_) ->
      case items {
        [first, ..] -> first
        [] -> panic as "random_element called with empty list"
      }
  }
  #(item, s2)
}

/// Get element at index (helper since list.at doesn't exist)
fn get_at(items: List(a), index: Int) -> Result(a, Nil) {
  do_get_at(items, index, 0)
}

fn do_get_at(items: List(a), target: Int, current: Int) -> Result(a, Nil) {
  case items {
    [] -> Error(Nil)
    [first, ..rest] ->
      case current == target {
        True -> Ok(first)
        False -> do_get_at(rest, target, current + 1)
      }
  }
}

/// Generate a list of n items using a generator function
pub fn random_list(
  s: Seed,
  count: Int,
  generator: fn(Seed) -> #(a, Seed),
) -> #(List(a), Seed) {
  do_random_list(s, count, generator, [])
}

fn do_random_list(
  s: Seed,
  count: Int,
  generator: fn(Seed) -> #(a, Seed),
  acc: List(a),
) -> #(List(a), Seed) {
  case count <= 0 {
    True -> #(list.reverse(acc), s)
    False -> {
      let #(item, s2) = generator(s)
      do_random_list(s2, count - 1, generator, [item, ..acc])
    }
  }
}

// ============================================================================
// FIELD NAME GENERATORS
// ============================================================================

/// Common database field names for realistic testing
const common_field_names = [
  "id", "name", "email", "status", "created_at", "updated_at", "active", "age",
  "price", "quantity", "user_id", "order_id", "title", "description", "type",
  "category", "score", "count", "amount", "is_deleted",
]

/// Generate a random field name
pub fn random_field_name(s: Seed) -> #(String, Seed) {
  random_element(s, common_field_names)
}

// ============================================================================
// TABLE NAME GENERATORS
// ============================================================================

/// Common table names for realistic testing
const common_table_names = [
  "users", "orders", "products", "comments", "posts", "sessions", "logs",
  "events", "categories", "tags", "items", "accounts", "profiles", "messages",
]

/// Generate a random table name
pub fn random_table_name(s: Seed) -> #(String, Seed) {
  random_element(s, common_table_names)
}

// ============================================================================
// VALUE GENERATORS
// ============================================================================

/// Generate a random integer value
pub fn random_int_value(s: Seed) -> #(Value, Seed) {
  let #(n, s2) = random_int(s, 2000)
  let value = n - 1000
  // Range: -1000 to 999
  #(IntValue(value), s2)
}

/// Generate a random positive integer value
pub fn random_positive_int_value(s: Seed) -> #(Value, Seed) {
  let #(n, s2) = random_int(s, 1000)
  #(IntValue(n + 1), s2)
}

/// Generate a random float value
pub fn random_float_value(s: Seed) -> #(Value, Seed) {
  let #(n, s2) = random_int(s, 10_000)
  let value = int.to_float(n - 5000) /. 100.0
  #(FloatValue(value), s2)
}

/// Sample strings for testing
const sample_strings = [
  "test", "hello", "world", "foo", "bar", "alice", "bob", "admin", "user",
  "active", "pending", "completed", "", "test@example.com", "John Doe",
]

/// Generate a random string value
pub fn random_string_value(s: Seed) -> #(Value, Seed) {
  let #(str, s2) = random_element(s, sample_strings)
  #(StringValue(str), s2)
}

/// Generate a random boolean value
pub fn random_bool_value(s: Seed) -> #(Value, Seed) {
  let #(b, s2) = random_bool(s)
  #(BoolValue(b), s2)
}

/// Generate a random null value
pub fn random_null_value(s: Seed) -> #(Value, Seed) {
  #(NullValue, s)
}

/// Generate a random value of any type
pub fn random_value(s: Seed) -> #(Value, Seed) {
  let #(choice, s2) = random_int(s, 5)
  case choice {
    0 -> random_int_value(s2)
    1 -> random_float_value(s2)
    2 -> random_string_value(s2)
    3 -> random_bool_value(s2)
    _ -> random_null_value(s2)
  }
}

// ============================================================================
// CONDITION GENERATORS
// ============================================================================

/// Generate a simple (non-nested) condition
pub fn random_simple_condition(s: Seed) -> #(Condition, Seed) {
  let #(choice, s2) = random_int(s, 12)
  let #(field, s3) = random_field_name(s2)

  case choice {
    0 -> {
      let #(val, s4) = random_value(s3)
      #(Eq(field, val), s4)
    }
    1 -> {
      let #(val, s4) = random_value(s3)
      #(NotEq(field, val), s4)
    }
    2 -> {
      let #(val, s4) = random_int_value(s3)
      #(Gt(field, val), s4)
    }
    3 -> {
      let #(val, s4) = random_int_value(s3)
      #(Gte(field, val), s4)
    }
    4 -> {
      let #(val, s4) = random_int_value(s3)
      #(Lt(field, val), s4)
    }
    5 -> {
      let #(val, s4) = random_int_value(s3)
      #(Lte(field, val), s4)
    }
    6 -> {
      let #(count, s4) = random_int(s3, 5)
      let #(vals, s5) = random_list(s4, count + 1, random_value)
      #(In(field, vals), s5)
    }
    7 -> {
      let #(count, s4) = random_int(s3, 5)
      let #(vals, s5) = random_list(s4, count + 1, random_value)
      #(NotIn(field, vals), s5)
    }
    8 -> {
      let #(pattern, s4) =
        random_element(s3, ["%test%", "test%", "%test", "t_st"])
      #(Like(field, pattern), s4)
    }
    9 -> {
      let #(pattern, s4) =
        random_element(s3, ["%test%", "test%", "%test", "t_st"])
      #(ILike(field, pattern), s4)
    }
    10 -> #(IsNull(field), s3)
    _ -> #(IsNotNull(field), s3)
  }
}

/// Generate a condition with optional nesting (controlled depth)
pub fn random_condition(s: Seed, max_depth: Int) -> #(Condition, Seed) {
  case max_depth <= 0 {
    True -> random_simple_condition(s)
    False -> {
      let #(choice, s2) = random_int(s, 10)
      case choice {
        // 70% chance of simple condition
        0 | 1 | 2 | 3 | 4 | 5 | 6 -> random_simple_condition(s2)
        // 10% chance of AND
        7 -> {
          let #(count, s3) = random_int(s2, 3)
          let #(conds, s4) =
            random_list(s3, count + 2, fn(seed) {
              random_condition(seed, max_depth - 1)
            })
          #(And(conds), s4)
        }
        // 10% chance of OR
        8 -> {
          let #(count, s3) = random_int(s2, 3)
          let #(conds, s4) =
            random_list(s3, count + 2, fn(seed) {
              random_condition(seed, max_depth - 1)
            })
          #(Or(conds), s4)
        }
        // 10% chance of NOT
        _ -> {
          let #(cond, s3) = random_condition(s2, max_depth - 1)
          #(Not(cond), s3)
        }
      }
    }
  }
}

/// Generate a between condition
pub fn random_between_condition(s: Seed) -> #(Condition, Seed) {
  let #(field, s2) = random_field_name(s)
  let #(low, s3) = random_int_value(s2)
  let #(high, s4) = random_int_value(s3)
  #(Between(field, low, high), s4)
}

// ============================================================================
// ORDER BY GENERATORS
// ============================================================================

/// Generate a random direction
pub fn random_direction(s: Seed) -> #(Direction, Seed) {
  let #(choice, s2) = random_int(s, 2)
  case choice {
    0 -> #(Asc, s2)
    _ -> #(Desc, s2)
  }
}

/// Generate a random nulls order
pub fn random_nulls_order(s: Seed) -> #(NullsOrder, Seed) {
  let #(choice, s2) = random_int(s, 3)
  case choice {
    0 -> #(NullsDefault, s2)
    1 -> #(NullsFirst, s2)
    _ -> #(NullsLast, s2)
  }
}

/// Generate a random order by clause
pub fn random_order_by(s: Seed) -> #(OrderBy, Seed) {
  let #(field, s2) = random_field_name(s)
  let #(dir, s3) = random_direction(s2)
  let #(nulls, s4) = random_nulls_order(s3)
  #(OB(field, dir, nulls), s4)
}

// ============================================================================
// JOIN GENERATORS
// ============================================================================

/// Generate a random join type
pub fn random_join_type(s: Seed) -> #(JoinType, Seed) {
  let #(choice, s2) = random_int(s, 5)
  case choice {
    0 -> #(InnerJoin, s2)
    1 -> #(LeftJoin, s2)
    2 -> #(RightJoin, s2)
    3 -> #(FullJoin, s2)
    _ -> #(CrossJoin, s2)
  }
}

/// Generate a random join clause
pub fn random_join(s: Seed) -> #(ast.Join, Seed) {
  let #(join_type, s2) = random_join_type(s)
  let #(table, s3) = random_table_name(s2)
  let #(cond, s4) = random_simple_condition(s3)
  let #(has_alias, s5) = random_bool(s4)
  let table_alias = case has_alias {
    True -> Some(string.slice(table, 0, 1))
    False -> None
  }
  #(Join(join_type, table, table_alias, cond), s5)
}

// ============================================================================
// SELECT GENERATORS
// ============================================================================

/// Generate a random select clause
pub fn random_select(s: Seed) -> #(Select, Seed) {
  let #(choice, s2) = random_int(s, 3)
  case choice {
    0 -> #(SelectAll, s2)
    1 -> {
      let #(count, s3) = random_int(s2, 5)
      let #(fields, s4) = random_list(s3, count + 1, random_field_name)
      #(SelectFields(fields), s4)
    }
    _ -> #(SelectAll, s2)
  }
}

// ============================================================================
// QUERY GENERATORS
// ============================================================================

/// Generate a random query with configurable complexity
pub fn random_query(s: Seed, config: QueryGenConfig) -> #(Query(Nil), Seed) {
  let #(table, s1) = random_table_name(s)
  let #(select, s2) = random_select(s1)

  // Generate WHERE conditions
  let #(where_count, s3) = random_int(s2, config.max_wheres + 1)
  let #(wheres, s4) =
    random_list(s3, where_count, fn(seed) {
      let #(cond, s_next) = random_condition(seed, config.max_condition_depth)
      #(ast.Where(cond), s_next)
    })

  // Generate ORDER BY clauses
  let #(order_count, s5) = random_int(s4, config.max_order_bys + 1)
  let #(order_bys, s6) = random_list(s5, order_count, random_order_by)

  // Generate LIMIT and OFFSET
  let #(has_limit, s7) = random_bool(s6)
  let #(limit, s8) = case has_limit {
    True -> {
      let #(n, s_next) = random_int(s7, 100)
      #(Some(n + 1), s_next)
    }
    False -> #(None, s7)
  }

  let #(has_offset, s9) = random_bool(s8)
  let #(offset, s10) = case has_offset {
    True -> {
      let #(n, s_next) = random_int(s9, 100)
      #(Some(n), s_next)
    }
    False -> #(None, s9)
  }

  // Generate JOINs
  let #(join_count, s11) = random_int(s10, config.max_joins + 1)
  let #(joins, s12) = random_list(s11, join_count, random_join)

  // Generate DISTINCT
  let #(is_distinct, s13) = random_bool(s12)

  // Generate GROUP BY
  let #(group_count, s14) = random_int(s13, config.max_group_bys + 1)
  let #(group_bys, s15) = random_list(s14, group_count, random_field_name)

  // Generate HAVING (only if GROUP BY exists)
  let #(havings, s16) = case group_count > 0 {
    True -> {
      let #(having_count, s_next) = random_int(s15, 2)
      random_list(s_next, having_count, fn(seed) {
        let #(cond, s_n) = random_simple_condition(seed)
        #(ast.Where(cond), s_n)
      })
    }
    False -> #([], s15)
  }

  let query =
    Q(
      source: TableSource(table, None),
      select: select,
      wheres: wheres,
      order_bys: order_bys,
      limit: limit,
      offset: offset,
      joins: joins,
      distinct: is_distinct,
      group_bys: group_bys,
      havings: havings,
    )

  #(query, s16)
}

/// Configuration for query generation complexity
pub type QueryGenConfig {
  QueryGenConfig(
    max_wheres: Int,
    max_order_bys: Int,
    max_joins: Int,
    max_group_bys: Int,
    max_condition_depth: Int,
  )
}

/// Simple query generation config (good for most tests)
pub fn simple_config() -> QueryGenConfig {
  QueryGenConfig(
    max_wheres: 3,
    max_order_bys: 2,
    max_joins: 1,
    max_group_bys: 2,
    max_condition_depth: 1,
  )
}

/// Complex query generation config (for stress testing)
pub fn complex_config() -> QueryGenConfig {
  QueryGenConfig(
    max_wheres: 5,
    max_order_bys: 3,
    max_joins: 3,
    max_group_bys: 3,
    max_condition_depth: 3,
  )
}

/// Minimal query generation config (simple queries only)
pub fn minimal_config() -> QueryGenConfig {
  QueryGenConfig(
    max_wheres: 1,
    max_order_bys: 1,
    max_joins: 0,
    max_group_bys: 0,
    max_condition_depth: 0,
  )
}

// ============================================================================
// UNICODE STRING GENERATORS
// ============================================================================

/// Unicode test strings for value handling tests
const unicode_strings = [
  // Japanese
  "\u{3053}\u{3093}\u{306B}\u{3061}\u{306F}",
  // Chinese
  "\u{4F60}\u{597D}",
  // Korean
  "\u{C548}\u{B155}\u{D558}\u{C138}\u{C694}",
  // Arabic
  "\u{0645}\u{0631}\u{062D}\u{0628}\u{0627}",
  // Emoji
  "\u{1F44B}\u{1F30D}\u{2764}\u{FE0F}",
  // Mixed
  "Hello \u{4E16}\u{754C} \u{1F30E}",
  // Special chars
  "O'Brien", "test\"quote", "back\\slash", "new\nline", "tab\there",
]

/// Generate a random unicode string value
pub fn random_unicode_string_value(s: Seed) -> #(Value, Seed) {
  let #(str, s2) = random_element(s, unicode_strings)
  #(StringValue(str), s2)
}

// ============================================================================
// BOUNDARY VALUE GENERATORS
// ============================================================================

/// Boundary integer values for edge case testing
const boundary_ints = [
  0, 1, -1, 2_147_483_647, -2_147_483_648, 999_999_999, -999_999_999,
]

/// Generate a boundary integer value
pub fn random_boundary_int_value(s: Seed) -> #(Value, Seed) {
  let #(val, s2) = random_element(s, boundary_ints)
  #(IntValue(val), s2)
}

/// Empty and edge case strings
const edge_case_strings = [
  "",
  // Empty
  " ",
  // Single space
  "   ",
  // Multiple spaces
  "\t",
  // Tab
  "\n",
  // Newline
  "NULL",
  // Literal NULL string
  "null",
  // Lowercase null
  "'",
  // Single quote
  "\"",
  // Double quote
  "\\",
  // Backslash
  "'; DROP TABLE users; --",
  // SQL injection attempt
  "<script>alert('xss')</script>",
]

/// Generate an edge case string value
pub fn random_edge_case_string_value(s: Seed) -> #(Value, Seed) {
  let #(str, s2) = random_element(s, edge_case_strings)
  #(StringValue(str), s2)
}
