# Pagination Recipes

Patterns for implementing pagination in cquill applications.

## Offset-Based Pagination

The simplest pagination approach, using LIMIT and OFFSET.

### Basic Implementation

```gleam
import cquill/query
import cquill/query/ast
import cquill/repo

pub type Page(a) {
  Page(
    items: List(a),
    page: Int,
    per_page: Int,
    total_count: Int,
    total_pages: Int,
  )
}

pub fn paginate(
  adapter,
  base_query: query.Query(a),
  page: Int,
  per_page: Int,
) -> Result(Page(Row), error.AdapterError) {
  let page = int.max(1, page)
  let per_page = int.clamp(per_page, 1, 100)
  let offset = (page - 1) * per_page

  // Get total count
  use total_count <- result.try(get_count(adapter, base_query))

  // Get page items
  let paginated_query =
    base_query
    |> query.limit(per_page)
    |> query.offset(offset)

  use items <- result.try(repo.all(adapter, paginated_query))

  let total_pages = { total_count + per_page - 1 } / per_page

  Ok(Page(
    items: items,
    page: page,
    per_page: per_page,
    total_count: total_count,
    total_pages: total_pages,
  ))
}

fn get_count(adapter, base_query) -> Result(Int, error.AdapterError) {
  let count_query =
    query.from(get_table_name(base_query))
    |> copy_where_conditions(base_query)
    |> query.select_expr([ast.AggregateExpr(ast.Count, "*")])

  use rows <- result.try(repo.all(adapter, count_query))

  case rows {
    [row] -> {
      case dict.get(row, "count") {
        Ok(ast.IntValue(n)) -> Ok(n)
        _ -> Ok(0)
      }
    }
    _ -> Ok(0)
  }
}
```

### Usage

```gleam
// Get page 2 with 20 items per page
let users_query =
  query.from("users")
  |> query.where(query.eq("active", ast.BoolValue(True)))
  |> query.order_by_asc("name")

case paginate(adapter, users_query, 2, 20) {
  Ok(page) -> {
    io.println("Page " <> int.to_string(page.page) <> " of " <> int.to_string(page.total_pages))
    io.println("Showing " <> int.to_string(list.length(page.items)) <> " of " <> int.to_string(page.total_count))
    list.each(page.items, display_user)
  }
  Error(e) -> handle_error(e)
}
```

### Pros and Cons

**Pros:**
- Simple to implement
- Can jump to any page
- Easy to understand

**Cons:**
- Performance degrades with large offsets
- Inconsistent results if data changes between pages
- Not suitable for real-time data

## Cursor-Based Pagination

Better performance for large datasets, using a cursor (typically the last ID or timestamp).

### Basic Implementation

```gleam
pub type CursorPage(a, cursor) {
  CursorPage(
    items: List(a),
    next_cursor: Option(cursor),
    has_more: Bool,
  )
}

pub fn paginate_by_cursor(
  adapter,
  base_query: query.Query(a),
  cursor: Option(Int),  // Previous page's last ID
  limit: Int,
) -> Result(CursorPage(Row, Int), error.AdapterError) {
  let limit = int.clamp(limit, 1, 100)

  // Build query with cursor condition
  let query = case cursor {
    Some(last_id) ->
      base_query
      |> query.where(query.gt("id", ast.IntValue(last_id)))
    None -> base_query
  }

  // Fetch limit + 1 to check if there are more
  let query =
    query
    |> query.order_by_asc("id")
    |> query.limit(limit + 1)

  use rows <- result.try(repo.all(adapter, query))

  let has_more = list.length(rows) > limit
  let items = list.take(rows, limit)

  let next_cursor = case list.last(items) {
    Ok(last_item) -> {
      case dict.get(last_item, "id") {
        Ok(ast.IntValue(id)) -> Some(id)
        _ -> None
      }
    }
    Error(_) -> None
  }

  Ok(CursorPage(
    items: items,
    next_cursor: next_cursor,
    has_more: has_more,
  ))
}
```

### Usage

```gleam
// First page
case paginate_by_cursor(adapter, users_query, None, 20) {
  Ok(page) -> {
    display_items(page.items)

    case page.has_more, page.next_cursor {
      True, Some(cursor) -> {
        // Fetch next page
        paginate_by_cursor(adapter, users_query, Some(cursor), 20)
      }
      _, _ -> Ok(page)
    }
  }
  Error(e) -> Error(e)
}
```

### Timestamp-Based Cursor

For time-ordered data:

```gleam
pub fn paginate_by_timestamp(
  adapter,
  base_query: query.Query(a),
  cursor: Option(String),  // ISO timestamp
  limit: Int,
) -> Result(CursorPage(Row, String), error.AdapterError) {
  let query = case cursor {
    Some(timestamp) ->
      base_query
      |> query.where(query.lt("created_at", ast.StringValue(timestamp)))
    None -> base_query
  }

  let query =
    query
    |> query.order_by_desc("created_at")
    |> query.limit(limit + 1)

  use rows <- result.try(repo.all(adapter, query))

  let has_more = list.length(rows) > limit
  let items = list.take(rows, limit)

  let next_cursor = case list.last(items) {
    Ok(last_item) -> {
      case dict.get(last_item, "created_at") {
        Ok(ast.StringValue(ts)) -> Some(ts)
        _ -> None
      }
    }
    Error(_) -> None
  }

  Ok(CursorPage(items: items, next_cursor: next_cursor, has_more: has_more))
}
```

### Pros and Cons

**Pros:**
- Consistent performance regardless of page depth
- Stable results even with data changes
- Better for infinite scroll UIs

**Cons:**
- Can't jump to arbitrary pages
- More complex to implement
- Requires stable, unique, ordered column

## Keyset Pagination

A variant of cursor pagination using composite keys.

### Implementation

```gleam
pub type KeysetCursor {
  KeysetCursor(created_at: String, id: Int)
}

pub fn paginate_by_keyset(
  adapter,
  base_query: query.Query(a),
  cursor: Option(KeysetCursor),
  limit: Int,
) -> Result(CursorPage(Row, KeysetCursor), error.AdapterError) {
  let query = case cursor {
    Some(KeysetCursor(created_at, id)) ->
      base_query
      |> query.where(query.or_where([
        // Either older timestamp
        query.lt("created_at", ast.StringValue(created_at)),
        // Or same timestamp but lower ID (tie-breaker)
        query.and_where([
          query.eq("created_at", ast.StringValue(created_at)),
          query.lt("id", ast.IntValue(id)),
        ]),
      ]))
    None -> base_query
  }

  let query =
    query
    |> query.order_by_desc("created_at")
    |> query.order_by_desc("id")
    |> query.limit(limit + 1)

  use rows <- result.try(repo.all(adapter, query))

  let has_more = list.length(rows) > limit
  let items = list.take(rows, limit)

  let next_cursor = case list.last(items) {
    Ok(last_item) -> {
      case dict.get(last_item, "created_at"), dict.get(last_item, "id") {
        Ok(ast.StringValue(ts)), Ok(ast.IntValue(id)) ->
          Some(KeysetCursor(ts, id))
        _, _ -> None
      }
    }
    Error(_) -> None
  }

  Ok(CursorPage(items: items, next_cursor: next_cursor, has_more: has_more))
}
```

## Seek Pagination

Optimized for large datasets with filtering.

```gleam
pub fn seek_paginate(
  adapter,
  table: String,
  filters: List(#(String, ast.Value)),
  order_by: String,
  seek_value: Option(ast.Value),
  limit: Int,
) -> Result(List(Row), error.AdapterError) {
  let base = query.from(table)

  // Apply filters
  let query = list.fold(filters, base, fn(q, filter) {
    let #(field, value) = filter
    q |> query.where(query.eq(field, value))
  })

  // Apply seek condition
  let query = case seek_value {
    Some(value) -> query |> query.where(query.gt(order_by, value))
    None -> query
  }

  query
  |> query.order_by_asc(order_by)
  |> query.limit(limit)
  |> repo.all(adapter, _)
}
```

## Pagination Helpers

### Page Info

```gleam
pub type PageInfo {
  PageInfo(
    current_page: Int,
    per_page: Int,
    total_count: Int,
    total_pages: Int,
    has_previous: Bool,
    has_next: Bool,
    start_index: Int,
    end_index: Int,
  )
}

pub fn page_info(page: Int, per_page: Int, total_count: Int) -> PageInfo {
  let total_pages = case total_count {
    0 -> 1
    n -> { n + per_page - 1 } / per_page
  }

  let start_index = { page - 1 } * per_page + 1
  let end_index = int.min(page * per_page, total_count)

  PageInfo(
    current_page: page,
    per_page: per_page,
    total_count: total_count,
    total_pages: total_pages,
    has_previous: page > 1,
    has_next: page < total_pages,
    start_index: start_index,
    end_index: end_index,
  )
}
```

### Encode/Decode Cursor

For API responses:

```gleam
import gleam/bit_array
import gleam/base64

pub fn encode_cursor(cursor: KeysetCursor) -> String {
  let data = cursor.created_at <> "|" <> int.to_string(cursor.id)
  base64.encode(bit_array.from_string(data))
}

pub fn decode_cursor(encoded: String) -> Result(KeysetCursor, Nil) {
  use decoded <- result.try(base64.decode(encoded))
  use string <- result.try(bit_array.to_string(decoded))

  case string.split(string, "|") {
    [created_at, id_str] -> {
      use id <- result.try(int.parse(id_str))
      Ok(KeysetCursor(created_at, id))
    }
    _ -> Error(Nil)
  }
}
```

## Testing Pagination

```gleam
pub fn pagination_returns_correct_page_test() {
  let store = setup_test_store_with_100_users()

  let query = query.from("users") |> query.order_by_asc("id")

  case paginate(store, query, 3, 10) {
    Ok(page) -> {
      page.page |> should.equal(3)
      page.per_page |> should.equal(10)
      page.total_count |> should.equal(100)
      page.total_pages |> should.equal(10)
      list.length(page.items) |> should.equal(10)

      // First item on page 3 should have id 21
      case list.first(page.items) {
        Ok(item) -> {
          dict.get(item, "id") |> should.equal(Ok(ast.IntValue(21)))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn cursor_pagination_handles_empty_result_test() {
  let store = setup_empty_test_store()

  let query = query.from("users")

  case paginate_by_cursor(store, query, None, 10) {
    Ok(page) -> {
      page.items |> should.equal([])
      page.has_more |> should.be_false()
      page.next_cursor |> should.be_none()
    }
    Error(_) -> should.fail()
  }
}
```
