/// Post schema definitions with multiple schema types for different use cases.
///
/// - `Post` - Full post record from database
/// - `NewPost` - Data required to create a new post
/// - `PostUpdate` - Fields that can be updated
/// - `PostWithAuthor` - Post with author information (for display)
import cquill/query/ast
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Post publication status
pub type PostStatus {
  Draft
  Published
  Archived
}

/// Full post record from database
pub type Post {
  Post(
    id: Int,
    user_id: Int,
    title: String,
    slug: String,
    body: String,
    status: PostStatus,
    published_at: Option(String),
    inserted_at: String,
    updated_at: String,
  )
}

/// Data required to create a new post
pub type NewPost {
  NewPost(user_id: Int, title: String, body: String, status: PostStatus)
}

/// Fields that can be updated on a post
pub type PostUpdate {
  PostUpdate(
    title: Option(String),
    body: Option(String),
    status: Option(PostStatus),
  )
}

/// Post with author information for display
pub type PostWithAuthor {
  PostWithAuthor(
    id: Int,
    title: String,
    slug: String,
    body: String,
    status: PostStatus,
    published_at: Option(String),
    author_name: Option(String),
    author_email: String,
  )
}

/// Table name constant
pub const table = "posts"

/// Convert status to string for storage
pub fn status_to_string(status: PostStatus) -> String {
  case status {
    Draft -> "draft"
    Published -> "published"
    Archived -> "archived"
  }
}

/// Parse status from string
pub fn status_from_string(s: String) -> Result(PostStatus, String) {
  case s {
    "draft" -> Ok(Draft)
    "published" -> Ok(Published)
    "archived" -> Ok(Archived)
    _ -> Error("Invalid status: " <> s)
  }
}

/// Generate a URL-friendly slug from a title
pub fn generate_slug(title: String) -> String {
  title
  |> string.lowercase()
  |> string.replace(" ", "-")
  |> string.replace("'", "")
  |> string.replace("\"", "")
  |> string.replace(".", "")
  |> string.replace(",", "")
  |> string.replace("!", "")
  |> string.replace("?", "")
}

/// Decode a Post from a database row
pub fn decode(row: Dict(String, ast.Value)) -> Result(Post, String) {
  use id <- result.try(get_int(row, "id"))
  use user_id <- result.try(get_int(row, "user_id"))
  use title <- result.try(get_string(row, "title"))
  use slug <- result.try(get_string(row, "slug"))
  use body <- result.try(get_string(row, "body"))
  use status_str <- result.try(get_string(row, "status"))
  use status <- result.try(status_from_string(status_str))
  let published_at = get_optional_string(row, "published_at")
  use inserted_at <- result.try(get_string(row, "inserted_at"))
  use updated_at <- result.try(get_string(row, "updated_at"))

  Ok(Post(
    id: id,
    user_id: user_id,
    title: title,
    slug: slug,
    body: body,
    status: status,
    published_at: published_at,
    inserted_at: inserted_at,
    updated_at: updated_at,
  ))
}

/// Encode NewPost to values for insertion
pub fn encode_new(new_post: NewPost, now: String) -> Dict(String, ast.Value) {
  let slug = generate_slug(new_post.title)

  let base =
    dict.new()
    |> dict.insert("user_id", ast.IntValue(new_post.user_id))
    |> dict.insert("title", ast.StringValue(new_post.title))
    |> dict.insert("slug", ast.StringValue(slug))
    |> dict.insert("body", ast.StringValue(new_post.body))
    |> dict.insert("status", ast.StringValue(status_to_string(new_post.status)))
    |> dict.insert("inserted_at", ast.StringValue(now))
    |> dict.insert("updated_at", ast.StringValue(now))

  // Set published_at if status is Published
  case new_post.status {
    Published -> dict.insert(base, "published_at", ast.StringValue(now))
    _ -> dict.insert(base, "published_at", ast.NullValue)
  }
}

/// Encode PostUpdate to values for updating
pub fn encode_update(update: PostUpdate, now: String) -> Dict(String, ast.Value) {
  let base = dict.new() |> dict.insert("updated_at", ast.StringValue(now))

  let base = case update.title {
    Some(title) -> {
      base
      |> dict.insert("title", ast.StringValue(title))
      |> dict.insert("slug", ast.StringValue(generate_slug(title)))
    }
    None -> base
  }

  let base = case update.body {
    Some(body) -> dict.insert(base, "body", ast.StringValue(body))
    None -> base
  }

  case update.status {
    Some(status) -> {
      let base =
        dict.insert(base, "status", ast.StringValue(status_to_string(status)))
      // Set published_at when publishing
      case status {
        Published -> dict.insert(base, "published_at", ast.StringValue(now))
        _ -> base
      }
    }
    None -> base
  }
}

/// Check if a post is visible to the public
pub fn is_public(post: Post) -> Bool {
  post.status == Published
}

// Helper functions for decoding

fn get_int(row: Dict(String, ast.Value), key: String) -> Result(Int, String) {
  case dict.get(row, key) {
    Ok(ast.IntValue(i)) -> Ok(i)
    Ok(_) -> Error("Expected int for " <> key)
    Error(_) -> Error("Missing field: " <> key)
  }
}

fn get_string(
  row: Dict(String, ast.Value),
  key: String,
) -> Result(String, String) {
  case dict.get(row, key) {
    Ok(ast.StringValue(s)) -> Ok(s)
    Ok(_) -> Error("Expected string for " <> key)
    Error(_) -> Error("Missing field: " <> key)
  }
}

fn get_optional_string(
  row: Dict(String, ast.Value),
  key: String,
) -> Option(String) {
  case dict.get(row, key) {
    Ok(ast.StringValue(s)) -> Some(s)
    Ok(ast.NullValue) -> None
    _ -> None
  }
}
