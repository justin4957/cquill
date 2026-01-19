/// Comment schema definitions.
///
/// - `Comment` - Full comment record from database
/// - `NewComment` - Data required to create a new comment
import cquill/query/ast
import gleam/dict.{type Dict}
import gleam/result

/// Full comment record from database
pub type Comment {
  Comment(
    id: Int,
    post_id: Int,
    author_name: String,
    author_email: String,
    body: String,
    approved: Bool,
    inserted_at: String,
    updated_at: String,
  )
}

/// Data required to create a new comment
pub type NewComment {
  NewComment(
    post_id: Int,
    author_name: String,
    author_email: String,
    body: String,
  )
}

/// Table name constant
pub const table = "comments"

/// Decode a Comment from a database row
pub fn decode(row: Dict(String, ast.Value)) -> Result(Comment, String) {
  use id <- result.try(get_int(row, "id"))
  use post_id <- result.try(get_int(row, "post_id"))
  use author_name <- result.try(get_string(row, "author_name"))
  use author_email <- result.try(get_string(row, "author_email"))
  use body <- result.try(get_string(row, "body"))
  use approved <- result.try(get_bool(row, "approved"))
  use inserted_at <- result.try(get_string(row, "inserted_at"))
  use updated_at <- result.try(get_string(row, "updated_at"))

  Ok(Comment(
    id: id,
    post_id: post_id,
    author_name: author_name,
    author_email: author_email,
    body: body,
    approved: approved,
    inserted_at: inserted_at,
    updated_at: updated_at,
  ))
}

/// Encode NewComment to values for insertion
pub fn encode_new(
  new_comment: NewComment,
  now: String,
) -> Dict(String, ast.Value) {
  dict.new()
  |> dict.insert("post_id", ast.IntValue(new_comment.post_id))
  |> dict.insert("author_name", ast.StringValue(new_comment.author_name))
  |> dict.insert("author_email", ast.StringValue(new_comment.author_email))
  |> dict.insert("body", ast.StringValue(new_comment.body))
  |> dict.insert("approved", ast.BoolValue(False))
  |> dict.insert("inserted_at", ast.StringValue(now))
  |> dict.insert("updated_at", ast.StringValue(now))
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

fn get_bool(row: Dict(String, ast.Value), key: String) -> Result(Bool, String) {
  case dict.get(row, key) {
    Ok(ast.BoolValue(b)) -> Ok(b)
    Ok(_) -> Error("Expected bool for " <> key)
    Error(_) -> Error("Missing field: " <> key)
  }
}
