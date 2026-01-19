/// Tag schema definitions for blog post categorization.
///
/// - `Tag` - Full tag record from database
/// - `NewTag` - Data required to create a new tag
/// - `PostTag` - Join table record for post-tag relationships
import cquill/query/ast
import gleam/dict.{type Dict}
import gleam/result
import gleam/string

/// Full tag record from database
pub type Tag {
  Tag(id: Int, name: String, slug: String)
}

/// Data required to create a new tag
pub type NewTag {
  NewTag(name: String)
}

/// Join table record linking posts to tags
pub type PostTag {
  PostTag(post_id: Int, tag_id: Int)
}

/// Table name constants
pub const table = "tags"

pub const post_tags_table = "post_tags"

/// Generate a URL-friendly slug from a tag name
pub fn generate_slug(name: String) -> String {
  name
  |> string.lowercase()
  |> string.replace(" ", "-")
  |> string.replace("'", "")
  |> string.replace("\"", "")
}

/// Decode a Tag from a database row
pub fn decode(row: Dict(String, ast.Value)) -> Result(Tag, String) {
  use id <- result.try(get_int(row, "id"))
  use name <- result.try(get_string(row, "name"))
  use slug <- result.try(get_string(row, "slug"))

  Ok(Tag(id: id, name: name, slug: slug))
}

/// Decode a PostTag from a database row
pub fn decode_post_tag(row: Dict(String, ast.Value)) -> Result(PostTag, String) {
  use post_id <- result.try(get_int(row, "post_id"))
  use tag_id <- result.try(get_int(row, "tag_id"))

  Ok(PostTag(post_id: post_id, tag_id: tag_id))
}

/// Encode NewTag to values for insertion
pub fn encode_new(new_tag: NewTag) -> Dict(String, ast.Value) {
  dict.new()
  |> dict.insert("name", ast.StringValue(new_tag.name))
  |> dict.insert("slug", ast.StringValue(generate_slug(new_tag.name)))
}

/// Encode PostTag to values for insertion
pub fn encode_post_tag(post_tag: PostTag) -> Dict(String, ast.Value) {
  dict.new()
  |> dict.insert("post_id", ast.IntValue(post_tag.post_id))
  |> dict.insert("tag_id", ast.IntValue(post_tag.tag_id))
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
