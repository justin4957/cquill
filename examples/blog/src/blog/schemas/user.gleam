/// User schema definitions demonstrating multiple schema types per domain.
///
/// This module shows how to define different schema types for different use cases:
/// - `User` - Full user record from database
/// - `NewUser` - Data required to create a new user
/// - `PublicUser` - Safe for public display (no password)
/// - `UserUpdate` - Fields that can be updated
import cquill/query/ast
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/result

/// User roles
pub type UserRole {
  Admin
  Author
  Reader
}

/// Full user record from database
pub type User {
  User(
    id: Int,
    email: String,
    name: Option(String),
    role: UserRole,
    password_hash: String,
    inserted_at: String,
    updated_at: String,
  )
}

/// Data required to create a new user
pub type NewUser {
  NewUser(email: String, name: Option(String), password: String)
}

/// User data safe for public display (no password)
pub type PublicUser {
  PublicUser(id: Int, email: String, name: Option(String), role: UserRole)
}

/// Fields that can be updated on a user
pub type UserUpdate {
  UserUpdate(
    name: Option(String),
    email: Option(String),
    role: Option(UserRole),
  )
}

/// Table name constant
pub const table = "users"

/// Convert role to string for storage
pub fn role_to_string(role: UserRole) -> String {
  case role {
    Admin -> "admin"
    Author -> "author"
    Reader -> "reader"
  }
}

/// Parse role from string
pub fn role_from_string(s: String) -> Result(UserRole, String) {
  case s {
    "admin" -> Ok(Admin)
    "author" -> Ok(Author)
    "reader" -> Ok(Reader)
    _ -> Error("Invalid role: " <> s)
  }
}

/// Decode a User from a database row
pub fn decode(row: Dict(String, ast.Value)) -> Result(User, String) {
  use id <- result.try(get_int(row, "id"))
  use email <- result.try(get_string(row, "email"))
  let name = get_optional_string(row, "name")
  use role_str <- result.try(get_string(row, "role"))
  use role <- result.try(role_from_string(role_str))
  use password_hash <- result.try(get_string(row, "password_hash"))
  use inserted_at <- result.try(get_string(row, "inserted_at"))
  use updated_at <- result.try(get_string(row, "updated_at"))

  Ok(User(
    id: id,
    email: email,
    name: name,
    role: role,
    password_hash: password_hash,
    inserted_at: inserted_at,
    updated_at: updated_at,
  ))
}

/// Decode a PublicUser from a database row
pub fn decode_public(row: Dict(String, ast.Value)) -> Result(PublicUser, String) {
  use id <- result.try(get_int(row, "id"))
  use email <- result.try(get_string(row, "email"))
  let name = get_optional_string(row, "name")
  use role_str <- result.try(get_string(row, "role"))
  use role <- result.try(role_from_string(role_str))

  Ok(PublicUser(id: id, email: email, name: name, role: role))
}

/// Convert User to PublicUser (strips sensitive data)
pub fn to_public(user: User) -> PublicUser {
  PublicUser(id: user.id, email: user.email, name: user.name, role: user.role)
}

/// Encode NewUser to values for insertion
pub fn encode_new(
  new_user: NewUser,
  password_hash: String,
  now: String,
) -> Dict(String, ast.Value) {
  let base =
    dict.new()
    |> dict.insert("email", ast.StringValue(new_user.email))
    |> dict.insert("role", ast.StringValue(role_to_string(Author)))
    |> dict.insert("password_hash", ast.StringValue(password_hash))
    |> dict.insert("inserted_at", ast.StringValue(now))
    |> dict.insert("updated_at", ast.StringValue(now))

  case new_user.name {
    Some(name) -> dict.insert(base, "name", ast.StringValue(name))
    None -> dict.insert(base, "name", ast.NullValue)
  }
}

/// Encode UserUpdate to values for updating
pub fn encode_update(update: UserUpdate, now: String) -> Dict(String, ast.Value) {
  let base = dict.new() |> dict.insert("updated_at", ast.StringValue(now))

  let base = case update.name {
    Some(name) -> dict.insert(base, "name", ast.StringValue(name))
    None -> base
  }

  let base = case update.email {
    Some(email) -> dict.insert(base, "email", ast.StringValue(email))
    None -> base
  }

  case update.role {
    Some(role) ->
      dict.insert(base, "role", ast.StringValue(role_to_string(role)))
    None -> base
  }
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
