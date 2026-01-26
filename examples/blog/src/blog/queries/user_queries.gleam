/// Reusable query builders for users.
///
/// This module demonstrates composable query patterns where small
/// query functions can be combined to build complex queries.
import blog/schemas/user
import cquill/query
import cquill/query/ast.{type Query}

/// Base query for users table
pub fn base() -> Query(Nil) {
  query.from_table(user.table)
}

/// Filter by user ID
pub fn by_id(q: Query(a), id: Int) -> Query(a) {
  q
  |> query.where(query.eq("id", id))
}

/// Filter by email
pub fn by_email(q: Query(a), email: String) -> Query(a) {
  q
  |> query.where(query.eq("email", email))
}

/// Filter by role
pub fn by_role(q: Query(a), role: user.UserRole) -> Query(a) {
  q
  |> query.where(query.eq("role", user.role_to_string(role)))
}

/// Filter to admins only
pub fn admins(q: Query(a)) -> Query(a) {
  by_role(q, user.Admin)
}

/// Filter to authors only
pub fn authors(q: Query(a)) -> Query(a) {
  by_role(q, user.Author)
}

/// Order by name ascending
pub fn order_by_name(q: Query(a)) -> Query(a) {
  q
  |> query.order_by_asc("name")
}

/// Order by creation date (newest first)
pub fn newest_first(q: Query(a)) -> Query(a) {
  q
  |> query.order_by_desc("inserted_at")
}

/// Limit results
pub fn limit(q: Query(a), n: Int) -> Query(a) {
  q
  |> query.limit(n)
}

/// Select only public fields (for PublicUser decoding)
pub fn select_public(q: Query(a)) -> Query(a) {
  q
  |> query.select(["id", "email", "name", "role"])
}

// Composed queries

/// Get a single user by ID
pub fn get_by_id(id: Int) -> Query(Nil) {
  base()
  |> by_id(id)
}

/// Get a user by email
pub fn get_by_email(email: String) -> Query(Nil) {
  base()
  |> by_email(email)
}

/// Get all admins
pub fn all_admins() -> Query(Nil) {
  base()
  |> admins()
  |> order_by_name()
}

/// Get all authors
pub fn all_authors() -> Query(Nil) {
  base()
  |> authors()
  |> order_by_name()
}

/// Get recent users
pub fn recent(n: Int) -> Query(Nil) {
  base()
  |> newest_first()
  |> limit(n)
}
