/// Reusable query builders for comments.
///
/// Demonstrates composable query patterns for filtering and
/// managing blog post comments.
import blog/schemas/comment
import cquill/query
import cquill/query/ast.{type Query}

/// Base query for comments table
pub fn base() -> Query(Nil) {
  query.from_table(comment.table)
}

/// Filter by comment ID
pub fn by_id(q: Query(a), id: Int) -> Query(a) {
  q
  |> query.where(query.eq_int("id", id))
}

/// Filter by post ID
pub fn by_post(q: Query(a), post_id: Int) -> Query(a) {
  q
  |> query.where(query.eq_int("post_id", post_id))
}

/// Filter by author email
pub fn by_author_email(q: Query(a), email: String) -> Query(a) {
  q
  |> query.where(query.eq_string("author_email", email))
}

/// Filter to approved comments only
pub fn approved(q: Query(a)) -> Query(a) {
  q
  |> query.where(query.eq_bool("approved", True))
}

/// Filter to pending (unapproved) comments only
pub fn pending(q: Query(a)) -> Query(a) {
  q
  |> query.where(query.eq_bool("approved", False))
}

/// Order by creation date (oldest first, for chronological display)
pub fn chronological(q: Query(a)) -> Query(a) {
  q
  |> query.order_by_asc("inserted_at")
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

// Composed queries

/// Get a single comment by ID
pub fn get_by_id(id: Int) -> Query(Nil) {
  base()
  |> by_id(id)
}

/// Get approved comments for a post (for public display)
pub fn for_post_approved(post_id: Int) -> Query(Nil) {
  base()
  |> by_post(post_id)
  |> approved()
  |> chronological()
}

/// Get all comments for a post (for moderation)
pub fn for_post_all(post_id: Int) -> Query(Nil) {
  base()
  |> by_post(post_id)
  |> chronological()
}

/// Get pending comments (for moderation queue)
pub fn pending_all() -> Query(Nil) {
  base()
  |> pending()
  |> newest_first()
}

/// Get recent comments by an author email
pub fn by_author_recent(email: String, n: Int) -> Query(Nil) {
  base()
  |> by_author_email(email)
  |> newest_first()
  |> limit(n)
}

/// Query for counting approved comments for a post
pub fn count_for_post(post_id: Int) -> Query(Nil) {
  base()
  |> by_post(post_id)
  |> approved()
}
