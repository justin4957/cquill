/// Reusable query builders for posts.
///
/// Demonstrates composable query patterns for filtering, sorting,
/// and pagination of blog posts.
import blog/schemas/post
import cquill/query
import cquill/query/ast.{type Query}

/// Base query for posts table
pub fn base() -> Query(Nil) {
  query.from_table(post.table)
}

/// Filter by post ID
pub fn by_id(q: Query(a), id: Int) -> Query(a) {
  q
  |> query.where(query.eq_int("id", id))
}

/// Filter by slug
pub fn by_slug(q: Query(a), slug: String) -> Query(a) {
  q
  |> query.where(query.eq_string("slug", slug))
}

/// Filter by author (user_id)
pub fn by_author(q: Query(a), user_id: Int) -> Query(a) {
  q
  |> query.where(query.eq_int("user_id", user_id))
}

/// Filter by status
pub fn by_status(q: Query(a), status: post.PostStatus) -> Query(a) {
  q
  |> query.where(query.eq_string("status", post.status_to_string(status)))
}

/// Filter to published posts only
pub fn published(q: Query(a)) -> Query(a) {
  by_status(q, post.Published)
}

/// Filter to drafts only
pub fn drafts(q: Query(a)) -> Query(a) {
  by_status(q, post.Draft)
}

/// Filter to archived posts only
pub fn archived(q: Query(a)) -> Query(a) {
  by_status(q, post.Archived)
}

/// Order by publication date (newest first)
pub fn newest_first(q: Query(a)) -> Query(a) {
  q
  |> query.order_by_desc("published_at")
}

/// Order by creation date (newest first)
pub fn recently_created(q: Query(a)) -> Query(a) {
  q
  |> query.order_by_desc("inserted_at")
}

/// Order by title alphabetically
pub fn order_by_title(q: Query(a)) -> Query(a) {
  q
  |> query.order_by_asc("title")
}

/// Limit results
pub fn limit(q: Query(a), n: Int) -> Query(a) {
  q
  |> query.limit(n)
}

/// Offset for pagination
pub fn offset(q: Query(a), n: Int) -> Query(a) {
  q
  |> query.offset(n)
}

/// Paginate results
pub fn paginate(q: Query(a), page: Int, per_page: Int) -> Query(a) {
  let page_offset = { page - 1 } * per_page
  q
  |> limit(per_page)
  |> offset(page_offset)
}

// Composed queries

/// Get a single post by ID
pub fn get_by_id(id: Int) -> Query(Nil) {
  base()
  |> by_id(id)
}

/// Get a post by slug
pub fn get_by_slug(slug: String) -> Query(Nil) {
  base()
  |> by_slug(slug)
}

/// Get all posts by an author
pub fn by_author_all(user_id: Int) -> Query(Nil) {
  base()
  |> by_author(user_id)
  |> recently_created()
}

/// Get published posts by an author
pub fn by_author_published(user_id: Int) -> Query(Nil) {
  base()
  |> by_author(user_id)
  |> published()
  |> newest_first()
}

/// Get recent published posts (for homepage)
pub fn recent_published(n: Int) -> Query(Nil) {
  base()
  |> published()
  |> newest_first()
  |> limit(n)
}

/// Get draft posts by an author (for author dashboard)
pub fn author_drafts(user_id: Int) -> Query(Nil) {
  base()
  |> by_author(user_id)
  |> drafts()
  |> recently_created()
}

/// Paginated published posts
pub fn published_paginated(page: Int, per_page: Int) -> Query(Nil) {
  base()
  |> published()
  |> newest_first()
  |> paginate(page, per_page)
}
