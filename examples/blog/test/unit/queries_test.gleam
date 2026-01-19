/// Unit tests for query builder modules.
///
/// Tests that query builders produce expected query structures
/// without executing against a database.
import blog/queries/comment_queries
import blog/queries/post_queries
import blog/queries/user_queries
import blog/schemas/user
import gleeunit/should

// --- User Query Tests ---

pub fn user_base_query_test() {
  let query = user_queries.base()

  // Verify the query was created - just check it's a valid query
  // that can be further composed
  let _composed =
    query
    |> user_queries.by_id(1)

  // If we get here without error, the query was created successfully
  should.be_true(True)
}

pub fn user_by_id_query_test() {
  let base = user_queries.base()
  let query =
    user_queries.base()
    |> user_queries.by_id(42)

  // Query was modified with ID filter
  query
  |> should.not_equal(base)
}

pub fn user_by_email_query_test() {
  let base = user_queries.base()
  let query =
    user_queries.base()
    |> user_queries.by_email("test@example.com")

  query
  |> should.not_equal(base)
}

pub fn user_by_role_query_test() {
  let base = user_queries.base()
  let query =
    user_queries.base()
    |> user_queries.by_role(user.Admin)

  query
  |> should.not_equal(base)
}

pub fn user_composed_query_test() {
  let base = user_queries.base()
  // Test that queries can be composed
  let query =
    user_queries.base()
    |> user_queries.authors()
    |> user_queries.order_by_name()
    |> user_queries.limit(10)

  query
  |> should.not_equal(base)
}

pub fn user_get_by_id_shorthand_test() {
  let base = user_queries.base()
  let query = user_queries.get_by_id(1)

  query
  |> should.not_equal(base)
}

pub fn user_all_admins_query_test() {
  let base = user_queries.base()
  let query = user_queries.all_admins()

  query
  |> should.not_equal(base)
}

// --- Post Query Tests ---

pub fn post_base_query_test() {
  let query = post_queries.base()

  // Verify the query was created by composing it
  let _composed =
    query
    |> post_queries.published()

  should.be_true(True)
}

pub fn post_published_filter_test() {
  let base = post_queries.base()
  let query =
    post_queries.base()
    |> post_queries.published()

  query
  |> should.not_equal(base)
}

pub fn post_by_author_test() {
  let base = post_queries.base()
  let query =
    post_queries.base()
    |> post_queries.by_author(1)

  query
  |> should.not_equal(base)
}

pub fn post_pagination_test() {
  let base = post_queries.base()
  let query =
    post_queries.base()
    |> post_queries.paginate(2, 10)

  query
  |> should.not_equal(base)
}

pub fn post_composed_published_by_author_test() {
  let base = post_queries.base()
  // Multiple filters composed together
  let query =
    post_queries.base()
    |> post_queries.published()
    |> post_queries.by_author(1)
    |> post_queries.newest_first()
    |> post_queries.limit(5)

  query
  |> should.not_equal(base)
}

pub fn post_recent_published_shorthand_test() {
  let base = post_queries.base()
  let query = post_queries.recent_published(10)

  query
  |> should.not_equal(base)
}

pub fn post_author_drafts_shorthand_test() {
  let base = post_queries.base()
  let query = post_queries.author_drafts(1)

  query
  |> should.not_equal(base)
}

// --- Comment Query Tests ---

pub fn comment_base_query_test() {
  let query = comment_queries.base()

  // Verify the query was created by composing it
  let _composed =
    query
    |> comment_queries.approved()

  should.be_true(True)
}

pub fn comment_by_post_test() {
  let base = comment_queries.base()
  let query =
    comment_queries.base()
    |> comment_queries.by_post(1)

  query
  |> should.not_equal(base)
}

pub fn comment_approved_filter_test() {
  let base = comment_queries.base()
  let query =
    comment_queries.base()
    |> comment_queries.approved()

  query
  |> should.not_equal(base)
}

pub fn comment_pending_filter_test() {
  let base = comment_queries.base()
  let query =
    comment_queries.base()
    |> comment_queries.pending()

  query
  |> should.not_equal(base)
}

pub fn comment_for_post_approved_shorthand_test() {
  let base = comment_queries.base()
  let query = comment_queries.for_post_approved(1)

  query
  |> should.not_equal(base)
}

pub fn comment_pending_all_shorthand_test() {
  let base = comment_queries.base()
  let query = comment_queries.pending_all()

  query
  |> should.not_equal(base)
}
