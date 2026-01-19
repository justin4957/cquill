/// Test setup helpers.
///
/// Provides utilities for setting up test environments.
/// Demonstrates the "test context" pattern.
import blog/services/accounts
import blog/services/comments
import blog/services/posts

/// Test context containing all stores
pub type TestContext {
  TestContext(
    user_store: accounts.UserStore,
    post_store: posts.PostStore,
    comment_store: comments.CommentStore,
  )
}

/// Create a fresh test context
pub fn new_context() -> TestContext {
  TestContext(
    user_store: accounts.new_store(),
    post_store: posts.new_store(),
    comment_store: comments.new_store(),
  )
}

/// Execute a test function with a fresh context
///
/// ## Example
///
/// ```gleam
/// pub fn create_user_test() {
///   use ctx <- setup.with_context()
///
///   let new_user = factory.new_user()
///   let result = accounts.create_user(ctx.user_store, new_user)
///
///   let assert Ok(#(_, user)) = result
///   user.email |> should.equal(new_user.email)
/// }
/// ```
pub fn with_context(test_fn: fn(TestContext) -> a) -> a {
  let ctx = new_context()
  test_fn(ctx)
}
