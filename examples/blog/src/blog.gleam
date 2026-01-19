/// Blog Example Application
///
/// This example demonstrates cquill best practices for building a complete
/// data access layer. It includes:
///
/// - Multiple schema types per domain (User, NewUser, PublicUser, UserUpdate)
/// - Composable query builders
/// - Service modules with business logic
/// - Testing patterns
///
/// ## Quick Start
///
/// ```gleam
/// import blog/services/accounts
/// import blog/services/posts
/// import blog/schemas/user.{NewUser}
/// import blog/schemas/post.{NewPost, Draft}
/// import gleam/option.{Some}
///
/// pub fn main() {
///   // Initialize stores
///   let user_store = accounts.new_store()
///   let post_store = posts.new_store()
///
///   // Create a user
///   let assert Ok(#(user_store, user)) = accounts.create_user(user_store, NewUser(
///     email: "alice@example.com",
///     name: Some("Alice"),
///     password: "secret123",
///   ))
///
///   // Create a post
///   let assert Ok(#(post_store, post)) = posts.create_post(post_store, NewPost(
///     user_id: user.id,
///     title: "Hello World",
///     body: "My first post!",
///     status: Draft,
///   ))
///
///   io.println("Created post: " <> post.title)
/// }
/// ```
import gleam/io

/// Main entry point demonstrating the blog application
pub fn main() {
  io.println("Blog Example Application")
  io.println("========================")
  io.println("")
  io.println("This is an example application demonstrating cquill patterns.")
  io.println(
    "See the source code in examples/blog/ for implementation details.",
  )
  io.println("")
  io.println("Run tests with: cd examples/blog && gleam test")
}
