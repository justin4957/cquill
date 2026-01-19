/// Test data factories.
///
/// Provides functions for creating test data with sensible defaults.
/// Demonstrates factory patterns for test data generation.
import blog/schemas/comment.{type NewComment, NewComment}
import blog/schemas/post.{type NewPost, Draft, NewPost, Published}
import blog/schemas/user.{type NewUser, NewUser}
import gleam/option.{None, Some}

// --- User Factories ---

/// Create a NewUser with default values
pub fn new_user() -> NewUser {
  NewUser(
    email: "testuser@example.com",
    name: Some("Test User"),
    password: "password123",
  )
}

/// Create a NewUser with custom email
pub fn new_user_with_email(email: String) -> NewUser {
  NewUser(email: email, name: Some("Test User"), password: "password123")
}

/// Create a NewUser with no name
pub fn new_user_anonymous(email: String) -> NewUser {
  NewUser(email: email, name: None, password: "password123")
}

// --- Post Factories ---

/// Create a NewPost with default values (draft)
pub fn new_post(user_id: Int) -> NewPost {
  NewPost(
    user_id: user_id,
    title: "Test Post",
    body: "This is the body of the test post. It contains some content for testing purposes.",
    status: Draft,
  )
}

/// Create a NewPost that's published
pub fn new_published_post(user_id: Int) -> NewPost {
  NewPost(
    user_id: user_id,
    title: "Published Test Post",
    body: "This is a published post with some content.",
    status: Published,
  )
}

/// Create a NewPost with custom title
pub fn new_post_with_title(user_id: Int, title: String) -> NewPost {
  NewPost(
    user_id: user_id,
    title: title,
    body: "This is the body of the test post.",
    status: Draft,
  )
}

// --- Comment Factories ---

/// Create a NewComment with default values
pub fn new_comment(post_id: Int) -> NewComment {
  NewComment(
    post_id: post_id,
    author_name: "Test Commenter",
    author_email: "commenter@example.com",
    body: "This is a test comment.",
  )
}

/// Create a NewComment with custom author
pub fn new_comment_by(post_id: Int, name: String, email: String) -> NewComment {
  NewComment(
    post_id: post_id,
    author_name: name,
    author_email: email,
    body: "This is a test comment.",
  )
}
