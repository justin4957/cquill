/// Integration tests for the comments service.
///
/// Tests comment creation, moderation, and retrieval
/// using in-memory stores.
import blog/schemas/comment.{NewComment}
import blog/services/accounts
import blog/services/comments
import blog/services/posts
import gleam/list
import gleeunit/should
import support/factory
import support/setup

// --- Comment Creation Tests ---

pub fn create_comment_test() {
  use ctx <- setup.with_context()

  // Create a user and post first
  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  let new_comment =
    NewComment(
      post_id: post.id,
      author_name: "Bob",
      author_email: "bob@example.com",
      body: "Great post!",
    )

  let result = comments.create_comment(ctx.comment_store, new_comment)

  let assert Ok(#(_, created)) = result
  created.author_name
  |> should.equal("Bob")
  created.author_email
  |> should.equal("bob@example.com")
  created.body
  |> should.equal("Great post!")
  // New comments are not approved by default
  created.approved
  |> should.be_false()
}

pub fn create_comment_pending_by_default_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  let new_comment = factory.new_comment(post.id)
  let assert Ok(#(_, created)) =
    comments.create_comment(ctx.comment_store, new_comment)

  created.approved
  |> should.be_false()
}

// --- Comment Retrieval Tests ---

pub fn get_comment_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  let new_comment = factory.new_comment(post.id)
  let assert Ok(#(store, created)) =
    comments.create_comment(ctx.comment_store, new_comment)

  let result = comments.get_comment(store, created.id)

  let assert Ok(found) = result
  found.id
  |> should.equal(created.id)
  found.body
  |> should.equal(created.body)
}

pub fn get_comment_not_found_test() {
  use ctx <- setup.with_context()

  let result = comments.get_comment(ctx.comment_store, 99_999)

  result
  |> should.equal(Error(comments.CommentNotFound))
}

// --- Comment Moderation Tests ---

pub fn approve_comment_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  let new_comment = factory.new_comment(post.id)
  let assert Ok(#(store, created)) =
    comments.create_comment(ctx.comment_store, new_comment)

  created.approved
  |> should.be_false()

  let result = comments.approve_comment(store, created.id)

  let assert Ok(#(_, approved)) = result
  approved.approved
  |> should.be_true()
}

pub fn approve_nonexistent_comment_test() {
  use ctx <- setup.with_context()

  let result = comments.approve_comment(ctx.comment_store, 99_999)

  result
  |> should.equal(Error(comments.CommentNotFound))
}

pub fn delete_comment_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  let new_comment = factory.new_comment(post.id)
  let assert Ok(#(store, created)) =
    comments.create_comment(ctx.comment_store, new_comment)

  let result = comments.delete_comment(store, created.id)

  let assert Ok(new_store) = result

  // Verify deletion
  let get_result = comments.get_comment(new_store, created.id)
  get_result
  |> should.equal(Error(comments.CommentNotFound))
}

pub fn delete_nonexistent_comment_test() {
  use ctx <- setup.with_context()

  let result = comments.delete_comment(ctx.comment_store, 99_999)

  result
  |> should.equal(Error(comments.CommentNotFound))
}

// --- Comment Listing Tests ---

pub fn list_for_post_shows_only_approved_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  // Create one pending comment
  let pending_comment = factory.new_comment(post.id)
  let assert Ok(#(store, _pending)) =
    comments.create_comment(ctx.comment_store, pending_comment)

  // Create one approved comment
  let approved_comment =
    factory.new_comment_by(post.id, "Approver", "approver@example.com")
  let assert Ok(#(store, created_for_approval)) =
    comments.create_comment(store, approved_comment)
  let assert Ok(#(store, _)) =
    comments.approve_comment(store, created_for_approval.id)

  let comments_list = comments.list_for_post(store, post.id)

  // Only approved comment should be visible
  list.length(comments_list)
  |> should.equal(1)
  list.all(comments_list, fn(c) { c.approved })
  |> should.be_true()
}

pub fn list_for_post_all_shows_everything_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  // Create one pending comment
  let pending_comment = factory.new_comment(post.id)
  let assert Ok(#(store, _)) =
    comments.create_comment(ctx.comment_store, pending_comment)

  // Create one approved comment
  let approved_comment =
    factory.new_comment_by(post.id, "Approver", "approver@example.com")
  let assert Ok(#(store, created_for_approval)) =
    comments.create_comment(store, approved_comment)
  let assert Ok(#(store, _)) =
    comments.approve_comment(store, created_for_approval.id)

  let comments_list = comments.list_for_post_all(store, post.id)

  // Both comments should be visible for moderation
  list.length(comments_list)
  |> should.equal(2)
}

pub fn list_pending_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  // Create two pending comments
  let comment1 = factory.new_comment(post.id)
  let comment2 =
    factory.new_comment_by(post.id, "Another", "another@example.com")
  let assert Ok(#(store, _)) =
    comments.create_comment(ctx.comment_store, comment1)
  let assert Ok(#(store, _)) = comments.create_comment(store, comment2)

  // Create one approved comment
  let approved_comment =
    factory.new_comment_by(post.id, "Approved", "approved@example.com")
  let assert Ok(#(store, created_for_approval)) =
    comments.create_comment(store, approved_comment)
  let assert Ok(#(store, _)) =
    comments.approve_comment(store, created_for_approval.id)

  let pending_list = comments.list_pending(store)

  // Only pending comments
  list.all(pending_list, fn(c) { !c.approved })
  |> should.be_true()
}

// --- Comment Counting Tests ---

pub fn count_for_post_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  // Create and approve several comments
  let c1 = factory.new_comment_by(post.id, "User1", "user1@example.com")
  let c2 = factory.new_comment_by(post.id, "User2", "user2@example.com")
  let c3 = factory.new_comment_by(post.id, "User3", "user3@example.com")

  let assert Ok(#(store, created1)) =
    comments.create_comment(ctx.comment_store, c1)
  let assert Ok(#(store, _)) = comments.approve_comment(store, created1.id)
  let assert Ok(#(store, created2)) = comments.create_comment(store, c2)
  let assert Ok(#(store, _)) = comments.approve_comment(store, created2.id)
  let assert Ok(#(store, created3)) = comments.create_comment(store, c3)
  let assert Ok(#(store, _)) = comments.approve_comment(store, created3.id)

  // One pending (shouldn't be counted)
  let pending = factory.new_comment(post.id)
  let assert Ok(#(store, _)) = comments.create_comment(store, pending)

  let count = comments.count_for_post(store, post.id)

  count
  |> should.equal(3)
}

pub fn count_for_post_empty_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(_, post)) = posts.create_post(ctx.post_store, new_post)

  let count = comments.count_for_post(ctx.comment_store, post.id)

  count
  |> should.equal(0)
}
