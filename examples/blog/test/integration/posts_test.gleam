/// Integration tests for the posts service.
///
/// Tests post creation, publishing, and retrieval
/// using in-memory stores.
import blog/schemas/post.{Archived, Draft, NewPost, PostUpdate, Published}
import blog/services/accounts
import blog/services/posts
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/factory
import support/setup

// --- Post Creation Tests ---

pub fn create_draft_post_test() {
  use ctx <- setup.with_context()

  // Create user first
  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  let new_post =
    NewPost(
      user_id: user.id,
      title: "My First Post",
      body: "Hello, world!",
      status: Draft,
    )

  let result = posts.create_post(ctx.post_store, new_post)

  let assert Ok(#(_, created)) = result
  created.title
  |> should.equal("My First Post")
  created.slug
  |> should.equal("my-first-post")
  created.status
  |> should.equal(Draft)
  created.published_at
  |> should.equal(None)
}

pub fn create_published_post_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  let new_post =
    NewPost(
      user_id: user.id,
      title: "Published Post",
      body: "Content here",
      status: Published,
    )

  let result = posts.create_post(ctx.post_store, new_post)

  let assert Ok(#(_, created)) = result
  created.status
  |> should.equal(Published)
  created.published_at
  |> should.not_equal(None)
}

pub fn create_post_generates_slug_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  let new_post =
    NewPost(
      user_id: user.id,
      title: "Hello World! This Is A Test",
      body: "Content",
      status: Draft,
    )

  let result = posts.create_post(ctx.post_store, new_post)

  let assert Ok(#(_, created)) = result
  created.slug
  |> should.equal("hello-world-this-is-a-test")
}

// --- Post Retrieval Tests ---

pub fn get_post_by_id_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let result = posts.get_post(store, post.id)

  let assert Ok(found) = result
  found.id
  |> should.equal(post.id)
  found.title
  |> should.equal(post.title)
}

pub fn get_post_not_found_test() {
  use ctx <- setup.with_context()

  let result = posts.get_post(ctx.post_store, 99_999)

  result
  |> should.equal(Error(posts.PostNotFound))
}

pub fn get_post_by_slug_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post_with_title(user.id, "Unique Title")
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let result = posts.get_post_by_slug(store, "unique-title")

  let assert Ok(found) = result
  found.id
  |> should.equal(post.id)
}

pub fn get_published_post_returns_published_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_published_post(user.id)
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let result = posts.get_published_post(store, post.slug)

  let assert Ok(found) = result
  found.id
  |> should.equal(post.id)
}

pub fn get_published_post_hides_drafts_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(store, draft)) = posts.create_post(ctx.post_store, new_post)

  let result = posts.get_published_post(store, draft.slug)

  result
  |> should.equal(Error(posts.PostNotFound))
}

// --- Post Update Tests ---

pub fn update_post_title_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let updates = PostUpdate(title: Some("New Title"), body: None, status: None)

  let result = posts.update_post(store, post.id, user.id, updates)

  let assert Ok(#(_, updated)) = result
  updated.title
  |> should.equal("New Title")
  updated.slug
  |> should.equal("new-title")
}

pub fn update_post_body_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let updates =
    PostUpdate(title: None, body: Some("Updated content"), status: None)

  let result = posts.update_post(store, post.id, user.id, updates)

  let assert Ok(#(_, updated)) = result
  updated.body
  |> should.equal("Updated content")
}

pub fn update_post_unauthorized_test() {
  use ctx <- setup.with_context()

  let owner_user = factory.new_user()
  let other_user = factory.new_user_with_email("other@example.com")
  let assert Ok(#(user_store, owner)) =
    accounts.create_user(ctx.user_store, owner_user)
  let assert Ok(#(_, other)) = accounts.create_user(user_store, other_user)

  let new_post = factory.new_post(owner.id)
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let updates = PostUpdate(title: Some("Hacked"), body: None, status: None)

  // Try to update as different user
  let result = posts.update_post(store, post.id, other.id, updates)

  result
  |> should.equal(Error(posts.Unauthorized))
}

// --- Publishing Tests ---

pub fn publish_post_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_post(user.id)
  let assert Ok(#(store, draft)) = posts.create_post(ctx.post_store, new_post)

  draft.status
  |> should.equal(Draft)

  let result = posts.publish_post(store, draft.id, user.id)

  let assert Ok(#(_, published)) = result
  published.status
  |> should.equal(Published)
  published.published_at
  |> should.not_equal(None)
}

pub fn archive_post_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)
  let new_post = factory.new_published_post(user.id)
  let assert Ok(#(store, post)) = posts.create_post(ctx.post_store, new_post)

  let result = posts.archive_post(store, post.id, user.id)

  let assert Ok(#(_, archived)) = result
  archived.status
  |> should.equal(Archived)
}

// --- Listing Tests ---

pub fn list_recent_published_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  // Create some drafts and published posts with unique titles
  let draft_post = factory.new_post(user.id)
  let pub_post1 = factory.new_post_with_title(user.id, "Published Post One")
  let pub_post1 = NewPost(..pub_post1, status: Published)
  let pub_post2 = factory.new_post_with_title(user.id, "Published Post Two")
  let pub_post2 = NewPost(..pub_post2, status: Published)

  let assert Ok(#(store, _)) = posts.create_post(ctx.post_store, draft_post)
  let assert Ok(#(store, _)) = posts.create_post(store, pub_post1)
  let assert Ok(#(store, _)) = posts.create_post(store, pub_post2)

  let published_posts = posts.list_recent_published(store, 10)

  // Should only return published posts
  list.all(published_posts, fn(p) { p.status == Published })
  |> should.be_true()
}

pub fn list_by_author_test() {
  use ctx <- setup.with_context()

  let user1_data = factory.new_user()
  let user2_data = factory.new_user_with_email("user2@example.com")
  let assert Ok(#(user_store, user1)) =
    accounts.create_user(ctx.user_store, user1_data)
  let assert Ok(#(_, user2)) = accounts.create_user(user_store, user2_data)

  // Use unique titles to avoid slug conflicts
  let post1 = factory.new_post_with_title(user1.id, "User One Post")
  let post2 = factory.new_post_with_title(user2.id, "User Two Post")

  let assert Ok(#(store, _)) = posts.create_post(ctx.post_store, post1)
  let assert Ok(#(store, _)) = posts.create_post(store, post2)

  let user1_posts = posts.list_by_author(store, user1.id)

  list.all(user1_posts, fn(p) { p.user_id == user1.id })
  |> should.be_true()
}

pub fn list_author_drafts_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  let draft_post = factory.new_post(user.id)
  let pub_post = factory.new_published_post(user.id)

  let assert Ok(#(store, _)) = posts.create_post(ctx.post_store, draft_post)
  let assert Ok(#(store, _)) = posts.create_post(store, pub_post)

  let drafts = posts.list_author_drafts(store, user.id)

  list.all(drafts, fn(p) { p.status == Draft })
  |> should.be_true()
}

// --- Post with Tags Tests ---

pub fn create_post_with_tags_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  let new_post =
    NewPost(
      user_id: user.id,
      title: "Tagged Post",
      body: "Content with tags",
      status: Draft,
    )

  let result =
    posts.create_post_with_tags(ctx.post_store, new_post, ["gleam", "tutorial"])

  let assert Ok(#(_, post_with_tags)) = result
  post_with_tags.post.title
  |> should.equal("Tagged Post")

  list.length(post_with_tags.tags)
  |> should.equal(2)
}

pub fn get_post_tags_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(_, user)) = accounts.create_user(ctx.user_store, new_user)

  let new_post =
    NewPost(user_id: user.id, title: "Post", body: "Content", status: Draft)

  let assert Ok(#(store, post_with_tags)) =
    posts.create_post_with_tags(ctx.post_store, new_post, ["tag1", "tag2"])

  let tags = posts.get_post_tags(store, post_with_tags.post.id)

  list.length(tags)
  |> should.equal(2)
}
