/// Unit tests for schema modules.
///
/// Tests schema encoding, decoding, and helper functions
/// without any database interaction.
import blog/schemas/post
import blog/schemas/tag
import blog/schemas/user
import cquill/query/ast
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should

// --- User Schema Tests ---

pub fn user_role_to_string_test() {
  user.role_to_string(user.Admin)
  |> should.equal("admin")

  user.role_to_string(user.Author)
  |> should.equal("author")

  user.role_to_string(user.Reader)
  |> should.equal("reader")
}

pub fn user_role_from_string_test() {
  user.role_from_string("admin")
  |> should.equal(Ok(user.Admin))

  user.role_from_string("author")
  |> should.equal(Ok(user.Author))

  user.role_from_string("reader")
  |> should.equal(Ok(user.Reader))

  user.role_from_string("invalid")
  |> should.be_error()
}

pub fn user_encode_new_test() {
  let new_user =
    user.NewUser(
      email: "test@example.com",
      name: Some("Test User"),
      password: "secret",
    )

  let encoded =
    user.encode_new(new_user, "hashed_secret", "2024-01-15T10:00:00Z")

  dict.get(encoded, "email")
  |> should.equal(Ok(ast.StringValue("test@example.com")))

  dict.get(encoded, "name")
  |> should.equal(Ok(ast.StringValue("Test User")))

  dict.get(encoded, "password_hash")
  |> should.equal(Ok(ast.StringValue("hashed_secret")))

  dict.get(encoded, "role")
  |> should.equal(Ok(ast.StringValue("author")))
}

pub fn user_encode_new_without_name_test() {
  let new_user =
    user.NewUser(email: "test@example.com", name: None, password: "secret")

  let encoded =
    user.encode_new(new_user, "hashed_secret", "2024-01-15T10:00:00Z")

  dict.get(encoded, "name")
  |> should.equal(Ok(ast.NullValue))
}

pub fn user_decode_test() {
  let row =
    dict.new()
    |> dict.insert("id", ast.IntValue(1))
    |> dict.insert("email", ast.StringValue("test@example.com"))
    |> dict.insert("name", ast.StringValue("Test User"))
    |> dict.insert("role", ast.StringValue("author"))
    |> dict.insert("password_hash", ast.StringValue("hashed"))
    |> dict.insert("inserted_at", ast.StringValue("2024-01-15T10:00:00Z"))
    |> dict.insert("updated_at", ast.StringValue("2024-01-15T10:00:00Z"))

  let result = user.decode(row)

  let assert Ok(decoded) = result
  decoded.id
  |> should.equal(1)
  decoded.email
  |> should.equal("test@example.com")
  decoded.name
  |> should.equal(Some("Test User"))
  decoded.role
  |> should.equal(user.Author)
}

pub fn user_to_public_test() {
  let full_user =
    user.User(
      id: 1,
      email: "test@example.com",
      name: Some("Test"),
      role: user.Author,
      password_hash: "secret_hash",
      inserted_at: "2024-01-15T10:00:00Z",
      updated_at: "2024-01-15T10:00:00Z",
    )

  let public = user.to_public(full_user)

  public.id
  |> should.equal(1)
  public.email
  |> should.equal("test@example.com")
  public.name
  |> should.equal(Some("Test"))
  public.role
  |> should.equal(user.Author)
}

// --- Post Schema Tests ---

pub fn post_status_to_string_test() {
  post.status_to_string(post.Draft)
  |> should.equal("draft")

  post.status_to_string(post.Published)
  |> should.equal("published")

  post.status_to_string(post.Archived)
  |> should.equal("archived")
}

pub fn post_status_from_string_test() {
  post.status_from_string("draft")
  |> should.equal(Ok(post.Draft))

  post.status_from_string("published")
  |> should.equal(Ok(post.Published))

  post.status_from_string("archived")
  |> should.equal(Ok(post.Archived))

  post.status_from_string("invalid")
  |> should.be_error()
}

pub fn post_generate_slug_test() {
  post.generate_slug("Hello World")
  |> should.equal("hello-world")

  post.generate_slug("My First Post!")
  |> should.equal("my-first-post")

  post.generate_slug("What's New?")
  |> should.equal("whats-new")
}

pub fn post_encode_new_draft_test() {
  let new_post =
    post.NewPost(
      user_id: 1,
      title: "Test Post",
      body: "Content here",
      status: post.Draft,
    )

  let encoded = post.encode_new(new_post, "2024-01-15T10:00:00Z")

  dict.get(encoded, "user_id")
  |> should.equal(Ok(ast.IntValue(1)))

  dict.get(encoded, "title")
  |> should.equal(Ok(ast.StringValue("Test Post")))

  dict.get(encoded, "slug")
  |> should.equal(Ok(ast.StringValue("test-post")))

  dict.get(encoded, "status")
  |> should.equal(Ok(ast.StringValue("draft")))

  dict.get(encoded, "published_at")
  |> should.equal(Ok(ast.NullValue))
}

pub fn post_encode_new_published_test() {
  let new_post =
    post.NewPost(
      user_id: 1,
      title: "Published Post",
      body: "Content",
      status: post.Published,
    )

  let encoded = post.encode_new(new_post, "2024-01-15T10:00:00Z")

  dict.get(encoded, "status")
  |> should.equal(Ok(ast.StringValue("published")))

  dict.get(encoded, "published_at")
  |> should.equal(Ok(ast.StringValue("2024-01-15T10:00:00Z")))
}

pub fn post_is_public_test() {
  let published_post =
    post.Post(
      id: 1,
      user_id: 1,
      title: "Test",
      slug: "test",
      body: "Content",
      status: post.Published,
      published_at: Some("2024-01-15"),
      inserted_at: "2024-01-15",
      updated_at: "2024-01-15",
    )

  let draft_post = post.Post(..published_post, status: post.Draft)

  post.is_public(published_post)
  |> should.be_true()

  post.is_public(draft_post)
  |> should.be_false()
}

// --- Tag Schema Tests ---

pub fn tag_generate_slug_test() {
  tag.generate_slug("Gleam")
  |> should.equal("gleam")

  tag.generate_slug("Web Development")
  |> should.equal("web-development")

  tag.generate_slug("How-To")
  |> should.equal("how-to")
}

pub fn tag_encode_new_test() {
  let new_tag = tag.NewTag(name: "Gleam")

  let encoded = tag.encode_new(new_tag)

  dict.get(encoded, "name")
  |> should.equal(Ok(ast.StringValue("Gleam")))

  dict.get(encoded, "slug")
  |> should.equal(Ok(ast.StringValue("gleam")))
}

pub fn tag_encode_post_tag_test() {
  let post_tag = tag.PostTag(post_id: 1, tag_id: 2)

  let encoded = tag.encode_post_tag(post_tag)

  dict.get(encoded, "post_id")
  |> should.equal(Ok(ast.IntValue(1)))

  dict.get(encoded, "tag_id")
  |> should.equal(Ok(ast.IntValue(2)))
}
