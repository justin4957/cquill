/// Post management service.
///
/// Handles post creation, publishing, and retrieval.
/// Demonstrates:
/// - Transaction patterns for related operations
/// - Composable queries
/// - Business logic validation
///
/// NOTE: This is a conceptual example showing the patterns. In a production
/// application, you would integrate with the full cquill adapter/repo layer.
import blog/schemas/post.{
  type NewPost, type Post, type PostUpdate, Draft, Post, PostUpdate, Published,
}
import blog/schemas/tag.{type Tag, Tag}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result

/// Service-specific errors
pub type PostError {
  PostNotFound
  SlugAlreadyExists
  Unauthorized
  DatabaseError(String)
}

/// Post with its tags
pub type PostWithTags {
  PostWithTags(post: Post, tags: List(Tag))
}

/// In-memory post storage for demonstration
pub opaque type PostStore {
  PostStore(
    posts: dict.Dict(Int, Post),
    tags: dict.Dict(Int, Tag),
    post_tags: List(#(Int, Int)),
    next_post_id: Int,
    next_tag_id: Int,
  )
}

/// Create a new post store
pub fn new_store() -> PostStore {
  PostStore(
    posts: dict.new(),
    tags: dict.new(),
    post_tags: [],
    next_post_id: 1,
    next_tag_id: 1,
  )
}

/// Create a new post
pub fn create_post(
  store: PostStore,
  new_post: NewPost,
) -> Result(#(PostStore, Post), PostError) {
  let slug = post.generate_slug(new_post.title)

  // Check slug uniqueness
  let slug_exists =
    dict.values(store.posts)
    |> list.any(fn(p) { p.slug == slug })

  case slug_exists {
    True -> Error(SlugAlreadyExists)
    False -> {
      let id = store.next_post_id
      let now = get_timestamp()

      let published_at = case new_post.status {
        Published -> Some(now)
        _ -> None
      }

      let created_post =
        Post(
          id: id,
          user_id: new_post.user_id,
          title: new_post.title,
          slug: slug,
          body: new_post.body,
          status: new_post.status,
          published_at: published_at,
          inserted_at: now,
          updated_at: now,
        )

      let new_posts = dict.insert(store.posts, id, created_post)
      let new_store = PostStore(..store, posts: new_posts, next_post_id: id + 1)

      Ok(#(new_store, created_post))
    }
  }
}

/// Create a post with tags
pub fn create_post_with_tags(
  store: PostStore,
  new_post: NewPost,
  tag_names: List(String),
) -> Result(#(PostStore, PostWithTags), PostError) {
  // Create the post
  case create_post(store, new_post) {
    Error(e) -> Error(e)
    Ok(#(store, created_post)) -> {
      // Find or create tags
      let #(store, tags) =
        list.fold(tag_names, #(store, []), fn(acc, name) {
          let #(s, ts) = acc
          let #(s2, t) = find_or_create_tag(s, name)
          #(s2, [t, ..ts])
        })

      // Create associations
      let new_post_tags =
        list.map(tags, fn(t) { #(created_post.id, t.id) })
        |> list.append(store.post_tags)

      let final_store = PostStore(..store, post_tags: new_post_tags)

      Ok(#(final_store, PostWithTags(post: created_post, tags: tags)))
    }
  }
}

/// Get a post by ID
pub fn get_post(store: PostStore, id: Int) -> Result(Post, PostError) {
  case dict.get(store.posts, id) {
    Ok(found_post) -> Ok(found_post)
    Error(_) -> Error(PostNotFound)
  }
}

/// Get a post by slug
pub fn get_post_by_slug(
  store: PostStore,
  slug: String,
) -> Result(Post, PostError) {
  dict.values(store.posts)
  |> list.find(fn(p) { p.slug == slug })
  |> result.replace_error(PostNotFound)
}

/// Get a published post by slug (for public viewing)
pub fn get_published_post(
  store: PostStore,
  slug: String,
) -> Result(Post, PostError) {
  dict.values(store.posts)
  |> list.find(fn(p) { p.slug == slug && p.status == Published })
  |> result.replace_error(PostNotFound)
}

/// Update a post
pub fn update_post(
  store: PostStore,
  id: Int,
  user_id: Int,
  updates: PostUpdate,
) -> Result(#(PostStore, Post), PostError) {
  case get_post(store, id) {
    Error(e) -> Error(e)
    Ok(existing_post) -> {
      case existing_post.user_id == user_id {
        False -> Error(Unauthorized)
        True -> {
          let now = get_timestamp()

          let updated_title = case updates.title {
            Some(title) -> title
            None -> existing_post.title
          }

          let updated_slug = case updates.title {
            Some(title) -> post.generate_slug(title)
            None -> existing_post.slug
          }

          let updated_body = case updates.body {
            Some(body) -> body
            None -> existing_post.body
          }

          let updated_status = case updates.status {
            Some(status) -> status
            None -> existing_post.status
          }

          let updated_published_at = case updates.status {
            Some(Published) -> Some(now)
            _ -> existing_post.published_at
          }

          let updated_post =
            Post(
              ..existing_post,
              title: updated_title,
              slug: updated_slug,
              body: updated_body,
              status: updated_status,
              published_at: updated_published_at,
              updated_at: now,
            )

          let new_posts = dict.insert(store.posts, id, updated_post)
          let new_store = PostStore(..store, posts: new_posts)

          Ok(#(new_store, updated_post))
        }
      }
    }
  }
}

/// Publish a draft post
pub fn publish_post(
  store: PostStore,
  id: Int,
  user_id: Int,
) -> Result(#(PostStore, Post), PostError) {
  update_post(
    store,
    id,
    user_id,
    PostUpdate(title: None, body: None, status: Some(Published)),
  )
}

/// Archive a post
pub fn archive_post(
  store: PostStore,
  id: Int,
  user_id: Int,
) -> Result(#(PostStore, Post), PostError) {
  update_post(
    store,
    id,
    user_id,
    PostUpdate(title: None, body: None, status: Some(post.Archived)),
  )
}

/// List recent published posts
pub fn list_recent_published(store: PostStore, limit: Int) -> List(Post) {
  dict.values(store.posts)
  |> list.filter(fn(p) { p.status == Published })
  |> list.take(limit)
}

/// List all posts by an author
pub fn list_by_author(store: PostStore, user_id: Int) -> List(Post) {
  dict.values(store.posts)
  |> list.filter(fn(p) { p.user_id == user_id })
}

/// List draft posts by an author
pub fn list_author_drafts(store: PostStore, user_id: Int) -> List(Post) {
  dict.values(store.posts)
  |> list.filter(fn(p) { p.user_id == user_id && p.status == Draft })
}

/// Get tags for a post
pub fn get_post_tags(store: PostStore, post_id: Int) -> List(Tag) {
  store.post_tags
  |> list.filter(fn(pt) { pt.0 == post_id })
  |> list.filter_map(fn(pt) {
    dict.get(store.tags, pt.1) |> result.replace_error(Nil)
  })
}

// Helper functions

fn find_or_create_tag(store: PostStore, name: String) -> #(PostStore, Tag) {
  let slug = tag.generate_slug(name)

  case dict.values(store.tags) |> list.find(fn(t) { t.slug == slug }) {
    Ok(existing_tag) -> #(store, existing_tag)
    Error(_) -> {
      let id = store.next_tag_id
      let new_tag = Tag(id: id, name: name, slug: slug)
      let new_tags = dict.insert(store.tags, id, new_tag)
      let new_store = PostStore(..store, tags: new_tags, next_tag_id: id + 1)
      #(new_store, new_tag)
    }
  }
}

fn get_timestamp() -> String {
  "2024-01-15T10:30:00Z"
}
