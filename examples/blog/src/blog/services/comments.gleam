/// Comment management service.
///
/// Handles comment creation, moderation, and retrieval.
/// Demonstrates:
/// - Content moderation workflow
/// - Query composition for different views
///
/// NOTE: This is a conceptual example showing the patterns. In a production
/// application, you would integrate with the full cquill adapter/repo layer.
import blog/schemas/comment.{type Comment, type NewComment, Comment}
import gleam/dict
import gleam/list

/// Service-specific errors
pub type CommentError {
  CommentNotFound
  PostNotFound
  DatabaseError(String)
}

/// In-memory comment storage for demonstration
pub opaque type CommentStore {
  CommentStore(comments: dict.Dict(Int, Comment), next_id: Int)
}

/// Create a new comment store
pub fn new_store() -> CommentStore {
  CommentStore(comments: dict.new(), next_id: 1)
}

/// Create a new comment (pending approval)
pub fn create_comment(
  store: CommentStore,
  new_comment: NewComment,
) -> Result(#(CommentStore, Comment), CommentError) {
  let id = store.next_id
  let now = get_timestamp()

  let created_comment =
    Comment(
      id: id,
      post_id: new_comment.post_id,
      author_name: new_comment.author_name,
      author_email: new_comment.author_email,
      body: new_comment.body,
      approved: False,
      inserted_at: now,
      updated_at: now,
    )

  let new_comments = dict.insert(store.comments, id, created_comment)
  let new_store = CommentStore(comments: new_comments, next_id: id + 1)

  Ok(#(new_store, created_comment))
}

/// Get a comment by ID
pub fn get_comment(
  store: CommentStore,
  id: Int,
) -> Result(Comment, CommentError) {
  case dict.get(store.comments, id) {
    Ok(comment) -> Ok(comment)
    Error(_) -> Error(CommentNotFound)
  }
}

/// Approve a comment (moderation)
pub fn approve_comment(
  store: CommentStore,
  id: Int,
) -> Result(#(CommentStore, Comment), CommentError) {
  case get_comment(store, id) {
    Error(e) -> Error(e)
    Ok(existing_comment) -> {
      let now = get_timestamp()
      let approved_comment =
        Comment(..existing_comment, approved: True, updated_at: now)
      let new_comments = dict.insert(store.comments, id, approved_comment)
      let new_store = CommentStore(..store, comments: new_comments)
      Ok(#(new_store, approved_comment))
    }
  }
}

/// Reject/delete a comment (moderation)
pub fn delete_comment(
  store: CommentStore,
  id: Int,
) -> Result(CommentStore, CommentError) {
  case dict.has_key(store.comments, id) {
    False -> Error(CommentNotFound)
    True -> {
      let new_comments = dict.delete(store.comments, id)
      let new_store = CommentStore(..store, comments: new_comments)
      Ok(new_store)
    }
  }
}

/// Get approved comments for a post (public view)
pub fn list_for_post(store: CommentStore, post_id: Int) -> List(Comment) {
  dict.values(store.comments)
  |> list.filter(fn(c) { c.post_id == post_id && c.approved })
}

/// Get all comments for a post (moderation view)
pub fn list_for_post_all(store: CommentStore, post_id: Int) -> List(Comment) {
  dict.values(store.comments)
  |> list.filter(fn(c) { c.post_id == post_id })
}

/// Get pending comments (moderation queue)
pub fn list_pending(store: CommentStore) -> List(Comment) {
  dict.values(store.comments)
  |> list.filter(fn(c) { !c.approved })
}

/// Count approved comments for a post
pub fn count_for_post(store: CommentStore, post_id: Int) -> Int {
  list_for_post(store, post_id)
  |> list.length
}

// Helper functions

fn get_timestamp() -> String {
  "2024-01-15T10:30:00Z"
}
