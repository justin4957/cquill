/// Account management service.
///
/// Handles user creation, authentication, and profile management.
/// This module demonstrates:
/// - Business logic separate from data access
/// - Multiple schema types for different operations
/// - Error handling patterns
///
/// NOTE: This is a conceptual example showing the patterns. In a production
/// application, you would integrate with the full cquill adapter/repo layer.
import blog/schemas/user.{
  type NewUser, type PublicUser, type User, type UserUpdate, User,
}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result

/// Service-specific errors
pub type AccountError {
  EmailAlreadyExists
  UserNotFound
  InvalidCredentials
  DatabaseError(String)
}

/// In-memory user storage for demonstration
/// In a real app, this would use the adapter layer
pub opaque type UserStore {
  UserStore(users: dict.Dict(Int, User), next_id: Int)
}

/// Create a new user store
pub fn new_store() -> UserStore {
  UserStore(users: dict.new(), next_id: 1)
}

/// Create a new user account
///
/// ## Example
///
/// ```gleam
/// let store = accounts.new_store()
/// let new_user = NewUser(
///   email: "alice@example.com",
///   name: Some("Alice"),
///   password: "secret123",
/// )
///
/// case accounts.create_user(store, new_user) {
///   Ok(#(store, user)) -> io.println("Created user: " <> user.email)
///   Error(EmailAlreadyExists) -> io.println("Email already in use")
///   Error(e) -> io.println("Error: " <> string.inspect(e))
/// }
/// ```
pub fn create_user(
  store: UserStore,
  new_user: NewUser,
) -> Result(#(UserStore, User), AccountError) {
  // Check if email already exists
  let email_exists =
    dict.values(store.users)
    |> list.any(fn(u) { u.email == new_user.email })

  case email_exists {
    True -> Error(EmailAlreadyExists)
    False -> {
      let id = store.next_id
      let now = get_timestamp()
      let password_hash = hash_password(new_user.password)

      let created_user =
        User(
          id: id,
          email: new_user.email,
          name: new_user.name,
          role: user.Author,
          password_hash: password_hash,
          inserted_at: now,
          updated_at: now,
        )

      let new_users = dict.insert(store.users, id, created_user)
      let new_store = UserStore(users: new_users, next_id: id + 1)

      Ok(#(new_store, created_user))
    }
  }
}

/// Get a user by ID
pub fn get_user(store: UserStore, id: Int) -> Result(User, AccountError) {
  case dict.get(store.users, id) {
    Ok(found_user) -> Ok(found_user)
    Error(_) -> Error(UserNotFound)
  }
}

/// Get a user by email
pub fn get_user_by_email(
  store: UserStore,
  email: String,
) -> Result(User, AccountError) {
  dict.values(store.users)
  |> list.find(fn(u) { u.email == email })
  |> result.replace_error(UserNotFound)
}

/// Get a user's public profile
pub fn get_public_profile(
  store: UserStore,
  id: Int,
) -> Result(PublicUser, AccountError) {
  case get_user(store, id) {
    Ok(found_user) -> Ok(user.to_public(found_user))
    Error(e) -> Error(e)
  }
}

/// Update a user's profile
pub fn update_user(
  store: UserStore,
  id: Int,
  updates: UserUpdate,
) -> Result(#(UserStore, User), AccountError) {
  case get_user(store, id) {
    Error(e) -> Error(e)
    Ok(existing_user) -> {
      let name = case updates.name {
        Some(n) -> Some(n)
        None -> existing_user.name
      }

      let email = case updates.email {
        Some(e) -> e
        None -> existing_user.email
      }

      let role = case updates.role {
        Some(r) -> r
        None -> existing_user.role
      }

      let updated_user =
        User(
          ..existing_user,
          name: name,
          email: email,
          role: role,
          updated_at: get_timestamp(),
        )

      let new_users = dict.insert(store.users, id, updated_user)
      let new_store = UserStore(..store, users: new_users)

      Ok(#(new_store, updated_user))
    }
  }
}

/// Authenticate a user with email and password
pub fn authenticate(
  store: UserStore,
  email: String,
  password: String,
) -> Result(User, AccountError) {
  case get_user_by_email(store, email) {
    Ok(found_user) -> {
      case verify_password(password, found_user.password_hash) {
        True -> Ok(found_user)
        False -> Error(InvalidCredentials)
      }
    }
    Error(UserNotFound) -> Error(InvalidCredentials)
    Error(e) -> Error(e)
  }
}

/// List all users
pub fn list_users(store: UserStore) -> List(PublicUser) {
  dict.values(store.users)
  |> list.map(user.to_public)
}

// Helper functions (simplified for demo)

fn hash_password(password: String) -> String {
  // In real app, use bcrypt or argon2
  "hashed_" <> password
}

fn verify_password(password: String, hash: String) -> Bool {
  // In real app, use proper hash verification
  hash == "hashed_" <> password
}

fn get_timestamp() -> String {
  // In real app, use proper datetime
  "2024-01-15T10:30:00Z"
}
