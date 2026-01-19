/// Integration tests for the accounts service.
///
/// Tests user creation, authentication, and profile management
/// using in-memory stores.
import blog/schemas/user.{Admin, Author, NewUser, UserUpdate}
import blog/services/accounts
import gleam/option.{None, Some}
import gleeunit/should
import support/factory
import support/setup

// --- User Creation Tests ---

pub fn create_user_test() {
  use ctx <- setup.with_context()

  let new_user =
    NewUser(
      email: "alice@example.com",
      name: Some("Alice"),
      password: "secret123",
    )

  let result = accounts.create_user(ctx.user_store, new_user)

  let assert Ok(#(_, created_user)) = result
  created_user.email
  |> should.equal("alice@example.com")
  created_user.name
  |> should.equal(Some("Alice"))
  created_user.role
  |> should.equal(Author)
}

pub fn create_user_without_name_test() {
  use ctx <- setup.with_context()

  let new_user =
    NewUser(email: "bob@example.com", name: None, password: "secret")

  let result = accounts.create_user(ctx.user_store, new_user)

  let assert Ok(#(_, created_user)) = result
  created_user.name
  |> should.equal(None)
}

pub fn create_user_duplicate_email_test() {
  use ctx <- setup.with_context()

  let new_user =
    NewUser(email: "dupe@example.com", name: None, password: "secret")

  // Create first user
  let assert Ok(#(store, _)) = accounts.create_user(ctx.user_store, new_user)

  // Try to create second user with same email
  let result = accounts.create_user(store, new_user)

  result
  |> should.equal(Error(accounts.EmailAlreadyExists))
}

// --- User Retrieval Tests ---

pub fn get_user_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(store, created)) =
    accounts.create_user(ctx.user_store, new_user)

  let result = accounts.get_user(store, created.id)

  let assert Ok(found) = result
  found.id
  |> should.equal(created.id)
  found.email
  |> should.equal(created.email)
}

pub fn get_user_not_found_test() {
  use ctx <- setup.with_context()

  let result = accounts.get_user(ctx.user_store, 99_999)

  result
  |> should.equal(Error(accounts.UserNotFound))
}

pub fn get_user_by_email_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user_with_email("findme@example.com")
  let assert Ok(#(store, created)) =
    accounts.create_user(ctx.user_store, new_user)

  let result = accounts.get_user_by_email(store, "findme@example.com")

  let assert Ok(found) = result
  found.id
  |> should.equal(created.id)
}

pub fn get_user_by_email_not_found_test() {
  use ctx <- setup.with_context()

  let result =
    accounts.get_user_by_email(ctx.user_store, "nonexistent@example.com")

  result
  |> should.equal(Error(accounts.UserNotFound))
}

pub fn get_public_profile_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(store, created)) =
    accounts.create_user(ctx.user_store, new_user)

  let result = accounts.get_public_profile(store, created.id)

  let assert Ok(profile) = result
  profile.id
  |> should.equal(created.id)
  profile.email
  |> should.equal(created.email)
  // PublicUser doesn't have password_hash field
}

// --- User Update Tests ---

pub fn update_user_name_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(store, created)) =
    accounts.create_user(ctx.user_store, new_user)

  let updates = UserUpdate(name: Some("New Name"), email: None, role: None)

  let result = accounts.update_user(store, created.id, updates)

  let assert Ok(#(_, updated)) = result
  updated.name
  |> should.equal(Some("New Name"))
  updated.email
  |> should.equal(created.email)
}

pub fn update_user_email_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(store, created)) =
    accounts.create_user(ctx.user_store, new_user)

  let updates =
    UserUpdate(name: None, email: Some("newemail@example.com"), role: None)

  let result = accounts.update_user(store, created.id, updates)

  let assert Ok(#(_, updated)) = result
  updated.email
  |> should.equal("newemail@example.com")
}

pub fn update_user_role_test() {
  use ctx <- setup.with_context()

  let new_user = factory.new_user()
  let assert Ok(#(store, created)) =
    accounts.create_user(ctx.user_store, new_user)

  let updates = UserUpdate(name: None, email: None, role: Some(Admin))

  let result = accounts.update_user(store, created.id, updates)

  let assert Ok(#(_, updated)) = result
  updated.role
  |> should.equal(Admin)
}

// --- Authentication Tests ---

pub fn authenticate_success_test() {
  use ctx <- setup.with_context()

  let new_user =
    NewUser(
      email: "auth@example.com",
      name: Some("Auth User"),
      password: "mypassword",
    )

  let assert Ok(#(store, _)) = accounts.create_user(ctx.user_store, new_user)

  let result = accounts.authenticate(store, "auth@example.com", "mypassword")

  let assert Ok(authenticated) = result
  authenticated.email
  |> should.equal("auth@example.com")
}

pub fn authenticate_wrong_password_test() {
  use ctx <- setup.with_context()

  let new_user =
    NewUser(email: "auth2@example.com", name: None, password: "correct")

  let assert Ok(#(store, _)) = accounts.create_user(ctx.user_store, new_user)

  let result = accounts.authenticate(store, "auth2@example.com", "wrong")

  result
  |> should.equal(Error(accounts.InvalidCredentials))
}

pub fn authenticate_unknown_email_test() {
  use ctx <- setup.with_context()

  let result =
    accounts.authenticate(ctx.user_store, "unknown@example.com", "password")

  result
  |> should.equal(Error(accounts.InvalidCredentials))
}

// --- List Users Tests ---

pub fn list_users_test() {
  use ctx <- setup.with_context()

  let user1 = factory.new_user_with_email("user1@example.com")
  let user2 = factory.new_user_with_email("user2@example.com")

  let assert Ok(#(store, _)) = accounts.create_user(ctx.user_store, user1)
  let assert Ok(#(store, _)) = accounts.create_user(store, user2)

  let users = accounts.list_users(store)

  users
  |> should.not_equal([])
}
