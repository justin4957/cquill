// Tests for the PostgreSQL adapter
//
// Note: These are unit tests for the adapter's pure functions and type
// construction. Integration tests that require a real PostgreSQL database
// should be run separately with a test database configured.

import cquill/adapter
import cquill/adapter/postgres
import gleam/erlang/process
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import pog

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// ADAPTER TESTS (No configuration needed)
// ============================================================================

pub fn postgres_adapter_name_test() {
  let adp = postgres.postgres_adapter()

  adapter.name(adp)
  |> should.equal("postgres")
}

pub fn postgres_adapter_capabilities_test() {
  let adp = postgres.postgres_adapter()
  let caps = adapter.capabilities(adp)

  caps.transactions |> should.be_true
  caps.returning |> should.be_true
  caps.batch_insert |> should.be_true
  caps.upsert |> should.be_true
  caps.json_operations |> should.be_true
  caps.array_types |> should.be_true
  caps.max_params |> should.equal(Some(65_535))
}

pub fn supports_transactions_test() {
  let adp = postgres.postgres_adapter()

  adapter.supports_transactions(adp)
  |> should.be_true
}

pub fn supports_returning_test() {
  let adp = postgres.postgres_adapter()

  adapter.supports_returning(adp)
  |> should.be_true
}

pub fn supports_batch_insert_test() {
  let adp = postgres.postgres_adapter()

  adapter.supports_batch_insert(adp)
  |> should.be_true
}

pub fn supports_upsert_test() {
  let adp = postgres.postgres_adapter()

  adapter.supports_upsert(adp)
  |> should.be_true
}

pub fn supports_json_test() {
  let adp = postgres.postgres_adapter()

  adapter.supports_json(adp)
  |> should.be_true
}

pub fn supports_arrays_test() {
  let adp = postgres.postgres_adapter()

  adapter.supports_arrays(adp)
  |> should.be_true
}

// ============================================================================
// CAPABILITIES COMPARISON TEST
// ============================================================================

pub fn postgres_capabilities_vs_sql_capabilities_test() {
  let pg_caps = postgres.postgres_capabilities()
  let sql_caps = adapter.sql_capabilities()

  // Postgres should have at least all the SQL capabilities
  pg_caps.transactions |> should.equal(sql_caps.transactions)
  pg_caps.returning |> should.equal(sql_caps.returning)
  pg_caps.batch_insert |> should.equal(sql_caps.batch_insert)
  pg_caps.upsert |> should.equal(sql_caps.upsert)
  pg_caps.json_operations |> should.equal(sql_caps.json_operations)
  pg_caps.array_types |> should.equal(sql_caps.array_types)
}

// ============================================================================
// CONFIGURATION TESTS (Using process.new_name)
// ============================================================================

pub fn default_config_test() {
  let name = process.new_name("cquill_test_pool_1")
  let config = postgres.default_config(name)

  config.host |> should.equal("127.0.0.1")
  config.port |> should.equal(5432)
  config.database |> should.equal("postgres")
  config.user |> should.equal("postgres")
  config.password |> should.equal(None)
  config.pool_size |> should.equal(10)
}

pub fn config_builder_functions_test() {
  let name = process.new_name("cquill_test_pool_2")
  let config =
    postgres.default_config(name)
    |> postgres.host("localhost")
    |> postgres.port(5433)
    |> postgres.database("test_db")
    |> postgres.user("test_user")
    |> postgres.password(Some("secret"))
    |> postgres.ssl(pog.SslVerified)
    |> postgres.pool_size(20)

  config.host |> should.equal("localhost")
  config.port |> should.equal(5433)
  config.database |> should.equal("test_db")
  config.user |> should.equal("test_user")
  config.password |> should.equal(Some("secret"))
  config.pool_size |> should.equal(20)
}

// ============================================================================
// URL PARSING TESTS
// ============================================================================

pub fn config_from_url_valid_test() {
  let name = process.new_name("cquill_url_pool_1")
  let url = "postgresql://myuser:mypass@dbhost:5432/mydb"

  case postgres.config_from_url(name, url) {
    Ok(config) -> {
      config.host |> should.equal("dbhost")
      config.port |> should.equal(5432)
      config.database |> should.equal("mydb")
      config.user |> should.equal("myuser")
      config.password |> should.equal(Some("mypass"))
    }
    Error(_) -> should.fail()
  }
}

pub fn config_from_url_no_password_test() {
  let name = process.new_name("cquill_url_pool_2")
  let url = "postgresql://myuser@dbhost:5432/mydb"

  case postgres.config_from_url(name, url) {
    Ok(config) -> {
      config.user |> should.equal("myuser")
      config.password |> should.equal(None)
    }
    Error(_) -> should.fail()
  }
}

pub fn config_from_url_invalid_test() {
  let name = process.new_name("cquill_url_pool_3")
  let url = "not-a-valid-url"

  postgres.config_from_url(name, url)
  |> should.be_error
}

pub fn config_from_url_wrong_scheme_test() {
  let name = process.new_name("cquill_url_pool_4")
  let url = "mysql://myuser:mypass@dbhost:3306/mydb"

  postgres.config_from_url(name, url)
  |> should.be_error
}

// ============================================================================
// NAMED CONNECTION TESTS
// ============================================================================

pub fn named_connection_test() {
  // We can create a named connection reference without starting a pool
  // This tests that the function works, though the connection won't be usable
  let name = process.new_name("cquill_named_pool")
  let _conn = postgres.named_connection(name)

  // If we got here without error, the function works
  True |> should.be_true
}
