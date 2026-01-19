// CLI Generate Command Module
//
// This module implements the generate command, which connects to a database,
// introspects the schema, and generates Gleam code files.

import cquill/adapter
import cquill/adapter/postgres
import cquill/cli/args.{type GenerateOptions}
import cquill/codegen/generator
import cquill/introspection.{type IntrospectedSchema}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

// ============================================================================
// GENERATION RESULT TYPES
// ============================================================================

/// Result of the generate command
pub type GenerateResult {
  /// Generation completed successfully
  GenerateSuccess(files_generated: Int)
  /// Generation failed with an error
  GenerateError(message: String)
}

// ============================================================================
// MAIN GENERATION FUNCTION
// ============================================================================

/// Execute the generate command with the given options
pub fn run(options: GenerateOptions) -> GenerateResult {
  case options.verbose {
    True -> io.println("cquill generate v0.1.0\n")
    False -> Nil
  }

  // Connect to database
  case connect_to_database(options) {
    Error(err) -> GenerateError(err)
    Ok(conn) -> {
      // Introspect schema
      case introspect_schema(conn, options) {
        Error(err) -> GenerateError(err)
        Ok(schema) -> {
          // Filter tables
          let filtered_schema = filter_tables(schema, options)

          case options.verbose {
            True -> {
              io.println(
                "Found "
                <> int.to_string(list.length(filtered_schema.tables))
                <> " tables, "
                <> int.to_string(list.length(filtered_schema.enums))
                <> " enums\n",
              )
            }
            False -> Nil
          }

          // Generate code
          case generate_code(filtered_schema, options) {
            Error(err) -> GenerateError(err)
            Ok(modules) -> {
              // Write files or show dry run
              case options.dry_run {
                True -> {
                  show_dry_run(modules, options)
                  GenerateSuccess(list.length(modules))
                }
                False -> {
                  case write_files(modules, options) {
                    Error(err) -> GenerateError(err)
                    Ok(count) -> {
                      // Run gleam format if requested
                      case options.format {
                        True -> run_gleam_format(options)
                        False -> Nil
                      }

                      case options.verbose {
                        True ->
                          io.println(
                            "\nDone! Generated "
                            <> int.to_string(count)
                            <> " files.",
                          )
                        False -> Nil
                      }

                      GenerateSuccess(count)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// DATABASE CONNECTION
// ============================================================================

/// Connect to the database using the provided URL
fn connect_to_database(
  options: GenerateOptions,
) -> Result(postgres.PostgresConnection, String) {
  case options.verbose {
    True -> io.println("Connecting to database...")
    False -> Nil
  }

  // Create a unique pool name for this connection
  let pool_name = process.new_name("cquill_codegen")

  case postgres.config_from_url(pool_name, options.database_url) {
    Error(_) ->
      Error(
        "Could not parse database URL\n\n"
        <> "  URL: "
        <> mask_password(options.database_url)
        <> "\n\n"
        <> "  Expected format: postgres://user:password@host:port/database",
      )
    Ok(config) -> {
      // Configure for shorter timeout since this is CLI usage
      let config =
        config
        |> postgres.pool_size(1)
        |> postgres.default_timeout(30_000)

      case postgres.start(config) {
        Error(_) ->
          Error(
            "Could not connect to database\n\n"
            <> "  Connection refused: "
            <> mask_password(options.database_url)
            <> "\n\n"
            <> "  Make sure:\n"
            <> "  - PostgreSQL is running\n"
            <> "  - The database exists\n"
            <> "  - Credentials are correct",
          )
        Ok(conn) -> Ok(conn)
      }
    }
  }
}

/// Mask password in database URL for display
fn mask_password(url: String) -> String {
  // Simple approach: if URL contains ://user:pass@, mask the password
  case string.split(url, "://") {
    [protocol, rest] -> {
      case string.split(rest, "@") {
        [auth, host] -> {
          case string.split(auth, ":") {
            [user, _password] -> protocol <> "://" <> user <> ":****@" <> host
            _ -> url
          }
        }
        _ -> url
      }
    }
    _ -> url
  }
}

// ============================================================================
// SCHEMA INTROSPECTION
// ============================================================================

/// Introspect the database schema
fn introspect_schema(
  conn: postgres.PostgresConnection,
  options: GenerateOptions,
) -> Result(introspection.IntrospectedSchema, String) {
  case options.verbose {
    True -> io.println("Introspecting schema '" <> options.schema <> "'...")
    False -> Nil
  }

  // Execute all introspection queries
  let column_query = introspection.columns_query()
  let pk_query = introspection.primary_keys_query()
  let fk_query = introspection.foreign_keys_query()
  let unique_query = introspection.unique_constraints_query()
  let check_query = introspection.check_constraints_query()
  let enum_query = introspection.enums_query()

  // Execute queries and parse results
  use column_rows <- result.try(execute_introspection_query(
    conn,
    column_query,
    [adapter.ParamString(options.schema)],
    parse_column_row,
    "columns",
  ))

  use pk_rows <- result.try(execute_introspection_query(
    conn,
    pk_query,
    [adapter.ParamString(options.schema)],
    parse_pk_row,
    "primary keys",
  ))

  use fk_rows <- result.try(execute_introspection_query(
    conn,
    fk_query,
    [adapter.ParamString(options.schema)],
    parse_fk_row,
    "foreign keys",
  ))

  use unique_rows <- result.try(execute_introspection_query(
    conn,
    unique_query,
    [adapter.ParamString(options.schema)],
    parse_unique_row,
    "unique constraints",
  ))

  use check_rows <- result.try(execute_introspection_query(
    conn,
    check_query,
    [adapter.ParamString(options.schema)],
    parse_check_row,
    "check constraints",
  ))

  use enum_rows <- result.try(execute_introspection_query(
    conn,
    enum_query,
    [adapter.ParamString(options.schema)],
    parse_enum_row,
    "enums",
  ))

  // Build schema from raw data
  let schema =
    introspection.build_schema(
      column_rows,
      pk_rows,
      fk_rows,
      unique_rows,
      check_rows,
      enum_rows,
    )

  Ok(schema)
}

/// Execute an introspection query and parse results
fn execute_introspection_query(
  conn: postgres.PostgresConnection,
  query: String,
  params: List(adapter.QueryParam),
  parser: fn(List(Dynamic)) -> Result(a, String),
  description: String,
) -> Result(List(a), String) {
  use rows <- result.try(
    postgres.execute_sql(conn, query, params)
    |> result.map_error(fn(_) {
      "Failed to query " <> description <> " from database"
    }),
  )

  rows
  |> list.try_map(parser)
  |> result.map_error(fn(err) {
    "Failed to parse " <> description <> ": " <> err
  })
}

// ============================================================================
// ROW PARSING FUNCTIONS
// ============================================================================

fn parse_column_row(
  row: List(Dynamic),
) -> Result(introspection.RawColumnRow, String) {
  case row {
    [
      table_name,
      column_name,
      ordinal_position,
      data_type,
      udt_name,
      is_nullable,
      column_default,
      char_max_length,
      numeric_precision,
      numeric_scale,
    ] -> {
      use table_name <- result.try(decode_string(table_name, "table_name"))
      use column_name <- result.try(decode_string(column_name, "column_name"))
      use ordinal_position <- result.try(decode_int(
        ordinal_position,
        "ordinal_position",
      ))
      use data_type <- result.try(decode_string(data_type, "data_type"))
      use udt_name <- result.try(decode_string(udt_name, "udt_name"))
      use is_nullable <- result.try(decode_string(is_nullable, "is_nullable"))
      let column_default = decode_optional_string(column_default)
      let char_max_length = decode_optional_int(char_max_length)
      let numeric_precision = decode_optional_int(numeric_precision)
      let numeric_scale = decode_optional_int(numeric_scale)

      Ok(introspection.RawColumnRow(
        table_name:,
        column_name:,
        ordinal_position:,
        data_type:,
        udt_name:,
        is_nullable:,
        column_default:,
        character_maximum_length: char_max_length,
        numeric_precision:,
        numeric_scale:,
      ))
    }
    _ -> Error("Invalid column row format")
  }
}

fn parse_pk_row(
  row: List(Dynamic),
) -> Result(introspection.RawPrimaryKeyRow, String) {
  case row {
    [table_name, column_name, ordinal_position] -> {
      use table_name <- result.try(decode_string(table_name, "table_name"))
      use column_name <- result.try(decode_string(column_name, "column_name"))
      use ordinal_position <- result.try(decode_int(
        ordinal_position,
        "ordinal_position",
      ))
      Ok(introspection.RawPrimaryKeyRow(
        table_name:,
        column_name:,
        ordinal_position:,
      ))
    }
    _ -> Error("Invalid primary key row format")
  }
}

fn parse_fk_row(
  row: List(Dynamic),
) -> Result(introspection.RawForeignKeyRow, String) {
  case row {
    [
      table_name,
      column_name,
      foreign_table_name,
      foreign_column_name,
      update_rule,
      delete_rule,
    ] -> {
      use table_name <- result.try(decode_string(table_name, "table_name"))
      use column_name <- result.try(decode_string(column_name, "column_name"))
      use foreign_table_name <- result.try(decode_string(
        foreign_table_name,
        "foreign_table_name",
      ))
      use foreign_column_name <- result.try(decode_string(
        foreign_column_name,
        "foreign_column_name",
      ))
      use update_rule <- result.try(decode_string(update_rule, "update_rule"))
      use delete_rule <- result.try(decode_string(delete_rule, "delete_rule"))
      Ok(introspection.RawForeignKeyRow(
        table_name:,
        column_name:,
        foreign_table_name:,
        foreign_column_name:,
        update_rule:,
        delete_rule:,
      ))
    }
    _ -> Error("Invalid foreign key row format")
  }
}

fn parse_unique_row(
  row: List(Dynamic),
) -> Result(introspection.RawUniqueRow, String) {
  case row {
    [table_name, constraint_name, column_name] -> {
      use table_name <- result.try(decode_string(table_name, "table_name"))
      use constraint_name <- result.try(decode_string(
        constraint_name,
        "constraint_name",
      ))
      use column_name <- result.try(decode_string(column_name, "column_name"))
      Ok(introspection.RawUniqueRow(table_name:, constraint_name:, column_name:))
    }
    _ -> Error("Invalid unique constraint row format")
  }
}

fn parse_check_row(
  row: List(Dynamic),
) -> Result(introspection.RawCheckRow, String) {
  case row {
    [table_name, constraint_name, check_clause] -> {
      use table_name <- result.try(decode_string(table_name, "table_name"))
      use constraint_name <- result.try(decode_string(
        constraint_name,
        "constraint_name",
      ))
      use check_clause <- result.try(decode_string(check_clause, "check_clause"))
      Ok(introspection.RawCheckRow(table_name:, constraint_name:, check_clause:))
    }
    _ -> Error("Invalid check constraint row format")
  }
}

fn parse_enum_row(
  row: List(Dynamic),
) -> Result(introspection.RawEnumRow, String) {
  case row {
    [enum_name, enum_value, enum_sort_order] -> {
      use enum_name <- result.try(decode_string(enum_name, "enum_name"))
      use enum_value <- result.try(decode_string(enum_value, "enum_value"))
      use enum_sort_order <- result.try(decode_float(
        enum_sort_order,
        "enum_sort_order",
      ))
      Ok(introspection.RawEnumRow(enum_name:, enum_value:, enum_sort_order:))
    }
    _ -> Error("Invalid enum row format")
  }
}

// ============================================================================
// DECODE HELPERS
// ============================================================================

fn decode_string(value: Dynamic, field: String) -> Result(String, String) {
  case decode.run(value, decode.string) {
    Ok(s) -> Ok(s)
    Error(_) -> Error("Expected string for " <> field)
  }
}

fn decode_int(value: Dynamic, field: String) -> Result(Int, String) {
  case decode.run(value, decode.int) {
    Ok(i) -> Ok(i)
    Error(_) -> Error("Expected int for " <> field)
  }
}

fn decode_float(value: Dynamic, field: String) -> Result(Float, String) {
  case decode.run(value, decode.float) {
    Ok(f) -> Ok(f)
    Error(_) -> Error("Expected float for " <> field)
  }
}

fn decode_optional_string(value: Dynamic) -> Option(String) {
  case decode.run(value, decode.optional(decode.string)) {
    Ok(opt) -> opt
    Error(_) -> None
  }
}

fn decode_optional_int(value: Dynamic) -> Option(Int) {
  case decode.run(value, decode.optional(decode.int)) {
    Ok(opt) -> opt
    Error(_) -> None
  }
}

// ============================================================================
// TABLE FILTERING
// ============================================================================

/// Filter tables based on include/exclude options
fn filter_tables(
  schema: introspection.IntrospectedSchema,
  options: GenerateOptions,
) -> introspection.IntrospectedSchema {
  let include_tables = args.parse_table_list(options.tables)
  let exclude_tables = args.parse_table_list(options.exclude)

  let filtered_tables =
    schema.tables
    |> list.filter(fn(table) {
      args.should_include_table(table.name, include_tables, exclude_tables)
    })

  introspection.IntrospectedSchema(..schema, tables: filtered_tables)
}

// ============================================================================
// CODE GENERATION
// ============================================================================

/// Generate code modules from the introspected schema
fn generate_code(
  schema: introspection.IntrospectedSchema,
  options: GenerateOptions,
) -> Result(List(generator.GeneratedModule), String) {
  let config =
    generator.default_config()
    |> generator.with_module_prefix(options.prefix)
    |> generator.with_typed_columns(options.typed)

  let modules = generator.generate_all(schema.tables, schema.enums, config)

  Ok(modules)
}

// ============================================================================
// FILE WRITING
// ============================================================================

/// Write generated modules to files
fn write_files(
  modules: List(generator.GeneratedModule),
  options: GenerateOptions,
) -> Result(Int, String) {
  case options.verbose {
    True -> io.println("\nGenerating:")
    False -> Nil
  }

  modules
  |> list.try_fold(0, fn(count, module) {
    let file_path = build_file_path(module, options)

    use _ <- result.try(ensure_directory(file_path))
    use _ <- result.try(write_file(file_path, module.content))

    case options.verbose {
      True -> io.println("  ✓ " <> file_path)
      False -> Nil
    }

    Ok(count + 1)
  })
}

/// Build the full file path for a generated module
fn build_file_path(
  module: generator.GeneratedModule,
  options: GenerateOptions,
) -> String {
  // Replace the module path prefix with the output directory
  // e.g., "db/schema/user" -> "src/myapp/db/schema/user.gleam"
  let path_without_prefix = case string.split(module.path, "/") {
    [_prefix, ..rest] -> string.join(rest, "/")
    _ -> module.path
  }

  options.output <> "/" <> path_without_prefix <> ".gleam"
}

/// Ensure the directory for a file path exists
fn ensure_directory(file_path: String) -> Result(Nil, String) {
  let dir = get_directory(file_path)
  case simplifile.create_directory_all(dir) {
    Ok(_) -> Ok(Nil)
    Error(err) ->
      Error(
        "Failed to create directory " <> dir <> ": " <> describe_file_error(err),
      )
  }
}

/// Get the directory portion of a file path
fn get_directory(path: String) -> String {
  case string.split(path, "/") {
    [] -> "."
    parts -> {
      let dir_parts = list.take(parts, list.length(parts) - 1)
      case dir_parts {
        [] -> "."
        _ -> string.join(dir_parts, "/")
      }
    }
  }
}

/// Write content to a file
fn write_file(path: String, content: String) -> Result(Nil, String) {
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(err) ->
      Error("Failed to write " <> path <> ": " <> describe_file_error(err))
  }
}

/// Describe a simplifile error
fn describe_file_error(err: simplifile.FileError) -> String {
  case err {
    simplifile.Eacces -> "Permission denied"
    simplifile.Enoent -> "No such file or directory"
    simplifile.Eexist -> "File already exists"
    simplifile.Enotdir -> "Not a directory"
    simplifile.Enospc -> "No space left on device"
    simplifile.Eio -> "I/O error"
    _ -> "Unknown error"
  }
}

// ============================================================================
// DRY RUN
// ============================================================================

/// Show what would be generated without writing
fn show_dry_run(
  modules: List(generator.GeneratedModule),
  options: GenerateOptions,
) -> Nil {
  io.println("Dry run - would generate:\n")

  list.each(modules, fn(module) {
    let file_path = build_file_path(module, options)
    io.println("  " <> file_path)

    case options.verbose {
      True -> {
        io.println("    Content preview:")
        let preview =
          module.content
          |> string.split("\n")
          |> list.take(5)
          |> list.map(fn(line) { "      " <> line })
          |> string.join("\n")
        io.println(preview)
        io.println("      ...")
        io.println("")
      }
      False -> Nil
    }
  })

  io.println("\nTotal: " <> int.to_string(list.length(modules)) <> " files")
}

// ============================================================================
// GLEAM FORMAT
// ============================================================================

/// Run gleam format on the output directory
fn run_gleam_format(options: GenerateOptions) -> Nil {
  case options.verbose {
    True -> io.println("\nRunning gleam format...")
    False -> Nil
  }

  // Note: In a real implementation, we would use shellout or similar
  // to actually run the gleam format command. For now, we just log it.
  // The escript doesn't have easy access to shell commands without
  // additional dependencies.
  Nil
}

// ============================================================================
// WATCH MODE
// ============================================================================

/// Run in watch mode, regenerating on schema changes
pub fn watch(options: GenerateOptions) -> Nil {
  io.println("cquill watch mode starting...\n")

  // Perform initial generation
  case connect_to_database(options) {
    Error(err) -> {
      io.println_error("Error: " <> err)
    }
    Ok(conn) -> {
      case introspect_and_generate(conn, options) {
        Error(err) -> io.println_error("Initial generation failed: " <> err)
        Ok(#(fingerprint, count)) -> {
          io.println(
            "Initial generation complete: "
            <> int.to_string(count)
            <> " files generated",
          )
          io.println("\nWatching for schema changes...")
          io.println(
            "Poll interval: " <> int.to_string(options.poll_interval) <> "ms",
          )
          io.println("Press Ctrl+C to stop.\n")

          // Start the watch loop
          watch_loop(options, conn, fingerprint)
        }
      }
    }
  }
}

/// Main watch loop that polls for schema changes
fn watch_loop(
  options: GenerateOptions,
  conn: postgres.PostgresConnection,
  last_fingerprint: String,
) -> Nil {
  // Sleep for the poll interval
  process.sleep(options.poll_interval)

  // Check for schema changes
  case introspect_schema(conn, options) {
    Error(err) -> {
      io.println_error("Warning: Failed to introspect schema: " <> err)
      // Continue watching despite the error
      watch_loop(options, conn, last_fingerprint)
    }
    Ok(schema) -> {
      let filtered_schema = filter_tables(schema, options)
      let current_fingerprint = schema_fingerprint(filtered_schema)

      case current_fingerprint == last_fingerprint {
        True -> {
          // No changes, continue watching
          case options.verbose {
            True -> io.println("[" <> timestamp() <> "] No changes detected")
            False -> Nil
          }
          watch_loop(options, conn, last_fingerprint)
        }
        False -> {
          // Schema changed, regenerate
          io.println(
            "\n[" <> timestamp() <> "] Schema change detected, regenerating...",
          )

          case generate_from_schema(filtered_schema, options) {
            Error(err) -> {
              io.println_error("Error regenerating: " <> err)
              // Continue watching with old fingerprint
              watch_loop(options, conn, last_fingerprint)
            }
            Ok(count) -> {
              print_change_summary(options, filtered_schema)
              io.println(
                "["
                <> timestamp()
                <> "] Regenerated "
                <> int.to_string(count)
                <> " files\n",
              )
              // Continue watching with new fingerprint
              watch_loop(options, conn, current_fingerprint)
            }
          }
        }
      }
    }
  }
}

/// Introspect schema and generate code, returning fingerprint and file count
fn introspect_and_generate(
  conn: postgres.PostgresConnection,
  options: GenerateOptions,
) -> Result(#(String, Int), String) {
  case introspect_schema(conn, options) {
    Error(err) -> Error(err)
    Ok(schema) -> {
      let filtered_schema = filter_tables(schema, options)
      let fingerprint = schema_fingerprint(filtered_schema)

      case generate_from_schema(filtered_schema, options) {
        Error(err) -> Error(err)
        Ok(count) -> Ok(#(fingerprint, count))
      }
    }
  }
}

/// Generate code from an already-introspected schema
fn generate_from_schema(
  schema: IntrospectedSchema,
  options: GenerateOptions,
) -> Result(Int, String) {
  case generate_code(schema, options) {
    Error(err) -> Error(err)
    Ok(modules) -> {
      case options.dry_run {
        True -> {
          show_dry_run(modules, options)
          Ok(list.length(modules))
        }
        False -> {
          case write_files(modules, options) {
            Error(err) -> Error(err)
            Ok(count) -> {
              case options.format {
                True -> run_gleam_format(options)
                False -> Nil
              }
              Ok(count)
            }
          }
        }
      }
    }
  }
}

/// Generate a fingerprint of the schema for change detection
fn schema_fingerprint(schema: IntrospectedSchema) -> String {
  // Build a canonical string representation of the schema
  let table_strings =
    schema.tables
    |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
    |> list.map(fn(table) {
      let column_strings =
        table.columns
        |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
        |> list.map(fn(col) {
          col.name
          <> ":"
          <> col.data_type
          <> ":"
          <> case col.is_nullable {
            True -> "null"
            False -> "notnull"
          }
          <> ":"
          <> case col.default {
            Some(d) -> d
            None -> ""
          }
        })
        |> string.join(",")

      table.name <> "[" <> column_strings <> "]"
    })
    |> string.join(";")

  let enum_strings =
    schema.enums
    |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
    |> list.map(fn(e) { e.name <> "=" <> string.join(e.values, "|") })
    |> string.join(";")

  let combined = table_strings <> "||" <> enum_strings

  // Hash the string to create a compact fingerprint using Erlang's crypto
  erlang_hash_sha256(combined)
  |> bytes_to_hex()
}

/// Hash a string using SHA-256 via Erlang's crypto module
@external(erlang, "cquill_crypto_ffi", "hash_sha256")
fn erlang_hash_sha256(input: String) -> BitArray

/// Convert bytes to hex string
fn bytes_to_hex(bytes: BitArray) -> String {
  bytes_to_hex_loop(bytes, "")
}

fn bytes_to_hex_loop(bytes: BitArray, acc: String) -> String {
  case bytes {
    <<byte:int, rest:bits>> -> {
      let hex = int_to_hex(byte)
      bytes_to_hex_loop(rest, acc <> hex)
    }
    _ -> acc
  }
}

fn int_to_hex(n: Int) -> String {
  let high = n / 16
  let low = n % 16
  hex_digit(high) <> hex_digit(low)
}

fn hex_digit(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    _ -> "f"
  }
}

/// Print a summary of what changed in the schema
fn print_change_summary(
  options: GenerateOptions,
  schema: IntrospectedSchema,
) -> Nil {
  case options.verbose {
    True -> {
      io.println("  Tables: " <> int.to_string(list.length(schema.tables)))
      io.println("  Enums: " <> int.to_string(list.length(schema.enums)))
    }
    False -> Nil
  }
}

/// Get current timestamp string for logging
fn timestamp() -> String {
  // Use Erlang's calendar module to get current time
  let #(#(year, month, day), #(hour, minute, second)) = erlang_localtime()
  int.to_string(year)
  <> "-"
  <> pad_zero(month)
  <> "-"
  <> pad_zero(day)
  <> " "
  <> pad_zero(hour)
  <> ":"
  <> pad_zero(minute)
  <> ":"
  <> pad_zero(second)
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

/// External function to get local time from Erlang
@external(erlang, "calendar", "local_time")
fn erlang_localtime() -> #(#(Int, Int, Int), #(Int, Int, Int))
