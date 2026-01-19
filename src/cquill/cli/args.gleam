// CLI Argument Parsing Module
//
// This module handles parsing command line arguments for the cquill CLI tool.
// It provides a simple manual parser for the generate command.

import envoy
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// COMMAND TYPES
// ============================================================================

/// Top-level CLI command
pub type Command {
  /// Generate Gleam code from database schema
  Generate(GenerateOptions)
  /// Show help message
  Help
  /// Show version
  Version
}

/// Options for the generate command
pub type GenerateOptions {
  GenerateOptions(
    /// Database connection URL (required)
    database_url: String,
    /// Output directory for generated files
    output: String,
    /// Database schema to introspect
    schema: String,
    /// Comma-separated list of tables to include (None = all tables)
    tables: Option(String),
    /// Comma-separated list of tables to exclude
    exclude: Option(String),
    /// Module prefix for generated code
    prefix: String,
    /// Watch for schema changes and regenerate
    watch: Bool,
    /// Polling interval in milliseconds for watch mode
    poll_interval: Int,
    /// Run gleam format after generation
    format: Bool,
    /// Show what would be generated without writing
    dry_run: Bool,
    /// Enable verbose output
    verbose: Bool,
    /// Generate typed columns for type-safe query builder
    typed: Bool,
  )
}

// ============================================================================
// PARSE ERROR TYPES
// ============================================================================

/// Errors that can occur during argument parsing
pub type ParseError {
  /// Missing required argument
  MissingRequired(name: String)
  /// Invalid argument value
  InvalidValue(name: String, value: String, reason: String)
  /// Unknown command
  UnknownCommand(name: String)
  /// Missing value for option
  MissingValue(name: String)
  /// Unknown option
  UnknownOption(name: String)
}

/// Format a parse error for display
pub fn format_error(error: ParseError) -> String {
  case error {
    MissingRequired(name) ->
      "Error: Missing required argument: --"
      <> name
      <> "\n\n"
      <> "Use --help for usage information."

    InvalidValue(name, value, reason) ->
      "Error: Invalid value for --" <> name <> ": '" <> value <> "'\n" <> reason

    UnknownCommand(name) ->
      "Error: Unknown command: "
      <> name
      <> "\n\n"
      <> "Available commands: generate\n"
      <> "Use --help for usage information."

    MissingValue(name) -> "Error: Option --" <> name <> " requires a value"

    UnknownOption(name) ->
      "Error: Unknown option: "
      <> name
      <> "\n\n"
      <> "Use --help for usage information."
  }
}

// ============================================================================
// GENERATE OPTIONS DEFAULTS
// ============================================================================

/// Default output directory
pub const default_output = "src/db"

/// Default database schema
pub const default_schema = "public"

/// Default module prefix
pub const default_prefix = "db"

/// Default poll interval in milliseconds (5 seconds)
pub const default_poll_interval = 5000

/// Create default generate options
pub fn default_generate_options() -> GenerateOptions {
  GenerateOptions(
    database_url: "",
    output: default_output,
    schema: default_schema,
    tables: None,
    exclude: None,
    prefix: default_prefix,
    watch: False,
    poll_interval: default_poll_interval,
    format: True,
    dry_run: False,
    verbose: False,
    typed: True,
  )
}

// ============================================================================
// ARGUMENT PARSING
// ============================================================================

/// Parse command line arguments into a Command
pub fn parse(args: List(String)) -> Result(Command, ParseError) {
  // Check for help/version flags first
  case args {
    ["--help"] | ["-h"] | ["help"] -> Ok(Help)
    ["--version"] | ["-V"] | ["version"] -> Ok(Version)
    ["generate", ..rest] -> parse_generate(rest)
    ["generate"] -> parse_generate([])
    [] -> Ok(Help)
    [cmd, ..] -> Error(UnknownCommand(cmd))
  }
}

/// Parse generate command arguments
fn parse_generate(args: List(String)) -> Result(Command, ParseError) {
  // Check for help in generate subcommand
  case list.contains(args, "--help") || list.contains(args, "-h") {
    True -> Ok(Help)
    False -> {
      let opts = default_generate_options()
      case parse_generate_args(args, opts) {
        Ok(parsed_opts) -> resolve_database_url(parsed_opts)
        Error(err) -> Error(err)
      }
    }
  }
}

/// Parse generate command arguments recursively
fn parse_generate_args(
  args: List(String),
  opts: GenerateOptions,
) -> Result(GenerateOptions, ParseError) {
  case args {
    [] -> Ok(opts)

    // Database URL
    ["--database-url", value, ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, database_url: value))

    // Output directory
    ["--output", value, ..rest] | ["-o", value, ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, output: value))

    // Schema
    ["--schema", value, ..rest] | ["-s", value, ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, schema: value))

    // Tables
    ["--tables", value, ..rest] | ["-t", value, ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, tables: Some(value)))

    // Exclude
    ["--exclude", value, ..rest] | ["-e", value, ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, exclude: Some(value)))

    // Prefix
    ["--prefix", value, ..rest] | ["-p", value, ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, prefix: value))

    // Watch flag
    ["--watch", ..rest] | ["-w", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, watch: True))

    // Poll interval
    ["--poll-interval", value, ..rest] ->
      case int.parse(value) {
        Ok(ms) if ms > 0 ->
          parse_generate_args(rest, GenerateOptions(..opts, poll_interval: ms))
        Ok(_) ->
          Error(InvalidValue(
            "poll-interval",
            value,
            "Must be a positive integer (milliseconds)",
          ))
        Error(_) ->
          Error(InvalidValue(
            "poll-interval",
            value,
            "Must be a valid integer (milliseconds)",
          ))
      }

    // Format flag
    ["--format", ..rest] | ["-f", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, format: True))

    ["--no-format", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, format: False))

    // Dry run flag
    ["--dry-run", ..rest] | ["-n", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, dry_run: True))

    // Verbose flag
    ["--verbose", ..rest] | ["-v", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, verbose: True))

    // Typed column generation flag
    ["--typed", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, typed: True))

    ["--no-typed", ..rest] ->
      parse_generate_args(rest, GenerateOptions(..opts, typed: False))

    // Handle options that need values but are at end of list
    ["--database-url"] -> Error(MissingValue("database-url"))
    ["--output"] | ["-o"] -> Error(MissingValue("output"))
    ["--schema"] | ["-s"] -> Error(MissingValue("schema"))
    ["--tables"] | ["-t"] -> Error(MissingValue("tables"))
    ["--exclude"] | ["-e"] -> Error(MissingValue("exclude"))
    ["--prefix"] | ["-p"] -> Error(MissingValue("prefix"))
    ["--poll-interval"] -> Error(MissingValue("poll-interval"))

    // Unknown option
    [opt, ..] ->
      case string.starts_with(opt, "-") {
        True -> Error(UnknownOption(opt))
        False -> Error(UnknownOption(opt))
      }
  }
}

/// Resolve database URL from options or environment variable
fn resolve_database_url(opts: GenerateOptions) -> Result(Command, ParseError) {
  case opts.database_url {
    "" -> {
      // Try environment variable
      case envoy.get("CQUILL_DATABASE_URL") {
        Ok(url) -> Ok(Generate(GenerateOptions(..opts, database_url: url)))
        Error(_) -> Error(MissingRequired("database-url"))
      }
    }
    _url -> Ok(Generate(opts))
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Parse a comma-separated list of table names
pub fn parse_table_list(input: Option(String)) -> List(String) {
  case input {
    None -> []
    Some(str) ->
      str
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(s) { s != "" })
  }
}

/// Check if a table should be included based on include/exclude lists
pub fn should_include_table(
  table_name: String,
  include_tables: List(String),
  exclude_tables: List(String),
) -> Bool {
  let included = case include_tables {
    [] -> True
    tables -> list.contains(tables, table_name)
  }

  let excluded = list.contains(exclude_tables, table_name)

  included && !excluded
}

// ============================================================================
// HELP TEXT
// ============================================================================

/// Generate the main help text
pub fn help_text() -> String {
  "cquill - A compile-time safe database library for Gleam

USAGE:
    cquill <COMMAND> [OPTIONS]

COMMANDS:
    generate    Generate Gleam code from database schema
    help        Show this help message
    version     Show version information

GENERATE OPTIONS:
    --database-url <URL>    Database connection URL (required)
                            Can also use CQUILL_DATABASE_URL env var

    -o, --output <PATH>     Output directory for generated files
                            Default: src/db

    -s, --schema <NAME>     Database schema to introspect
                            Default: public

    -t, --tables <LIST>     Comma-separated list of tables to include
                            Default: all tables

    -e, --exclude <LIST>    Comma-separated list of tables to exclude

    -p, --prefix <MODULE>   Module prefix for generated code
                            Default: db

    -w, --watch             Watch for schema changes and regenerate

    --poll-interval <MS>    Polling interval in milliseconds for watch mode
                            Default: 5000 (5 seconds)

    -f, --format            Run gleam format after generation
                            Default: true
    --no-format             Skip running gleam format

    -n, --dry-run           Show what would be generated without writing

    -v, --verbose           Enable verbose output

    --typed                 Generate typed columns for type-safe queries
                            Default: true
    --no-typed              Skip typed column generation

EXAMPLES:
    # Basic usage
    cquill generate --database-url postgres://user:pass@localhost/mydb

    # Specify output location
    cquill generate --database-url $DATABASE_URL --output src/myapp/db

    # Only specific tables
    cquill generate --database-url $DATABASE_URL --tables users,posts,comments

    # Exclude migration tables
    cquill generate --database-url $DATABASE_URL --exclude schema_migrations

    # Dry run to preview
    cquill generate --database-url $DATABASE_URL --dry-run

    # Watch mode - regenerate on schema changes
    cquill generate --database-url $DATABASE_URL --watch

    # Watch with custom poll interval (2 seconds)
    cquill generate --database-url $DATABASE_URL --watch --poll-interval 2000

For more information, visit: https://hexdocs.pm/cquill"
}

/// Get version string
pub fn version_text() -> String {
  "cquill v0.1.0"
}
