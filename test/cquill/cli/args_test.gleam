// CLI Argument Parsing Tests
//
// Tests for the argument parsing module that handles command line arguments.

import cquill/cli/args.{
  type Command, type GenerateOptions, type ParseError, Generate, Help,
  MissingRequired, MissingValue, UnknownCommand, UnknownOption, Version,
  default_generate_options, format_error, parse, parse_table_list,
  should_include_table,
}
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// COMMAND PARSING TESTS
// ============================================================================

pub fn parse_empty_args_returns_help_test() {
  parse([])
  |> should.equal(Ok(Help))
}

pub fn parse_help_flag_returns_help_test() {
  parse(["--help"])
  |> should.equal(Ok(Help))

  parse(["-h"])
  |> should.equal(Ok(Help))

  parse(["help"])
  |> should.equal(Ok(Help))
}

pub fn parse_version_flag_returns_version_test() {
  parse(["--version"])
  |> should.equal(Ok(Version))

  parse(["-V"])
  |> should.equal(Ok(Version))

  parse(["version"])
  |> should.equal(Ok(Version))
}

pub fn parse_unknown_command_returns_error_test() {
  parse(["unknown"])
  |> should.equal(Error(UnknownCommand("unknown")))

  parse(["foo", "bar"])
  |> should.equal(Error(UnknownCommand("foo")))
}

// ============================================================================
// GENERATE COMMAND PARSING TESTS
// ============================================================================

pub fn parse_generate_without_database_url_returns_error_test() {
  // Without env var set, should fail
  parse(["generate"])
  |> should.equal(Error(MissingRequired("database-url")))
}

pub fn parse_generate_with_database_url_test() {
  let result =
    parse(["generate", "--database-url", "postgres://localhost/mydb"])

  case result {
    Ok(Generate(opts)) -> {
      opts.database_url
      |> should.equal("postgres://localhost/mydb")
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_output_option_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--output",
      "src/custom/db",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.output
      |> should.equal("src/custom/db")
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_short_output_option_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "-o",
      "src/custom/db",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.output
      |> should.equal("src/custom/db")
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_schema_option_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--schema",
      "my_schema",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.schema
      |> should.equal("my_schema")
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_tables_option_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--tables",
      "users,posts,comments",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.tables
      |> should.equal(Some("users,posts,comments"))
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_exclude_option_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--exclude",
      "schema_migrations",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.exclude
      |> should.equal(Some("schema_migrations"))
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_prefix_option_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--prefix",
      "myapp/database",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.prefix
      |> should.equal("myapp/database")
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_watch_flag_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--watch",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.watch
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_short_watch_flag_test() {
  let result =
    parse(["generate", "--database-url", "postgres://localhost/mydb", "-w"])

  case result {
    Ok(Generate(opts)) -> {
      opts.watch
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_dry_run_flag_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--dry-run",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.dry_run
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_short_dry_run_flag_test() {
  let result =
    parse(["generate", "--database-url", "postgres://localhost/mydb", "-n"])

  case result {
    Ok(Generate(opts)) -> {
      opts.dry_run
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_verbose_flag_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--verbose",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.verbose
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_no_format_flag_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--no-format",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.format
      |> should.be_false
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_format_defaults_to_true_test() {
  let result =
    parse(["generate", "--database-url", "postgres://localhost/mydb"])

  case result {
    Ok(Generate(opts)) -> {
      opts.format
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_typed_defaults_to_true_test() {
  let result =
    parse(["generate", "--database-url", "postgres://localhost/mydb"])

  case result {
    Ok(Generate(opts)) -> {
      opts.typed
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_typed_flag_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--typed",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.typed
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_no_typed_flag_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--no-typed",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.typed
      |> should.be_false
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_with_multiple_options_test() {
  let result =
    parse([
      "generate",
      "--database-url",
      "postgres://localhost/mydb",
      "--output",
      "src/app/db",
      "--schema",
      "myschema",
      "--tables",
      "users,posts",
      "--prefix",
      "app/db",
      "--verbose",
      "--dry-run",
    ])

  case result {
    Ok(Generate(opts)) -> {
      opts.database_url
      |> should.equal("postgres://localhost/mydb")

      opts.output
      |> should.equal("src/app/db")

      opts.schema
      |> should.equal("myschema")

      opts.tables
      |> should.equal(Some("users,posts"))

      opts.prefix
      |> should.equal("app/db")

      opts.verbose
      |> should.be_true

      opts.dry_run
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn parse_generate_help_in_subcommand_returns_help_test() {
  parse(["generate", "--help"])
  |> should.equal(Ok(Help))

  parse(["generate", "-h"])
  |> should.equal(Ok(Help))

  parse(["generate", "--database-url", "postgres://localhost/mydb", "--help"])
  |> should.equal(Ok(Help))
}

// ============================================================================
// ERROR CASES
// ============================================================================

pub fn parse_generate_missing_database_url_value_test() {
  parse(["generate", "--database-url"])
  |> should.equal(Error(MissingValue("database-url")))
}

pub fn parse_generate_missing_output_value_test() {
  parse(["generate", "--database-url", "postgres://localhost/mydb", "--output"])
  |> should.equal(Error(MissingValue("output")))

  parse(["generate", "--database-url", "postgres://localhost/mydb", "-o"])
  |> should.equal(Error(MissingValue("output")))
}

pub fn parse_generate_unknown_option_test() {
  parse(["generate", "--database-url", "postgres://localhost/mydb", "--unknown"])
  |> should.equal(Error(UnknownOption("--unknown")))
}

// ============================================================================
// DEFAULT OPTIONS TESTS
// ============================================================================

pub fn default_generate_options_has_correct_defaults_test() {
  let opts = default_generate_options()

  opts.database_url
  |> should.equal("")

  opts.output
  |> should.equal("src/db")

  opts.schema
  |> should.equal("public")

  opts.tables
  |> should.equal(None)

  opts.exclude
  |> should.equal(None)

  opts.prefix
  |> should.equal("db")

  opts.watch
  |> should.be_false

  opts.format
  |> should.be_true

  opts.dry_run
  |> should.be_false

  opts.verbose
  |> should.be_false

  opts.typed
  |> should.be_true
}

// ============================================================================
// TABLE LIST PARSING TESTS
// ============================================================================

pub fn parse_table_list_none_returns_empty_list_test() {
  parse_table_list(None)
  |> should.equal([])
}

pub fn parse_table_list_single_table_test() {
  parse_table_list(Some("users"))
  |> should.equal(["users"])
}

pub fn parse_table_list_multiple_tables_test() {
  parse_table_list(Some("users,posts,comments"))
  |> should.equal(["users", "posts", "comments"])
}

pub fn parse_table_list_trims_whitespace_test() {
  parse_table_list(Some("users , posts , comments"))
  |> should.equal(["users", "posts", "comments"])
}

pub fn parse_table_list_filters_empty_strings_test() {
  parse_table_list(Some("users,,posts,"))
  |> should.equal(["users", "posts"])
}

// ============================================================================
// TABLE INCLUSION TESTS
// ============================================================================

pub fn should_include_table_empty_lists_includes_all_test() {
  should_include_table("users", [], [])
  |> should.be_true

  should_include_table("posts", [], [])
  |> should.be_true
}

pub fn should_include_table_include_list_filters_test() {
  should_include_table("users", ["users", "posts"], [])
  |> should.be_true

  should_include_table("posts", ["users", "posts"], [])
  |> should.be_true

  should_include_table("comments", ["users", "posts"], [])
  |> should.be_false
}

pub fn should_include_table_exclude_list_filters_test() {
  should_include_table("users", [], ["schema_migrations"])
  |> should.be_true

  should_include_table("schema_migrations", [], ["schema_migrations"])
  |> should.be_false
}

pub fn should_include_table_both_lists_test() {
  // Included in include list, not in exclude list
  should_include_table("users", ["users", "posts"], ["schema_migrations"])
  |> should.be_true

  // Included in include list, also in exclude list - exclude wins
  should_include_table("posts", ["posts"], ["posts"])
  |> should.be_false

  // Not in include list
  should_include_table("comments", ["users", "posts"], ["schema_migrations"])
  |> should.be_false
}

// ============================================================================
// ERROR FORMATTING TESTS
// ============================================================================

pub fn format_error_missing_required_test() {
  format_error(MissingRequired("database-url"))
  |> should.equal(
    "Error: Missing required argument: --database-url\n\nUse --help for usage information.",
  )
}

pub fn format_error_unknown_command_test() {
  format_error(UnknownCommand("foo"))
  |> should.equal(
    "Error: Unknown command: foo\n\nAvailable commands: generate\nUse --help for usage information.",
  )
}

pub fn format_error_missing_value_test() {
  format_error(MissingValue("output"))
  |> should.equal("Error: Option --output requires a value")
}

pub fn format_error_unknown_option_test() {
  format_error(UnknownOption("--foo"))
  |> should.equal(
    "Error: Unknown option: --foo\n\nUse --help for usage information.",
  )
}
