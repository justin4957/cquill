// cquill - A compile-time safe database library for Gleam
//
// This is the main entry point for the cquill CLI tool.
// The CLI provides code generation from database schema.

import argv
import cquill/cli/args
import cquill/cli/generate
import gleam/int
import gleam/io

/// Main entry point for the cquill CLI
pub fn main() -> Nil {
  let arguments = argv.load().arguments

  case args.parse(arguments) {
    Ok(command) -> execute(command)
    Error(err) -> {
      io.println_error(args.format_error(err))
      exit(1)
    }
  }
}

/// Execute a parsed command
fn execute(command: args.Command) -> Nil {
  case command {
    args.Help -> {
      io.println(args.help_text())
    }

    args.Version -> {
      io.println(args.version_text())
    }

    args.Generate(options) -> {
      case options.watch {
        True -> generate.watch(options)
        False -> {
          case generate.run(options) {
            generate.GenerateSuccess(count) -> {
              case options.verbose {
                False ->
                  io.println("Generated " <> int.to_string(count) <> " files.")
                True -> Nil
              }
            }
            generate.GenerateError(err) -> {
              io.println_error(err)
              exit(1)
            }
          }
        }
      }
    }
  }
}

/// Exit the program with a status code
@external(erlang, "erlang", "halt")
fn exit(code: Int) -> Nil
