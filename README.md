# cquill

A compile-time safe database library for Gleam.

[![Package Version](https://img.shields.io/hexpm/v/cquill)](https://hex.pm/packages/cquill)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/cquill/)

## Design Philosophy

- **Compile-time safety over runtime convenience** — Invalid queries should fail at compile time, not runtime
- **Explicit over implicit** — No magic; transformations are visible and traceable
- **Gleam-idiomatic** — Leverage Result types, pipelines, and the module system naturally
- **Pragmatic interop** — Use battle-tested Erlang infrastructure (pooling, drivers) rather than reinventing
- **Incremental adoption** — Raw SQL escape hatch always available

## Installation

```sh
gleam add cquill
```

## Quick Start

```gleam
import cquill

pub fn main() {
  // Coming soon!
}
```

## Status

This library is currently in early development. See the [GitHub Issues](https://github.com/coolbeans/cquill/issues) for the development roadmap.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

Further documentation can be found at <https://hexdocs.pm/cquill>.

## License

Apache-2.0
