# Contributing to cquill

Thank you for your interest in contributing to cquill! This guide will help you get started.

## Getting Started

### Prerequisites

- [Gleam](https://gleam.run/) 1.14.0 or later
- [Erlang/OTP](https://www.erlang.org/) 26 or later
- [PostgreSQL](https://www.postgresql.org/) 14+ (for integration testing)
- Git

### Setting Up Your Development Environment

1. **Fork and clone the repository:**
   ```bash
   git clone https://github.com/YOUR-USERNAME/cquill.git
   cd cquill
   ```

2. **Install dependencies:**
   ```bash
   gleam deps download
   ```

3. **Run the test suite:**
   ```bash
   gleam test
   ```

4. **Verify formatting:**
   ```bash
   gleam format --check
   ```

## How to Contribute

### Reporting Bugs

Before creating a bug report, please check if the issue already exists. If not, create a new issue with:

1. **Clear, descriptive title**
2. **Steps to reproduce** - Minimal code example if possible
3. **Expected behavior** - What you expected to happen
4. **Actual behavior** - What actually happened
5. **Environment details** - Gleam version, OTP version, OS

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an enhancement suggestion:

1. **Use a clear, descriptive title**
2. **Describe the current behavior** and why it's insufficient
3. **Describe the desired behavior**
4. **Explain why this would be useful** to most users
5. **List any alternatives** you've considered

### Pull Requests

1. **Open an issue first** - Discuss the change before implementing
2. **Create a feature branch:**
   ```bash
   git checkout -b feature/my-feature
   ```
3. **Make your changes** following the code style guidelines
4. **Add tests** for any new functionality
5. **Update documentation** if needed
6. **Run the full test suite:**
   ```bash
   gleam test
   gleam format --check
   ```
7. **Commit your changes** using conventional commit format
8. **Push and create a PR**

## Code Style Guidelines

### General Principles

- **Clarity over cleverness** - Code should be easy to read and understand
- **Explicit over implicit** - Avoid magic; make transformations visible
- **Small, focused functions** - Each function should do one thing well
- **Descriptive names** - Use clear, descriptive variable and function names

### Gleam-Specific Guidelines

```gleam
// Use meaningful variable names
let user_email = get_email(user)  // Good
let x = get_email(user)            // Bad

// Prefer pipelines for transformations
users
|> list.filter(is_active)
|> list.map(get_email)

// Use Result types for operations that can fail
pub fn find_user(id: Int) -> Result(User, FindError)

// Document public functions
/// Finds a user by their unique identifier.
///
/// ## Examples
///
/// ```gleam
/// find_user(123)
/// // -> Ok(User(id: 123, name: "Alice"))
/// ```
pub fn find_user(id: Int) -> Result(User, FindError) {
  // ...
}
```

### Architecture Guidelines

All code must belong to exactly one layer:

1. **Schema Layer** - Pure data definitions, no I/O
2. **Changeset Layer** - Pure validation functions
3. **Query Layer** - Query AST building, no I/O
4. **Repo/Adapter Layer** - The only place I/O happens

### Test Guidelines

- **Unit tests** for all pure functions
- **Adapter contract tests** for persistence operations
- **System tests** for end-to-end workflows
- Use the in-memory adapter for fast, isolated tests

## Commit Message Format

We use [Conventional Commits](https://www.conventionalcommits.org/):

```
type(scope): short description

Longer description if needed explaining the what and why.

Closes #123
```

### Types

- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation only changes
- `style` - Code style changes (formatting, etc.)
- `refactor` - Code change that neither fixes a bug nor adds a feature
- `test` - Adding or modifying tests
- `chore` - Maintenance tasks

### Examples

```
feat(query): add support for LIKE operator

Implements the LIKE and ILIKE operators for string pattern matching
in the query builder.

Closes #45
```

```
fix(adapter/postgres): handle connection timeout correctly

The postgres adapter was not properly handling connection timeouts,
leading to unhandled exceptions. This adds proper error mapping.

Fixes #67
```

## Testing

### Running Tests

```bash
# Run all tests
gleam test

# Run tests with verbose output
gleam test -- --verbose
```

### Writing Tests

```gleam
import gleeunit/should

pub fn user_schema_has_correct_fields_test() {
  let schema = create_user_schema()

  schema.fields
  |> list.length
  |> should.equal(4)
}

pub fn insert_returns_error_on_duplicate_test() {
  let adapter = memory.new()
  let user = User(id: 1, email: "test@example.com")

  // First insert succeeds
  adapter
  |> memory.insert(user_schema, user)
  |> should.be_ok

  // Second insert fails with unique constraint violation
  adapter
  |> memory.insert(user_schema, user)
  |> should.be_error
}
```

## Documentation

### When to Update Docs

- Adding a new public function
- Changing existing behavior
- Adding a new feature
- Fixing a bug that affects documented behavior

### Documentation Locations

- `README.md` - Project overview, quick start
- `docs/` - Detailed guides and references
- Inline documentation - Function and module docs

## Review Process

1. **Automated checks** - CI must pass (tests, formatting, build)
2. **Code review** - At least one maintainer approval
3. **Documentation review** - Docs updated if needed
4. **Merge** - Squash and merge preferred

## Getting Help

- Open a [Discussion](https://github.com/justin4957/cquill/discussions) for questions
- Join the [Gleam Discord](https://discord.gg/Fm8Pwmy) for community chat
- Check existing issues and PRs for similar topics

## Recognition

Contributors are recognized in:
- GitHub's contributor list
- Release notes for significant contributions
- The project README (for major contributors)

Thank you for contributing to cquill!
