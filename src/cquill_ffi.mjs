// JavaScript FFI for cquill query builder
// Provides value coercion for the query DSL

export function coerce_value(value) {
  if (typeof value === 'number') {
    if (Number.isInteger(value)) {
      return { type: 'IntVal', 0: value };
    } else {
      return { type: 'FloatVal', 0: value };
    }
  }

  if (typeof value === 'string') {
    return { type: 'StringVal', 0: value };
  }

  if (typeof value === 'boolean') {
    return { type: 'BoolVal', 0: value };
  }

  if (value === null || value === undefined) {
    return { type: 'NilVal' };
  }

  if (Array.isArray(value)) {
    return { type: 'ListVal', 0: value.map(coerce_value) };
  }

  // Handle Gleam's None type
  if (value && value.type === 'None') {
    return { type: 'NilVal' };
  }

  return { type: 'UnknownVal' };
}

// Escape single quotes in SQL strings by doubling them
export function escape_sql_string(str) {
  return str.replace(/'/g, "''");
}

// Convert float to string
export function float_to_string(f) {
  return String(f);
}
