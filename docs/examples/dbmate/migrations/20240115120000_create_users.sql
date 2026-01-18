-- Create users table
-- This is the foundation table for user accounts

-- migrate:up
CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL,
  name VARCHAR(255),
  avatar_url VARCHAR(500),
  is_active BOOLEAN NOT NULL DEFAULT true,
  inserted_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Email must be unique
CREATE UNIQUE INDEX IF NOT EXISTS idx_users_email ON users(email);

-- For querying active users
CREATE INDEX IF NOT EXISTS idx_users_is_active ON users(is_active) WHERE is_active = true;

-- Trigger to auto-update updated_at
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_users_updated_at
  BEFORE UPDATE ON users
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

-- migrate:down
DROP TRIGGER IF EXISTS update_users_updated_at ON users;
DROP FUNCTION IF EXISTS update_updated_at_column();
DROP INDEX IF EXISTS idx_users_is_active;
DROP INDEX IF EXISTS idx_users_email;
DROP TABLE IF EXISTS users;
