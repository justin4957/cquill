-- Deploy posts table
-- Requires: users

BEGIN;

CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  title VARCHAR(255) NOT NULL,
  slug VARCHAR(255) NOT NULL,
  body TEXT,
  is_published BOOLEAN NOT NULL DEFAULT false,
  published_at TIMESTAMP WITH TIME ZONE,
  inserted_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Slug must be unique
CREATE UNIQUE INDEX idx_posts_slug ON posts(slug);

-- For querying posts by user
CREATE INDEX idx_posts_user_id ON posts(user_id);

-- For querying published posts
CREATE INDEX idx_posts_published ON posts(is_published, published_at DESC)
  WHERE is_published = true;

-- Auto-update updated_at
CREATE TRIGGER update_posts_updated_at
  BEFORE UPDATE ON posts
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

COMMIT;
