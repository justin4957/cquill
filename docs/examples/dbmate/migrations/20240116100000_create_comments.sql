-- Create comments table
-- Comments belong to posts and optionally to users (for registered users)

-- migrate:up
CREATE TABLE IF NOT EXISTS comments (
  id SERIAL PRIMARY KEY,
  post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
  user_id INTEGER REFERENCES users(id) ON DELETE SET NULL,
  author_name VARCHAR(255),
  author_email VARCHAR(255),
  body TEXT NOT NULL,
  is_approved BOOLEAN NOT NULL DEFAULT false,
  inserted_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),

  -- Either user_id OR (author_name AND author_email) must be set
  CONSTRAINT comments_author_check CHECK (
    user_id IS NOT NULL OR (author_name IS NOT NULL AND author_email IS NOT NULL)
  )
);

-- For querying comments by post
CREATE INDEX IF NOT EXISTS idx_comments_post_id ON comments(post_id);

-- For querying comments by user
CREATE INDEX IF NOT EXISTS idx_comments_user_id ON comments(user_id) WHERE user_id IS NOT NULL;

-- For moderation queue
CREATE INDEX IF NOT EXISTS idx_comments_approval ON comments(is_approved, inserted_at)
  WHERE is_approved = false;

-- Auto-update updated_at
CREATE TRIGGER update_comments_updated_at
  BEFORE UPDATE ON comments
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

-- migrate:down
DROP TRIGGER IF EXISTS update_comments_updated_at ON comments;
DROP INDEX IF EXISTS idx_comments_approval;
DROP INDEX IF EXISTS idx_comments_user_id;
DROP INDEX IF EXISTS idx_comments_post_id;
DROP TABLE IF EXISTS comments;
