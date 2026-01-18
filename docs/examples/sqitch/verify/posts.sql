-- Verify posts table exists with expected columns

SELECT
  id,
  user_id,
  title,
  slug,
  body,
  is_published,
  published_at,
  inserted_at,
  updated_at
FROM posts
WHERE FALSE;

-- Verify foreign key to users
SELECT 1/COUNT(*)
FROM information_schema.table_constraints
WHERE table_name = 'posts'
  AND constraint_type = 'FOREIGN KEY';

-- Verify the unique index on slug
SELECT 1/COUNT(*)
FROM pg_indexes
WHERE tablename = 'posts'
  AND indexname = 'idx_posts_slug';
