-- Verify users table exists with expected columns
-- This query will fail if the table or columns don't exist

SELECT
  id,
  email,
  name,
  avatar_url,
  is_active,
  inserted_at,
  updated_at
FROM users
WHERE FALSE;

-- Verify the unique index exists
SELECT 1/COUNT(*)
FROM pg_indexes
WHERE tablename = 'users'
  AND indexname = 'idx_users_email';

-- Verify the trigger exists
SELECT 1/COUNT(*)
FROM pg_trigger
WHERE tgname = 'update_users_updated_at';
