-- Revert posts table

BEGIN;

DROP TRIGGER IF EXISTS update_posts_updated_at ON posts;
DROP INDEX IF EXISTS idx_posts_published;
DROP INDEX IF EXISTS idx_posts_user_id;
DROP INDEX IF EXISTS idx_posts_slug;
DROP TABLE IF EXISTS posts;

COMMIT;
