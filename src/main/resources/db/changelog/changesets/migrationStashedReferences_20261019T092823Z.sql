-- modification references are no longer stashed but deleted: delete those already stashed, along with those nested
-- in the stashed composites. Their directory-server references were removed when they were stashed.
CREATE TEMPORARY TABLE stashed_reference AS
WITH RECURSIVE stashed_tree(id) AS (
    SELECT m.id FROM modification m WHERE m.stashed = true
    UNION ALL
    SELECT m.id FROM modification m JOIN stashed_tree t ON m.container_id = t.id
)
SELECT r.id FROM modification_reference r JOIN stashed_tree t ON t.id = r.id;

DELETE FROM modification_application WHERE modification_uuid IN (SELECT id FROM stashed_reference);
DELETE FROM modification_root_network_applicability WHERE modification_id IN (SELECT id FROM stashed_reference);
DELETE FROM modification_reference WHERE id IN (SELECT id FROM stashed_reference);
DELETE FROM modification WHERE id IN (SELECT id FROM stashed_reference);

DROP TABLE stashed_reference;
