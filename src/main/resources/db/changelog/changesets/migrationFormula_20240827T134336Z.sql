--- Migrate partially data from formula table to new modification_by_filter table
INSERT INTO modification_by_filter (id, edited_field)
SELECT
    id,
    edited_field
FROM formula;

