-- set the new order system using the automatic one from composite_modification_sub_modifications before it is removed
UPDATE modification m
SET name = cm.name
FROM composite_modification cm
WHERE m.id = cm.modification_id
  AND cm.name IS NOT NULL;