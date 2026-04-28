-- set the new order system using the automatic one from composite_modification_sub_modifications before it is removed
UPDATE modification m
SET modifications_order = csm.modifications_order
from composite_modification_sub_modifications csm
WHERE m.id = csm.modification_id
  AND csm.modifications_order IS NOT NULL;