-- tabular_modification + tabular_creation -> tabular_modifications
insert into tabular_modifications (id, modification_type, csv_filename)
select
    id,
    modification_type,
    csv_filename
from tabular_modification;
insert into tabular_modifications (id, modification_type, csv_filename)
select
    id,
    creation_type,
    csv_filename
from tabular_creation;

-- tabular_modification_modifications + tabular_creation_creations -> tabular_modifications_modifications
insert into tabular_modifications_modifications (tabular_modifications_entity_id, modifications_id, modifications_order)
select
    tabular_modification_entity_id,
    modifications_id,
    modifications_order
from tabular_modification_modifications;
insert into tabular_modifications_modifications (tabular_modifications_entity_id, modifications_id, modifications_order)
select
    tabular_creation_entity_id,
    creations_id,
    creations_order
from tabular_creation_creations;