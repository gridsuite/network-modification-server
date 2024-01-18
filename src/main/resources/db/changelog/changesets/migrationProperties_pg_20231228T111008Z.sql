insert into free_property (id, added, deletion_mark, name, value_, equipment_modification_id, insert_position)
select
    id,
    added,
    deletion_mark,
    name,
    value_,
    substation_modification_id,
    insert_position
from substation_free_property;

insert into free_property (id, added, deletion_mark, name, value_, equipment_modification_id, insert_position)
select
    gen_random_uuid(),
    true,
    false,
    properties_key,
    properties,
    substation_creation_entity_id,
    (row_number() over (partition by substation_creation_entity_id order by properties_key) - 1)
from substation_creation_entity_properties;
