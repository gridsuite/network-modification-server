-- updates limits creations in line_creation on both sides
insert into operational_limits_group (uuid, id, current_limits_id)
select
    gen_random_uuid(),
    'DEFAULT',
    current_limits_id1
from line_creation;
insert into line_creation_operational_limits_groups1 (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    line_creation.id,
    operational_limits_group.uuid,
    0
from line_creation, operational_limits_group
where line_creation.current_limits_id1 = operational_limits_group.current_limits_id;

insert into operational_limits_group (uuid, id, current_limits_id)
select
    gen_random_uuid(),
    'DEFAULT',
    current_limits_id2
from line_creation;
insert into line_creation_operational_limits_groups2 (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    line_creation.id,
    operational_limits_group.uuid,
    0
from line_creation, operational_limits_group
where line_creation.current_limits_id2 = operational_limits_group.current_limits_id;

-- updates limits creations in two_windings_transformer_creation on both sides
insert into operational_limits_group (uuid, id, current_limits_id)
select
    gen_random_uuid(),
    'DEFAULT',
    current_limits_id1
from two_windings_transformer_creation;
insert into two_windings_transformer_creation_operational_limits_groups1 (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    two_windings_transformer_creation.id,
    operational_limits_group.uuid,
    0
from two_windings_transformer_creation, operational_limits_group
where two_windings_transformer_creation.current_limits_id1 = operational_limits_group.current_limits_id;

insert into operational_limits_group (uuid, id, current_limits_id)
select
    gen_random_uuid(),
    'DEFAULT',
    current_limits_id2
from two_windings_transformer_creation;
insert into two_windings_transformer_creation_operational_limits_groups2 (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    two_windings_transformer_creation.id,
    operational_limits_group.uuid,
    0
from two_windings_transformer_creation, operational_limits_group
where two_windings_transformer_creation.current_limits_id2 = operational_limits_group.current_limits_id;

-- set the previously created operational limits group as selected in line_creation and two_windings_transformer_creation
UPDATE line_creation
SET
    selected_operational_limits_group_id1 = 'DEFAULT',
    selected_operational_limits_group_id2 = 'DEFAULT';

UPDATE two_windings_transformer_creation
SET
    selected_operational_limits_group_id1 = 'DEFAULT',
    selected_operational_limits_group_id2 = 'DEFAULT';