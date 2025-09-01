-- updates limits modifications in line_modification on both sides
insert into operational_limits_group_modification (uuid, id, current_limits_id, applicability, modification_type)
select
    random_uuid(),
    'DEFAULT',
    current_limits_modification_id1,
    'SIDE1',
    'MODIFY_OR_ADD'
from line_modification;
insert into line_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    line_modification.id,
    operational_limits_group_modification.uuid,
    0
from line_modification, operational_limits_group_modification
where line_modification.current_limits_modification_id1 = operational_limits_group_modification.current_limits_id;

insert into operational_limits_group_modification (uuid, id, current_limits_id, applicability)
select
    random_uuid(),
    'DEFAULT',
    current_limits_modification_id2,
    'SIDE2'
from line_modification;
insert into line_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    line_modification.id,
    operational_limits_group_modification.uuid,
    1
from line_modification, operational_limits_group_modification
where line_modification.current_limits_modification_id2 = operational_limits_group_modification.current_limits_id;

-- updates limits modifications in two_windings_transformer_modification on both sides
insert into operational_limits_group_modification (uuid, id, current_limits_id, applicability)
select
    random_uuid(),
    'DEFAULT',
    current_limits_modification_id1,
    'SIDE1'
from two_windings_transformer_modification;
insert into two_windings_transformer_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    two_windings_transformer_modification.id,
    operational_limits_group_modification.uuid,
    0
from two_windings_transformer_modification, operational_limits_group_modification
where two_windings_transformer_modification.current_limits_modification_id1 = operational_limits_group_modification.current_limits_id;

insert into operational_limits_group_modification (uuid, id, current_limits_id, applicability)
select
    random_uuid(),
    'DEFAULT',
    current_limits_modification_id2,
    'SIDE2'
from two_windings_transformer_modification;
insert into two_windings_transformer_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    two_windings_transformer_modification.id,
    operational_limits_group_modification.uuid,
    1
from two_windings_transformer_modification, operational_limits_group_modification
where two_windings_transformer_modification.current_limits_modification_id2 = operational_limits_group_modification.current_limits_id;

-- set the previously created operational limits group as selected in line_modification and two_windings_transformer_modification
UPDATE line_modification
SET
    selected_operational_limits_group_id1 = 'DEFAULT',
    selected_operational_limits_group_id2 = 'DEFAULT',
    selected_operational_limits_group_id1op = 'SET',
    selected_operational_limits_group_id2op = 'SET';

UPDATE two_windings_transformer_modification
SET
    selected_operational_limits_group_id1 = 'DEFAULT',
    selected_operational_limits_group_id2 = 'DEFAULT',
    selected_operational_limits_group_id1op = 'SET',
    selected_operational_limits_group_id2op = 'SET';
