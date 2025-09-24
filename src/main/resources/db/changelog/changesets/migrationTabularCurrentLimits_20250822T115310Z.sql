-- updates operational limits groups modification (lines then transfos)
insert into line_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from line_modification_op_limits_groups1;
insert into line_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from line_modification_op_limits_groups2;

insert into two_windings_transformer_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from two_windings_transformer_modification_op_limits_groups1;
insert into two_windings_transformer_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from two_windings_transformer_modification_op_limits_groups2;

-- copy/converts data of column 'side' (ONE | TWO) --> to the column applicability (SIDE1 |SIDE2)

UPDATE operational_limits_group_modification
SET applicability = 'SIDE1'
WHERE side = 'ONE';
UPDATE operational_limits_group_modification
SET applicability = 'SIDE2'
WHERE side = 'TWO';

-- set the right selected operational limits groups
-- lines, side 1
UPDATE line_modification
SET 
	selected_operational_limits_group_id1 = operational_limits_group_modification.selected_operational_limits_group_id,
	selected_operational_limits_group_id1op = 'SET'
from operational_limits_group_modification, line_modification_op_limits_groups1
where operational_limits_group_modification.uuid = line_modification_op_limits_groups1.operational_limits_groups_id
and line_modification.id = line_modification_op_limits_groups1.branch_id
and operational_limits_group_modification.selected_operational_limits_group_id IS NOT NULL;

-- lines, side 2
UPDATE line_modification
SET 
	selected_operational_limits_group_id2 = operational_limits_group_modification.selected_operational_limits_group_id,
	selected_operational_limits_group_id2op = 'SET'
from operational_limits_group_modification, line_modification_op_limits_groups2
where operational_limits_group_modification.uuid = line_modification_op_limits_groups2.operational_limits_groups_id
and line_modification.id = line_modification_op_limits_groups2.branch_id
and operational_limits_group_modification.selected_operational_limits_group_id IS NOT NULL;

-- transfos, side 1
UPDATE two_windings_transformer_modification
SET 
	selected_operational_limits_group_id1 = operational_limits_group_modification.selected_operational_limits_group_id,
	selected_operational_limits_group_id1op = 'SET'
from operational_limits_group_modification, two_windings_transformer_modification_op_limits_groups1
where operational_limits_group_modification.uuid = two_windings_transformer_modification_op_limits_groups1.operational_limits_groups_id
and two_windings_transformer_modification.id = two_windings_transformer_modification_op_limits_groups1.branch_id
and operational_limits_group_modification.selected_operational_limits_group_id IS NOT NULL;

-- transfos, side 2
UPDATE two_windings_transformer_modification
SET 
	selected_operational_limits_group_id2 = operational_limits_group_modification.selected_operational_limits_group_id,
	selected_operational_limits_group_id2op = 'SET'
from operational_limits_group_modification, two_windings_transformer_modification_op_limits_groups2
where operational_limits_group_modification.uuid = two_windings_transformer_modification_op_limits_groups2.operational_limits_groups_id
and two_windings_transformer_modification.id = two_windings_transformer_modification_op_limits_groups2.branch_id
and operational_limits_group_modification.selected_operational_limits_group_id IS NOT NULL;






    
