-- updates operational limits groups modification (lines then transfos)
insert into line_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from line_modification_op_limits_groups1
WHERE EXISTS (SELECT 1
              FROM information_schema.tables
              WHERE table_schema = 'public'
              AND table_name = 'line_modification_op_limits_groups1');
insert into line_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from line_modification_op_limits_groups2
WHERE EXISTS (SELECT 1
              FROM information_schema.tables
              WHERE table_schema = 'public'
                AND table_name = 'line_modification_op_limits_groups2');

insert into two_windings_transformer_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from two_windings_transformer_modification_op_limits_groups1
WHERE EXISTS (SELECT 1
              FROM information_schema.tables
              WHERE table_schema = 'public'
                AND table_name = 'two_windings_transformer_modification_op_limits_groups1');
insert into two_windings_transformer_modification_operational_limits_groups (branch_id, operational_limits_groups_id, pos_operational_limits_groups)
select
    branch_id,
    operational_limits_groups_id,
    pos_operational_limits_groups
from two_windings_transformer_modification_op_limits_groups2
WHERE EXISTS (SELECT 1
              FROM information_schema.tables
              WHERE table_schema = 'public'
                AND table_name = 'two_windings_transformer_modification_op_limits_groups2');

-- copy/converts data of column 'side' (ONE | TWO) --> to the column applicability (SIDE1 |SIDE2)

UPDATE operational_limits_group_modification
SET applicability = 'SIDE1'
WHERE side = 'ONE';
UPDATE operational_limits_group_modification
SET applicability = 'SIDE2'
WHERE side = 'TWO';
