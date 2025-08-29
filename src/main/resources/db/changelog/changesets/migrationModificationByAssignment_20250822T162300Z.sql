with tmp as (
    select assi.id
    from assignment as assi
             inner join modification_by_assignment as mba
                        on mba.id=assi.modification_by_assignment_id
    where mba.equipment_type = 'LOAD'
      and assi.edited_field in ('ACTIVE_POWER', 'REACTIVE_POWER')
)
update assignment
set edited_field = edited_field || '_SET_POINT'
where id in (select id from tmp);

with tmp as (
    select form.id
    from formula as form
    inner join by_formula_modification as bfm
    on bfm.id=form.by_formula_modification_id
    where bfm.identifiable_type = 14 --'LOAD'
    and form.edited_field in ('ACTIVE_POWER', 'REACTIVE_POWER')
    )
update formula
set edited_field = edited_field || '_SET_POINT'
where id in (select id from tmp);

with tmp as (
    select form.id
    from formula as form
    inner join by_formula_modification as bfm
    on bfm.id=form.by_formula_modification_id
    where bfm.identifiable_type = 14 --'LOAD'
    and form.equipment_field1 in ('ACTIVE_POWER', 'REACTIVE_POWER')
    )
update formula
set equipment_field1 = equipment_field1 || '_SET_POINT'
where id in (select id from tmp);

with tmp as (
    select form.id
    from formula as form
    inner join by_formula_modification as bfm
    on bfm.id=form.by_formula_modification_id
    where bfm.identifiable_type = 14 --'LOAD'
    and form.equipment_field2 in ('ACTIVE_POWER', 'REACTIVE_POWER')
    )
update formula
set equipment_field2 = equipment_field2 || '_SET_POINT'
where id in (select id from tmp);
