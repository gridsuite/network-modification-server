with position_value_generator as (
    select
        generator_modification_entity_id,
        old_maxq,
        old_minq,
        oldp,
        (row_number() over (partition by generator_modification_entity_id) - 1) AS pos_point_new
    FROM generator_modification_entity_reactive_capability_curve_points
)

update generator_modification_entity_reactive_capability_curve_points g
set pos_point = c.pos_point_new
from position_value_generator c
where g.generator_modification_entity_id = c.generator_modification_entity_id
  and g.old_maxq = c.old_maxq and g.old_minq = c.old_minq and g.oldp = c.oldp;

with position_value_battery as (
    select
        battery_modification_entity_id,
        old_maxq,
        old_minq,
        oldp,
        (row_number() OVER (partition by battery_modification_entity_id) - 1) AS pos_point_new
    from battery_modification_entity_reactive_capability_curve_points
)
update battery_modification_entity_reactive_capability_curve_points g
set pos_point = c.pos_point_new
from position_value_battery c
where g.battery_modification_entity_id = c.battery_modification_entity_id
  and g.old_maxq = c.old_maxq and g.old_minq = c.old_minq and g.oldp = c.oldp;

with position_value_converter_station as (
    select
        converter_station_modification_entity_id,
        old_maxq,
        old_minq,
        oldp,
        (row_number() over (partition by converter_station_modification_entity_id) - 1) as pos_point_new
    from converter_station_modification_rcc_points
)
update converter_station_modification_rcc_points g
set pos_point = c.pos_point_new
from position_value_converter_station c
where g.converter_station_modification_entity_id = c.converter_station_modification_entity_id
  and g.old_maxq = c.old_maxq and g.old_minq = c.old_minq and g.oldp = c.oldp;
