alter table generator_modification_entity_reactive_capability_curve_points add column id UUID default gen_random_uuid();
alter table battery_modification_entity_reactive_capability_curve_points add column id UUID default gen_random_uuid();
alter table converter_station_modification_rcc_points add column id UUID default gen_random_uuid();

with position_value_generator as (
    select
        id,
        generator_modification_entity_id,
        (row_number() over (partition by generator_modification_entity_id) - 1) AS pos_point_new
    from generator_modification_entity_reactive_capability_curve_points
)
update generator_modification_entity_reactive_capability_curve_points
set pos_point = (
    select pos_point_new
    from position_value_generator
    where position_value_generator.id = generator_modification_entity_reactive_capability_curve_points.id
)
where id in (
    select id
    from position_value_generator
);

with position_value_battery as (
    select
        id,
        battery_modification_entity_id,
        (row_number() over (partition by battery_modification_entity_id) - 1) AS pos_point_new
    from battery_modification_entity_reactive_capability_curve_points
)
update battery_modification_entity_reactive_capability_curve_points
set pos_point = (
    select pos_point_new
    from position_value_battery
    where position_value_battery.id = battery_modification_entity_reactive_capability_curve_points.id
)
where id in (
    select id
    from position_value_battery
);

with position_value_converter_station as (
    select
        id,
        converter_station_modification_entity_id,
        (row_number() over (partition by converter_station_modification_entity_id) - 1) AS pos_point_new
    from converter_station_modification_rcc_points
)
update converter_station_modification_rcc_points
set pos_point = (
    select pos_point_new
    from position_value_converter_station
    where position_value_converter_station.id = converter_station_modification_rcc_points.id
)
where id in (
    select id
    from position_value_converter_station
);

alter table generator_modification_entity_reactive_capability_curve_points drop column id;
alter table battery_modification_entity_reactive_capability_curve_points drop column id;
alter table converter_station_modification_rcc_points drop column id;