WITH position_value_generator AS (
    SELECT
        generator_modification_entity_id,
        old_maxq,
        old_minq,
        oldp,
        (row_number() OVER (PARTITION BY generator_modification_entity_id) - 1) AS pos_point_new
    FROM generator_modification_entity_reactive_capability_curve_points
)

UPDATE generator_modification_entity_reactive_capability_curve_points g
SET pos_point = c.pos_point_new
FROM position_value_generator c
WHERE g.generator_modification_entity_id = c.generator_modification_entity_id
  AND g.old_maxq = c.old_maxq AND g.old_minq = c.old_minq AND g.oldp = c.oldp;

WITH position_value_battery AS (
    SELECT
        battery_modification_entity_id,
        old_maxq,
        old_minq,
        oldp,
        (row_number() OVER (PARTITION BY battery_modification_entity_id) - 1) AS pos_point_new
    FROM battery_modification_entity_reactive_capability_curve_points
)
UPDATE battery_modification_entity_reactive_capability_curve_points g
SET pos_point = c.pos_point_new
FROM position_value_battery c
WHERE g.battery_modification_entity_id = c.battery_modification_entity_id
  AND g.old_maxq = c.old_maxq AND g.old_minq = c.old_minq AND g.oldp = c.oldp;

WITH position_value_converter_station AS (
    SELECT
        converter_station_modification_entity_id,
        old_maxq,
        old_minq,
        oldp,
        (row_number() OVER (PARTITION BY converter_station_modification_entity_id) - 1) AS pos_point_new
    FROM converter_station_modification_rcc_points
)
UPDATE converter_station_modification_rcc_points g
SET pos_point = c.pos_point_new
FROM position_value_converter_station c
WHERE g.converter_station_modification_entity_id = c.converter_station_modification_entity_id
  AND g.old_maxq = c.old_maxq AND g.old_minq = c.old_minq AND g.oldp = c.oldp;
