UPDATE generator_modification_entity_reactive_capability_curve_points
SET
    p = coalesce(generator_modification_entity_reactive_capability_curve_points.p, oldp),
    maxq = coalesce(generator_modification_entity_reactive_capability_curve_points.maxq, old_maxq),
    minq = coalesce(generator_modification_entity_reactive_capability_curve_points.minq, old_minq);

UPDATE battery_modification_entity_reactive_capability_curve_points
SET
    p = coalesce(battery_modification_entity_reactive_capability_curve_points.p, oldp),
    maxq = coalesce(battery_modification_entity_reactive_capability_curve_points.maxq, old_maxq),
    minq = coalesce(battery_modification_entity_reactive_capability_curve_points.minq, old_minq);

UPDATE converter_station_modification_rcc_points
SET
    p = coalesce(converter_station_modification_rcc_points.p, oldp),
    maxq = coalesce(converter_station_modification_rcc_points.maxq, old_maxq),
    minq = coalesce(converter_station_modification_rcc_points.minq, old_minq);

