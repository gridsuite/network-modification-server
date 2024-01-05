update tabular_modification set equipment_type =
                                    (case
                                         when modification_type = 'BATTERY_MODIFICATION' then 'BATTERY'
                                         when modification_type = 'GENERATOR_MODIFICATION' then 'GENERATOR'
                                         when modification_type = 'LINE_MODIFICATION' then 'LINE'
                                         when modification_type = 'LOAD_MODIFICATION' then 'LOAD'
                                         when modification_type = 'SUBSTATION_MODIFICATION' then 'SUBSTATION'
                                         when modification_type = 'TWO_WINDINGS_TRANSFORMER_MODIFICATION' then 'TWO_WINDINGS_TRANSFORMER'
                                         when modification_type = 'VOLTAGE_LEVEL_MODIFICATION' then 'VOLTAGE_LEVEL'
                                        end)
where id = id;