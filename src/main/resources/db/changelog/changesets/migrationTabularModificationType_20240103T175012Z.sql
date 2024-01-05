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
update modification set message_values =
                                    (case
                                         when message_values = '{"tabularModificationType":"BATTERY_MODIFICATION"}' then '{"tabularEquipmentType":"BATTERY"}'
                                         when message_values = '{"tabularModificationType":"GENERATOR_MODIFICATION"}' then '{"tabularEquipmentType":"GENERATOR"}'
                                         when message_values = '{"tabularModificationType":"LINE_MODIFICATION"}' then '{"tabularEquipmentType":"LINE"}'
                                         when message_values = '{"tabularModificationType":"LOAD_MODIFICATION"}' then '{"tabularEquipmentType":"LOAD"}'
                                         when message_values = '{"tabularModificationType":"SUBSTATION_MODIFICATION"}' then '{"tabularEquipmentType":"SUBSTATION"}'
                                         when message_values = '{"tabularModificationType":"TWO_WINDINGS_TRANSFORMER_MODIFICATION"}' then '{"tabularEquipmentType":"TWO_WINDINGS_TRANSFORMER"}'
                                         when message_values = '{"tabularModificationType":"VOLTAGE_LEVEL_MODIFICATION"}' then '{"tabularEquipmentType":"VOLTAGE_LEVEL"}'
                                        end)
where type = 'TABULAR_MODIFICATION' and id = id;