UPDATE modification m1 SET
                           message_type = (select CASE
                                                      WHEN beam.id IS NOT NULL OR feam.id IS NOT NULL OR ieam.id IS NOT NULL OR deam.id IS NOT NULL OR seam.id IS NOT NULL THEN 'EQUIPMENT_ATTRIBUTE_MODIFICATION'
                                                      WHEN batteryc.id IS NOT NULL THEN 'BATTERY_CREATION'
                                                      WHEN batterym.id IS NOT NULL THEN 'BATTERY_MODIFICATION'
                                                      WHEN bsm.id IS NOT NULL THEN 'BRANCH_STATUS_MODIFICATION'
                                                      WHEN csc.id IS NOT NULL THEN 'CONVERTER_STATION_CREATION'
                                                      WHEN dal.id IS NOT NULL THEN 'DELETE_ATTACHING_LINE'
                                                      WHEN dvlol.id IS NOT NULL THEN 'DELETE_VOLTAGE_LEVEL_ON_LINE'
                                                      WHEN equipmentd.id IS NOT NULL THEN 'EQUIPMENT_DELETION'
                                                      WHEN gd.id IS NOT NULL THEN 'GENERATION_DISPATCH'
                                                      WHEN generatorc.id IS NOT NULL THEN 'GENERATOR_CREATION'
                                                      WHEN generatorm.id IS NOT NULL THEN 'GENERATOR_MODIFICATION'
                                                      WHEN gs.id IS NOT NULL THEN 'GENERATOR_SCALING'
                                                      WHEN lastsl.id IS NOT NULL THEN 'LINES_ATTACH_TO_SPLIT_LINES'
                                                      WHEN latvl.id IS NOT NULL THEN 'LINE_ATTACH_TO_VOLTAGE_LEVEL'
                                                      WHEN linec.id IS NOT NULL THEN 'LINE_CREATION'
                                                      WHEN linem.id IS NOT NULL THEN 'LINE_MODIFICATION'
                                                      WHEN loadc.id IS NOT NULL THEN 'LOAD_CREATION'
                                                      WHEN loadm.id IS NOT NULL THEN 'LOAD_MODIFICATION'
                                                      WHEN ls.id IS NOT NULL THEN 'LOAD_SCALING'
                                                      WHEN lswvl.id IS NOT NULL THEN 'LINE_SPLIT_WITH_VOLTAGE_LEVEL'
                                                      WHEN shuntcc.id IS NOT NULL THEN 'SHUNT_COMPENSATOR_CREATION'
                                                      WHEN shuntcm.id IS NOT NULL THEN 'SHUNT_COMPENSATOR_MODIFICATION'
                                                      WHEN substationc.id IS NOT NULL THEN 'SUBSTATION_CREATION'
                                                      WHEN substationm.id IS NOT NULL THEN 'SUBSTATION_MODIFICATION'
                                                      WHEN twtc.id IS NOT NULL THEN 'TWO_WINDINGS_TRANSFORMER_CREATION'
                                                      WHEN twtm.id IS NOT NULL THEN 'TWO_WINDINGS_TRANSFORMER_MODIFICATION'
                                                      WHEN vim.id IS NOT NULL THEN 'VOLTAGE_INIT_MODIFICATION'
                                                      WHEN voltageLevelc.id IS NOT NULL THEN 'VOLTAGE_LEVEL_CREATION'
                                                      WHEN voltageLevelm.id IS NOT NULL THEN 'VOLTAGE_LEVEL_MODIFICATION'
                                                      WHEN vsc.id IS NOT NULL THEN 'VSC_CREATION'
                                                      ELSE 'NoModificationType'
                                                      END --as msg_type
                                           FROM modification m
                                                    LEFT JOIN battery_creation batteryc ON m.id = batteryc.id
                                                    LEFT JOIN battery_modification batterym ON m.id = batterym.id
                                                    LEFT JOIN boolean_equipment_attribute_modification beam ON m.id = beam.id
                                                    LEFT JOIN branch_status_modification bsm ON m.id = bsm.id
                                                    LEFT JOIN converter_station_creation csc ON m.id = csc.id
                                                    LEFT JOIN delete_attaching_line dal ON m.id = dal.id
                                                    LEFT JOIN double_equipment_attribute_modification deam ON m.id = deam.id
                                                    LEFT JOIN delete_voltage_level_on_line dvlol ON m.id = dvlol.id
                                                    LEFT JOIN equipment_deletion equipmentd ON m.id = equipmentd.id
                                                    LEFT JOIN float_equipment_attribute_modification feam ON m.id = feam.id
                                                    LEFT JOIN generation_dispatch gd ON m.id = gd.id
                                                    LEFT JOIN generator_creation generatorc ON m.id = generatorc.id
                                                    LEFT JOIN generator_modification generatorm ON m.id = generatorm.id
                                                    LEFT JOIN generator_scaling gs ON m.id = gs.id
                                                    LEFT JOIN integer_equipment_attribute_modification ieam ON m.id = ieam.id
                                                    LEFT JOIN lines_attach_to_split_lines lastsl ON m.id = lastsl.id
                                                    LEFT JOIN line_attach_to_voltage_level latvl ON m.id = latvl.id
                                                    LEFT JOIN line_creation linec ON m.id = linec.id
                                                    LEFT JOIN line_modification linem ON m.id = linem.id
                                                    LEFT JOIN load_creation loadc ON m.id = loadc.id
                                                    LEFT JOIN load_modification loadm ON m.id = loadm.id
                                                    LEFT JOIN load_scaling ls ON m.id = ls.id
                                                    LEFT JOIN line_split_with_voltage_level lswvl ON m.id = lswvl.id
                                                    LEFT JOIN string_equipment_attribute_modification seam ON m.id = seam.id
                                                    LEFT JOIN shunt_compensator_creation_entity shuntcc ON m.id = shuntcc.id
                                                    LEFT JOIN shunt_compensator_modification shuntcm ON m.id = shuntcm.id
                                                    LEFT JOIN substation_creation substationc ON m.id = substationc.id
                                                    LEFT JOIN substation_modification substationm ON m.id = substationm.id
                                                    LEFT JOIN two_windings_transformer_creation twtc ON m.id = twtc.id
                                                    LEFT JOIN two_windings_transformer_modification twtm ON m.id = twtm.id
                                                    LEFT JOIN voltage_init_modification vim ON m.id = vim.id
                                                    LEFT JOIN voltage_level_creation_entity voltageLevelc ON m.id = voltageLevelc.id
                                                    LEFT JOIN voltage_level_modification voltageLevelm ON m.id = voltageLevelm.id
                                                    LEFT JOIN vsc_creation vsc ON m.id = vsc.id
                                           where m1.id = m.id
                           ),
                           message_values = (select CASE
                                                        WHEN batteryc.id IS NOT NULL THEN '{"equipmentId":"' || batteryc.equipment_id || '"}'
                                                        WHEN batterym.id IS NOT NULL THEN '{"equipmentId":"' || batterym.equipment_id || '"}'
                                                        WHEN beam.id IS NOT NULL THEN '{"equipmentAttributeName":"' || beam.attribute_name || '","equipmentId":"' || beam.equipment_id || '","equipmentAttributeValue":"' || beam.attribute_value || '"}'
                                                        WHEN bsm.id IS NOT NULL THEN '{"energizedVoltageLevelId":"' || bsm.energized_voltage_level_id || '","action":"' || bsm.action || '","equipmentId":"' || bsm.equipment_id || '"}'
                                                        WHEN csc.id IS NOT NULL THEN '{"equipmentId":"' || csc.equipment_id || '"}'
                                                        WHEN dal.id IS NOT NULL THEN '{"attachedLineId":"' || dal.attached_line_id || '","lineToAttachTo1Id":"' || dal.line_to_attach_to1id || '","lineToAttachTo2Id":"' || dal.line_to_attach_to2id || '"}'
                                                        WHEN deam.id IS NOT NULL THEN '{"equipmentAttributeName":"' || deam.attribute_name || '","equipmentId":"' || deam.equipment_id || '","equipmentAttributeValue":"' || deam.attribute_value || '"}'
                                                        WHEN dvlol.id IS NOT NULL THEN '{"lineToAttachTo1Id":"' || dvlol.line_to_attach_to1id || '","lineToAttachTo2Id":"' || dvlol.line_to_attach_to2id || '"}'
                                                        WHEN equipmentd.id IS NOT NULL THEN '{"equipmentId":"' || equipmentd.equipment_id || '"}'
                                                        WHEN feam.id IS NOT NULL THEN '{"equipmentAttributeName":"' || feam.attribute_name || '","equipmentId":"' || feam.equipment_id || '","equipmentAttributeValue":"' || feam.attribute_value || '"}'
                                                        --WHEN gd.id IS NOT NULL THEN gd.equipment_id
                                                        WHEN generatorc.id IS NOT NULL THEN '{"equipmentId":"' || generatorc.equipment_id || '"}'
                                                        WHEN generatorm.id IS NOT NULL THEN '{"equipmentId":"' || generatorm.equipment_id || '"}'
                                                        --WHEN gs.id IS NOT NULL THEN gs.equipment_id GROOVY_SCRIPT
                                                        WHEN ieam.id IS NOT NULL THEN '{"equipmentAttributeName":"' || ieam.attribute_name || '","equipmentId":"' || ieam.equipment_id || '","equipmentAttributeValue":"' || ieam.attribute_value || '"}'
                                                        WHEN lastsl.id IS NOT NULL THEN '{"attachedLineId":"' || lastsl.attached_line_id || '"}'
                                                        WHEN latvl.id IS NOT NULL THEN '{"lineToAttachToId":"' || latvl.line_to_attach_to_id || '"}'
                                                        WHEN linec.id IS NOT NULL THEN '{"equipmentId":"' || linec.equipment_id || '"}'
                                                        WHEN linem.id IS NOT NULL THEN '{"equipmentId":"' || linem.equipment_id || '"}'
                                                        WHEN loadc.id IS NOT NULL THEN '{"equipmentId":"' || loadc.equipment_id || '"}'
                                                        WHEN loadm.id IS NOT NULL THEN '{"equipmentId":"' || loadm.equipment_id || '"}'
                                                        --WHEN ls.id IS NOT NULL THEN ls.equipment_id
                                                        WHEN lswvl.id IS NOT NULL THEN '{"lineToSplitId":"' || lswvl.line_to_split_id || '"}'
                                                        WHEN seam.id IS NOT NULL THEN '{"equipmentAttributeName":"' || seam.attribute_name || '","equipmentId":"' || seam.equipment_id || '","equipmentAttributeValue":"' || seam.attribute_value || '"}'
                                                        WHEN shuntcc.id IS NOT NULL THEN '{"equipmentId":"' || shuntcc.equipment_id || '"}'
                                                        WHEN shuntcm.id IS NOT NULL THEN '{"equipmentId":"' || shuntcm.equipment_id || '"}'
                                                        WHEN substationc.id IS NOT NULL THEN '{"equipmentId":"' || substationc.equipment_id || '"}'
                                                        WHEN substationm.id IS NOT NULL THEN '{"equipmentId":"' || substationm.equipment_id || '"}'
                                                        WHEN twtc.id IS NOT NULL THEN '{"equipmentId":"' || twtc.equipment_id || '"}'
                                                        WHEN twtm.id IS NOT NULL THEN '{"equipmentId":"' || twtm.equipment_id || '"}'
                                                        --WHEN vim.id IS NOT NULL THEN vim.equipment_id
                                                        WHEN voltageLevelc.id IS NOT NULL THEN '{"equipmentId":"' || voltageLevelc.equipment_id || '"}'
                                                        WHEN voltageLevelm.id IS NOT NULL THEN '{"equipmentId":"' || voltageLevelm.equipment_id || '"}'
                                                        WHEN vsc.id IS NOT NULL THEN '{"equipmentId":"' || vsc.equipment_id || '"}'
                                                        ELSE '{}'
                                                        END --as msg_values
                                             FROM modification m
                                                      LEFT JOIN battery_creation batteryc ON m.id = batteryc.id
                                                      LEFT JOIN battery_modification batterym ON m.id = batterym.id
                                                      LEFT JOIN boolean_equipment_attribute_modification beam ON m.id = beam.id
                                                      LEFT JOIN branch_status_modification bsm ON m.id = bsm.id
                                                      LEFT JOIN converter_station_creation csc ON m.id = csc.id
                                                      LEFT JOIN delete_attaching_line dal ON m.id = dal.id
                                                      LEFT JOIN double_equipment_attribute_modification deam ON m.id = deam.id
                                                      LEFT JOIN delete_voltage_level_on_line dvlol ON m.id = dvlol.id
                                                      LEFT JOIN equipment_deletion equipmentd ON m.id = equipmentd.id
                                                      LEFT JOIN float_equipment_attribute_modification feam ON m.id = feam.id
                                                 --LEFT JOIN generation_dispatch gd ON m.id = gd.id
                                                      LEFT JOIN generator_creation generatorc ON m.id = generatorc.id
                                                      LEFT JOIN generator_modification generatorm ON m.id = generatorm.id
                                                 --LEFT JOIN generator_scaling gs ON m.id = gs.id
                                                      LEFT JOIN integer_equipment_attribute_modification ieam ON m.id = ieam.id
                                                      LEFT JOIN lines_attach_to_split_lines lastsl ON m.id = lastsl.id
                                                      LEFT JOIN line_attach_to_voltage_level latvl ON m.id = latvl.id
                                                      LEFT JOIN line_creation linec ON m.id = linec.id
                                                      LEFT JOIN line_modification linem ON m.id = linem.id
                                                      LEFT JOIN load_creation loadc ON m.id = loadc.id
                                                      LEFT JOIN load_modification loadm ON m.id = loadm.id
                                                 --LEFT JOIN load_scaling ls ON m.id = ls.id
                                                      LEFT JOIN line_split_with_voltage_level lswvl ON m.id = lswvl.id
                                                      LEFT JOIN string_equipment_attribute_modification seam ON m.id = seam.id
                                                      LEFT JOIN shunt_compensator_creation_entity shuntcc ON m.id = shuntcc.id
                                                      LEFT JOIN shunt_compensator_modification shuntcm ON m.id = shuntcm.id
                                                      LEFT JOIN substation_creation substationc ON m.id = substationc.id
                                                      LEFT JOIN substation_modification substationm ON m.id = substationm.id
                                                      LEFT JOIN two_windings_transformer_creation twtc ON m.id = twtc.id
                                                      LEFT JOIN two_windings_transformer_modification twtm ON m.id = twtm.id
                                                 --LEFT JOIN voltage_init_modification vim ON m.id = vim.id
                                                      LEFT JOIN voltage_level_creation_entity voltageLevelc ON m.id = voltageLevelc.id
                                                      LEFT JOIN voltage_level_modification voltageLevelm ON m.id = voltageLevelm.id
                                                      LEFT JOIN vsc_creation vsc ON m.id = vsc.id
                                                      where m1.id = m.id
                           );