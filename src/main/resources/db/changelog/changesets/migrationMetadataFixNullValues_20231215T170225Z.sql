-- REMOVE USELESS DATA FROM TABLE BRANCH_STATUS_MODIFICATION
UPDATE branch_status_modification
 SET energized_voltage_level_id = NULL
 WHERE ( action = 'TRIP' OR action = 'LOCKOUT' OR action = 'SWITCH_ON' )
 AND energized_voltage_level_id IS NOT NULL;
-- RECREATE MESSAGE_VALUES IN TABLE MODIFICATION
UPDATE modification m1
 SET message_values = (
	 SELECT
	 	CASE
	 		WHEN bsm.energized_voltage_level_id IS NULL AND bsm.action IS NOT NULL AND bsm.equipment_id IS NOT NULL
	 		THEN  '{"action":"' || bsm.action || '","equipmentId":"' || bsm.equipment_id || '"}'
	 		WHEN bsm.energized_voltage_level_id IS NOT NULL AND bsm.action IS NOT NULL AND bsm.equipment_id IS NOT NULL
	 		THEN  '{"energizedVoltageLevelId":"' || bsm.energized_voltage_level_id || '","action":"' || bsm.action || '","equipmentId":"' || bsm.equipment_id || '"}'
	 		ELSE m1.message_values
	 	END
	 FROM modification m
	 LEFT JOIN branch_status_modification bsm ON m.id = bsm.id
	 WHERE m1.id = m.id)
 WHERE m1.type = 'BRANCH_STATUS_MODIFICATION';