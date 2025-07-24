UPDATE two_windings_transformer_creation SET phase_tap_changer_regulating = 'false' WHERE phasetapchangerregulationmode = 'FIXED_TAP';
UPDATE two_windings_transformer_creation SET phase_tap_changer_regulating = 'true' WHERE phasetapchangerregulationmode <> 'FIXED_TAP';
UPDATE two_windings_transformer_creation SET phasetapchangerregulationmode = NULL WHERE phasetapchangerregulationmode = 'FIXED_TAP';

UPDATE two_windings_transformer_modification
SET phase_tap_changer_regulating = 'false', phase_tap_changer_regulating_op = 'SET'
WHERE phasetapchangerregulationmode = 'FIXED_TAP' AND phasetapchangerregulationmode_op = 'SET';

UPDATE two_windings_transformer_modification
SET phase_tap_changer_regulating = 'true', phase_tap_changer_regulating_op = 'SET'
WHERE phasetapchangerregulationmode <> 'FIXED_TAP' AND phasetapchangerregulationmode_op = 'SET';

UPDATE two_windings_transformer_modification
SET phasetapchangerregulationmode = NULL, phasetapchangerregulationmode_op = NULL
WHERE phasetapchangerregulationmode = 'FIXED_TAP';

UPDATE static_var_compensator_creation SET regulating = 'false' WHERE regulation_mode = 'OFF';
UPDATE static_var_compensator_creation SET regulating = 'true' WHERE regulation_mode <> 'OFF';
UPDATE static_var_compensator_creation SET regulation_mode = NULL WHERE regulation_mode = 'OFF';
