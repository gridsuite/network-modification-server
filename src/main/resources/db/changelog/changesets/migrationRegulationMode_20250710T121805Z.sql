UPDATE two_windings_transformer_creation SET phasetapchangerregulating = 'false' WHERE phasetapchangerregulationmode = 'FIXED_TAP';
UPDATE two_windings_transformer_creation SET phasetapchangerregulating = 'true' WHERE phasetapchangerregulationmode <> 'FIXED_TAP';
UPDATE two_windings_transformer_creation SET phasetapchangerregulationmode = NULL WHERE phasetapchangerregulationmode = 'FIXED_TAP';

UPDATE two_windings_transformer_modification
SET phasetapchangerregulating = 'false', phasetapchangerregulating_op = 'SET'
WHERE phasetapchangerregulationmode = 'FIXED_TAP' AND phasetapchangerregulationmode_op = 'SET';

UPDATE two_windings_transformer_modification
SET phasetapchangerregulating = 'true', phasetapchangerregulating_op = 'SET'
WHERE phasetapchangerregulationmode <> 'FIXED_TAP' AND phasetapchangerregulationmode_op = 'SET';

UPDATE two_windings_transformer_modification
SET phasetapchangerregulationmode = NULL, phasetapchangerregulationmode_op = NULL
WHERE phasetapchangerregulationmode = 'FIXED_TAP';

UPDATE static_var_compensator_creation SET regulating = 'false' WHERE regulation_mode = 'OFF';
UPDATE static_var_compensator_creation SET regulating = 'true' WHERE regulation_mode <> 'OFF';
UPDATE static_var_compensator_creation SET regulation_mode = NULL WHERE regulation_mode = 'OFF';
