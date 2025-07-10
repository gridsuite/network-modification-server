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
