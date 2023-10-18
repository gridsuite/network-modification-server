/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.TabularModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABULAR_MODIFICATION_ERROR;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class TabularModification extends AbstractModification {
    private final TabularModificationInfos modificationInfos;

    public TabularModification(TabularModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(TABULAR_MODIFICATION_ERROR, "No tabular modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        modificationInfos.getModifications().forEach(modification -> modification.toModification().apply(network));
        String defaultMessage;
        switch (modificationInfos.getModificationType()) {
            case "GENERATOR_MODIFICATION":
                defaultMessage = "generators have been modified.";
                break;
            case "LOAD_MODIFICATION":
                defaultMessage = "loads have been modified.";
                break;
            default:
                defaultMessage = "equipments of unknown type have been modified";
                break;
        }

        subReporter.report(Report.builder()
                .withKey("tabularModification")
                .withDefaultMessage("Tabular modification: ${modificationsCount} " + defaultMessage)
                .withValue("modificationsCount", modificationInfos.getModifications().size())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }
}
