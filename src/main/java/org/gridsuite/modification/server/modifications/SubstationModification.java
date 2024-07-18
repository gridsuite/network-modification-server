/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.SubstationModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.SUBSTATION_NOT_FOUND;

/*
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class SubstationModification extends AbstractModification {

    private final SubstationModificationInfos modificationInfos;

    public SubstationModification(SubstationModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Substation station = network.getSubstation(modificationInfos.getEquipmentId());
        if (station == null) {
            throw new NetworkModificationException(SUBSTATION_NOT_FOUND,
                    "Substation " + modificationInfos.getEquipmentId() + " does not exist in network");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Substation station = network.getSubstation(modificationInfos.getEquipmentId());

        // modify the substation in the network
        subReportNode.newReportNode()
                .withMessageTemplate("substationModification", "Substation with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        // name and country
        ModificationUtils.getInstance().applyElementaryModifications(station::setName, () -> station.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(station::setCountry, station::getNullableCountry, modificationInfos.getCountry(), subReportNode, "Country");
        // properties
        PropertiesUtils.applyProperties(station, subReportNode, modificationInfos.getProperties(), "SubstationProperties");
    }
}
