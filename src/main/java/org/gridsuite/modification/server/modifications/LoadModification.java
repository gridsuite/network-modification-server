/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.LoadModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class LoadModification extends AbstractModification {

    private final LoadModificationInfos modificationInfos;

    public LoadModification(LoadModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Load load = network.getLoad(modificationInfos.getEquipmentId());
        if (load == null) {
            throw new NetworkModificationException(LOAD_NOT_FOUND,
                    "Load " + modificationInfos.getEquipmentId() + " does not exist in network");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Load load = network.getLoad(modificationInfos.getEquipmentId());
        // modify the load in the network
        modifyLoad(load, subReportNode);
    }

    @Override
    public String getName() {
        return "LoadModification";
    }

    private void modifyLoad(Load load, ReportNode subReportNode) {
        subReportNode.newReportNode()
            .withMessageTemplate("loadModification", "Load with id=${id} modified :")
            .withUntypedValue("id", modificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        ModificationUtils.getInstance().applyElementaryModifications(load::setName, () -> load.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(load::setLoadType, load::getLoadType, modificationInfos.getLoadType(), subReportNode, "Type");
        modifyP0(load, modificationInfos.getP0(), subReportNode);
        modifyQ0(load, modificationInfos.getQ0(), subReportNode);
        modifyLoadConnectivityAttributes(modificationInfos, load, subReportNode);
        // properties
        PropertiesUtils.applyProperties(load, subReportNode, modificationInfos.getProperties(), "LoadProperties");
    }

    public static void modifyQ0(Load load, AttributeModification<Double> q0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setQ0, load::getQ0, q0, subReportNode, "Constant reactive power");
    }

    public static void modifyP0(Load load, AttributeModification<Double> p0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setP0, load::getP0, p0, subReportNode, "Constant active power");
    }

    private ReportNode modifyLoadConnectivityAttributes(LoadModificationInfos modificationInfos,
                                                        Load load, ReportNode subReportNode) {
        ConnectablePosition<Load> connectablePosition = load.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Load> connectablePositionAdder = load.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, load, modificationInfos, subReportNode);
    }
}
