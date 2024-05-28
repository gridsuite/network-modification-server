/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CompositeModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.COMPOSITE_MODIFICATION_ERROR;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class CompositeModification extends AbstractModification {

    private final CompositeModificationInfos modificationInfos;

    public CompositeModification(CompositeModificationInfos compositeModificationInfos) {
        this.modificationInfos = compositeModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(COMPOSITE_MODIFICATION_ERROR, "No composite modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) { }
}