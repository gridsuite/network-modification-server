/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.CompositeModificationInfos;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class CompositeModification extends AbstractModification {

    public CompositeModification(CompositeModificationInfos compositeModificationInfos) {
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        throw new UnsupportedOperationException();
    }
}
