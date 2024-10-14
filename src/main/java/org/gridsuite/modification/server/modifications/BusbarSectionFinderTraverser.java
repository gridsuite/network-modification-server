/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.math.graph.TraverseResult;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class BusbarSectionFinderTraverser implements Terminal.TopologyTraverser {

    private final boolean onlyConnectedBbs;

    private String firstTraversedBbsId;

    public BusbarSectionFinderTraverser(boolean onlyConnectedBbs) {
        this.onlyConnectedBbs = onlyConnectedBbs;
    }

    @Override
    public TraverseResult traverse(Terminal terminal, boolean connected) {
        if (terminal.getConnectable().getType() == IdentifiableType.BUSBAR_SECTION) {
            firstTraversedBbsId = terminal.getConnectable().getId();
            return TraverseResult.TERMINATE_TRAVERSER;
        }
        return TraverseResult.CONTINUE;
    }

    @Override
    public TraverseResult traverse(Switch aSwitch) {
        if (onlyConnectedBbs && aSwitch.isOpen()) {
            return TraverseResult.TERMINATE_PATH;
        }
        return TraverseResult.CONTINUE;
    }

    public String getFirstTraversedBbsId() {
        return firstTraversedBbsId;
    }
}

