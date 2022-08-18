/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.network.Network;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public interface Modification extends com.powsybl.iidm.modification.NetworkModification {
    @Override
    default void apply(Network network, ComputationManager computationManager) {
        apply(network);
    }

    @Override
    default void apply(Network network) {
        apply(network, Reporter.NO_OP);
    }
}
