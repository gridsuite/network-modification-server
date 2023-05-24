/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.AbstractNetworkModification;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractModification extends AbstractNetworkModification {
    @Override
    public void apply(Network network, boolean throwException, ComputationManager computationManager, Reporter reporter) {
        apply(network, reporter);
    }

    public void apply(Network network, Reporter reporter, NetworkModificationApplicator applicator) {
        apply(network, reporter);
    }

    public void check(Network network) throws NetworkModificationException {
        // To perform input data check before hypothesis apply. Nothing to check here
    }
}
