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

import java.util.Set;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.impacts.BaseImpact;
import org.springframework.context.ApplicationContext;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractModification extends AbstractNetworkModification {
    @Override
    public void apply(Network network, boolean throwException, ComputationManager computationManager, Reporter reporter) {
        apply(network, reporter);
    }

    public Set<BaseImpact> apply(Network network, Reporter reporter, ApplicationContext context) {
        apply(network, reporter);
        return Set.of();
    }

    public void check(Network network) throws NetworkModificationException {
        // To perform input data check before hypothesis apply. Nothing to check here
    }
}
