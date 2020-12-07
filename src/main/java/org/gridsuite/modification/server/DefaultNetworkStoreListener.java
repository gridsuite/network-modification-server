/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Injection;
import com.powsybl.iidm.network.NetworkListener;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.ThreeWindingsTransformer;

import java.util.HashSet;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class DefaultNetworkStoreListener implements NetworkListener {

    private Set<String> substationsIds = new HashSet<>();  // set of substations modified

    private void storeModification(Identifiable identifiable) {
        if (identifiable instanceof Switch) {
            substationsIds.add(((Switch) identifiable).getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Injection) {
            substationsIds.add(((Injection) identifiable).getTerminal().getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Branch) {
            substationsIds.add(((Branch) identifiable).getTerminal1().getVoltageLevel().getSubstation().getId());
            substationsIds.add(((Branch) identifiable).getTerminal2().getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getSubstation().getId());
        }
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        storeModification(identifiable);
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        storeModification(identifiable);
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        // empty default implementation
    }

    @Override
    public void onRemoval(Identifiable identifiable) {
        // empty default implementation
    }

    public Set<String> getModifications() {
        return substationsIds;
    }
}
