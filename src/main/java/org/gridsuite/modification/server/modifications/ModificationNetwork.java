/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;

import java.util.List;
import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public record ModificationNetwork(
    UUID networkUuid,
    Network network,
    NetworkStoreListener networkStoreListener
) {

    public List<AbstractBaseImpact> flush() {
        return networkStoreListener.flushModificationApplications();
    }

    public void initModificationApplication(UUID groupUuid, ModificationEntity modification) {
        networkStoreListener.initModificationApplication(groupUuid, modification);
    }
}

