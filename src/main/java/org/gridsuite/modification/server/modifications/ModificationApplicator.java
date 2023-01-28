/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class ModificationApplicator {
    private enum ApplicationMode {
        UNITARY,
        MULTIPLE
    }

    public void apply(ModificationInfos modificationInfos, Reporter subReporter, NetworkStoreListener listener, ApplicationContext context) {
        modificationInfos.toModification().apply(listener.getNetwork(), subReporter, context);
        listener.addNetworkDamage(modificationInfos);
    }
}
