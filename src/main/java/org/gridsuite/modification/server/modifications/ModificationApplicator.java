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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class ModificationApplicator {
    @Autowired
    private ApplicationContext context;

    private static List<ModificationInfos> getNetworkDamage(ModificationInfos modificationInfos, NetworkStoreListener listener) {
        modificationInfos.setSubstationIds(listener.getSubstationsIds());
        modificationInfos.setDate(ZonedDateTime.now(ZoneOffset.UTC));
        return Stream
            .of(List.of(modificationInfos), listener.getDeletions())
            .flatMap(Collection::stream)
            .collect(Collectors.toList());
    }

    public List<ModificationInfos> apply(ModificationInfos modificationInfos, Reporter subReporter, NetworkStoreListener listener, ApplicationContext context) {
        modificationInfos.toModification().apply(listener.getNetwork(), subReporter, context);
        return getNetworkDamage(modificationInfos, listener);
    }
}
