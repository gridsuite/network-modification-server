/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.NetworkStoreListener;
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
    private static List<ModificationInfos> getNetworkDamage(ModificationInfos modificationInfos, NetworkStoreListener listener) {
        modificationInfos.setSubstationIds(listener.getSubstationsIds());
        modificationInfos.setDate(ZonedDateTime.now(ZoneOffset.UTC));
        return Stream
            .of(List.of(modificationInfos), listener.getDeletions())
            .flatMap(Collection::stream)
            .collect(Collectors.toList());
    }

    public List<ModificationInfos> apply(ModificationInfos modificationInfos, ReporterModel reporter, NetworkStoreListener listener) {
        Reporter subReporter = modificationInfos.createSubReporter(reporter);
        try {
            modificationInfos.toModification().apply(listener.getNetwork(), subReporter);
            return getNetworkDamage(modificationInfos, listener);
        } catch (PowsyblException e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(modificationInfos.getErrorType(), e);

            if (!listener.isBuild()) {
                throw exc;
            }
        } catch (Exception e) {
            if (!listener.isBuild()) {
                throw new NetworkModificationException(modificationInfos.getErrorType(), e);
            } else {
                throw e;
            }
        }
        return List.of();
    }
}
