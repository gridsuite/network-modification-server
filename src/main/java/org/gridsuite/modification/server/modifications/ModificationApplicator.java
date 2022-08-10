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
import java.util.List;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class ModificationApplicator {

    // TODO create a network damage object for each modification from the listener
    protected ModificationInfos getNetworkDamage(ModificationInfos modificationInfos, NetworkStoreListener listener) {
        modificationInfos.setSubstationIds(listener.getSubstationsIds());
        modificationInfos.setDate(ZonedDateTime.now(ZoneOffset.UTC));
        return modificationInfos;
    }

    public List<ModificationInfos> apply(ModificationInfos modificationInfos, ReporterModel reporter, NetworkStoreListener listener) {
        Reporter subReporter = modificationInfos.createSubReporter(reporter);
        try {
            modificationInfos.toModification().apply(listener.getNetwork(), subReporter);
            return List.of(getNetworkDamage(modificationInfos, listener));
        } catch (PowsyblException e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(modificationInfos.getErrorType(), e);
            subReporter.report(Report.builder()
                .withKey(modificationInfos.getErrorType().name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
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
