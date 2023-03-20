/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.NetworkModificationResult;

import java.util.Comparator;
import java.util.List;

public class ApplicationStatusComparator implements Comparator<NetworkModificationResult.ApplicationStatus> {

    private static List<NetworkModificationResult.ApplicationStatus> orderedStatus = List.of(
            NetworkModificationResult.ApplicationStatus.ALL_OK,
            NetworkModificationResult.ApplicationStatus.WITH_WARNINGS,
            NetworkModificationResult.ApplicationStatus.WITH_ERRORS
    );

    @Override
    public int compare(NetworkModificationResult.ApplicationStatus firstTypedValue, NetworkModificationResult.ApplicationStatus secondTypedValue) {
        return Integer.compare(orderedStatus.indexOf(firstTypedValue), orderedStatus.indexOf(secondTypedValue));
    }
}
