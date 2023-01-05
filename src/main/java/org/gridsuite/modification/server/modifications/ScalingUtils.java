/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public final class ScalingUtils {
    private ScalingUtils() { }

    public static void createReport(Reporter reporter, String reporterKey, String message, TypedValue errorSeverity) {
        reporter.report(Report.builder()
                .withKey(reporterKey)
                .withDefaultMessage(message)
                .withSeverity(errorSeverity)
                .build());
    }
}
