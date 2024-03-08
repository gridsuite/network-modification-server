/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public interface ReactiveLimitsHolderInfos {
    Boolean getReactiveCapabilityCurve();

    Double getMinQ();

    Double getMaxQ();

    List<ReactiveCapabilityCurveCreationInfos> getReactiveCapabilityCurvePoints();
}
