/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.BusbarSectionCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import java.util.ArrayList;
import java.util.List;

public final class ModificationCreation {

    private ModificationCreation() {
    }

    public static VoltageLevelCreationInfos getCreationVoltageLevel(String substationId, String voltageLevelId, String voltageLevelName) {
        List<BusbarSectionCreationInfos> busbarSectionInfos = new ArrayList<>();
        busbarSectionInfos.add(BusbarSectionCreationInfos.builder().id("bbs.nw").name("SJB NO").vertPos(1).horizPos(1).build());
        busbarSectionInfos.add(BusbarSectionCreationInfos.builder().id("bbs.ne").name("SJB NE").vertPos(1).horizPos(2).build());
        busbarSectionInfos.add(BusbarSectionCreationInfos.builder().id("bbs.sw").name("SJB SW").vertPos(2).horizPos(1).build());

        List<BusbarConnectionCreationInfos> busbarConnectionInfos = new ArrayList<>();
        busbarConnectionInfos.add(
            BusbarConnectionCreationInfos.builder().fromBBS("bbs.nw").toBBS("bbs.ne").switchKind(SwitchKind.BREAKER).build());
        busbarConnectionInfos.add(
            BusbarConnectionCreationInfos.builder().fromBBS("bbs.nw").toBBS("bbs.sw").switchKind(SwitchKind.DISCONNECTOR).build());

        return VoltageLevelCreationInfos.builder()
            .equipmentId(voltageLevelId)
            .equipmentName(voltageLevelName)
            .nominalVoltage(379.1)
            .substationId(substationId)
            .busbarSections(busbarSectionInfos)
            .busbarConnections(busbarConnectionInfos)
            .build();
    }
}
