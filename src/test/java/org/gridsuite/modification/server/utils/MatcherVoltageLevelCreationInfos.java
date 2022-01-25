/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import java.util.List;
import java.util.Objects;

import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.BusbarSectionCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
public class MatcherVoltageLevelCreationInfos extends MatcherEquipmentModificationInfos<VoltageLevelCreationInfos> {
    protected MatcherVoltageLevelCreationInfos(VoltageLevelCreationInfos ref) {
        super(ref);
    }

    public static MatcherVoltageLevelCreationInfos createMatcherVoltageLevelCreationInfos(VoltageLevelCreationInfos ref) {
        return new MatcherVoltageLevelCreationInfos(ref);
    }

    public boolean matchesSafely(VoltageLevelCreationInfos m) {
        return super.matchesSafely(m)
            && Math.abs(reference.getNominalVoltage() - m.getNominalVoltage()) < 0.2
            && Objects.equals(reference.getSubstationId(), m.getSubstationId())
            && matchesBusbarSections(m.getBusbarSections())
            && matchesBusbarConnections(m.getBusbarConnections());
    }

    private boolean matchesBusbarSections(List<BusbarSectionCreationInfos> bbsis) {
        if ((bbsis == null) != (reference.getBusbarSections() == null)) {
            return false;
        }
        if (bbsis == null) {
            return true;
        }
        if (bbsis.size() != reference.getBusbarSections().size()) {
            return false;
        }

        for (int i = 0; i < bbsis.size(); i++) {
            if (!matches(reference.getBusbarSections().get(i), bbsis.get(i))) {
                return false;
            }
        }

        return true;
    }

    private static boolean matches(BusbarSectionCreationInfos a, BusbarSectionCreationInfos b) {
        if ((a == null) != (b == null)) {
            return false;
        }
        if (a == null) {
            return true;
        }
        return Objects.equals(a.getId(), b.getId())
            && Objects.equals(a.getName(), b.getName())
            && a.getVertPos() == b.getVertPos()
            && a.getHorizPos() == b.getHorizPos();
    }

    private boolean matchesBusbarConnections(List<BusbarConnectionCreationInfos> cnxis) {
        if ((cnxis == null) != (reference.getBusbarConnections() == null)) {
            return false;
        }
        if (cnxis == null) {
            return true;
        }
        if (cnxis.size() != reference.getBusbarConnections().size()) {
            return false;
        }

        for (int i = 0; i < cnxis.size(); i++) {
            if (!matches(reference.getBusbarConnections().get(i), cnxis.get(i))) {
                return false;
            }
        }

        return true;
    }

    private static boolean matches(BusbarConnectionCreationInfos a, BusbarConnectionCreationInfos b) {
        if ((a == null) != (b == null)) {
            return false;
        }
        if (a == null) {
            return true;
        }
        return Objects.equals(a.getFromBBS(), b.getFromBBS())
            && Objects.equals(a.getToBBS(), b.getToBBS())
            && a.getSwitchKind() == b.getSwitchKind();
    }
}
