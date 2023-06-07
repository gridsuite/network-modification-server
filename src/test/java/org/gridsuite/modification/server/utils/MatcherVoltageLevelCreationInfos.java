/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.server.dto.CouplingDeviceInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import java.util.List;
import java.util.Objects;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
public class MatcherVoltageLevelCreationInfos extends MatcherEquipmentModificationInfos<VoltageLevelCreationInfos> {
    public MatcherVoltageLevelCreationInfos(VoltageLevelCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(VoltageLevelCreationInfos m) {
        return super.matchesSafely(m)
            && Math.abs(getReference().getNominalVoltage() - m.getNominalVoltage()) < 0.2
            && Objects.equals(getReference().getSubstationId(), m.getSubstationId())
            && matchesCouplingDevices(m.getCouplingDevices())
            && matchesSwitchKinds(m.getSwitchKinds());
    }

    private boolean matchesSwitchKinds(List<SwitchKind> switchKinds) {
        if ((switchKinds == null) != (getReference().getSwitchKinds() == null)) {
            return false;
        }
        if (switchKinds == null) {
            return true;
        }
        if (switchKinds.size() != getReference().getSwitchKinds().size()) {
            return false;
        }

        for (int i = 0; i < switchKinds.size(); i++) {
            if (!Objects.equals(getReference().getSwitchKinds().get(i), switchKinds.get(i))) {
                return false;
            }
        }

        return true;
    }

    private boolean matchesCouplingDevices(List<CouplingDeviceInfos> couplingDevices) {
        if ((couplingDevices == null) != (getReference().getCouplingDevices() == null)) {
            return false;
        }
        if (couplingDevices == null) {
            return true;
        }
        if (couplingDevices.size() != getReference().getCouplingDevices().size()) {
            return false;
        }

        for (int i = 0; i < couplingDevices.size(); i++) {
            if (!matches(getReference().getCouplingDevices().get(i), couplingDevices.get(i))) {
                return false;
            }
        }

        return true;
    }

    private static boolean matches(CouplingDeviceInfos a, CouplingDeviceInfos b) {
        if ((a == null) != (b == null)) {
            return false;
        }
        if (a == null) {
            return true;
        }
        return Objects.equals(a.getBusbarSectionId1(), b.getBusbarSectionId1())
            && Objects.equals(a.getBusbarSectionId2(), b.getBusbarSectionId2());
    }
}
