package org.gridsuite.modification.server.dto;

import java.util.List;

public interface ReactiveLimitsHolderInfos {
    Boolean getReactiveCapabilityCurve();

    Double getMinimumReactivePower();

    Double getMaximumReactivePower();

    List<ReactiveCapabilityCurveCreationInfos> getReactiveCapabilityCurvePoints();
}
