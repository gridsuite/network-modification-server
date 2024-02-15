package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.server.entities.equipment.modification.ReactiveCapabilityCurveModificationEmbeddable;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
public final class DTOUtils {
    private DTOUtils() {
    }

    public static List<ReactiveCapabilityCurveModificationInfos> convertToReactiveCapabilityCurveModificationInfos(List<ReactiveCapabilityCurveModificationEmbeddable> rCCpoints) {
        List<ReactiveCapabilityCurveModificationEmbeddable> pointsEmbeddable = !CollectionUtils.isEmpty(rCCpoints) ? rCCpoints : null;
        return pointsEmbeddable != null ? rCCpoints
                .stream()
                .map(value -> new ReactiveCapabilityCurveModificationInfos(value.getQminP(), value.getOldQminP(),
                        value.getQmaxP(), value.getOldQmaxP(),
                        value.getP(), value.getOldP()))
                .toList() : null;
    }
    public static List<ReactiveCapabilityCurveModificationInfos> convertToReactiveCapabilityCurveModificationInfos(List<ReactiveCapabilityCurveModificationEmbeddable> rCCpoints) {
        return CollectionUtils.isEmpty(rCCpoints) ? List.of() : rCCpoints
                .stream()
                .map(value -> new ReactiveCapabilityCurveModificationInfos(value.getQminP(), value.getOldQminP(),
                        value.getQmaxP(), value.getOldQmaxP(),
                        value.getP(), value.getOldP()))
                .toList();
    }
}
