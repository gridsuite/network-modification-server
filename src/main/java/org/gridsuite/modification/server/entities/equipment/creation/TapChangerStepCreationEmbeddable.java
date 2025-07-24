/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.TapChangerType;
import org.gridsuite.modification.dto.TapChangerStepCreationInfos;

import jakarta.persistence.*;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class TapChangerStepCreationEmbeddable {

    @Column(name = "tap_changer_type")
    @Enumerated(EnumType.STRING)
    private TapChangerType tapChangerType;

    @Column(name = "index")
    private int index;

    @Column(name = "rho")
    private double rho;

    @Column(name = "r")
    private double r;

    @Column(name = "x")
    private double x;

    @Column(name = "g")
    private double g;

    @Column(name = "b")
    private double b;

    @Column(name = "alpha")
    private Double alpha;

    public static List<TapChangerStepCreationEmbeddable> toEmbeddableRatioTapChangerSteps(List<TapChangerStepCreationInfos> tapChangerSteps) {
        return tapChangerSteps == null ? null :
                tapChangerSteps.stream()
                        .map(tapChangerStep -> new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, tapChangerStep.getIndex(), tapChangerStep.getRho(), tapChangerStep.getR(), tapChangerStep.getX(), tapChangerStep.getG(), tapChangerStep.getB(), null))
                        .collect(Collectors.toList());
    }

    public static List<TapChangerStepCreationEmbeddable> toEmbeddablePhaseTapChangerSteps(List<TapChangerStepCreationInfos> tapChangerSteps) {
        return tapChangerSteps == null ? null :
                tapChangerSteps.stream()
                        .map(tapChangerStep -> new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, tapChangerStep.getIndex(), tapChangerStep.getRho(), tapChangerStep.getR(), tapChangerStep.getX(), tapChangerStep.getG(), tapChangerStep.getB(), tapChangerStep.getAlpha()))
                        .collect(Collectors.toList());
    }

    public TapChangerStepCreationInfos toModificationInfos() {
        return toTapChangerStepCreationInfosBuilder().build();
    }

    private TapChangerStepCreationInfos.TapChangerStepCreationInfosBuilder toTapChangerStepCreationInfosBuilder() {
        if (getTapChangerType().equals(TapChangerType.PHASE)) {
            return TapChangerStepCreationInfos.builder()
                    .index(getIndex())
                    .rho(getRho())
                    .r(getR())
                    .x(getX())
                    .g(getG())
                    .b(getB())
                    .alpha(getAlpha());
        } else if (getTapChangerType().equals(TapChangerType.RATIO)) {
            return TapChangerStepCreationInfos.builder()
                    .index(getIndex())
                    .rho(getRho())
                    .r(getR())
                    .x(getX())
                    .g(getG())
                    .b(getB());
        }
        return TapChangerStepCreationInfos.builder();
    }
}
