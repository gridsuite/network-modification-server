/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.TapChangerType;
import org.gridsuite.modification.model.TapChangerStepCreationModel;

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

    @Column(name = "tapchangertype")
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

    public static List<TapChangerStepCreationEmbeddable> toEmbeddableRatioTapChangerSteps(List<TapChangerStepCreationModel> tapChangerSteps) {
        return tapChangerSteps == null ? null :
                tapChangerSteps.stream()
                        .map(tapChangerStep -> new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, tapChangerStep.getIndex(), tapChangerStep.getRho(), tapChangerStep.getR(), tapChangerStep.getX(), tapChangerStep.getG(), tapChangerStep.getB(), null))
                        .collect(Collectors.toList());
    }

    public static List<TapChangerStepCreationEmbeddable> toEmbeddablePhaseTapChangerSteps(List<TapChangerStepCreationModel> tapChangerSteps) {
        return tapChangerSteps == null ? null :
                tapChangerSteps.stream()
                        .map(tapChangerStep -> new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, tapChangerStep.getIndex(), tapChangerStep.getRho(), tapChangerStep.getR(), tapChangerStep.getX(), tapChangerStep.getG(), tapChangerStep.getB(), tapChangerStep.getAlpha()))
                        .collect(Collectors.toList());
    }

    public TapChangerStepCreationModel toModificationInfos() {
        return toTapChangerStepCreationInfosBuilder().build();
    }

    private TapChangerStepCreationModel.TapChangerStepCreationModelBuilder toTapChangerStepCreationInfosBuilder() {
        if (getTapChangerType().equals(TapChangerType.PHASE)) {
            return TapChangerStepCreationModel.builder()
                    .index(getIndex())
                    .rho(getRho())
                    .r(getR())
                    .x(getX())
                    .g(getG())
                    .b(getB())
                    .alpha(getAlpha());
        } else if (getTapChangerType().equals(TapChangerType.RATIO)) {
            return TapChangerStepCreationModel.builder()
                    .index(getIndex())
                    .rho(getRho())
                    .r(getR())
                    .x(getX())
                    .g(getG())
                    .b(getB());
        }
        return TapChangerStepCreationModel.builder();
    }
}
