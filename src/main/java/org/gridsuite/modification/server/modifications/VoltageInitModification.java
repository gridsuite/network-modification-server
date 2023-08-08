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
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ThreeWindingsTransformer;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.VoltageInitModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATOR_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.THREE_WINDINGS_TRANSFORMER_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.THREE_WINDINGS_TRANSFORMER_RATIO_TAP_CHANGER_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_RATIO_TAP_CHANGER_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_INIT_MODIFICATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

public class VoltageInitModification extends AbstractModification {
    private VoltageInitModificationInfos voltageInitModificationInfos;

    public VoltageInitModification(VoltageInitModificationInfos voltageInitModificationInfos) {
        this.voltageInitModificationInfos = voltageInitModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (voltageInitModificationInfos == null) {
            throw new NetworkModificationException(VOLTAGE_INIT_MODIFICATION_ERROR, "No voltage init modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // apply generators modifications
        voltageInitModificationInfos.getGenerators().forEach(m -> {
            Generator generator = network.getGenerator(m.getGeneratorId());
            if (generator == null) {
                throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + m.getGeneratorId() + " does not exist in network");
            }
            if (m.getVoltageSetpoint() != null || m.getReactivePowerSetpoint() != null) {
                Reporter genReporter = subReporter.createSubReporter("Generator " + m.getGeneratorId(), "Generator " + m.getGeneratorId());
                genReporter.report(Report.builder()
                    .withKey("generatorModification")
                    .withDefaultMessage("Generator with id=${id} modified :")
                    .withValue("id", m.getGeneratorId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

                if (m.getVoltageSetpoint() != null) {
                    genReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(generator.getTargetV(), m.getVoltageSetpoint(), "Voltage set point", 1));
                    generator.setTargetV(m.getVoltageSetpoint());
                }
                if (m.getReactivePowerSetpoint() != null) {
                    genReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(generator.getTargetQ(), m.getReactivePowerSetpoint(), "Reactive power set point", 1));
                    generator.setTargetQ(m.getReactivePowerSetpoint());
                }
            }
        });

        // apply transformers modifications
        voltageInitModificationInfos.getTransformers().forEach(t -> {
            if (t.getRatioTapChangerPosition() == null) {
                return;
            }

            if (t.getLegSide() != null) {
                ThreeWindingsTransformer threeWindingsTransformer = network.getThreeWindingsTransformer(t.getTransformerId());
                if (threeWindingsTransformer == null) {
                    throw new NetworkModificationException(THREE_WINDINGS_TRANSFORMER_NOT_FOUND, "3 windings transformer " + t.getTransformerId() + " does not exist in network");
                }
                if (threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger() == null) {
                    throw new NetworkModificationException(THREE_WINDINGS_TRANSFORMER_RATIO_TAP_CHANGER_NOT_FOUND, "3 windings transformer " + t.getTransformerId() + " : Ratio tap changer for leg " + t.getLegSide().name() + " does not exist in network");
                }

                Reporter transformerReporter = subReporter.createSubReporter("3 windings transformer " + t.getTransformerId(), "3 windings transformer " + t.getTransformerId());
                transformerReporter.report(Report.builder()
                    .withKey("3WindingsTransformerModification")
                    .withDefaultMessage("3 windings transformer with id=${id} modified :")
                    .withValue("id", t.getTransformerId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
                transformerReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().getTapPosition(), t.getRatioTapChangerPosition(), "Leg " + t.getLegSide().name() + " ratio tap changer position", 1));
                threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
            } else {
                TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(t.getTransformerId());
                if (twoWindingsTransformer == null) {
                    throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_NOT_FOUND, "2 windings transformer " + t.getTransformerId() + " does not exist in network");
                }
                if (twoWindingsTransformer.getRatioTapChanger() == null) {
                    throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_RATIO_TAP_CHANGER_NOT_FOUND, "2 windings transformer " + t.getTransformerId() + " : Ratio tap changer does not exist in network");
                }

                Reporter transformerReporter = subReporter.createSubReporter("2 windings transformer " + t.getTransformerId(), "2 windings transformer " + t.getTransformerId());
                transformerReporter.report(Report.builder()
                    .withKey("2WindingsTransformerModification")
                    .withDefaultMessage("2 windings transformer with id=${id} modified :")
                    .withValue("id", t.getTransformerId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
                transformerReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatioTapChanger().getTapPosition(), t.getRatioTapChangerPosition(), "Ratio tap changer position", 1));
                twoWindingsTransformer.getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
            }
        });
    }
}
