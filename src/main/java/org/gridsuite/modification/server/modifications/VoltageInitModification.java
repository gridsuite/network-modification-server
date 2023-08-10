/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.ReportBuilder;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.ThreeWindingsTransformer;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.VscConverterStation;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.VoltageInitModificationInfos;

import java.util.Map;

import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_INIT_MODIFICATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

public class VoltageInitModification extends AbstractModification {
    private VoltageInitModificationInfos voltageInitModificationInfos;

    private static final String GENERATOR_MSG = "Generator";
    private static final String TWO_WINDINGS_TRANSFORMER_MSG = "2 windings transformer ";
    private static final String THREE_WINDINGS_TRANSFORMER_MSG = "3 windings transformer ";
    private static final String STATIC_VAR_COMPENSATOR_MSG = "Static var compensator ";
    private static final String VSC_CONVERTER_STATION_MSG = "Vsc converter station ";

    public VoltageInitModification(VoltageInitModificationInfos voltageInitModificationInfos) {
        this.voltageInitModificationInfos = voltageInitModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (voltageInitModificationInfos == null) {
            throw new NetworkModificationException(VOLTAGE_INIT_MODIFICATION_ERROR, "No voltage init modification to apply !!");
        }
    }

    private void report(Reporter reporter, String key, String defaultMessage, Map<String, Object> values, TypedValue severity) {
        ReportBuilder builder = Report.builder()
            .withKey(key)
            .withDefaultMessage(defaultMessage)
            .withSeverity(severity);
        for (Map.Entry<String, Object> valueEntry : values.entrySet()) {
            builder.withValue(valueEntry.getKey(), valueEntry.getValue().toString());
        }
        reporter.report(builder.build());
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // apply generators modifications
        voltageInitModificationInfos.getGenerators().forEach(m -> {
            Generator generator = network.getGenerator(m.getGeneratorId());
            if (generator == null) {
                Reporter reporter = subReporter.createSubReporter(GENERATOR_MSG + m.getGeneratorId(), GENERATOR_MSG + m.getGeneratorId());
                report(reporter, "generatorNotFound", "Generator with id=${id} not found", Map.of("id", m.getGeneratorId()), TypedValue.WARN_SEVERITY);
                return;
            }
            if (m.getVoltageSetpoint() != null || m.getReactivePowerSetpoint() != null) {
                Reporter reporter = subReporter.createSubReporter(GENERATOR_MSG + m.getGeneratorId(), GENERATOR_MSG + m.getGeneratorId());
                report(reporter, "generatorModification", "Generator with id=${id} modified :", Map.of("id", m.getGeneratorId()), TypedValue.INFO_SEVERITY);
                if (m.getVoltageSetpoint() != null) {
                    double oldTargetV = generator.getTargetV();
                    generator.setTargetV(m.getVoltageSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV, m.getVoltageSetpoint(), "Voltage set point", 1));
                }
                if (m.getReactivePowerSetpoint() != null) {
                    double oldTargetQ = generator.getTargetQ();
                    generator.setTargetQ(m.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetQ, m.getReactivePowerSetpoint(), "Reactive power set point", 1));
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
                    Reporter reporter = subReporter.createSubReporter(THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    report(reporter, "3WindingsTransformerNotFound", "3 windings transformer with id=${id} not found", Map.of("id", t.getTransformerId()), TypedValue.WARN_SEVERITY);
                    return;
                }
                if (threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger() == null) {
                    Reporter reporter = subReporter.createSubReporter(THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    report(reporter, "3WindingsTransformerRatioTapChangerNotFound", "3 windings transformer with id=${id} : Ratio tap changer for leg ${leg} not found", Map.of("id", t.getTransformerId(), "leg", t.getLegSide().name()), TypedValue.WARN_SEVERITY);
                    return;
                }

                Reporter reporter = subReporter.createSubReporter(THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                report(reporter, "3WindingsTransformerModification", "3 windings transformer with id=${id} modified :", Map.of("id", t.getTransformerId()), TypedValue.INFO_SEVERITY);

                int oldTapPosition = threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().getTapPosition();
                threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTapPosition, t.getRatioTapChangerPosition(), "Leg " + t.getLegSide().name() + " ratio tap changer position", 1));
            } else {
                TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(t.getTransformerId());
                if (twoWindingsTransformer == null) {
                    Reporter reporter = subReporter.createSubReporter(TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    report(reporter, "2WindingsTransformerNotFound", "2 windings transformer with id=${id} not found", Map.of("id", t.getTransformerId()), TypedValue.WARN_SEVERITY);
                    return;
                }
                if (twoWindingsTransformer.getRatioTapChanger() == null) {
                    Reporter reporter = subReporter.createSubReporter(TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    report(reporter, "2WindingsTransformerRatioTapChangerNotFound", "2 windings transformer with id=${id} : Ratio tap changer not found", Map.of("id", t.getTransformerId()), TypedValue.WARN_SEVERITY);
                    return;
                }

                Reporter reporter = subReporter.createSubReporter(TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                report(reporter, "2WindingsTransformerModification", "2 windings transformer with id=${id} modified :", Map.of("id", t.getTransformerId()), TypedValue.INFO_SEVERITY);

                int oldTapPosition = twoWindingsTransformer.getRatioTapChanger().getTapPosition();
                twoWindingsTransformer.getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTapPosition, t.getRatioTapChangerPosition(), "Ratio tap changer position", 1));
            }
        });

        // apply static var compenstors modifications
        voltageInitModificationInfos.getStaticVarCompensators().forEach(s -> {
            StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(s.getStaticVarCompensatorId());
            if (staticVarCompensator == null) {
                Reporter reporter = subReporter.createSubReporter(STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId(), STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId());
                report(reporter, "staticVarCompensatorNotFound", "Static var compensator with id=${id} not found", Map.of("id", s.getStaticVarCompensatorId()), TypedValue.WARN_SEVERITY);
                return;
            }
            if (s.getVoltageSetpoint() != null || s.getReactivePowerSetpoint() != null) {
                Reporter reporter = subReporter.createSubReporter(STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId(), GENERATOR_MSG + s.getStaticVarCompensatorId());
                report(reporter, "staticVarCompensatorModification", "Static var compensator with id=${id} modified :", Map.of("id", s.getStaticVarCompensatorId()), TypedValue.INFO_SEVERITY);
                if (s.getVoltageSetpoint() != null) {
                    double oldTargetV = staticVarCompensator.getVoltageSetpoint();
                    staticVarCompensator.setVoltageSetpoint(s.getVoltageSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV, s.getVoltageSetpoint(), "Voltage set point", 1));
                }
                if (s.getReactivePowerSetpoint() != null) {
                    double oldTargetQ = staticVarCompensator.getReactivePowerSetpoint();
                    staticVarCompensator.setReactivePowerSetpoint(s.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetQ, s.getReactivePowerSetpoint(), "Reactive power set point", 1));
                }
            }
        });

        // apply vsc converter stations modifications
        voltageInitModificationInfos.getVscConverterStations().forEach(v -> {
            VscConverterStation vscConverterStation = network.getVscConverterStation(v.getVscConverterStationId());
            if (vscConverterStation == null) {
                Reporter reporter = subReporter.createSubReporter(VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId(), VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId());
                report(reporter, "vscConverterStationNotFound", "Vsc converter station with id=${id} not found", Map.of("id", v.getVscConverterStationId()), TypedValue.WARN_SEVERITY);
                return;
            }
            if (v.getVoltageSetpoint() != null || v.getReactivePowerSetpoint() != null) {
                Reporter reporter = subReporter.createSubReporter(VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId(), VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId());
                report(reporter, "vscConverterStationModification", "Vsc converter station with id=${id} modified :", Map.of("id", v.getVscConverterStationId()), TypedValue.INFO_SEVERITY);
                if (v.getVoltageSetpoint() != null) {
                    double oldTargetV = vscConverterStation.getVoltageSetpoint();
                    vscConverterStation.setVoltageSetpoint(v.getVoltageSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV, v.getVoltageSetpoint(), "Voltage set point", 1));
                }
                if (v.getReactivePowerSetpoint() != null) {
                    double oldTargetQ = vscConverterStation.getReactivePowerSetpoint();
                    vscConverterStation.setReactivePowerSetpoint(v.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetQ, v.getReactivePowerSetpoint(), "Reactive power set point", 1));
                }
            }
        });
    }
}
