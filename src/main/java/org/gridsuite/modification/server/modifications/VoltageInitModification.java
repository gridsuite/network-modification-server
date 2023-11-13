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
import com.powsybl.iidm.network.*;
import lombok.AllArgsConstructor;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.NetworkModificationException.Type;
import org.gridsuite.modification.server.dto.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@AllArgsConstructor
public class VoltageInitModification extends AbstractModification {
    private final VoltageInitModificationInfos voltageInitModificationInfos;

    private static final String GENERATOR_MSG = "Generator ";
    private static final String TWO_WINDINGS_TRANSFORMER_MSG = "2 windings transformer ";
    private static final String THREE_WINDINGS_TRANSFORMER_MSG = "3 windings transformer ";
    private static final String STATIC_VAR_COMPENSATOR_MSG = "Static var compensator ";
    private static final String VSC_CONVERTER_STATION_MSG = "Vsc converter station ";
    private static final String VOLTAGE_SET_POINT = "Voltage set point";
    private static final String REACTIVE_POWER_SET_POINT = "Reactive power set point";
    private static final String SHUNT_COMPENSATOR_MSG = "Shunt compensator ";
    private static final String SECTION_COUNT = "Section count";

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (voltageInitModificationInfos == null) {
            throw new NetworkModificationException(Type.VOLTAGE_INIT_MODIFICATION_ERROR, "No voltage init modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // apply generators modifications
        applyGeneratorModification(network, subReporter);

        // apply transformers modifications
        applyTransformerModification(network, subReporter);

        // apply static var compensators modifications
        applyStaticVarCompensatorModification(network, subReporter);

        // apply shunt compensators modifications
        applyShuntCompensatorModification(network, subReporter);

        // apply vsc converter stations modifications
        applyVscConverterStationModification(network, subReporter);
    }

    private static Report createReport(String key, String defaultMessage, Map<String, Object> values, TypedValue severity, int indentationLevel) {
        ReportBuilder builder = Report.builder()
            .withKey(key)
            .withDefaultMessage(" ".repeat(indentationLevel * 4) + defaultMessage)
            .withSeverity(severity);
        values.forEach((eKey, eValue) -> builder.withValue(eKey, String.valueOf(eValue)));
        return builder.build();
    }

    private void applyGeneratorModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (final VoltageInitGeneratorModificationInfos m : voltageInitModificationInfos.getGenerators()) {
            final Generator generator = network.getGenerator(m.getGeneratorId());
            if (generator == null) {
                Reporter reporter = subReporter.createSubReporter(GENERATOR_MSG + m.getGeneratorId(), GENERATOR_MSG + m.getGeneratorId());
                reporter.report(createReport("generatorNotFound", "Generator with id=${id} not found", Map.of("id", m.getGeneratorId()), TypedValue.WARN_SEVERITY, 0));
            } else if (m.getVoltageSetpoint() != null || m.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                Reporter reporter = subReporter.createSubReporter(GENERATOR_MSG + m.getGeneratorId(), GENERATOR_MSG + m.getGeneratorId());
                reporter.report(createReport("generatorModification", "Generator with id=${id} modified :", Map.of("id", m.getGeneratorId()), TypedValue.TRACE_SEVERITY, 0));
                if (m.getVoltageSetpoint() != null) {
                    final double oldTargetV = generator.getTargetV();
                    generator.setTargetV(m.getVoltageSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV, m.getVoltageSetpoint(), VOLTAGE_SET_POINT, 1));
                }
                if (m.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = generator.getTargetQ();
                    generator.setTargetQ(m.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetQ, m.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, 1));
                }
            }
        }
        if(modificationsCount > 0) {
            subReporter.report(Report.builder()
                .withKey("generatorModifications")
                .withDefaultMessage("${count} generator(s) have been modified.")
                .withValue("count", modificationsCount)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        }
    }

    private void applyTransformerModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (final VoltageInitTransformerModificationInfos t : voltageInitModificationInfos.getTransformers()) {
            if (t.getRatioTapChangerPosition() == null) {
                continue;
            }
            modificationsCount++;
            if (t.getLegSide() != null) {
                final ThreeWindingsTransformer threeWindingsTransformer = network.getThreeWindingsTransformer(t.getTransformerId());
                if (threeWindingsTransformer == null) {
                    Reporter reporter = subReporter.createSubReporter(THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    reporter.report(createReport("3WindingsTransformerNotFound", "3 windings transformer with id=${id} not found", Map.of("id", t.getTransformerId()), TypedValue.WARN_SEVERITY, 0));
                } else if (threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger() == null) {
                    Reporter reporter = subReporter.createSubReporter(THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    reporter.report(createReport("3WindingsTransformerRatioTapChangerNotFound", "3 windings transformer with id=${id} : Ratio tap changer for leg ${leg} not found", Map.of("id", t.getTransformerId(), "leg", t.getLegSide().name()), TypedValue.WARN_SEVERITY, 0));
                } else {
                    Reporter reporter = subReporter.createSubReporter(THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), THREE_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    reporter.report(createReport("3WindingsTransformerModification", "3 windings transformer with id=${id} modified :", Map.of("id", t.getTransformerId()), TypedValue.TRACE_SEVERITY, 0));
                    final int oldTapPosition = threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().getTapPosition();
                    threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTapPosition, t.getRatioTapChangerPosition(), "Leg " + t.getLegSide().name() + " ratio tap changer position", 1));
                }
            } else {
                final TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(t.getTransformerId());
                if (twoWindingsTransformer == null) {
                    Reporter reporter = subReporter.createSubReporter(TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    reporter.report(createReport("2WindingsTransformerNotFound", "2 windings transformer with id=${id} not found", Map.of("id", t.getTransformerId()), TypedValue.WARN_SEVERITY, 0));
                } else if (twoWindingsTransformer.getRatioTapChanger() == null) {
                    Reporter reporter = subReporter.createSubReporter(TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    reporter.report(createReport("2WindingsTransformerRatioTapChangerNotFound", "2 windings transformer with id=${id} : Ratio tap changer not found", Map.of("id", t.getTransformerId()), TypedValue.WARN_SEVERITY, 0));
                } else {
                    Reporter reporter = subReporter.createSubReporter(TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId(), TWO_WINDINGS_TRANSFORMER_MSG + t.getTransformerId());
                    reporter.report(createReport("2WindingsTransformerModification", "2 windings transformer with id=${id} modified :", Map.of("id", t.getTransformerId()), TypedValue.TRACE_SEVERITY, 0));
                    final int oldTapPosition = twoWindingsTransformer.getRatioTapChanger().getTapPosition();
                    twoWindingsTransformer.getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTapPosition, t.getRatioTapChangerPosition(), "Ratio tap changer position", 1));
                }
            }
        }
        if(modificationsCount > 0) {
            subReporter.report(Report.builder()
                    .withKey("transformerModifications")
                    .withDefaultMessage("${count} transformer(s) have been modified.")
                    .withValue("count", modificationsCount)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    private void applyStaticVarCompensatorModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (VoltageInitStaticVarCompensatorModificationInfos s : voltageInitModificationInfos.getStaticVarCompensators()) {
            final StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(s.getStaticVarCompensatorId());
            if (staticVarCompensator == null) {
                Reporter reporter = subReporter.createSubReporter(STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId(), STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId());
                reporter.report(createReport("staticVarCompensatorNotFound", "Static var compensator with id=${id} not found", Map.of("id", s.getStaticVarCompensatorId()), TypedValue.WARN_SEVERITY, 0));
            } else if (s.getVoltageSetpoint() != null || s.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                Reporter reporter = subReporter.createSubReporter(STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId(), STATIC_VAR_COMPENSATOR_MSG + s.getStaticVarCompensatorId());
                reporter.report(createReport("staticVarCompensatorModification", "Static var compensator with id=${id} modified :", Map.of("id", s.getStaticVarCompensatorId()), TypedValue.TRACE_SEVERITY, 0));
                if (s.getVoltageSetpoint() != null) {
                    final double oldTargetV = staticVarCompensator.getVoltageSetpoint();
                    staticVarCompensator.setVoltageSetpoint(s.getVoltageSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV, s.getVoltageSetpoint(), VOLTAGE_SET_POINT, 1));
                }
                if (s.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = staticVarCompensator.getReactivePowerSetpoint();
                    staticVarCompensator.setReactivePowerSetpoint(s.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetQ, s.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, 1));
                }
            }
        }
        if(modificationsCount > 0) {
            subReporter.report(Report.builder()
                    .withKey("svcModifications")
                    .withDefaultMessage("${count} static var compensator(s) have been modified.")
                    .withValue("count", modificationsCount)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    private void applyVscConverterStationModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (VoltageInitVscConverterStationModificationInfos v : voltageInitModificationInfos.getVscConverterStations()) {
            final VscConverterStation vscConverterStation = network.getVscConverterStation(v.getVscConverterStationId());
            if (vscConverterStation == null) {
                Reporter reporter = subReporter.createSubReporter(VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId(), VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId());
                reporter.report(createReport("vscConverterStationNotFound", "Vsc converter station with id=${id} not found", Map.of("id", v.getVscConverterStationId()), TypedValue.WARN_SEVERITY, 0));
            } else if (v.getVoltageSetpoint() != null || v.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                Reporter reporter = subReporter.createSubReporter(VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId(), VSC_CONVERTER_STATION_MSG + v.getVscConverterStationId());
                reporter.report(createReport("vscConverterStationModification", "Vsc converter station with id=${id} modified :", Map.of("id", v.getVscConverterStationId()), TypedValue.TRACE_SEVERITY, 0));
                if (v.getVoltageSetpoint() != null) {
                    final double oldTargetV = vscConverterStation.getVoltageSetpoint();
                    vscConverterStation.setVoltageSetpoint(v.getVoltageSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV, v.getVoltageSetpoint(), VOLTAGE_SET_POINT, 1));
                }
                if (v.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = vscConverterStation.getReactivePowerSetpoint();
                    vscConverterStation.setReactivePowerSetpoint(v.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetQ, v.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, 1));
                }
            }
        }
        if(modificationsCount > 0) {
            subReporter.report(Report.builder()
                    .withKey("vscModifications")
                    .withDefaultMessage("${count} vsc converter station(s) have been modified.")
                    .withValue("count", modificationsCount)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    private void applyShuntCompensatorModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (VoltageInitShuntCompensatorModificationInfos m : voltageInitModificationInfos.getShuntCompensators()) {
            final ShuntCompensator shuntCompensator = network.getShuntCompensator(m.getShuntCompensatorId());
            if (shuntCompensator == null) {
                Reporter reporter = subReporter.createSubReporter(SHUNT_COMPENSATOR_MSG + m.getShuntCompensatorId(), SHUNT_COMPENSATOR_MSG + m.getShuntCompensatorId());
                reporter.report(createReport("shuntCompensatorNotFound", "Shunt compensator with id=${id} not found", Map.of("id", m.getShuntCompensatorId()), TypedValue.WARN_SEVERITY, 0));
            } else if (m.getSectionCount() != null || m.getConnect() != null) {
                List<Report> reports = new ArrayList<>();
                final int currentSectionCount = shuntCompensator.getSectionCount();
                final Terminal shuntCompensatorTerminal = shuntCompensator.getTerminal();
                if (shuntCompensatorTerminal.isConnected()) {  // shunt compensator is connected
                    if (m.getSectionCount() == null) {
                        reports.add(createReport("shuntCompensatorSectionCountUndefined", "Section count value is undefined", Map.of(), TypedValue.WARN_SEVERITY, 1));
                    } else {
                        if (m.getSectionCount() == 0) {
                            shuntCompensatorTerminal.disconnect();
                            reports.add(createReport("shuntCompensatorDisconnected", "Shunt compensator disconnected", Map.of(), TypedValue.TRACE_SEVERITY, 1));
                        }
                        if (m.getSectionCount() != currentSectionCount) {
                            shuntCompensator.setSectionCount(m.getSectionCount());
                            reports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(currentSectionCount, m.getSectionCount(), SECTION_COUNT, 1));
                        }
                    }
                } else {  // shunt compensator is disconnected
                    if (m.getConnect() == null) {
                        reports.add(createReport("shuntCompensatorConnectUndefined", "Connect value is undefined", Map.of(), TypedValue.WARN_SEVERITY, 1));
                    } else {
                        if (Boolean.TRUE.equals(m.getConnect())) {
                            shuntCompensatorTerminal.connect();
                            reports.add(createReport("shuntCompensatorReconnected", "Shunt compensator reconnected", Map.of(), TypedValue.TRACE_SEVERITY, 1));
                        }
                        if (m.getSectionCount() != currentSectionCount) {
                            shuntCompensator.setSectionCount(m.getSectionCount());
                            reports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(currentSectionCount, m.getSectionCount(), SECTION_COUNT, 1));
                        }
                    }
                }
                if (!reports.isEmpty()) {
                    modificationsCount++;
                    Reporter reporter = subReporter.createSubReporter(SHUNT_COMPENSATOR_MSG + m.getShuntCompensatorId(), SHUNT_COMPENSATOR_MSG + m.getShuntCompensatorId());
                    reporter.report(createReport("shuntCompensatorModification", "Shunt compensator with id=${id} modified :", Map.of("id", m.getShuntCompensatorId()), TypedValue.TRACE_SEVERITY, 0));
                    reports.forEach(reporter::report);
                }
            }
        }
        if(modificationsCount > 0) {
            subReporter.report(Report.builder()
                    .withKey("scModifications")
                    .withDefaultMessage("${count} shunt compensator(s) have been modified.")
                    .withValue("count", modificationsCount)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }
}
