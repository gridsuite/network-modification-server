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

    private static final String GENERATOR_KEY = "GeneratorModifications";
    private static final String GENERATOR_NAME = "Generator ${id}";
    private static final String STATIC_VAR_COMPENSATOR_KEY = "StaticVarCompensatorModifications";
    private static final String STATIC_VAR_COMPENSATOR_NAME = "Static var compensator ${id}";
    private static final String VSC_CONVERTER_STATION_KEY = "VscConverterStationModifications";
    private static final String VSC_CONVERTER_STATION_NAME = "Vsc converter station ${id}";
    private static final String SHUNT_COMPENSATOR_KEY = "ShuntCompensatorModifications";
    private static final String SHUNT_COMPENSATOR_NAME = "Shunt compensator ${id}";

    private static final String VOLTAGE_SET_POINT = "Voltage set point";
    private static final String REACTIVE_POWER_SET_POINT = "Reactive power set point";
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

    private void applyGeneratorModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (final VoltageInitGeneratorModificationInfos m : voltageInitModificationInfos.getGenerators()) {
            final Generator generator = network.getGenerator(m.getGeneratorId());
            if (generator == null) {
                Reporter reporter = subReporter.createSubReporter(GENERATOR_KEY, GENERATOR_NAME, "id", m.getGeneratorId());
                reporter.report(Report.builder().withKey("generatorNotFound")
                                                .withDefaultMessage("Generator with id=${id} not found")
                                                .withValue("id", m.getGeneratorId())
                                                .withSeverity(TypedValue.WARN_SEVERITY).build());
            } else if (m.getVoltageSetpoint() != null || m.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                Reporter reporter = subReporter.createSubReporter(GENERATOR_KEY, GENERATOR_NAME, "id", m.getGeneratorId());
                reporter.report(Report.builder().withKey("generatorModification")
                                                .withDefaultMessage("Generator with id=${id} modified :")
                                                .withValue("id", m.getGeneratorId())
                                                .withSeverity(TypedValue.TRACE_SEVERITY).build());
                if (m.getVoltageSetpoint() != null) {
                    final double oldTargetV = generator.getTargetV();
                    generator.setTargetV(m.getVoltageSetpoint());
                    reporter.report(ModificationUtils.buildModificationReport(oldTargetV, m.getVoltageSetpoint(), VOLTAGE_SET_POINT, 1, TypedValue.TRACE_SEVERITY));
                }
                if (m.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = generator.getTargetQ();
                    generator.setTargetQ(m.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.buildModificationReport(oldTargetQ, m.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, 1, TypedValue.TRACE_SEVERITY));
                }
            }
        }
        if (modificationsCount > 0) {
            subReporter.report(new Report("generatorModificationsResume", "${count} generator(s) have been modified.", Map.of(
                    "count", new TypedValue(modificationsCount, TypedValue.UNTYPED),
                    Report.REPORT_SEVERITY_KEY, TypedValue.INFO_SEVERITY
            )));
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
                Reporter reporter = subReporter.createSubReporter("3WindingsTransformerModifications", "3 windings transformer ${id}", "id", t.getTransformerId());
                if (threeWindingsTransformer == null) {
                    reporter.report(Report.builder().withKey("3WindingsTransformerNotFound")
                                                    .withDefaultMessage("3 windings transformer with id=${id} not found")
                                                    .withValue("id", t.getTransformerId())
                                                    .withSeverity(TypedValue.WARN_SEVERITY).build());
                } else if (threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger() == null) {
                    reporter.report(Report.builder().withKey("3WindingsTransformerRatioTapChangerNotFound")
                                                    .withDefaultMessage("3 windings transformer with id=${id} : Ratio tap changer for leg ${leg} not found")
                                                    .withValue("id", t.getTransformerId())
                                                    .withValue("leg", t.getLegSide().name())
                                                    .withSeverity(TypedValue.WARN_SEVERITY).build());
                } else {
                    reporter.report(Report.builder().withKey("3WindingsTransformerModification")
                                                    .withDefaultMessage("3 windings transformer with id=${id} modified :")
                                                    .withValue("id", t.getTransformerId())
                                                    .withSeverity(TypedValue.TRACE_SEVERITY).build());
                    final int oldTapPosition = threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().getTapPosition();
                    threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                    reporter.report(ModificationUtils.buildModificationReport(oldTapPosition, t.getRatioTapChangerPosition(), "Leg " + t.getLegSide().name() + " ratio tap changer position", 1, TypedValue.TRACE_SEVERITY));
                }
            } else {
                final TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(t.getTransformerId());
                Reporter reporter = subReporter.createSubReporter("2WindingsTransformerModifications", "2 windings transformer ${id}", "id", t.getTransformerId());
                if (twoWindingsTransformer == null) {
                    reporter.report(Report.builder().withKey("2WindingsTransformerNotFound")
                                                    .withDefaultMessage("2 windings transformer with id=${id} not found")
                                                    .withValue("id", t.getTransformerId())
                                                    .withSeverity(TypedValue.WARN_SEVERITY).build());
                } else if (twoWindingsTransformer.getRatioTapChanger() == null) {
                    reporter.report(Report.builder().withKey("2WindingsTransformerRatioTapChangerNotFound")
                                                    .withDefaultMessage("2 windings transformer with id=${id} : Ratio tap changer not found")
                                                    .withValue("id", t.getTransformerId())
                                                    .withSeverity(TypedValue.WARN_SEVERITY).build());
                } else {
                    reporter.report(Report.builder().withKey("2WindingsTransformerModification")
                                                    .withDefaultMessage("2 windings transformer with id=${id} modified :")
                                                    .withValue("id", t.getTransformerId())
                                                    .withSeverity(TypedValue.TRACE_SEVERITY).build());
                    final int oldTapPosition = twoWindingsTransformer.getRatioTapChanger().getTapPosition();
                    twoWindingsTransformer.getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                    reporter.report(ModificationUtils.buildModificationReport(oldTapPosition, t.getRatioTapChangerPosition(), "Ratio tap changer position", 1, TypedValue.TRACE_SEVERITY));
                }
            }
        }
        if (modificationsCount > 0) {
            subReporter.report(new Report("windingsTransformerModificationsResume", "${count} transformer(s) have been modified.", Map.of(
                    "count", new TypedValue(modificationsCount, TypedValue.UNTYPED),
                    Report.REPORT_SEVERITY_KEY, TypedValue.INFO_SEVERITY
            )));
        }
    }

    private void applyStaticVarCompensatorModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (VoltageInitStaticVarCompensatorModificationInfos s : voltageInitModificationInfos.getStaticVarCompensators()) {
            final StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(s.getStaticVarCompensatorId());
            if (staticVarCompensator == null) {
                Reporter reporter = subReporter.createSubReporter(STATIC_VAR_COMPENSATOR_KEY, STATIC_VAR_COMPENSATOR_NAME, "id", s.getStaticVarCompensatorId());
                reporter.report(Report.builder().withKey("staticVarCompensatorNotFound")
                                                .withDefaultMessage("Static var compensator with id=${id} not found")
                                                .withValue("id", s.getStaticVarCompensatorId())
                                                .withSeverity(TypedValue.WARN_SEVERITY).build());
            } else if (s.getVoltageSetpoint() != null || s.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                Reporter reporter = subReporter.createSubReporter(STATIC_VAR_COMPENSATOR_KEY, STATIC_VAR_COMPENSATOR_NAME, "id", s.getStaticVarCompensatorId());
                reporter.report(Report.builder().withKey("staticVarCompensatorModification")
                                                .withDefaultMessage("Static var compensator with id=${id} modified :")
                                                .withValue("id", s.getStaticVarCompensatorId())
                                                .withSeverity(TypedValue.TRACE_SEVERITY).build());
                if (s.getVoltageSetpoint() != null) {
                    final double oldTargetV = staticVarCompensator.getVoltageSetpoint();
                    staticVarCompensator.setVoltageSetpoint(s.getVoltageSetpoint());
                    reporter.report(ModificationUtils.buildModificationReport(oldTargetV, s.getVoltageSetpoint(), VOLTAGE_SET_POINT, 1, TypedValue.TRACE_SEVERITY));
                }
                if (s.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = staticVarCompensator.getReactivePowerSetpoint();
                    staticVarCompensator.setReactivePowerSetpoint(s.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.buildModificationReport(oldTargetQ, s.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, 1, TypedValue.TRACE_SEVERITY));
                }
            }
        }
        if (modificationsCount > 0) {
            subReporter.report(new Report("svcModificationsResume", "${count} static var compensator(s) have been modified.", Map.of(
                    "count", new TypedValue(modificationsCount, TypedValue.UNTYPED),
                    Report.REPORT_SEVERITY_KEY, TypedValue.INFO_SEVERITY
            )));
        }
    }

    private void applyVscConverterStationModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (VoltageInitVscConverterStationModificationInfos v : voltageInitModificationInfos.getVscConverterStations()) {
            final VscConverterStation vscConverterStation = network.getVscConverterStation(v.getVscConverterStationId());
            if (vscConverterStation == null) {
                Reporter reporter = subReporter.createSubReporter(VSC_CONVERTER_STATION_KEY, VSC_CONVERTER_STATION_NAME, "id", v.getVscConverterStationId());
                reporter.report(Report.builder().withKey("vscConverterStationNotFound")
                                                .withDefaultMessage("Vsc converter station with id=${id} not found")
                                                .withValue("id", v.getVscConverterStationId())
                                                .withSeverity(TypedValue.WARN_SEVERITY).build());
            } else if (v.getVoltageSetpoint() != null || v.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                Reporter reporter = subReporter.createSubReporter(VSC_CONVERTER_STATION_KEY, VSC_CONVERTER_STATION_NAME, "id", v.getVscConverterStationId());
                reporter.report(Report.builder().withKey("vscConverterStationModification")
                                                .withDefaultMessage("Vsc converter station with id=${id} modified :")
                                                .withValue("id", v.getVscConverterStationId())
                                                .withSeverity(TypedValue.TRACE_SEVERITY).build());
                if (v.getVoltageSetpoint() != null) {
                    final double oldTargetV = vscConverterStation.getVoltageSetpoint();
                    vscConverterStation.setVoltageSetpoint(v.getVoltageSetpoint());
                    reporter.report(ModificationUtils.buildModificationReport(oldTargetV, v.getVoltageSetpoint(), VOLTAGE_SET_POINT, 1, TypedValue.TRACE_SEVERITY));
                }
                if (v.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = vscConverterStation.getReactivePowerSetpoint();
                    vscConverterStation.setReactivePowerSetpoint(v.getReactivePowerSetpoint());
                    reporter.report(ModificationUtils.buildModificationReport(oldTargetQ, v.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, 1, TypedValue.TRACE_SEVERITY));
                }
            }
        }
        if (modificationsCount > 0) {
            subReporter.report(new Report("vscModificationsResume", "${count} vsc converter station(s) have been modified.", Map.of(
                    "count", new TypedValue(modificationsCount, TypedValue.UNTYPED),
                    Report.REPORT_SEVERITY_KEY, TypedValue.INFO_SEVERITY
            )));
        }
    }

    private void applyShuntCompensatorModification(Network network, Reporter subReporter) {
        int modificationsCount = 0;
        for (VoltageInitShuntCompensatorModificationInfos m : voltageInitModificationInfos.getShuntCompensators()) {
            final ShuntCompensator shuntCompensator = network.getShuntCompensator(m.getShuntCompensatorId());
            if (shuntCompensator == null) {
                Reporter reporter = subReporter.createSubReporter(SHUNT_COMPENSATOR_KEY, SHUNT_COMPENSATOR_NAME, "id", m.getShuntCompensatorId());
                reporter.report(Report.builder().withKey("shuntCompensatorNotFound")
                                                .withDefaultMessage("Shunt compensator with id=${id} not found")
                                                .withValue("id", m.getShuntCompensatorId())
                                                .withSeverity(TypedValue.WARN_SEVERITY).build());
            } else if (m.getSectionCount() != null || m.getConnect() != null) {
                List<Report> reports = new ArrayList<>();
                final int currentSectionCount = shuntCompensator.getSectionCount();
                final Terminal shuntCompensatorTerminal = shuntCompensator.getTerminal();
                if (shuntCompensatorTerminal.isConnected()) {  // shunt compensator is connected
                    if (m.getSectionCount() == null) {
                        reports.add(Report.builder().withKey("shuntCompensatorSectionCountUndefined")
                                                    .withDefaultMessage("\tSection count value is undefined")
                                                    .withSeverity(TypedValue.WARN_SEVERITY).build());
                    } else {
                        if (m.getSectionCount() == 0) {
                            shuntCompensatorTerminal.disconnect();
                            reports.add(Report.builder().withKey("shuntCompensatorDisconnected")
                                                        .withDefaultMessage("\tShunt compensator disconnected")
                                                        .withSeverity(TypedValue.TRACE_SEVERITY).build());
                        }
                        if (m.getSectionCount() != currentSectionCount) {
                            shuntCompensator.setSectionCount(m.getSectionCount());
                            reports.add(ModificationUtils.buildModificationReport(currentSectionCount, m.getSectionCount(), SECTION_COUNT, 1, TypedValue.TRACE_SEVERITY));
                        }
                    }
                } else {  // shunt compensator is disconnected
                    if (m.getConnect() == null) {
                        reports.add(Report.builder().withKey("shuntCompensatorConnectUndefined")
                                                    .withDefaultMessage("\tConnect value is undefined")
                                                    .withSeverity(TypedValue.WARN_SEVERITY).build());
                    } else {
                        if (Boolean.TRUE.equals(m.getConnect())) {
                            shuntCompensatorTerminal.connect();
                            reports.add(Report.builder().withKey("shuntCompensatorReconnected")
                                                        .withDefaultMessage("\tShunt compensator reconnected")
                                                        .withSeverity(TypedValue.TRACE_SEVERITY).build());
                        }
                        if (m.getSectionCount() != currentSectionCount) {
                            shuntCompensator.setSectionCount(m.getSectionCount());
                            reports.add(ModificationUtils.buildModificationReport(currentSectionCount, m.getSectionCount(), SECTION_COUNT, 1, TypedValue.TRACE_SEVERITY));
                        }
                    }
                }
                if (!reports.isEmpty()) {
                    modificationsCount++;
                    Reporter reporter = subReporter.createSubReporter(SHUNT_COMPENSATOR_KEY, SHUNT_COMPENSATOR_NAME, "id", m.getShuntCompensatorId());
                    reporter.report(Report.builder().withKey("shuntCompensatorModification")
                                                    .withDefaultMessage("Shunt compensator with id=${id} modified :")
                                                    .withValue("id", m.getShuntCompensatorId())
                                                    .withSeverity(TypedValue.TRACE_SEVERITY).build());
                    reports.forEach(reporter::report);
                }
            }
        }
        if (modificationsCount > 0) {
            subReporter.report(new Report("shuntCompensatorModificationsResume", "${count} shunt compensator(s) have been modified.", Map.of(
                    "count", new TypedValue(modificationsCount, TypedValue.UNTYPED),
                    Report.REPORT_SEVERITY_KEY, TypedValue.INFO_SEVERITY
            )));
        }
    }
}
