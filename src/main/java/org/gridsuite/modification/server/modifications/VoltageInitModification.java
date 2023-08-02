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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.VoltageInitModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATOR_NOT_FOUND;
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
    }
}
