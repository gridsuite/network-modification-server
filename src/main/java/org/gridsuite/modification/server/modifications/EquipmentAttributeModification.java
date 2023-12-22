/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.WRONG_EQUIPMENT_TYPE;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class EquipmentAttributeModification extends AbstractModification {

    private final EquipmentAttributeModificationInfos modificationInfos;

    public EquipmentAttributeModification(EquipmentAttributeModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Identifiable<?> identifiable = network.getIdentifiable(modificationInfos.getEquipmentId());
        if (identifiable == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, modificationInfos.getEquipmentId());
        }
        if (identifiable.getType() != modificationInfos.getEquipmentType()) {
            throw new NetworkModificationException(WRONG_EQUIPMENT_TYPE, String.format("Type of '%s' is not %s but %s", modificationInfos.getEquipmentId(), modificationInfos.getEquipmentType(), identifiable.getType()));
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Identifiable<?> identifiable = network.getIdentifiable(modificationInfos.getEquipmentId());
        if (identifiable instanceof Switch) {
            changeSwitchAttribute((Switch) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReporter);
        } else if (identifiable instanceof Injection) {
            if (identifiable instanceof Generator) {
                changeGeneratorAttribute((Generator) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReporter);
            }
        } else if (identifiable instanceof Branch) {
            if (identifiable instanceof Line) {
                changeLineAttribute((Line) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReporter);
            } else if (identifiable instanceof TwoWindingsTransformer) {
                changeTwoWindingsTransformerAttribute((TwoWindingsTransformer) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReporter);
            }
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            changeThreeWindingsTransformerAttribute((ThreeWindingsTransformer) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReporter);
        } else if (identifiable instanceof HvdcLine) {
            // no hvdc line modifications yet
        }
    }

    private void changeSwitchAttribute(Switch aSwitch, String attributeName, Object attributeValue, Reporter reporter) {
        if (attributeName.equals("open")) {
            if (Boolean.TRUE.equals(aSwitch.isOpen() != (Boolean) attributeValue)) {
                aSwitch.setOpen((Boolean) attributeValue);
                reporter.report(Report.builder()
                    .withKey("switchChanged")
                    .withDefaultMessage("${operation} switch '${id}' in voltage level ${voltageLevelId}")
                    .withValue("id", aSwitch.getId())
                    .withValue("operation", Boolean.TRUE.equals(attributeValue) ? "Opening" : "Closing")
                    .withValue("voltageLevelId", aSwitch.getVoltageLevel().getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        } else {
            throw NetworkModificationException.createEquipementAttributeNotEditable(aSwitch.getType(), attributeName);
        }
    }

    // TODO remove only for switch
    private void changeGeneratorAttribute(Generator generator, String attributeName, Object attributeValue, Reporter reporter) {
        if (attributeName.equals("targetP")) {
            generator.setTargetP((Double) attributeValue);
            reporter.report(Report.builder()
                .withKey("generatorChanged")
                .withDefaultMessage("Generator with id=${id} targetP changed")
                .withValue("id", generator.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        } else {
            throw NetworkModificationException.createEquipementAttributeNotEditable(generator.getType(), attributeName);
        }
    }

    // TODO remove only for switch
    private void changeLineAttribute(Line line, String attributeName, Object attributeValue, Reporter reporter) {
        if (attributeName.equals("branchStatus")) {
            line.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.valueOf((String) attributeValue)).add();
            reporter.report(Report.builder()
                .withKey("lineStatusChanged")
                .withDefaultMessage("Branch with id=${id} status changed")
                .withValue("id", line.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        } else {
            throw NetworkModificationException.createEquipementAttributeNotEditable(line.getType(), attributeName);
        }
    }

    // TODO remove only for switch
    private void changeTwoWindingsTransformerAttribute(TwoWindingsTransformer transformer, String attributeName, Object attributeValue, Reporter reporter) {
        String reportKey;
        String reportDefaultMessage;

        switch (attributeName) {
            case "ratioTapChanger.tapPosition":
                transformer.getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "ratioTapPositionChanged";
                reportDefaultMessage = "2WT with id=${id} ratio tap changer position changed";
                break;
            case "phaseTapChanger.tapPosition":
                reportKey = "phaseTapPositionChanged";
                reportDefaultMessage = "2WT with id=${id} phase tap changer position changed";
                break;
            default:
                throw NetworkModificationException.createEquipementAttributeNotEditable(transformer.getType(), attributeName);
        }

        reporter.report(Report.builder()
            .withKey(reportKey)
            .withDefaultMessage(reportDefaultMessage)
            .withValue("id", transformer.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    // TODO remove only for switch
    private void changeThreeWindingsTransformerAttribute(ThreeWindingsTransformer transformer, String attributeName, Object attributeValue, Reporter reporter) {
        String reportKey;
        String reportDefaultMessage;

        switch (attributeName) {
            case "ratioTapChanger1.tapPosition":
                transformer.getLeg1().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "ratioTapChanger1.tapPosition";
                reportDefaultMessage = "3WT with id=${id} ratio tap changer 1 position changed";
                break;
            case "ratioTapChanger2.tapPosition":
                transformer.getLeg2().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "ratioTapChanger2.tapPosition";
                reportDefaultMessage = "3WT with id=${id} ratio tap changer 2 position changed";
                break;
            case "ratioTapChanger3.tapPosition":
                transformer.getLeg3().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "ratioTapChanger3.tapPosition";
                reportDefaultMessage = "3WT with id=${id} ratio tap changer 3 position changed";
                break;
            case "phaseTapChanger1.tapPosition":
                transformer.getLeg1().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
                reportKey = "phaseTapChanger1.tapPosition";
                reportDefaultMessage = "3WT with id=${id} phase tap changer 1 position changed";
                break;
            case "phaseTapChanger2.tapPosition":
                transformer.getLeg2().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
                reportKey = "phaseTapChanger2.tapPosition";
                reportDefaultMessage = "3WT with id=${id} phase tap changer 2 position changed";
                break;
            case "phaseTapChanger3.tapPosition":
                transformer.getLeg3().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
                reportKey = "phaseTapChanger3.tapPosition";
                reportDefaultMessage = "3WT with id=${id} phase tap changer 3 position changed";
                break;
            default:
                throw NetworkModificationException.createEquipementAttributeNotEditable(transformer.getType(), attributeName);
        }

        reporter.report(Report.builder()
            .withKey(reportKey)
            .withDefaultMessage(reportDefaultMessage)
            .withValue("id", transformer.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }
}
