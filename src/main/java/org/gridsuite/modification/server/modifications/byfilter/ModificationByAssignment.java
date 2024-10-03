/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.PropertyAssignmentInfos;

import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_BY_ASSIGNMENT_ERROR;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ModificationByAssignment extends AbstractModificationByAssignment {
    private final ModificationByAssignmentInfos modificationInfos;

    public ModificationByAssignment(ModificationByAssignmentInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getModificationTypeLabel() {
        return "assignment";
    }

    @Override
    public ModificationInfos getModificationInfos() {
        return modificationInfos;
    }

    @Override
    public IdentifiableType getEquipmentType() {
        return modificationInfos.getEquipmentType();
    }

    @Override
    public List<AbstractAssignmentInfos> getAssignmentInfosList() {
        return Collections.unmodifiableList(modificationInfos.getAssignmentInfosList());
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return MODIFICATION_BY_ASSIGNMENT_ERROR;
    }

    @Override
    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> equipmentsReport) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            return true;
        } else {
            return super.isEquipmentEditable(equipment, abstractAssignmentInfos, equipmentsReport);
        }
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        return true;
    }

    @Override
    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            return equipment.getProperty(((PropertyAssignmentInfos) assignmentInfos).getPropertyName());
        } else {
            return super.getOldValue(equipment, abstractAssignmentInfos);
        }
    }

    @Override
    protected String getNewValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getValue() == null) {
            throw new NetworkModificationException(MODIFICATION_BY_ASSIGNMENT_ERROR, "Missing a value in the assignment");
        }
        return assignmentInfos.getValue().toString();
    }

    @Override
    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            String newValue = getNewValue(equipment, abstractAssignmentInfos);
            equipment.setProperty(((PropertyAssignmentInfos) assignmentInfos).getPropertyName(), newValue);
            return newValue;
        } else {
            return super.applyValue(equipment, abstractAssignmentInfos);
        }
    }
}
