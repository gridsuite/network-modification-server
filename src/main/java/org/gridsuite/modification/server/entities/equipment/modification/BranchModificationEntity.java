/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class BranchModificationEntity extends BasicEquipmentModificationEntity {

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "r")),
        @AttributeOverride(name = "opType", column = @Column(name = "rOp"))
    })
    private DoubleModificationEmbedded r;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "x")),
        @AttributeOverride(name = "opType", column = @Column(name = "xOp"))
    })
    private DoubleModificationEmbedded x;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinTable(
            joinColumns = @JoinColumn(name = "branch_id"), foreignKey = @ForeignKey(name = "branch_modification_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "operational_limits_groups_id"), inverseForeignKey = @ForeignKey(name = "operational_limits_groups_id_fk"))
    @OrderColumn(name = "pos_operationalLimitsGroups")
    private List<OperationalLimitsGroupModificationEntity> operationalLimitsGroups;

    @Column(name = "all_olg_modif_type")
    @Enumerated(EnumType.STRING)
    private OperationalLimitsGroupsModificationType operationalLimitsGroupsModificationType;

    @Column(name = "enable_olg_modification", columnDefinition = "boolean default true")
    private Boolean enableOLGModification;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "selected_operational_limits_group_id1")),
        @AttributeOverride(name = "opType", column = @Column(name = "selected_operational_limits_group_id1op"))
    })
    private StringModificationEmbedded selectedOperationalLimitsGroupId1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "selected_operational_limits_group_id2")),
        @AttributeOverride(name = "opType", column = @Column(name = "selected_operational_limits_group_id2op"))
    })
    private StringModificationEmbedded selectedOperationalLimitsGroupId2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltage_level_id1")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltage_level_id1op"))
    })
    private StringModificationEmbedded voltageLevelId1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltage_level_id2")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltage_level_id2op"))
    })
    private StringModificationEmbedded voltageLevelId2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "bus_or_busbar_section_id1")),
        @AttributeOverride(name = "opType", column = @Column(name = "bus_or_busbar_section_id1op"))
    })
    private StringModificationEmbedded busOrBusbarSectionId1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "bus_or_busbar_section_id2")),
        @AttributeOverride(name = "opType", column = @Column(name = "bus_or_busbar_section_id2op"))
    })
    private StringModificationEmbedded busOrBusbarSectionId2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connection_name1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connection_name1op"))
    })
    private StringModificationEmbedded connectionName1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connection_name2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connection_name2op"))
    })
    private StringModificationEmbedded connectionName2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connection_position1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connection_position1op"))
    })
    private IntegerModificationEmbedded connectionPosition1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connection_position2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connection_position2op"))
    })
    private IntegerModificationEmbedded connectionPosition2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connection_direction1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connection_direction1op"))
    })
    private EnumModificationEmbedded<ConnectablePosition.Direction> connectionDirection1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connection_direction2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connection_direction2op"))
    })
    private EnumModificationEmbedded<ConnectablePosition.Direction> connectionDirection2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connected1op"))
    })
    private BooleanModificationEmbedded terminal1Connected;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connected2op"))
    })
    private BooleanModificationEmbedded terminal2Connected;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p1measurement_value")),
        @AttributeOverride(name = "opType", column = @Column(name = "p1measurement_value_op"))
    })
    private DoubleModificationEmbedded p1MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p1measurement_validity")),
        @AttributeOverride(name = "opType", column = @Column(name = "p1measurement_validity_op"))
    })
    private BooleanModificationEmbedded p1MeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q1measurement_value")),
        @AttributeOverride(name = "opType", column = @Column(name = "q1measurement_value_op"))
    })
    private DoubleModificationEmbedded q1MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q1measurement_validity")),
        @AttributeOverride(name = "opType", column = @Column(name = "q1measurement_validity_op"))
    })
    private BooleanModificationEmbedded q1MeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p2measurement_value")),
        @AttributeOverride(name = "opType", column = @Column(name = "p2measurement_value_op"))
    })
    private DoubleModificationEmbedded p2MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p2measurement_validity")),
        @AttributeOverride(name = "opType", column = @Column(name = "p2measurement_validity_op"))
    })
    private BooleanModificationEmbedded p2MeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q2measurement_value")),
        @AttributeOverride(name = "opType", column = @Column(name = "q2measurement_value_op"))
    })
    private DoubleModificationEmbedded q2MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q2measurement_validity")),
        @AttributeOverride(name = "opType", column = @Column(name = "q2measurement_validity_op"))
    })
    private BooleanModificationEmbedded q2MeasurementValidity;

    protected BranchModificationEntity(BranchModificationInfos branchModificationInfos) {
        super(branchModificationInfos);
        assignAttributes(branchModificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        BranchModificationInfos branchModificationInfos = (BranchModificationInfos) modificationInfos;
        assignAttributes(branchModificationInfos);
    }

    private void assignAttributes(BranchModificationInfos branchModificationInfos) {
        x = new DoubleModificationEmbedded(branchModificationInfos.getX());
        r = new DoubleModificationEmbedded(branchModificationInfos.getR());
        this.operationalLimitsGroups = assignOperationalLimitsGroups(branchModificationInfos.getOperationalLimitsGroups(), operationalLimitsGroups);
        this.enableOLGModification = branchModificationInfos.getEnableOLGModification() == null || branchModificationInfos.getEnableOLGModification();
        this.voltageLevelId1 = branchModificationInfos.getVoltageLevelId1() != null ? new StringModificationEmbedded(branchModificationInfos.getVoltageLevelId1()) : null;
        this.voltageLevelId2 = branchModificationInfos.getVoltageLevelId2() != null ? new StringModificationEmbedded(branchModificationInfos.getVoltageLevelId2()) : null;
        this.operationalLimitsGroupsModificationType = branchModificationInfos.getOperationalLimitsGroupsModificationType();
        this.busOrBusbarSectionId1 = branchModificationInfos.getBusOrBusbarSectionId1() != null ? new StringModificationEmbedded(branchModificationInfos.getBusOrBusbarSectionId1()) : null;
        this.busOrBusbarSectionId2 = branchModificationInfos.getBusOrBusbarSectionId2() != null ? new StringModificationEmbedded(branchModificationInfos.getBusOrBusbarSectionId2()) : null;
        this.connectionName1 = branchModificationInfos.getConnectionName1() != null ? new StringModificationEmbedded(branchModificationInfos.getConnectionName1()) : null;
        this.connectionName2 = branchModificationInfos.getConnectionName2() != null ? new StringModificationEmbedded(branchModificationInfos.getConnectionName2()) : null;
        this.selectedOperationalLimitsGroupId1 = branchModificationInfos.getSelectedOperationalLimitsGroupId1() != null ? new StringModificationEmbedded(
                branchModificationInfos.getSelectedOperationalLimitsGroupId1()) : null;
        this.selectedOperationalLimitsGroupId2 = branchModificationInfos.getSelectedOperationalLimitsGroupId2() != null ? new StringModificationEmbedded(
                branchModificationInfos.getSelectedOperationalLimitsGroupId2()) : null;
        this.connectionDirection1 = branchModificationInfos.getConnectionDirection1() != null ? new EnumModificationEmbedded<>(branchModificationInfos.getConnectionDirection1()) : null;
        this.connectionDirection2 = branchModificationInfos.getConnectionDirection2() != null ? new EnumModificationEmbedded<>(branchModificationInfos.getConnectionDirection2()) : null;
        this.connectionPosition1 = branchModificationInfos.getConnectionPosition1() != null ? new IntegerModificationEmbedded(branchModificationInfos.getConnectionPosition1()) : null;
        this.connectionPosition2 = branchModificationInfos.getConnectionPosition2() != null ? new IntegerModificationEmbedded(branchModificationInfos.getConnectionPosition2()) : null;
        this.terminal1Connected = branchModificationInfos.getTerminal1Connected() != null ? new BooleanModificationEmbedded(branchModificationInfos.getTerminal1Connected()) : null;
        this.terminal2Connected = branchModificationInfos.getTerminal2Connected() != null ? new BooleanModificationEmbedded(branchModificationInfos.getTerminal2Connected()) : null;
        this.p1MeasurementValue = branchModificationInfos.getP1MeasurementValue() != null ? new DoubleModificationEmbedded(branchModificationInfos.getP1MeasurementValue()) : null;
        this.p1MeasurementValidity = branchModificationInfos.getP1MeasurementValidity() != null ? new BooleanModificationEmbedded(branchModificationInfos.getP1MeasurementValidity()) : null;
        this.q1MeasurementValue = branchModificationInfos.getQ1MeasurementValue() != null ? new DoubleModificationEmbedded(branchModificationInfos.getQ1MeasurementValue()) : null;
        this.q1MeasurementValidity = branchModificationInfos.getQ1MeasurementValidity() != null ? new BooleanModificationEmbedded(branchModificationInfos.getQ1MeasurementValidity()) : null;
        this.p2MeasurementValue = branchModificationInfos.getP2MeasurementValue() != null ? new DoubleModificationEmbedded(branchModificationInfos.getP2MeasurementValue()) : null;
        this.p2MeasurementValidity = branchModificationInfos.getP2MeasurementValidity() != null ? new BooleanModificationEmbedded(branchModificationInfos.getP2MeasurementValidity()) : null;
        this.q2MeasurementValue = branchModificationInfos.getQ2MeasurementValue() != null ? new DoubleModificationEmbedded(branchModificationInfos.getQ2MeasurementValue()) : null;
        this.q2MeasurementValidity = branchModificationInfos.getQ2MeasurementValidity() != null ? new BooleanModificationEmbedded(branchModificationInfos.getQ2MeasurementValidity()) : null;
    }

    /**
     * the point of this function is to avoid dereferencing operationalLimitsGroups if it already exists,
     * in order to prevent Hibernate from losing the reference during cascade cleaning
     */
    private List<OperationalLimitsGroupModificationEntity> assignOperationalLimitsGroups(
            List<OperationalLimitsGroupModificationInfos> operationalLimitsGroupInfos,
            List<OperationalLimitsGroupModificationEntity> operationalLimitsGroups
    ) {
        List<OperationalLimitsGroupModificationEntity> updatedLimitsGroups = operationalLimitsGroups;
        if (operationalLimitsGroups == null) {
            updatedLimitsGroups = new ArrayList<>();
        } else {
            updatedLimitsGroups.clear();
        }
        if (operationalLimitsGroupInfos != null) {
            updatedLimitsGroups.addAll(OperationalLimitsGroupModificationEntity.toOperationalLimitsGroupsEntities(operationalLimitsGroupInfos));
        }
        return updatedLimitsGroups;
    }
}
