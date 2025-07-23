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
import org.gridsuite.modification.dto.BranchModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
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

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "current_limits_modification_id1",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "current_limits_modification_id1_fk"
        ), nullable = true)
    private CurrentLimitsModificationEntity currentLimits1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "current_limits_modification_id2",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "current_limits_modification_id2_fk"
        ), nullable = true)
    private CurrentLimitsModificationEntity currentLimits2;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinTable(
            joinColumns = @JoinColumn(name = "branch_id"), foreignKey = @ForeignKey(name = "branch_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "operational_limits_groups_id"), inverseForeignKey = @ForeignKey(name = "operational_limits_groups_id1_fk"))
    @OrderColumn(name = "pos_operationalLimitsGroups")
    private List<OperationalLimitsGroupModificationEntity> opLimitsGroups1;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinTable(
            joinColumns = @JoinColumn(name = "branch_id"), foreignKey = @ForeignKey(name = "branch_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "operational_limits_groups_id"), inverseForeignKey = @ForeignKey(name = "operational_limits_groups_id2_fk"))
    @OrderColumn(name = "pos_operationalLimitsGroups")
    private List<OperationalLimitsGroupModificationEntity> opLimitsGroups2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageLevelId1")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageLevelId1Op"))
    })
    private StringModificationEmbedded voltageLevelId1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageLevelId2")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageLevelId2Op"))
    })
    private StringModificationEmbedded voltageLevelId2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "busOrBusbarSectionId1")),
        @AttributeOverride(name = "opType", column = @Column(name = "busOrBusbarSectionId1Op"))
    })
    private StringModificationEmbedded busOrBusbarSectionId1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "busOrBusbarSectionId2")),
        @AttributeOverride(name = "opType", column = @Column(name = "busOrBusbarSectionId2Op"))
    })
    private StringModificationEmbedded busOrBusbarSectionId2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionName1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionName1Op"))
    })
    private StringModificationEmbedded connectionName1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionName2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionName2Op"))
    })
    private StringModificationEmbedded connectionName2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionPosition1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionPosition1Op"))
    })
    private IntegerModificationEmbedded connectionPosition1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionPosition2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionPosition2Op"))
    })
    private IntegerModificationEmbedded connectionPosition2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionDirection1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionDirection1Op"))
    })
    private EnumModificationEmbedded<ConnectablePosition.Direction> connectionDirection1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionDirection2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionDirection2Op"))
    })
    private EnumModificationEmbedded<ConnectablePosition.Direction> connectionDirection2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connected1Op"))
    })
    private BooleanModificationEmbedded terminal1Connected;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connected2Op"))
    })
    private BooleanModificationEmbedded terminal2Connected;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p1MeasurementValue")),
        @AttributeOverride(name = "opType", column = @Column(name = "p1MeasurementValueOp"))
    })
    private DoubleModificationEmbedded p1MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p1MeasurementValidity")),
        @AttributeOverride(name = "opType", column = @Column(name = "p1MeasurementValidityOp"))
    })
    private BooleanModificationEmbedded p1MeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q1MeasurementValue")),
        @AttributeOverride(name = "opType", column = @Column(name = "q1MeasurementValueOp"))
    })
    private DoubleModificationEmbedded q1MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q1MeasurementValidity")),
        @AttributeOverride(name = "opType", column = @Column(name = "q1MeasurementValidityOp"))
    })
    private BooleanModificationEmbedded q1MeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p2MeasurementValue")),
        @AttributeOverride(name = "opType", column = @Column(name = "p2MeasurementValueOp"))
    })
    private DoubleModificationEmbedded p2MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p2MeasurementValidity")),
        @AttributeOverride(name = "opType", column = @Column(name = "p2MeasurementValidityOp"))
    })
    private BooleanModificationEmbedded p2MeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q2MeasurementValue")),
        @AttributeOverride(name = "opType", column = @Column(name = "q2MeasurementValueOp"))
    })
    private DoubleModificationEmbedded q2MeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q2MeasurementValidity")),
        @AttributeOverride(name = "opType", column = @Column(name = "q2MeasurementValidityOp"))
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
        if (branchModificationInfos.getCurrentLimits1() != null) {
            currentLimits1 = new CurrentLimitsModificationEntity(branchModificationInfos.getCurrentLimits1());
        } else {
            currentLimits1 = null;
        }
        if (branchModificationInfos.getOperationalLimitsGroup1() != null) {
            opLimitsGroups1 = new ArrayList<>();
            for (var operationalLimitsGroup : branchModificationInfos.getOperationalLimitsGroup1()) {
                opLimitsGroups1.add(new OperationalLimitsGroupModificationEntity(operationalLimitsGroup));
            }
        } else {
            opLimitsGroups1 = null;
        }
        if (branchModificationInfos.getCurrentLimits2() != null) {
            currentLimits2 = new CurrentLimitsModificationEntity(branchModificationInfos.getCurrentLimits2());
        } else {
            currentLimits2 = null;
        }
        if (branchModificationInfos.getOperationalLimitsGroup2() != null) {
            opLimitsGroups2 = new ArrayList<>();
            for (var operationalLimitsGroup : branchModificationInfos.getOperationalLimitsGroup2()) {
                opLimitsGroups2.add(new OperationalLimitsGroupModificationEntity(operationalLimitsGroup));
            }
        } else {
            opLimitsGroups2 = null;
        }
        this.voltageLevelId1 = branchModificationInfos.getVoltageLevelId1() != null ? new StringModificationEmbedded(branchModificationInfos.getVoltageLevelId1()) : null;
        this.voltageLevelId2 = branchModificationInfos.getVoltageLevelId2() != null ? new StringModificationEmbedded(branchModificationInfos.getVoltageLevelId2()) : null;
        this.busOrBusbarSectionId1 = branchModificationInfos.getBusOrBusbarSectionId1() != null ? new StringModificationEmbedded(branchModificationInfos.getBusOrBusbarSectionId1()) : null;
        this.busOrBusbarSectionId2 = branchModificationInfos.getBusOrBusbarSectionId2() != null ? new StringModificationEmbedded(branchModificationInfos.getBusOrBusbarSectionId2()) : null;
        this.connectionName1 = branchModificationInfos.getConnectionName1() != null ? new StringModificationEmbedded(branchModificationInfos.getConnectionName1()) : null;
        this.connectionName2 = branchModificationInfos.getConnectionName2() != null ? new StringModificationEmbedded(branchModificationInfos.getConnectionName2()) : null;
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
}
