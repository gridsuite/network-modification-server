/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.BranchCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import jakarta.persistence.*;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class BranchCreationEntity extends EquipmentCreationEntity {

    @Column(name = "r")
    private double r;

    @Column(name = "x")
    private double x;

    @Column(name = "voltageLevelId1")
    private String voltageLevelId1;

    @Column(name = "voltageLevelId2")
    private String voltageLevelId2;

    @Column(name = "busOrBusbarSectionId1")
    private String busOrBusbarSectionId1;

    @Column(name = "busOrBusbarSectionId2")
    private String busOrBusbarSectionId2;

    @Column(name = "connectionName1")
    private String connectionName1;

    @Column(name = "connectionDirection1")
    private ConnectablePosition.Direction connectionDirection1;

    @Column(name = "connectionName2")
    private String connectionName2;

    @Column(name = "connectionDirection2")
    private ConnectablePosition.Direction connectionDirection2;

    @Column(name = "connectionPosition1")
    private Integer connectionPosition1;

    @Column(name = "connectionPosition2")
    private Integer connectionPosition2;

    @Column(name = "connected1", columnDefinition = "boolean default true")
    private boolean connected1;

    @Column(name = "connected2", columnDefinition = "boolean default true")
    private boolean connected2;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "current_limits_id1",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "current_limits_id1_fk"
        ), nullable = true)
    private CurrentLimitsEntity currentLimits1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "current_limits_id2",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "current_limits_id2_fk"
        ), nullable = true)
    private CurrentLimitsEntity currentLimits2;

    protected BranchCreationEntity(BranchCreationInfos branchCreationInfos) {
        super(branchCreationInfos);
        assignAttributes(branchCreationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        BranchCreationInfos branchCreationInfos = (BranchCreationInfos) modificationInfos;
        assignAttributes(branchCreationInfos);
    }

    private void assignAttributes(BranchCreationInfos branchCreationInfos) {
        x = branchCreationInfos.getX();
        r = branchCreationInfos.getR();
        voltageLevelId1 = branchCreationInfos.getVoltageLevelId1();
        voltageLevelId2 = branchCreationInfos.getVoltageLevelId2();
        busOrBusbarSectionId1 = branchCreationInfos.getBusOrBusbarSectionId1();
        busOrBusbarSectionId2 = branchCreationInfos.getBusOrBusbarSectionId2();
        if (branchCreationInfos.getCurrentLimits1() == null) {
            currentLimits1 = null;
        } else {
            currentLimits1 = branchCreationInfos.getCurrentLimits1().toEntity();
        }
        if (branchCreationInfos.getCurrentLimits2() == null) {
            currentLimits2 = null;
        } else {
            currentLimits2 = branchCreationInfos.getCurrentLimits2().toEntity();
        }
        connectionDirection1 = branchCreationInfos.getConnectionDirection1();
        connectionName1 = branchCreationInfos.getConnectionName1();
        connectionDirection2 = branchCreationInfos.getConnectionDirection2();
        connectionName2 = branchCreationInfos.getConnectionName2();
        connectionPosition1 = branchCreationInfos.getConnectionPosition1();
        connectionPosition2 = branchCreationInfos.getConnectionPosition2();
        connected1 = branchCreationInfos.isConnected1();
        connected2 = branchCreationInfos.isConnected2();
    }
}
