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
import org.gridsuite.modification.dto.BranchCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;

import jakarta.persistence.*;
import org.gridsuite.modification.dto.OperationalLimitsGroupInfos;

import java.util.ArrayList;
import java.util.List;

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

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn(name = "pos_operationalLimitsGroups1")
    private List<OperationalLimitsGroupEntity> operationalLimitsGroups1;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn(name = "pos_operationalLimitsGroups2")
    private List<OperationalLimitsGroupEntity> operationalLimitsGroups2;

    @Column(name = "selectedOperationalLimitsGroupId1")
    private String selectedOperationalLimitsGroupId1;

    @Column(name = "selectedOperationalLimitsGroupId2")
    private String selectedOperationalLimitsGroupId2;

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
        operationalLimitsGroups1 = assignOperationalLimitsGroups(branchCreationInfos.getOperationalLimitsGroups1(), operationalLimitsGroups1);
        operationalLimitsGroups2 = assignOperationalLimitsGroups(branchCreationInfos.getOperationalLimitsGroups2(), operationalLimitsGroups2);
        connectionDirection1 = branchCreationInfos.getConnectionDirection1();
        connectionName1 = branchCreationInfos.getConnectionName1();
        connectionDirection2 = branchCreationInfos.getConnectionDirection2();
        connectionName2 = branchCreationInfos.getConnectionName2();
        selectedOperationalLimitsGroupId1 = branchCreationInfos.getSelectedOperationalLimitsGroup1();
        selectedOperationalLimitsGroupId2 = branchCreationInfos.getSelectedOperationalLimitsGroup2();
        connectionPosition1 = branchCreationInfos.getConnectionPosition1();
        connectionPosition2 = branchCreationInfos.getConnectionPosition2();
        connected1 = branchCreationInfos.isConnected1();
        connected2 = branchCreationInfos.isConnected2();
    }

    /**
     * the point of this function is to avoid dereferencing operationalLimitsGroups if it already exists,
     * in order to prevent Hibernate from losing the reference during cascade cleaning
     */
    private List<OperationalLimitsGroupEntity> assignOperationalLimitsGroups(
            List<OperationalLimitsGroupInfos> operationalLimitsGroupInfos,
            List<OperationalLimitsGroupEntity> operationalLimitsGroups
    ) {
        List<OperationalLimitsGroupEntity> updatedLimitsGroups = operationalLimitsGroups;
        if (operationalLimitsGroups == null) {
            updatedLimitsGroups = new ArrayList<>();
        } else {
            updatedLimitsGroups.clear();
        }
        if (operationalLimitsGroupInfos != null) {
            updatedLimitsGroups.addAll(OperationalLimitsGroupEntity.toOperationalLimitsGroupsEntities(operationalLimitsGroupInfos));
        }
        return updatedLimitsGroups;
    }
}
