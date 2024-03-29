/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.BranchModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;

import jakarta.persistence.*;

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

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected1")),
        @AttributeOverride(name = "opType", column = @Column(name = "connected1Op"))
    })
    private BooleanModificationEmbedded connected1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected2")),
        @AttributeOverride(name = "opType", column = @Column(name = "connected2Op"))
    })
    private BooleanModificationEmbedded connected2;

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
        if (branchModificationInfos.getCurrentLimits1() == null) {
            currentLimits1 = null;
        } else {
            currentLimits1 = branchModificationInfos.getCurrentLimits1().toEntity();
        }
        if (branchModificationInfos.getCurrentLimits2() == null) {
            currentLimits2 = null;
        } else {
            currentLimits2 = branchModificationInfos.getCurrentLimits2().toEntity();
        }
        this.connected1 = branchModificationInfos.getConnected1() != null ? new BooleanModificationEmbedded(branchModificationInfos.getConnected1()) : null;
        this.connected2 = branchModificationInfos.getConnected2() != null ? new BooleanModificationEmbedded(branchModificationInfos.getConnected2()) : null;
    }
}
