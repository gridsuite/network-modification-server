/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelSectionsCreationInfos;

/**
 * @author Rehili Ghazwa <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "voltageLevelSectionsCreation_id_fk_constraint"))
public class VoltageLevelSectionsCreationEntity extends EquipmentCreationEntity {

    @Column
    private String voltageLevelId;

    @Column
    private boolean createTheBusbarSectionsAfterTheReferenceBusbarSection;

    @Column
    private String leftSwitchKind;

    @Column
    private String rightSwitchKind;

    @Column
    private boolean allBusbars;

    @Column
    private String referenceBusbarSectionId;

    public VoltageLevelSectionsCreationEntity(VoltageLevelSectionsCreationInfos voltageLevelSectionsCreationInfos) {
        super(voltageLevelSectionsCreationInfos);
        assignAttributes(voltageLevelSectionsCreationInfos);
    }

    @Override
    public VoltageLevelSectionsCreationInfos toModificationInfos() {
        return toVoltageLevelSectionsCreationInfosBuilder().build();
    }

    public VoltageLevelSectionsCreationInfos toVoltageLevelSectionsCreationInfos() {
        return toVoltageLevelSectionsCreationInfosBuilder()
                .build();
    }

    private VoltageLevelSectionsCreationInfos.VoltageLevelSectionsCreationInfosBuilder<?, ?> toVoltageLevelSectionsCreationInfosBuilder() {
        return VoltageLevelSectionsCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .voltageLevelId(getVoltageLevelId())
                .referenceBusbarSectionId(getReferenceBusbarSectionId())
                .allBusbars(isAllBusbars())
                .rightSwitchKind(getRightSwitchKind())
                .leftSwitchKind(getLeftSwitchKind())
                .createTheBusbarSectionsAfterTheReferenceBusbarSection(isCreateTheBusbarSectionsAfterTheReferenceBusbarSection());
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VoltageLevelSectionsCreationInfos) modificationInfos);
    }

    private void assignAttributes(VoltageLevelSectionsCreationInfos voltageLevelSectionsCreationInfos) {
        this.voltageLevelId = voltageLevelSectionsCreationInfos.getVoltageLevelId();
        this.referenceBusbarSectionId = voltageLevelSectionsCreationInfos.getReferenceBusbarSectionId();
        this.allBusbars = voltageLevelSectionsCreationInfos.isAllBusbars();
        this.leftSwitchKind = voltageLevelSectionsCreationInfos.getLeftSwitchKind();
        this.rightSwitchKind = voltageLevelSectionsCreationInfos.getRightSwitchKind();
        this.createTheBusbarSectionsAfterTheReferenceBusbarSection = voltageLevelSectionsCreationInfos.isCreateTheBusbarSectionsAfterTheReferenceBusbarSection();
    }
}

