/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.SwitchKind;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "create_voltage_level_topology")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "create_voltage_level_topology_id_fk_constraint"))
public class CreateVoltageLevelTopologyEntity extends ModificationEntity {

    @Column(name = "voltage_level_id")
    private String voltageLevelId;

    @Column(name = "section_count")
    private Integer sectionCount;

    @ElementCollection
    @CollectionTable
    private List<String> switchKinds;

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((CreateVoltageLevelTopologyInfos) modificationInfos);
    }

    private void assignAttributes(CreateVoltageLevelTopologyInfos createVoltageLevelTopologyInfos) {
        this.voltageLevelId = createVoltageLevelTopologyInfos.getVoltageLevelId();
        this.switchKinds = createVoltageLevelTopologyInfos.getSwitchKinds().stream().map(Enum::name).collect(Collectors.toList());
        this.sectionCount = createVoltageLevelTopologyInfos.getSectionCount();
    }

    public CreateVoltageLevelTopologyEntity(CreateVoltageLevelTopologyInfos createVoltageLevelTopologyInfos) {
        super(createVoltageLevelTopologyInfos);
        assignAttributes(createVoltageLevelTopologyInfos);
    }

    @Override
    public CreateVoltageLevelTopologyInfos toModificationInfos() {
        return toCreateVoltageLevelTopologyInfos().build();
    }

    private CreateVoltageLevelTopologyInfos.CreateVoltageLevelTopologyInfosBuilder<?, ?> toCreateVoltageLevelTopologyInfos() {
        return CreateVoltageLevelTopologyInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .voltageLevelId(getVoltageLevelId())
            .switchKinds(getSwitchKinds().stream().map(SwitchKind::valueOf).collect(Collectors.toList()))
            .sectionCount(getSectionCount());
    }
}
