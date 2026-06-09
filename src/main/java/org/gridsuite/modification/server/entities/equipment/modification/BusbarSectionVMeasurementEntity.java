/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.model.BusbarSectionVMeasurementModel;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;

import java.util.UUID;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

/**
 * @author Mohamed Ben Rejeb <mohamed.ben-rejeb at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "busbar_section_v_measurement")
public class BusbarSectionVMeasurementEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "busbar_section_id")
    private String busbarSectionId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "v_measurement_value")),
        @AttributeOverride(name = "opType", column = @Column(name = "v_measurement_value_op"))
    })
    private DoubleModificationEmbedded vMeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "v_measurement_validity")),
        @AttributeOverride(name = "opType", column = @Column(name = "v_measurement_validity_op"))
    })
    private BooleanModificationEmbedded vMeasurementValidity;

    public BusbarSectionVMeasurementEntity(BusbarSectionVMeasurementModel infos) {
        this.busbarSectionId = infos.getBusbarSectionId();
        this.vMeasurementValue = infos.getVMeasurementValue() != null ? new DoubleModificationEmbedded(infos.getVMeasurementValue()) : null;
        this.vMeasurementValidity = infos.getVMeasurementValidity() != null ? new BooleanModificationEmbedded(infos.getVMeasurementValidity()) : null;
    }

    public BusbarSectionVMeasurementModel toInfos() {
        return BusbarSectionVMeasurementModel.builder()
                .busbarSectionId(busbarSectionId)
                .vMeasurementValue(toAttributeModification(vMeasurementValue))
                .vMeasurementValidity(toAttributeModification(vMeasurementValidity))
                .build();
    }
}
