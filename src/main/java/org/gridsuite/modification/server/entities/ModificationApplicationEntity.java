/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.utils.JsonListConverter;

import java.util.List;
import java.util.UUID;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@Builder
@Embeddable
@AllArgsConstructor
@NoArgsConstructor
@Setter
@Entity
@Table(name = "modification_application")
public class ModificationApplicationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private Long id;

    @Column(name = "network_uuid")
    UUID networkUuid;

    @Column(name = "impacted_equipment_ids", columnDefinition = "CLOB")
    @Convert(converter = JsonListConverter.class)
    private List<String> impactedEquipmentIds;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "modification_uuid", foreignKey = @ForeignKey(name = "modification_uuid_fk_constraint"))
    ModificationEntity modification;
}
