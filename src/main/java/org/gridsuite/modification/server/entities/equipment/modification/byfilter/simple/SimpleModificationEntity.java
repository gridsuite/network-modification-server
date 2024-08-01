/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple;

import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;

import java.util.List;
import java.util.UUID;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Data
@Entity
@Table(name = "simpleModification", indexes = @Index(name = "by_simple_modification_id_idx", columnList = "by_simple_modification_id"))
@Inheritance(strategy = InheritanceType.JOINED)
public class SimpleModificationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "simple_modification_id",
            foreignKey = @ForeignKey(name = "simple_modification_id_fk"))
    private List<VariationFilterEntity> filters;

    @Column
    private String editedField;

    @Column
    @Enumerated(EnumType.STRING)
    private DataType dataType;

    public SimpleModificationEntity(AbstractSimpleModificationInfos<?> simpleModificationInfos) {
        this.id = null;
        this.filters = simpleModificationInfos.getFilters().stream().map(FilterInfos::toEntity).toList();
        this.editedField = simpleModificationInfos.getEditedField();
        this.dataType = simpleModificationInfos.getDataType();
    }

    protected void assignAttributes(AbstractSimpleModificationInfos<?> simpleModificationInfos) {
        simpleModificationInfos.setId(id);
        simpleModificationInfos.setFilters(filters.stream()
                .map(filterEntity -> new FilterInfos(filterEntity.getFilterId(), filterEntity.getName()))
                .toList());
        simpleModificationInfos.setEditedField(editedField);
        simpleModificationInfos.setDataType(dataType);
    }

    public <T> AbstractSimpleModificationInfos<T> toSimpleModificationInfos() {
        return null;
    }
}
