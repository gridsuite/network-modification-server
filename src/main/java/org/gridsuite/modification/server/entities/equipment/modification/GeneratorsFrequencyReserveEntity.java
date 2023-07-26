/*
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.List;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Entity
@Table(name = "GeneratorsFrequencyReserve")
public class GeneratorsFrequencyReserveEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @ElementCollection
    @CollectionTable(name = "generatorsFrequencyReserveFilters")
    private List<GeneratorsFilterEmbeddable> generatorsFilters;

    @Column(name = "frequencyReserve")
    private double frequencyReserve;

    public GeneratorsFrequencyReserveEntity(List<GeneratorsFilterEmbeddable> generatorsFilters, double frequencyReserve) {
        this.id = null;
        this.generatorsFilters = generatorsFilters;
        this.frequencyReserve = frequencyReserve;
    }
}
