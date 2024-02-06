/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
import org.gridsuite.modification.server.repositories.equipmentmodification.*;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Repository
public class TabularModificationRepository {

    private final ModificationRepository modificationRepository;

    private final BatteryModificationRepository batteryModificationRepository;

    private final GeneratorModificationRepository generatorModificationRepository;

    private final LineModificationRepository lineModificationRepository;

    private final LoadModificationRepository loadModificationRepository;

    private final ShuntCompensatorModificationRepository shuntCompensatorModificationRepository;

    private final SubstationModificationRepository substationModificationRepository;

    private final TwoWindingsTransformerModificationRepository twoWindingsTransformerModificationRepository;

    private final VoltageLevelModificationRepository voltageLevelModificationRepository;

    public TabularModificationRepository(
        ModificationRepository modificationRepository, BatteryModificationRepository batteryModificationRepository,
        GeneratorModificationRepository generatorModificationRepository,
        LineModificationRepository lineModificationRepository,
        LoadModificationRepository loadModificationRepository,
        ShuntCompensatorModificationRepository shuntCompensatorModificationRepository,
        SubstationModificationRepository substationModificationRepository,
        TwoWindingsTransformerModificationRepository twoWindingsTransformerModificationRepository,
        VoltageLevelModificationRepository voltageLevelModificationRepository
    ) {
        this.modificationRepository = modificationRepository;
        this.batteryModificationRepository = batteryModificationRepository;
        this.generatorModificationRepository = generatorModificationRepository;
        this.lineModificationRepository = lineModificationRepository;
        this.loadModificationRepository = loadModificationRepository;
        this.shuntCompensatorModificationRepository = shuntCompensatorModificationRepository;
        this.substationModificationRepository = substationModificationRepository;
        this.twoWindingsTransformerModificationRepository = twoWindingsTransformerModificationRepository;
        this.voltageLevelModificationRepository = voltageLevelModificationRepository;
    }

    @Transactional
    public void deleteAll() {
        batteryModificationRepository.deleteAll();
        generatorModificationRepository.deleteAll();
        lineModificationRepository.deleteAll();
        loadModificationRepository.deleteAll();
        shuntCompensatorModificationRepository.deleteAll();
        substationModificationRepository.deleteAll();
        twoWindingsTransformerModificationRepository.deleteAll();
        voltageLevelModificationRepository.deleteAll();
    }

    public void fillSubEntities(TabularModificationEntity tabularModificationEntity) {
        List<UUID> ids = modificationRepository.findSubModificationsIds(tabularModificationEntity.getId());
        List<ModificationEntity> modifications = getRepositoryByModificationType(tabularModificationEntity.getModificationType()).findAllEagerlyByIdIn(ids)
            .stream()
            .map(m -> (ModificationEntity) m)
            .toList();
        tabularModificationEntity.setModifications(modifications);
    }

    private EagerNetworkModificationRepository<? extends ModificationEntity> getRepositoryByModificationType(ModificationType modificationType) {
        switch (modificationType) {
            case BATTERY_MODIFICATION -> {
                return batteryModificationRepository;
            }
            case GENERATOR_MODIFICATION -> {
                return generatorModificationRepository;
            }
            case LINE_MODIFICATION -> {
                return lineModificationRepository;
            }
            case LOAD_MODIFICATION -> {
                return loadModificationRepository;
            }
            case SHUNT_COMPENSATOR_MODIFICATION -> {
                return shuntCompensatorModificationRepository;
            }
            case SUBSTATION_MODIFICATION -> {
                return substationModificationRepository;
            }
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> {
                return twoWindingsTransformerModificationRepository;
            }
            case VOLTAGE_LEVEL_MODIFICATION -> {
                return voltageLevelModificationRepository;
            }
            default -> throw new TabularModificationRepositoryException("Type " + modificationType + " should not be present in tabular modifications.");
        }
    }
}
