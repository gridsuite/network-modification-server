package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
import org.gridsuite.modification.server.repositories.equipmentmodification.*;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class EquipmentModificationRepositories {

    private final ModificationRepository modificationRepository;

    private final BatteryModificationRepository batteryModificationRepository;

    private final GeneratorModificationRepository generatorModificationRepository;

    private final LineModificationRepository lineModificationRepository;

    private final LoadModificationRepository loadModificationRepository;

    private final ShuntCompensatorModificationRepository shuntCompensatorModificationRepository;

    private final SubstationModificationRepository substationModificationRepository;

    private final TwoWindingsTransformerModificationRepository twoWindingsTransformerModificationRepository;

    private final VoltageLevelModificationRepository voltageLevelModificationRepository;

    public EquipmentModificationRepositories(
            ModificationRepository modificationRepository,
            BatteryModificationRepository batteryModificationRepository,
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

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteAll() {
        modificationRepository.deleteAll();
        batteryModificationRepository.deleteAll();
        generatorModificationRepository.deleteAll();
        lineModificationRepository.deleteAll();
        loadModificationRepository.deleteAll();
        shuntCompensatorModificationRepository.deleteAll();
        substationModificationRepository.deleteAll();
        twoWindingsTransformerModificationRepository.deleteAll();
        voltageLevelModificationRepository.deleteAll();
    }

    public GeneratorModificationRepository getGeneratorModificationRepository() {
        return generatorModificationRepository;
    }

    public List<ModificationEntity> findSubEntities(ModificationType modificationType, List<UUID> ids) {
        List<ModificationEntity> subEntities = new ArrayList<>();
        switch (modificationType) {
            case GENERATOR_MODIFICATION -> subEntities.addAll(generatorModificationRepository.findAllWithReactiveCapabilityCurvePointsByIdIn(ids));
            default -> subEntities.addAll(getRepositoryByModificationType(modificationType).findAllById(ids));
        }
        return subEntities;
    }

    public JpaRepository<? extends ModificationEntity, UUID> getRepositoryByModificationType(ModificationType modificationType) {
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
            default -> {
                return null;
            }
        }
    }

    public void deleteTabularModification(TabularModificationEntity tabularModificationEntity) {
        ((EquipmentModificationRepository) getRepositoryByModificationType(tabularModificationEntity.getModificationType())).deleteSubModificationsByIds(modificationRepository.findSubModificationsIds(tabularModificationEntity.getId()));
        modificationRepository.deleteTabularModificationInJoinTableByIds(tabularModificationEntity.getId());
        modificationRepository.deleteModificationByIds(modificationRepository.findSubModificationsIds(tabularModificationEntity.getId()));
    }
}
