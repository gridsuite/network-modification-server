/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.*;
import lombok.*;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;

import java.lang.reflect.Constructor;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.MISSING_MODIFICATION_DESCRIPTION;

/**
 * @author Slimane Amar {@literal <slimane.amar at rte-france.com>}
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Table(
        name = "modification",
        indexes = {
            // The cascade / lookup queries hit (container_type, container_id) constantly.
            @Index(name = "modification_container_idx", columnList = "container_type, container_id")
        }
)
public class ModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "type")
    private String type;

    @Column(name = "date", columnDefinition = "timestamptz")
    private Instant date;

    @Column(name = "stashed", columnDefinition = "boolean default false")
    private Boolean stashed = false;

    // manages the order inside a group of modification in gridstudy AND for the order inside a composite modification
    @Column(name = "modifications_order")
    private int modificationsOrder;

    @Column(name = "message_type")
    private String messageType;

    @Column(name = "message_values")
    private String messageValues;

    @Column(name = "activated", columnDefinition = "boolean default true")
    private Boolean activated = true;

    @Column(name = "description", columnDefinition = "CLOB")
    private String description;

    // Updates are managed by the container collection to avoid duplicate writes
    @Column(name = "container_id", updatable = false)
    private UUID containerId;

    @Column(name = "container_type")
    @Enumerated(EnumType.STRING)
    private ModificationContainerType containerType;

    public ModificationEntity(UUID id, String type, Instant date, Boolean stashed, Boolean activated,
                              String messageType, String messageValues, String description) {
        this.id = id;
        this.type = type;
        this.date = date;
        this.stashed = stashed;
        this.activated = activated;
        this.messageType = messageType;
        this.messageValues = messageValues;
        this.description = description;
    }

    public ModificationEntity(UUID id, String type) {
        this.id = id;
        this.type = type;
    }

    protected ModificationEntity(ModificationInfos modificationInfos) {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MISSING_MODIFICATION_DESCRIPTION, "Missing network modification description");
        }
        //We need to limit the precision to avoid database precision storage limit issue (postgres has a precision of 6 digits while h2 can go to 9)
        this.date = Instant.now().truncatedTo(ChronoUnit.MICROS);
        // Do not put this stashed status in assignAttributes, it's not part of a network modification as such.
        this.stashed = modificationInfos.getStashed();
        this.activated = modificationInfos.getActivated();

        assignAttributes(modificationInfos);
    }

    public ModificationInfos toModificationInfos() {
        ModificationInfos modificationInfos = ModificationInfos.builder()
                .uuid(this.id)
                .date(this.date)
                .stashed(this.stashed)
                .activated(this.activated)
                .description(this.description)
                .messageType(this.messageType)
                .messageValues(this.messageValues)
                .build();
        modificationInfos.setType(ModificationType.valueOf(this.type));
        return modificationInfos;
    }

    public void update(ModificationInfos modificationInfos) {
        // Basic attributes are immutable in the database
        if (modificationInfos == null) {
            throw new NullPointerException("Impossible to update entity from null DTO");
        }
        assignAttributes(modificationInfos);
    }

    @SneakyThrows
    private void assignAttributes(ModificationInfos modificationInfos) {
        this.setType(modificationInfos.getType().name());
        this.setMessageType(modificationInfos.getType().name());
        if (modificationInfos.getDescription() != null) {
            this.setDescription(modificationInfos.getDescription());
        }
        this.setMessageValues(new ObjectMapper().writeValueAsString(modificationInfos.getMapMessageValues()));
    }

    public void attachToContainer(@NonNull ModificationContainer container) {
        // containerId is written by the parent's @OneToMany @JoinColumn — don't touch.
        this.containerType = container.getContainerType();
    }

    public void detachFromContainer() {
        this.containerType = null;
        // Similarly: Hibernate will null containerId when we leave the collection.
    }

    public boolean isIn(ModificationContainerType type) {
        return this.containerType == type;
    }

    public static ModificationEntity fromDTO(ModificationInfos dto) {
        if (dto instanceof EquipmentAttributeModificationInfos infos) {
            return EquipmentAttributeModificationEntity.createAttributeEntity(infos);
        }

        Class<? extends ModificationEntity> entityClass = EntityRegistry.getEntityClass(dto.getClass());
        if (entityClass != null) {
            try {
                Constructor<? extends ModificationEntity> constructor = entityClass.getConstructor(dto.getClass());
                return constructor.newInstance(dto);
            } catch (Exception e) {
                throw new RuntimeException("Failed to map DTO to Entity", e);
            }
        } else {
            throw new IllegalArgumentException("No entity class registered for DTO class: " + dto.getClass());
        }
    }
}
