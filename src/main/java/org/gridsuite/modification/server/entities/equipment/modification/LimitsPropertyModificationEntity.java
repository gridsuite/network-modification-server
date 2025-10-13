package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.dto.LimitsPropertyInfos;

import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "limits_property_modification")
public class LimitsPropertyModificationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "value", nullable = false)
    private String value;

    public static LimitsPropertyModificationEntity fromLimitsPropertyInfos(LimitsPropertyInfos propertyInfos) {
        return new LimitsPropertyModificationEntity(null, propertyInfos.name(), propertyInfos.value());
    }

    public LimitsPropertyInfos toLimitsPropertyInfos() {
        return new LimitsPropertyInfos(name, value);
    }

}
