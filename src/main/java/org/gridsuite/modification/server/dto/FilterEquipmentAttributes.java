package org.gridsuite.modification.server.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class FilterEquipmentAttributes {
    private String id;
    private Double distributionKey;
}
