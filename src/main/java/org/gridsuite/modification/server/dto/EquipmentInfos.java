/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.*;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.lang.NonNull;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@ToString
@EqualsAndHashCode
@Document(indexName = "network-modification-server")
@TypeAlias(value = "EquipmentInfos")
public class EquipmentInfos {
    @Id
    String uniqueId;

    @Field("equipmentId")
    String id;

    @Field("equipmentName")
    String name;

    @Field("equipmentType")
    String type;

    Set<String> voltageLevelsIds;

    UUID networkUuid;

    public static Set<String> getVoltageLevelsIds(@NonNull Identifiable<?> identifiable) {
        if (identifiable instanceof Substation) {
            return ((Substation) identifiable).getVoltageLevelStream().map(VoltageLevel::getId).collect(Collectors.toSet());
        } else if (identifiable instanceof VoltageLevel) {
            return Set.of(identifiable.getId());
        } else if (identifiable instanceof Switch) {
            return Set.of(((Switch) identifiable).getVoltageLevel().getId());
        } else if (identifiable instanceof Injection) {
            return Set.of(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getId());
        } else if (identifiable instanceof HvdcLine) {
            HvdcLine hvdcLine = (HvdcLine) identifiable;
            return Set.of(
                hvdcLine.getConverterStation1().getTerminal().getVoltageLevel().getId(),
                hvdcLine.getConverterStation2().getTerminal().getVoltageLevel().getId()
            );
        } else if (identifiable instanceof Branch) {
            Branch<?> branch = (Branch<?>) identifiable;
            String vlId1 = branch.getTerminal1().getVoltageLevel().getId();
            String vlId2 = branch.getTerminal2().getVoltageLevel().getId();
            return vlId1.equals(vlId2) ? Set.of(vlId1) : Set.of(vlId1, vlId2); // Internal line
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ThreeWindingsTransformer w3t = (ThreeWindingsTransformer) identifiable;
            return Set.of(
                w3t.getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getId(),
                w3t.getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getId(),
                w3t.getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getId()
            );
        }

        throw NetworkModificationException.createEquipmentTypeUnknown(identifiable.getClass().getSimpleName());
    }
}
