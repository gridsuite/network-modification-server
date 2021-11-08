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
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.lang.NonNull;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    @Field(type = FieldType.Nested, includeInParent = true)
    Set<VoltageLevelInfos> voltageLevels;

    UUID networkUuid;

    public static Set<VoltageLevelInfos> getVoltageLevels(@NonNull Identifiable<?> identifiable) {
        if (identifiable instanceof Substation) {
            return ((Substation) identifiable).getVoltageLevelStream().map(vl -> new VoltageLevelInfos(vl.getId(), vl.getNameOrId())).collect(Collectors.toSet());
        } else if (identifiable instanceof VoltageLevel) {
            return Set.of(new VoltageLevelInfos(identifiable.getId(), identifiable.getNameOrId()));
        } else if (identifiable instanceof Switch) {
            return Set.of(new VoltageLevelInfos(((Switch) identifiable).getVoltageLevel().getId(), ((Switch) identifiable).getVoltageLevel().getNameOrId()));
        } else if (identifiable instanceof Injection) {
            return Set.of(new VoltageLevelInfos(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getId(), ((Injection<?>) identifiable).getTerminal().getVoltageLevel().getNameOrId()));
        } else if (identifiable instanceof HvdcLine) {
            HvdcLine hvdcLine = (HvdcLine) identifiable;
            return Set.of(
                    new VoltageLevelInfos(hvdcLine.getConverterStation1().getTerminal().getVoltageLevel().getId(), hvdcLine.getConverterStation1().getTerminal().getVoltageLevel().getNameOrId()),
                    new VoltageLevelInfos(hvdcLine.getConverterStation2().getTerminal().getVoltageLevel().getId(), hvdcLine.getConverterStation2().getTerminal().getVoltageLevel().getNameOrId())
            );
        } else if (identifiable instanceof Branch) {
            Branch<?> branch = (Branch<?>) identifiable;
            return Stream.of(
                    new VoltageLevelInfos(branch.getTerminal1().getVoltageLevel().getId(), branch.getTerminal1().getVoltageLevel().getNameOrId()),
                    new VoltageLevelInfos(branch.getTerminal2().getVoltageLevel().getId(), branch.getTerminal2().getVoltageLevel().getNameOrId())
            )
            .collect(Collectors.toSet());
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ThreeWindingsTransformer w3t = (ThreeWindingsTransformer) identifiable;
            return Stream.of(
                    new VoltageLevelInfos(w3t.getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getId(), w3t.getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getNameOrId()),
                    new VoltageLevelInfos(w3t.getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getId(), w3t.getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getNameOrId()),
                    new VoltageLevelInfos(w3t.getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getId(), w3t.getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getNameOrId())
            )
            .collect(Collectors.toSet());
        }

        throw NetworkModificationException.createEquipmentTypeUnknown(identifiable.getClass().getSimpleName());
    }
}
