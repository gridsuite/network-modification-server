/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.annotations.InnerField;
import org.springframework.data.elasticsearch.annotations.MultiField;
import org.springframework.data.elasticsearch.annotations.Setting;
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
@Document(indexName = "#{@environment.getProperty('powsybl-ws.elasticsearch.index.prefix')}equipments")
@Setting(settingPath = "elasticsearch_settings.json")
@TypeAlias(value = "EquipmentInfos")
public class EquipmentInfos {
    @Id
    String uniqueId;

    @MultiField(
        mainField = @Field(name = "equipmentId", type = FieldType.Text),
        otherFields = {
            @InnerField(suffix = "fullascii", type = FieldType.Keyword, normalizer = "fullascii"),
            @InnerField(suffix = "raw", type = FieldType.Keyword)
        }
    )
    String id;

    @MultiField(
        mainField = @Field(name = "equipmentName", type = FieldType.Text),
        otherFields = {
            @InnerField(suffix = "fullascii", type = FieldType.Keyword, normalizer = "fullascii"),
            @InnerField(suffix = "raw", type = FieldType.Keyword)
        }
    )
    String name;

    @Field("equipmentType")
    String type;

    @Field(type = FieldType.Nested, includeInParent = true)
    Set<VoltageLevelInfos> voltageLevels;

    UUID networkUuid;

    public static Set<VoltageLevelInfos> getVoltageLevels(@NonNull Identifiable<?> identifiable) {
        if (identifiable instanceof Substation) {
            return ((Substation) identifiable).getVoltageLevelStream().map(vl -> VoltageLevelInfos.builder().id(vl.getId()).name(vl.getNameOrId()).build()).collect(Collectors.toSet());
        } else if (identifiable instanceof VoltageLevel) {
            return Set.of(VoltageLevelInfos.builder().id(identifiable.getId()).name(identifiable.getNameOrId()).build());
        } else if (identifiable instanceof Switch) {
            return Set.of(VoltageLevelInfos.builder().id(((Switch) identifiable).getVoltageLevel().getId()).name(((Switch) identifiable).getVoltageLevel().getNameOrId()).build());
        } else if (identifiable instanceof Injection) {
            return Set.of(VoltageLevelInfos.builder().id(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getId()).name(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getNameOrId()).build());
        } else if (identifiable instanceof Bus) {
            return Set.of(VoltageLevelInfos.builder().id(((Bus) identifiable).getVoltageLevel().getId()).name(((Bus) identifiable).getVoltageLevel().getNameOrId()).build());
        } else if (identifiable instanceof HvdcLine) {
            HvdcLine hvdcLine = (HvdcLine) identifiable;
            return Set.of(
                VoltageLevelInfos.builder().id(hvdcLine.getConverterStation1().getTerminal().getVoltageLevel().getId()).name(hvdcLine.getConverterStation1().getTerminal().getVoltageLevel().getNameOrId()).build(),
                VoltageLevelInfos.builder().id(hvdcLine.getConverterStation2().getTerminal().getVoltageLevel().getId()).name(hvdcLine.getConverterStation2().getTerminal().getVoltageLevel().getNameOrId()).build()
            );
        } else if (identifiable instanceof Branch) {
            Branch<?> branch = (Branch<?>) identifiable;
            return Stream.of(
                    VoltageLevelInfos.builder().id(branch.getTerminal1().getVoltageLevel().getId()).name(branch.getTerminal1().getVoltageLevel().getNameOrId()).build(),
                    VoltageLevelInfos.builder().id(branch.getTerminal2().getVoltageLevel().getId()).name(branch.getTerminal2().getVoltageLevel().getNameOrId()).build()
                )
                .collect(Collectors.toSet());
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ThreeWindingsTransformer w3t = (ThreeWindingsTransformer) identifiable;
            return Stream.of(
                    VoltageLevelInfos.builder().id(w3t.getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getId()).name(w3t.getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getNameOrId()).build(),
                    VoltageLevelInfos.builder().id(w3t.getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getId()).name(w3t.getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getNameOrId()).build(),
                    VoltageLevelInfos.builder().id(w3t.getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getId()).name(w3t.getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getNameOrId()).build()
                )
                .collect(Collectors.toSet());
        }

        throw NetworkModificationException.createEquipmentTypeUnknown(identifiable.getClass().getSimpleName());
    }
}
