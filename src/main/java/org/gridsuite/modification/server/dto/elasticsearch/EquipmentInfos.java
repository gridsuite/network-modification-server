/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.elasticsearch;

import com.powsybl.iidm.network.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.VoltageLevelInfos;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.*;
import org.springframework.lang.NonNull;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
@Document(indexName = "#{@environment.getProperty('powsybl-ws.elasticsearch.index.prefix')}equipments")
@Setting(settingPath = "elasticsearch_settings.json")
@TypeAlias(value = "EquipmentInfos")
public class EquipmentInfos extends BasicEquipmentInfos {
    @MultiField(
        mainField = @Field(name = "equipmentName", type = FieldType.Text),
        otherFields = {
            @InnerField(suffix = "fullascii", type = FieldType.Keyword, normalizer = "fullascii"),
            @InnerField(suffix = "raw", type = FieldType.Keyword)
        }
    )
    private String name;

    @Field("equipmentType")
    private String type;

    @Field(type = FieldType.Nested, includeInParent = true)
    private Set<VoltageLevelInfos> voltageLevels;

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
