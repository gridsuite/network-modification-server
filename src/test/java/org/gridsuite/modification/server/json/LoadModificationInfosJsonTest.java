/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.json.JsonUtil;
import com.powsybl.commons.test.AbstractConverterTest;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.Objects;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LoadModificationInfosJsonTest extends AbstractConverterTest {

    private static LoadModificationInfos createVersion1() {
        return LoadModificationInfos.builder()
                .uuid(UUID.fromString("d0f3efc0-1e41-4669-98ab-34167641578e"))
                .date(ZonedDateTime.parse("2023-12-29T11:29:24.089680Z"))
                .stashed(false)
                .equipmentId("l1")
                .equipmentName(AttributeModification.toAttributeModification("l1_name", OperationType.SET))
                .voltageLevelId(AttributeModification.toAttributeModification("vl1", OperationType.SET))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification("vl1_b11", OperationType.SET))
                .loadType(AttributeModification.toAttributeModification(LoadType.FICTITIOUS, OperationType.SET))
                .constantActivePower(AttributeModification.toAttributeModification(10.0, OperationType.SET))
                .constantReactivePower(AttributeModification.toAttributeModification(10.0, OperationType.SET))
                .build();
    }

    public static void write(LoadModificationInfos modification, Path jsonFile) {
        Objects.requireNonNull(modification);
        Objects.requireNonNull(jsonFile);

        try (OutputStream os = Files.newOutputStream(jsonFile)) {
            ObjectMapper objectMapper = JsonUtil.createObjectMapper();
            objectMapper.registerModule(new ModificationInfosJsonModule());
            ObjectWriter writer = objectMapper.writerWithDefaultPrettyPrinter();
            writer.writeValue(os, modification);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static LoadModificationInfos read(InputStream is) throws IOException {
        Objects.requireNonNull(is);
        ObjectMapper objectMapper = JsonUtil.createObjectMapper();
        objectMapper.registerModule(new ModificationInfosJsonModule());
        return objectMapper.readValue(is, LoadModificationInfos.class);
    }

    public static LoadModificationInfos read(Path jsonFile) {
        Objects.requireNonNull(jsonFile);
        try (InputStream is = Files.newInputStream(jsonFile)) {
            return read(is);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Test
    void roundTripVersion1Test() throws IOException {
        roundTripTest(createVersion1(), LoadModificationInfosJsonTest::write, LoadModificationInfosJsonTest::read, "/json/LoadModificationInfosVersion1.json");
    }

}
