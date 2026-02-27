/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;
import org.gridsuite.modification.server.entities.catalog.LineTypeEntity;
import org.gridsuite.modification.server.repositories.LineTypesCatalogRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Service
public class LineTypesCatalogService {
    private final LineTypesCatalogRepository lineTypesCatalogRepository;
    private final ObjectMapper mapper;

    private static final Logger LOGGER = LoggerFactory.getLogger(LineTypesCatalogService.class);

    public LineTypesCatalogService(LineTypesCatalogRepository lineTypesCatalogRepository, ObjectMapper objectMapper) {
        this.lineTypesCatalogRepository = lineTypesCatalogRepository;
        this.mapper = objectMapper;
    }

    @Transactional(readOnly = true)
    public List<LineTypeInfos> getAllLineTypes() {
        return lineTypesCatalogRepository.findAll().stream()
            .map(LineTypeEntity::toDto)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public LineTypeInfos getLineTypesWithLimits(UUID id, String area, String temperature, String shapeFactor) {
        Optional<LineTypeEntity> lineTypeEntity = lineTypesCatalogRepository.findById(id);
        return lineTypeEntity.map((LineTypeEntity lineType) -> lineType.toDtoWithLimits(area, temperature, shapeFactor)).orElse(null);
    }

    @Transactional(readOnly = true)
    public LineTypeInfos getLineTypesWithAreaTemperatureShapeFactors(UUID id) {
        Optional<LineTypeEntity> lineTypeEntity = lineTypesCatalogRepository.findById(id);
        return lineTypeEntity.map(LineTypeEntity::toDtoWithAreaTemperatureShapeFactors).orElse(null);
    }

    public void deleteLineTypesCatalog() {
        LOGGER.info("Starting to delete all line types from the catalog");
        lineTypesCatalogRepository.truncateCatalogFast();
        LOGGER.info("All line types from the catalog deleted");
    }

    public void resetLineTypes(MultipartFile file) {
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(file.getInputStream())) {
            List<LineTypeInfos> lineTypes = mapper.readValue(gzipInputStream, new TypeReference<List<LineTypeInfos>>() {
            });
            deleteLineTypesCatalog();
            // remove duplicates in file
            Set<LineTypeInfos> lineTypesSet = lineTypes.stream().collect(Collectors.toSet());

            List<LineTypeEntity> lineTypesEntities = lineTypesSet.stream()
                .map(LineTypeInfos::toEntity)
                .collect(Collectors.toList());
            LOGGER.info("Starting to save {} line types in the catalog", lineTypesEntities.size());
            lineTypesCatalogRepository.saveAll(lineTypesEntities);
            LOGGER.info("all line types saved in the catalog");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
