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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Service
public class LineTypesCatalogService {
    private final LineTypesCatalogRepository lineTypesCatalogRepository;
    private final ObjectMapper mapper;

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
    public LineTypeInfos getLineTypesWithLimits(UUID id) {
        Optional<LineTypeEntity> lineTypeEntity = lineTypesCatalogRepository.findById(id);
        return lineTypeEntity.map(LineTypeEntity::toDtoWithLimits).orElse(null);
    }

    public void deleteLineTypesCatalog() {
        lineTypesCatalogRepository.deleteAll();
    }

    public void resetLineTypes(MultipartFile file) throws IOException {
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(file.getInputStream())) {
            List<LineTypeInfos> lineTypes = mapper.readValue(gzipInputStream, new TypeReference<>() {
            });
            deleteLineTypesCatalog();
            // remove duplicates in file
            Set<LineTypeInfos> lineTypesSet = lineTypes.stream().collect(Collectors.toSet());

            List<LineTypeEntity> lineTypesEntities = lineTypesSet.stream()
                .map(LineTypeInfos::toEntity)
                .collect(Collectors.toList());
            lineTypesCatalogRepository.saveAll(lineTypesEntities);
        }
    }
}
