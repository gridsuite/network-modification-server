/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.NetworkModificationException;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_TYPE_CATEGORY_MISMATCH;

import org.gridsuite.modification.server.dto.catalog.LineTypeCategory;
import org.gridsuite.modification.server.dto.catalog.LineType;
import org.gridsuite.modification.server.entities.catalog.LineTypeEntity;
import org.gridsuite.modification.server.repositories.LineTypesCatalogRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Service
public class LineTypesCatalogService {
    private final LineTypesCatalogRepository lineTypesCatalogRepository;

    public LineTypesCatalogService(LineTypesCatalogRepository lineTypesCatalogRepository) {
        this.lineTypesCatalogRepository = lineTypesCatalogRepository;
    }

    @Transactional(readOnly = true)
    public List<LineType> getLineTypesCatalog(LineTypeCategory category) {
        switch (category) {
            case AERIAL:
            case UNDERGROUND:
                return lineTypesCatalogRepository.findAllByCategory(category).stream()
                .map(LineTypeEntity::toDto)
                .collect(Collectors.toList());
            default:
                throw new NetworkModificationException(LINE_TYPE_CATEGORY_MISMATCH);
        }
    }

    @Transactional(readOnly = true)
    public List<LineType> getAllLineTypesCatalog() {
        return lineTypesCatalogRepository.findAll().stream()
            .map(LineTypeEntity::toDto)
            .collect(Collectors.toList());
    }

    public void deleteLineTypesCatalog() {
        lineTypesCatalogRepository.deleteAll();
    }

    public void resetLineTypesCatalog(List<LineType> lineTypesCatalog) {
        lineTypesCatalogRepository.deleteAll();
        // remove duplicates in file
        Set<LineType> lineTypesCatalogSet = lineTypesCatalog.stream().collect(Collectors.toSet());

        List<LineTypeEntity> lineTypesEntityCatalog = lineTypesCatalogSet.stream()
            .map(LineType::toEntity)
            .collect(Collectors.toList());
        lineTypesCatalogRepository.saveAll(lineTypesEntityCatalog);
    }
}
