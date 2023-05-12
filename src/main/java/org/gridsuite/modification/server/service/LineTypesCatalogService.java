/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.LineKind;
import org.gridsuite.modification.server.NetworkModificationException;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_TYPE_KIND_MISMATCH;
import org.gridsuite.modification.server.dto.LineType;
import org.gridsuite.modification.server.entities.LineTypeEntity;
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
    public List<LineType> getLineTypesCatalog(LineKind kind) {
        switch (kind) {
            case AERIAL:
            case UNDERGROUND:
                return lineTypesCatalogRepository.findAllByKind(kind).stream()
                .map(LineTypeEntity::toDto)
                .collect(Collectors.toList());
            default:
                throw new NetworkModificationException(LINE_TYPE_KIND_MISMATCH);
        }
    }

    @Transactional(readOnly = true)
    public List<LineType> getAllLineTypesCatalog() {
        return lineTypesCatalogRepository.findAll().stream()
            .map(LineTypeEntity::toDto)
            .collect(Collectors.toList());
    }

    @Transactional
    public void fillLineTypesCatalog(List<LineType> lineCatalog) {
        Set<LineType> lineCatalogSet = lineCatalog.stream().collect(Collectors.toSet());
        Set<LineType> currentCatalog = getAllLineTypesCatalog().stream().collect(Collectors.toSet());
        Set<LineType> filteredLineCatalog = lineCatalogSet.stream()
            .filter(lineType -> !currentCatalog.contains(lineType))
            .collect(Collectors.toSet());

        List<LineTypeEntity> lineTypeEntityCatalog = filteredLineCatalog.stream()
            .map(LineType::toEntity)
            .collect(Collectors.toList());

        lineTypesCatalogRepository.saveAll(lineTypeEntityCatalog);
    }

    public void deleteLineTypesCatalog() {
        lineTypesCatalogRepository.deleteAll();
    }
}
