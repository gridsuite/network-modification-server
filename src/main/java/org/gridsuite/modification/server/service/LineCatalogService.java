/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.LineKind;
import org.gridsuite.modification.server.dto.LineType;
import org.gridsuite.modification.server.entities.LineTypeEntity;
import org.gridsuite.modification.server.repositories.LineCatalogRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Service
public class LineCatalogService {
    private final LineCatalogRepository lineCatalogRepository;

    public LineCatalogService(LineCatalogRepository lineCatalogRepository) {
        this.lineCatalogRepository = lineCatalogRepository;
    }

    @Transactional(readOnly = true)
    public List<LineType> getLineCatalog(LineKind kind) {
        switch (kind) {
            case AERIAL:
            case UNDERGROUND:
                return lineCatalogRepository.findAllByKind(kind).stream()
                .map(LineTypeEntity::toLineType)
                .collect(Collectors.toList());
            default:
                return lineCatalogRepository.findAll().stream()
                    .map(LineTypeEntity::toLineType)
                    .collect(Collectors.toList());
        }
    }

    @Transactional
    public void fillLineCatalog(List<LineType> lineCatalog) {
        List<LineTypeEntity> lineTypeEntityCatalog = lineCatalog.stream()
            .map(LineType::toEntity)
            .collect(Collectors.toList());

        lineCatalogRepository.saveAll(lineTypeEntityCatalog);
    }

    public void deleteLineCatalog() {
        lineCatalogRepository.deleteAll();
    }
}
