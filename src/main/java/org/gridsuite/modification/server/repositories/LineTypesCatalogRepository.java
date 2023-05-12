/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.dto.catalog.LineTypeCategory;
import org.gridsuite.modification.server.entities.catalog.LineTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Repository
public interface LineTypesCatalogRepository extends JpaRepository<LineTypeEntity, UUID> {

    List<LineTypeEntity> findAllByCategory(LineTypeCategory category);
}
