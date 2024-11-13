/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public final class MappingUtil {

    private MappingUtil() {
    }

    @SuppressWarnings("unchecked")
    public static <D, E> E mapToEntity(D dto) {
        if (dto == null) {
            return null;
        }
        EntityMapper<D, E> mapper = (EntityMapper<D, E>) MapperRegistry.getInstance().getMapper(dto.getClass());
        return mapper.toEntity(dto);
    }

    @SuppressWarnings("unchecked")
    public static <D, E> D mapToDto(E entity, Class<D> dtoClass) {
        if (entity == null) {
            return null;
        }
        EntityMapper<D, E> mapper = (EntityMapper<D, E>) MapperRegistry.getInstance().getMapper(dtoClass);
        return mapper.toDto(entity);
    }
}
