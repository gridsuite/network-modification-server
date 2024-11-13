/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import java.lang.reflect.Constructor;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class ModificationMapper <D, E> implements EntityMapper<D, E> {
    private final Class<E> entityClass;
    private final Class<D> dtoClass;

    public ModificationMapper(Class<E> entityClass, Class<D> dtoClass) {
        this.entityClass = entityClass;
        this.dtoClass = dtoClass;
    }

    @Override
    public E toEntity(D dto) {
        try {
            Constructor<E> constructor = entityClass.getConstructor(dtoClass);
            return constructor.newInstance(dto);
        } catch (Exception e) {
            throw new RuntimeException("Failed to map DTO to Entity", e);
        }
    }

    @Override
    public D toDto(E entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
