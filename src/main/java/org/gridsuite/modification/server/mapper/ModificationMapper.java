package org.gridsuite.modification.server.mapper;

import java.lang.reflect.Constructor;

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
