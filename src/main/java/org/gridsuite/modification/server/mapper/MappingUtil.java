package org.gridsuite.modification.server.mapper;

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
