package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.IndexOperations;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verifyNoMoreInteractions;

@SpringBootTest
@DisableElasticsearch
public class SupervisionTest {
    @Autowired
    private SupervisionService supervisionService;

    @MockBean
    private ElasticsearchOperations elasticsearchOperations;

    @MockBean
    private IndexOperations indexOperations;

    @MockBean
    private ModificationApplicationRepository modificationApplicationRepository;

    @MockBean
    private ModificationApplicationInfosRepository modificationApplicationInfosRepository;

    @Captor
    ArgumentCaptor<List<ModificationApplicationInfos>> modificationListCaptor;

    @Test
    void testGetModificationsCount() {
        supervisionService.getIndexModificationsCount();
        verify(modificationApplicationInfosRepository, times(1)).count();
    }

    @Test
    void testGetModificationsToIndexCount() {
        supervisionService.getModificationsToReindexCount();
        verify(modificationApplicationRepository, times(1)).count();
    }

    @Test
    void testReindexElements() {
        UUID networkUuid = UUID.randomUUID();
        UUID groupUuid = UUID.randomUUID();
        ModificationEntity modificationMock = Mockito.mock(ModificationEntity.class);
        ModificationGroupEntity groupMock = Mockito.mock(ModificationGroupEntity.class);

        ModificationApplicationEntity modificationApplicationEntity = ModificationApplicationEntity.builder()
            .networkUuid(networkUuid)
            .modifiedEquipmentIds(List.of("equipment1"))
            .createdEquipmentIds(List.of("equipment2"))
            .deletedEquipmentIds(List.of("equipment3"))
            .build();
        ModificationApplicationEntity modificationApplicationEntity2 = ModificationApplicationEntity.builder()
            .networkUuid(networkUuid)
            .modifiedEquipmentIds(List.of("equipment21"))
            .createdEquipmentIds(List.of("equipment22"))
            .deletedEquipmentIds(List.of("equipment23"))
            .build();

        modificationApplicationEntity = Mockito.spy(modificationApplicationEntity);
        modificationApplicationEntity2 = Mockito.spy(modificationApplicationEntity2);
        Mockito.when(modificationMock.getId()).thenReturn(UUID.randomUUID());
        Mockito.when(groupMock.getId()).thenReturn(groupUuid);
        Mockito.when(modificationMock.getGroup()).thenReturn(groupMock);
        Mockito.when(modificationApplicationEntity.getModification()).thenReturn(modificationMock);
        Mockito.when(modificationApplicationEntity2.getModification()).thenReturn(modificationMock);

        List<ModificationApplicationEntity> allModifications = List.of(modificationApplicationEntity, modificationApplicationEntity2);
        when(modificationApplicationRepository.findAllWithModificationAndGroup()).thenReturn(allModifications);

        supervisionService.reindexAll();

        verify(modificationApplicationInfosRepository, times(1)).deleteAll();
        verify(modificationApplicationRepository, times(1)).findAllWithModificationAndGroup();
        verify(modificationApplicationInfosRepository, times(1)).saveAll(modificationListCaptor.capture());
        assertThat(modificationListCaptor.getValue()).usingRecursiveComparison().isEqualTo(allModifications.stream().map(ModificationApplicationEntity::toModificationApplicationInfos).toList());
    }

    @Test
    void testRecreateIndexThrowsExceptionWhenDeleteFails() {
        when(elasticsearchOperations.indexOps(ModificationApplicationInfos.class)).thenReturn(indexOperations);
        when(indexOperations.delete()).thenReturn(false);

        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
            supervisionService.recreateIndex();
        });

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exception.getStatusCode());
        assertEquals("Failed to delete modifications ElasticSearch index", exception.getReason());
        verify(elasticsearchOperations, times(1)).indexOps(ModificationApplicationInfos.class);
        verify(indexOperations, times(1)).delete();
        verify(indexOperations, never()).createWithMapping();
    }

    @Test
    void testRecreateIndexThrowsExceptionWhenCreateFails() {
        when(elasticsearchOperations.indexOps(ModificationApplicationInfos.class)).thenReturn(indexOperations);
        when(indexOperations.delete()).thenReturn(true);
        when(indexOperations.createWithMapping()).thenReturn(false);

        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
            supervisionService.recreateIndex();
        });

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exception.getStatusCode());
        assertEquals("Failed to create modifications ElasticSearch index", exception.getReason());
        verify(elasticsearchOperations, times(1)).indexOps(ModificationApplicationInfos.class);
        verify(indexOperations, times(1)).delete();
        verify(indexOperations, times(1)).createWithMapping();
    }

    @Test
    void recreateIndexSuccess() {
        when(elasticsearchOperations.indexOps(ModificationApplicationInfos.class)).thenReturn(indexOperations);
        when(indexOperations.delete()).thenReturn(true);
        when(indexOperations.createWithMapping()).thenReturn(true);

        supervisionService.recreateIndex();

        verify(elasticsearchOperations, times(1)).indexOps(ModificationApplicationInfos.class);
        verify(indexOperations, times(1)).delete();
        verify(indexOperations, times(1)).createWithMapping();
    }

    @AfterEach
    public void verifyNoMoreInteractionsMocks() {
        verifyNoMoreInteractions(modificationApplicationInfosRepository);
        verifyNoMoreInteractions(modificationApplicationRepository);
        verifyNoMoreInteractions(elasticsearchOperations);
        verifyNoMoreInteractions(indexOperations);
    }
}
