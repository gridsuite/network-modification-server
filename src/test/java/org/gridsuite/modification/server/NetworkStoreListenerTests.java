package org.gridsuite.modification.server;

import com.google.common.collect.Iterables;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {"spring.data.elasticsearch.enabled=true"})
public class NetworkStoreListenerTests {

    private static final UUID NETWORK_UUID = UUID.fromString("38400000-8cf0-11bd-b23e-10b96e4ef00d");

    private static final UUID GROUP_UUID = UUID.fromString("99990000-8cf0-11bd-b23e-10b96e4ef00d");

    private static final String VARIANT_TWO_ID = "VARIANT_TWO_ID";

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Autowired
    private EquipmentInfosRepository equipmentInfosRepository;

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Before
    public void setup() {
        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of(VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    @Test
    public void testAddDeleteEquipmentInfosInVariants() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);

        NetworkStoreListener listenerInitialVariant = NetworkStoreListener.create(network, NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID, GROUP_UUID, modificationRepository, equipmentInfosService, false, true);
        NetworkStoreListener listenerDeltaVariantOne = NetworkStoreListener.create(network, NETWORK_UUID, NetworkCreation.VARIANT_ID, GROUP_UUID, modificationRepository, equipmentInfosService, false, true);
        NetworkStoreListener listenerDeltaVariantTwo = NetworkStoreListener.create(network, NETWORK_UUID, VARIANT_TWO_ID, GROUP_UUID, modificationRepository, equipmentInfosService, false, true);

        // Get equipments in network initial variant
        Collection<Identifiable<?>> equipmentsInInitialVariant = network.getIdentifiables();
        assertEquals(41, equipmentsInInitialVariant.size());
        // Call onCreation on network store listener for each equipment in initial variant
        equipmentsInInitialVariant.stream().forEach(i -> listenerInitialVariant.onCreation(i));

        // Check equipment infos of initial variant are in ES
        Iterable<EquipmentInfos> equipments = equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID);
        assertEquals(41, Iterables.size(equipments));

        network.getVariantManager().setWorkingVariant(NetworkCreation.VARIANT_ID);
        Collection<Identifiable<?>> equipmentsInVariant = network.getIdentifiables();
        assertEquals(58, equipmentsInVariant.size());

        // Call onCreation on network store listener for each equipment only in VARIANT_ID variant
        List<Identifiable<?>> equipmentsInVariantOnly = equipmentsInVariant.stream().filter(eq -> !equipmentsInInitialVariant.contains(eq)).collect(Collectors.toList());
        equipmentsInVariantOnly.stream().forEach(i -> listenerDeltaVariantOne.onCreation(i));

        // Check equipments infos of VARIANT_ID variant are in ES
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NetworkCreation.VARIANT_ID);
        assertEquals(17, Iterables.size(equipments));

        // Clone variant
        equipmentInfosService.cloneVariantModifications(NETWORK_UUID, NetworkCreation.VARIANT_ID, VARIANT_TWO_ID);

        // Check equipments infos of VARIANT_TWO_ID variant are in ES
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_TWO_ID);
        assertEquals(17, Iterables.size(equipments));

        // Simulate removal of load1Variant in VARIANT_TWO_ID
        listenerDeltaVariantTwo.deleteEquipmentInfos("load1Variant");
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_TWO_ID);
        assertEquals(16, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NetworkCreation.VARIANT_ID);
        assertEquals(17, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID);
        assertEquals(41, Iterables.size(equipments));

        // Simulate readdition of load1Variant in VARIANT_TWO_ID
        listenerDeltaVariantTwo.onCreation(network.getIdentifiable("load1Variant"));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_TWO_ID);
        assertEquals(17, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NetworkCreation.VARIANT_ID);
        assertEquals(17, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID);
        assertEquals(41, Iterables.size(equipments));

        // Simulate removal of idGenerator in VARIANT_TWO_ID
        // idGenerator is present in initial variant, but not in any delta variant
        listenerDeltaVariantTwo.deleteEquipmentInfos("idGenerator");
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_TWO_ID);
        assertEquals(18, Iterables.size(equipments));
        // Equipment still exist in ES as tombstoned equipment
        equipments = equipmentInfosRepository.findByIdAndNetworkUuidAndVariantIdAndTombstoned("idGenerator", NETWORK_UUID, VARIANT_TWO_ID, Boolean.TRUE);
        assertEquals(1, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NetworkCreation.VARIANT_ID);
        assertEquals(17, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID);
        assertEquals(41, Iterables.size(equipments));

        // Simulate readdition of idGenerator in VARIANT_TWO_ID
        listenerDeltaVariantTwo.onCreation(network.getIdentifiable("idGenerator"));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_TWO_ID);
        assertEquals(19, Iterables.size(equipments));
        // Equipment still exists in ES as tombstoned equipment
        equipments = equipmentInfosRepository.findByIdAndNetworkUuidAndVariantIdAndTombstoned("idGenerator", NETWORK_UUID, VARIANT_TWO_ID, Boolean.TRUE);
        assertEquals(1, Iterables.size(equipments));
        // Equipment now exists in ES as normal (i.e. not tombstoned) equipment
        equipments = equipmentInfosRepository.findByIdAndNetworkUuidAndVariantIdAndTombstoned("idGenerator", NETWORK_UUID, VARIANT_TWO_ID, null);
        assertEquals(1, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NetworkCreation.VARIANT_ID);
        assertEquals(17, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID);
        assertEquals(41, Iterables.size(equipments));

        // Simulate reremoval of idGenerator in VARIANT_TWO_ID
        // idGenerator is present in initial variant, but not in any delta variant
        listenerDeltaVariantTwo.deleteEquipmentInfos("idGenerator");
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_TWO_ID);
        assertEquals(18, Iterables.size(equipments));
        // Equipment still exist in ES as tombstoned equipment
        equipments = equipmentInfosRepository.findByIdAndNetworkUuidAndVariantIdAndTombstoned("idGenerator", NETWORK_UUID, VARIANT_TWO_ID, Boolean.TRUE);
        assertEquals(1, Iterables.size(equipments));
        // Equipment no more exists in ES as normal (i.e. not tombstoned) equipment
        equipments = equipmentInfosRepository.findByIdAndNetworkUuidAndVariantIdAndTombstoned("idGenerator", NETWORK_UUID, VARIANT_TWO_ID, null);
        assertEquals(0, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NetworkCreation.VARIANT_ID);
        assertEquals(17, Iterables.size(equipments));
        equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VariantManagerConstants.INITIAL_VARIANT_ID);
        assertEquals(41, Iterables.size(equipments));
    }
}
