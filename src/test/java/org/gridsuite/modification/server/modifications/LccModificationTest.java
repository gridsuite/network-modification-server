package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

public class LccModificationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createWithLcc(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LccModificationInfos.builder()
            .stashed(false)
            .equipmentId("hvdcLine")
            .equipmentName(new AttributeModification<>("lcc1Name", OperationType.SET))
            .nominalV(new AttributeModification<>(39., OperationType.SET))
            .r(new AttributeModification<>(4., OperationType.SET))
            .maxP(new AttributeModification<>(56., OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(5., OperationType.SET))
            .convertersMode(new AttributeModification<>(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, OperationType.SET))
            .converterStation1(buildLccConverterStation("v1lcc", "lccStationName1", "v1", "1.1"))
            .converterStation2(buildLccConverterStation("v2lcc", "lccStationName2", "v2", "1.1"))
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LccModificationInfos.builder()
            .stashed(false)
            .equipmentId("lcc1Edited")
            .equipmentName(new AttributeModification<>("lcc1NameEdited", OperationType.SET))
            .nominalV(new AttributeModification<>(53., OperationType.SET))
            .r(new AttributeModification<>(2., OperationType.SET))
            .maxP(new AttributeModification<>(77., OperationType.SET))
            .convertersMode(new AttributeModification<>(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER, OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(7., OperationType.SET))
            .converterStation1(buildLccConverterStation("v1lcc", "lccStationName1", "v1", "1.1"))
            .converterStation2(buildLccConverterStation("v2lcc", "lccStationName2", "v2", "1.1"))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getHvdcLine("hvdcLine"));

        assertEquals(1, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("v1lcc")).count());

        HvdcLine hvdcLine = getNetwork().getHvdcLine("hvdcLine");
        assertNotNull(hvdcLine);
        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());
        assertEquals(39, hvdcLine.getNominalV(), 0);
        assertEquals(4, hvdcLine.getR(), 0);
        LccConverterStation lccConverterStation1 = (LccConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(lccConverterStation1);
        assertEquals(40, lccConverterStation1.getLossFactor(), 0);
        assertEquals("v1", lccConverterStation1.getTerminal().getVoltageLevel().getId());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LCC_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("hvdcLine", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LCC_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("lcc1Edited", updatedValues.get("equipmentId"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getHvdcLine("lcc1Edited"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("lccStationId1")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v2").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("lccStationId2")).count());

    }

    private static LccConverterStationModificationInfos buildLccConverterStation(String equipmentId, String equipmentName, String voltageLevel, String busOrBusbarSectionId) {
        return LccConverterStationModificationInfos.builder()
            .equipmentId(equipmentId)
            .equipmentName(new AttributeModification<>(equipmentName, OperationType.SET))
            .lossFactor(new AttributeModification<>(40.f, OperationType.SET))
            .powerFactor(new AttributeModification<>(1.f, OperationType.SET))
            .shuntCompensatorsOnSide(List.of())
            .voltageLevelId(new AttributeModification<>(voltageLevel, OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>(busOrBusbarSectionId, OperationType.SET))
            .connectionName(new AttributeModification<>("top", OperationType.SET))
            .connectionDirection(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
            .build();
    }
}
