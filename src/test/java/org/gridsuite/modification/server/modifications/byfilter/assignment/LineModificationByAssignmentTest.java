package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.IntegerAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.StringAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.LineField;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createLine;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class LineModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String LINE_ID_1 = "line_1";
    private static final String LINE_ID_2 = "line_2";
    private static final String LINE_ID_3 = "line_3";
    private static final String LINE_ID_4 = "line_4";
    private static final String LINE_ID_5 = "line_5";
    private static final String LINE_ID_6 = "line_6";

    @Override
    protected void createEquipments() {
        createLine(getNetwork(), LINE_ID_1, LINE_ID_1, "v1", "v2", 21, 21, 2,
            1, 3, 4, 0.001, 0.0015,
            "line_1", 11, ConnectablePosition.Direction.TOP,
            "line_1", 22, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_2, LINE_ID_2, "v1", "v2", 33, 44, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_2", 33, ConnectablePosition.Direction.TOP,
            "line_2", 44, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_3, LINE_ID_3, "v2", "v4", 33, 44, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_3", 10, ConnectablePosition.Direction.TOP,
            "line_3", 20, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_4, LINE_ID_4, "v2", "v4", 35, 45, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_4", 11, ConnectablePosition.Direction.TOP,
            "line_4", 21, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_5, LINE_ID_5, "v2", "v4", 45, 55, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_5", 12, ConnectablePosition.Direction.TOP,
            "line_5", 22, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_6, LINE_ID_6, "v2", "v4", 55, 65, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_6", 13, ConnectablePosition.Direction.TOP,
            "line_6", 23, ConnectablePosition.Direction.BOTTOM);
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1)
            .modificationDate(new Date()).equipmentType(EquipmentType.LINE)
            .filterEquipmentsAttributes(List.of(
                new IdentifierListFilterEquipmentAttributes(LINE_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(LINE_ID_2, 2.0)
            )).build();

        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2)
            .modificationDate(new Date()).equipmentType(EquipmentType.LINE)
            .filterEquipmentsAttributes(List.of(
                new IdentifierListFilterEquipmentAttributes(LINE_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(LINE_ID_3, 2.0)
            )).build();

        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3)
            .modificationDate(new Date()).equipmentType(EquipmentType.LINE)
            .filterEquipmentsAttributes(List.of(
                new IdentifierListFilterEquipmentAttributes(LINE_ID_4, 5.0),
                new IdentifierListFilterEquipmentAttributes(LINE_ID_5, 6.0)
            )).build();

        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4)
            .modificationDate(new Date()).equipmentType(EquipmentType.LINE)
            .filterEquipmentsAttributes(List.of(
                new IdentifierListFilterEquipmentAttributes(LINE_ID_4, 5.0),
                new IdentifierListFilterEquipmentAttributes(LINE_ID_6, 7.0)
            )).build();

        return List.of(filter1, filter2, filter3, filter4);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
            .filters(List.of(filter1))
            .editedField(LineField.X.name())
            .value(20.)
            .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
            .filters(List.of(filter2))
            .editedField(LineField.R.name())
            .value(40)
            .build();

        IntegerAssignmentInfos assignmentInfos3 = IntegerAssignmentInfos.builder()
            .filters(List.of(filter2))
            .editedField(LineField.G1.name())
            .value(35)
            .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
            .filters(List.of(filter1))
            .editedField(LineField.G2.name())
            .value(10.)
            .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
            .filters(List.of(filter4))
            .editedField(LineField.B1.name())
            .value(21.)
            .build();

        IntegerAssignmentInfos assignmentInfos6 = IntegerAssignmentInfos.builder()
            .filters(List.of(filter3))
            .editedField(LineField.B2.name())
            .value(90)
            .build();

        StringAssignmentInfos assignmentInfos7 = StringAssignmentInfos.builder()
            .filters(List.of(filter1))
            .editedField(LineField.OPERATIONAL_LIMITS_GROUP_1.name())
            .value("group1")
            .build();

        StringAssignmentInfos assignmentInfos8 = StringAssignmentInfos.builder()
            .filters(List.of(filter2))
            .editedField(LineField.OPERATIONAL_LIMITS_GROUP_2.name())
            .value("group2")
            .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1,
            assignmentInfos2,
            assignmentInfos3,
            assignmentInfos4,
            assignmentInfos5,
            assignmentInfos6,
            assignmentInfos7,
            assignmentInfos8));

        return infosList;
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Line line1 = getNetwork().getLine(LINE_ID_1);
        assertEquals(40, line1.getR(), 0);
        assertEquals(20, line1.getX(), 0);
        assertEquals(0.001, line1.getB1(), 0);
        assertEquals(0.0015, line1.getB2(), 0);
        assertEquals(35, line1.getG1(), 0);
        assertEquals(10, line1.getG2(), 0);
        assertTrue(line1.getSelectedOperationalLimitsGroupId1().isPresent());
        assertEquals("group1", line1.getSelectedOperationalLimitsGroupId1().get());
        assertTrue(line1.getSelectedOperationalLimitsGroupId2().isPresent());
        assertEquals("group2", line1.getSelectedOperationalLimitsGroupId2().get());

        Line line2 = getNetwork().getLine(LINE_ID_2);
        assertEquals(3, line2.getR(), 0);
        assertEquals(20, line2.getX(), 0);
        assertEquals(0.002, line2.getB1(), 0);
        assertEquals(0.0025, line2.getB2(), 0);
        assertEquals(5, line2.getG1(), 0);
        assertEquals(10, line2.getG2(), 0);
        assertTrue(line2.getSelectedOperationalLimitsGroupId1().isPresent());
        assertEquals("group1", line2.getSelectedOperationalLimitsGroupId1().get());
        assertTrue(line2.getSelectedOperationalLimitsGroupId2().isPresent());
        assertEquals("group0", line2.getSelectedOperationalLimitsGroupId2().get());

        Line line3 = getNetwork().getLine(LINE_ID_3);
        assertEquals(40, line3.getR(), 0);
        assertEquals(3, line3.getX(), 0);
        assertEquals(0.002, line3.getB1(), 0);
        assertEquals(0.0025, line3.getB2(), 0);
        assertEquals(35, line3.getG1(), 0);
        assertEquals(1, line3.getG2(), 0);
        assertTrue(line3.getSelectedOperationalLimitsGroupId1().isPresent());
        assertEquals("group0", line3.getSelectedOperationalLimitsGroupId1().get());
        assertTrue(line3.getSelectedOperationalLimitsGroupId2().isPresent());
        assertEquals("group2", line3.getSelectedOperationalLimitsGroupId2().get());

        Line line4 = getNetwork().getLine(LINE_ID_4);
        assertEquals(3, line4.getR(), 0);
        assertEquals(3, line4.getX(), 0);
        assertEquals(21, line4.getB1(), 0);
        assertEquals(90, line4.getB2(), 0);
        assertEquals(5, line4.getG1(), 0);
        assertEquals(1, line4.getG2(), 0);

        Line line5 = getNetwork().getLine(LINE_ID_5);
        assertEquals(3, line5.getR(), 0);
        assertEquals(3, line5.getX(), 0);
        assertEquals(0.002, line5.getB1(), 0);
        assertEquals(90, line5.getB2(), 0);
        assertEquals(5, line5.getG1(), 0);
        assertEquals(1, line5.getG2(), 0);

        Line line6 = getNetwork().getLine(LINE_ID_6);
        assertEquals(3, line6.getR(), 0);
        assertEquals(3, line6.getX(), 0);
        assertEquals(21, line6.getB1(), 0);
        assertEquals(0.0025, line6.getB2(), 0);
        assertEquals(5, line6.getG1(), 0);
        assertEquals(1, line6.getG2(), 0);
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
            .editedField(LineField.B1.name())
            .value(0.1)
            .filters(List.of(filter1))
            .build();

        return List.of(assignmentInfos1);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LINE;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LINE;
    }
}
