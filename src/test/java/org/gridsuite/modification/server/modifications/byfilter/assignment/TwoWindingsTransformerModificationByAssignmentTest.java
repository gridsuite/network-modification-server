/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.IntegerAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.TwoWindingsTransformerField;
import org.junit.Test;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.createTwoWindingsTransformer;
import static org.junit.Assert.*;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class TwoWindingsTransformerModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String TWT_ID_1 = "twt1";
    private static final String TWT_ID_2 = "twt2";
    private static final String TWT_ID_3 = "twt3";
    private static final String TWT_ID_4 = "twt4";
    private static final String TWT_ID_5 = "twt5";
    private static final String TWT_ID_6 = "twt6";

    @Test
    public void testModifyTwtWithError() throws Exception {
        // Test modifying ratio tab changer field when ratio tab changer is null
        IdentifierListFilterEquipmentAttributes identifiableAttributes1 = getIdentifiableAttributes(TWT_ID_4, 1.);
        IdentifierListFilterEquipmentAttributes identifiableAttributes2 = getIdentifiableAttributes(TWT_ID_6, 1.);
        AbstractFilter filter = getFilterEquipments(FILTER_ID_4, List.of(identifiableAttributes1, identifiableAttributes2));
        DoubleAssignmentInfos assignmentInfos = DoubleAssignmentInfos.builder()
                .editedField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name())
                .value(1.)
                .filters(List.of(filter4))
                .build();

        checkCreateWithError(List.of(assignmentInfos), List.of(filter));

        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_4).getRatioTapChanger());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_6).getRatioTapChanger());

        // Test modifying phase tab changer field when phase tab changer is null
        IdentifierListFilterEquipmentAttributes identifiableAttributes3 = getIdentifiableAttributes(TWT_ID_1, 1.);
        IdentifierListFilterEquipmentAttributes identifiableAttributes4 = getIdentifiableAttributes(TWT_ID_2, 1.);
        AbstractFilter filter2 = getFilterEquipments(FILTER_ID_1, List.of(identifiableAttributes3, identifiableAttributes4));
        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(TwoWindingsTransformerField.PHASE_TAP_POSITION.name())
                .value(1.)
                .filters(List.of(filter1))
                .build();

        checkCreateWithError(List.of(assignmentInfos2), List.of(filter2));

        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_1).getPhaseTapChanger());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_2).getPhaseTapChanger());
    }

    @Test
    public void testModifyTwtWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes1 = getIdentifiableAttributes(TWT_ID_1, 1.);
        IdentifierListFilterEquipmentAttributes identifiableAttributes2 = getIdentifiableAttributes(TWT_ID_2, 1.);
        IdentifierListFilterEquipmentAttributes identifiableAttributes3 = getIdentifiableAttributes(TWT_ID_4, 1.);
        IdentifierListFilterEquipmentAttributes identifiableAttributes4 = getIdentifiableAttributes(TWT_ID_6, 1.);
        AbstractFilter filterTwt1 = getFilterEquipments(FILTER_ID_1, List.of(identifiableAttributes1, identifiableAttributes2));
        AbstractFilter filterTwt2 = getFilterEquipments(FILTER_ID_4, List.of(identifiableAttributes3, identifiableAttributes4));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + ".{2,}"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filterTwt1, filterTwt2)))
                        .withHeader("Content-Type", "application/json"))).getId();

        IntegerAssignmentInfos assignmentInfos = IntegerAssignmentInfos.builder()
                .filters(List.of(filter1, filter4))
                .editedField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name())
                .value(4)
                .build();

        checkCreationApplicationStatus(List.of(assignmentInfos), NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);

        assertNotNull(getNetwork().getTwoWindingsTransformer(TWT_ID_1).getRatioTapChanger());
        assertNotNull(getNetwork().getTwoWindingsTransformer(TWT_ID_2).getRatioTapChanger());
        assertEquals(4, getNetwork().getTwoWindingsTransformer(TWT_ID_1).getRatioTapChanger().getTapPosition());
        assertEquals(4, getNetwork().getTwoWindingsTransformer(TWT_ID_2).getRatioTapChanger().getTapPosition());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_4).getRatioTapChanger());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_6).getRatioTapChanger());

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(List.of(FILTER_ID_1, FILTER_ID_4)), false);
    }

    @Override
    protected void createEquipments() {
        Substation s1 = getNetwork().getSubstation("s1");
        Substation s3 = getNetwork().getSubstation("s3");
        TwoWindingsTransformer twt1 = createTwoWindingsTransformer(s1, TWT_ID_1, TWT_ID_1, 30, 40, 50,
            60, 10, 20, 100, 100,
                "v1", "v2",
                "trf1", 11, ConnectablePosition.Direction.TOP,
                "trf1", 22, ConnectablePosition.Direction.BOTTOM);
        twt1.setRatedS(11);
        addRatioTapChangerSteps(twt1.newRatioTapChanger().setTargetV(50).setLowTapPosition(0).setTapPosition(1).setTargetDeadband(55));

        TwoWindingsTransformer twt2 = createTwoWindingsTransformer(s1, TWT_ID_2, TWT_ID_2, 35, 45, 55,
            65, 15, 25, 101, 100,
                "v1", "v4",
                "trf1", 33, ConnectablePosition.Direction.TOP,
                "trf1", 44, ConnectablePosition.Direction.BOTTOM);
        twt2.setRatedS(10);
        addRatioTapChangerSteps(twt2.newRatioTapChanger().setTargetV(53).setLowTapPosition(3).setTapPosition(4).setTargetDeadband(58));

        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(s1, TWT_ID_3, TWT_ID_3, 40, 50, 60,
            70, 20, 30, 101, 101,
                "v2", "v4",
                "trf1", 10, ConnectablePosition.Direction.TOP,
                "trf1", 20, ConnectablePosition.Direction.BOTTOM);
        twt3.setRatedS(25);
        addRatioTapChangerSteps(twt3.newRatioTapChanger().setTargetV(56).setLowTapPosition(0).setTapPosition(1).setTargetDeadband(61));

        TwoWindingsTransformer twt4 = createTwoWindingsTransformer(s3, TWT_ID_4, TWT_ID_4, 45, 55, 65,
            75, 25, 35, 100, 100,
                "v5", "v6",
                "trf1", 30, ConnectablePosition.Direction.TOP,
                "trf1", 40, ConnectablePosition.Direction.BOTTOM);
        twt4.setRatedS(15);
        addPhaseTapChangerSteps(twt4.newPhaseTapChanger()
                .setRegulationValue(45)
                .setLowTapPosition(1)
                .setTapPosition(2)
                .setTargetDeadband(34)
                .setRegulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL));

        TwoWindingsTransformer twt5 = createTwoWindingsTransformer(s3, TWT_ID_5, TWT_ID_5, 50, 60, 70,
            80, 30, 40, 101, 101,
                "v5", "v6",
                "trf1", 15, ConnectablePosition.Direction.TOP,
                "trf1", 26, ConnectablePosition.Direction.BOTTOM);
        twt5.setRatedS(30);
        addPhaseTapChangerSteps(twt5.newPhaseTapChanger().setRegulationValue(46).setLowTapPosition(2).setTapPosition(2).setTargetDeadband(35));

        TwoWindingsTransformer twt6 = createTwoWindingsTransformer(s3, TWT_ID_6, TWT_ID_6, 55, 65, 75, 85,
            35, 45, 102, 102,
                "v5", "v6",
                "trf1", 38, ConnectablePosition.Direction.TOP,
                "trf1", 49, ConnectablePosition.Direction.BOTTOM);
        twt6.setRatedS(20);
        addPhaseTapChangerSteps(twt6.newPhaseTapChanger()
                .setRegulationMode(PhaseTapChanger.RegulationMode.FIXED_TAP)
                .setRegulationValue(47)
                .setTargetDeadband(36)
                .setLowTapPosition(1)
                .setTapPosition(1)
        );
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.TWO_WINDINGS_TRANSFORMER)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(TWT_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(TWT_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.TWO_WINDINGS_TRANSFORMER)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(TWT_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(TWT_ID_3, 2.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.TWO_WINDINGS_TRANSFORMER)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(TWT_ID_4, 5.0),
                new IdentifierListFilterEquipmentAttributes(TWT_ID_5, 6.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.TWO_WINDINGS_TRANSFORMER)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(TWT_ID_4, 5.0),
                new IdentifierListFilterEquipmentAttributes(TWT_ID_6, 7.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1))
                .editedField(TwoWindingsTransformerField.TARGET_V.name())
                .value(2.)
                .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name())
                .value(4)
                .build();

        IntegerAssignmentInfos assignmentInfos3 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(TwoWindingsTransformerField.RATIO_LOW_TAP_POSITION.name())
                .value(4)
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1))
                .editedField(TwoWindingsTransformerField.RATIO_TARGET_DEADBAND.name())
                .value(5.)
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(TwoWindingsTransformerField.REGULATION_VALUE.name())
                .value(2.)
                .build();

        IntegerAssignmentInfos assignmentInfos6 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(TwoWindingsTransformerField.PHASE_TAP_POSITION.name())
                .value(2)
                .build();

        IntegerAssignmentInfos assignmentInfos7 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(TwoWindingsTransformerField.PHASE_LOW_TAP_POSITION.name())
                .value(2)
                .build();

        DoubleAssignmentInfos assignmentInfos8 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(TwoWindingsTransformerField.PHASE_TARGET_DEADBAND.name())
                .value(10.)
                .build();

        DoubleAssignmentInfos assignmentInfos9 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter4))
                .editedField(TwoWindingsTransformerField.X.name())
                .value(20.)
                .build();

        DoubleAssignmentInfos assignmentInfos10 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter2, filter3))
                .editedField(TwoWindingsTransformerField.R.name())
                .value(2.)
                .build();

        DoubleAssignmentInfos assignmentInfos11 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4, filter2))
                .editedField(TwoWindingsTransformerField.G.name())
                .value(25.)
                .build();

        DoubleAssignmentInfos assignmentInfos12 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter3))
                .editedField(TwoWindingsTransformerField.B.name())
                .value(2.5)
                .build();

        DoubleAssignmentInfos assignmentInfos13 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(TwoWindingsTransformerField.RATED_U1.name())
                .value(15.)
                .build();

        DoubleAssignmentInfos assignmentInfos14 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter3, filter2))
                .editedField(TwoWindingsTransformerField.RATED_U2.name())
                .value(0.5)
                .build();

        DoubleAssignmentInfos assignmentInfos15 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter2))
                .editedField(TwoWindingsTransformerField.RATED_S.name())
                .value(2.)
                .build();
        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1,
                assignmentInfos2,
                assignmentInfos3,
                assignmentInfos4,
                assignmentInfos5,
                assignmentInfos6,
                assignmentInfos7,
                assignmentInfos8,
                assignmentInfos9,
                assignmentInfos10,
                assignmentInfos11,
                assignmentInfos12,
                assignmentInfos13,
                assignmentInfos14,
                assignmentInfos15));

        return infosList;
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(TwoWindingsTransformerField.TARGET_V.name())
                .value(2.)
                .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name())
                .value(3)
                .build();

        IntegerAssignmentInfos assignmentInfos3 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter1))
                .editedField(TwoWindingsTransformerField.RATIO_LOW_TAP_POSITION.name())
                .value(3)
                .build();

        return List.of(assignmentInfos1,
                assignmentInfos2,
                assignmentInfos3);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.TWO_WINDINGS_TRANSFORMER;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.TWO_WINDINGS_TRANSFORMER;
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TwoWindingsTransformer twt1 = getNetwork().getTwoWindingsTransformer(TWT_ID_1);
        RatioTapChanger ratioTapChanger1 = twt1.getRatioTapChanger();
        assertNotNull(ratioTapChanger1);
        assertEquals(2, ratioTapChanger1.getTargetV(), 0);
        assertEquals(4, ratioTapChanger1.getLowTapPosition());
        assertEquals(4, ratioTapChanger1.getTapPosition());
        assertEquals(5, ratioTapChanger1.getTargetDeadband(), 0);
        assertEquals(20, twt1.getX(), 0);
        assertEquals(2.5, twt1.getB(), 0);
        assertEquals(2, twt1.getR(), 0);
        assertEquals(25, twt1.getG(), 0);
        assertEquals(15, twt1.getRatedU1(), 0);
        assertEquals(0.5, twt1.getRatedU2(), 0);
        assertEquals(2, twt1.getRatedS(), 0);

        TwoWindingsTransformer twt2 = getNetwork().getTwoWindingsTransformer(TWT_ID_2);
        RatioTapChanger ratioTapChanger2 = twt2.getRatioTapChanger();
        assertNotNull(ratioTapChanger2);
        assertEquals(2, ratioTapChanger2.getTargetV(), 0);
        assertEquals(3, ratioTapChanger2.getLowTapPosition());
        assertEquals(4, ratioTapChanger2.getTapPosition());
        assertEquals(5, ratioTapChanger2.getTargetDeadband(), 0);
        assertEquals(20, twt2.getX(), 0);
        assertEquals(2.5, twt2.getB(), 0);
        assertEquals(15, twt2.getRatedU1(), 0);
        assertEquals(2, twt2.getRatedS(), 0);

        TwoWindingsTransformer twt3 = getNetwork().getTwoWindingsTransformer(TWT_ID_3);
        RatioTapChanger ratioTapChanger3 = twt3.getRatioTapChanger();
        assertNotNull(ratioTapChanger3);
        assertEquals(4, ratioTapChanger3.getLowTapPosition());
        assertEquals(4, ratioTapChanger3.getTapPosition());
        assertEquals(2, twt3.getR(), 0);
        assertEquals(25, twt3.getG(), 0);
        assertEquals(15, twt3.getRatedU1(), 0);
        assertEquals(0.5, twt3.getRatedU2(), 0);
        assertEquals(2, twt3.getRatedS(), 0);

        TwoWindingsTransformer twt4 = getNetwork().getTwoWindingsTransformer(TWT_ID_4);
        PhaseTapChanger phaseTapChanger4 = twt4.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(2, phaseTapChanger4.getRegulationValue(), 0);
        assertEquals(2, phaseTapChanger4.getLowTapPosition());
        assertEquals(2, phaseTapChanger4.getTapPosition());
        assertEquals(10, phaseTapChanger4.getTargetDeadband(), 0);
        assertEquals(2, twt4.getR(), 0);
        assertEquals(20, twt4.getX(), 0);
        assertEquals(25, twt4.getG(), 0);
        assertEquals(2.5, twt4.getB(), 0);
        assertEquals(25, twt4.getRatedU1(), 0);
        assertEquals(0.5, twt4.getRatedU2(), 0);
        assertEquals(15, twt4.getRatedS(), 0);

        TwoWindingsTransformer twt5 = getNetwork().getTwoWindingsTransformer(TWT_ID_5);
        PhaseTapChanger phaseTapChanger5 = twt5.getPhaseTapChanger();
        assertNotNull(phaseTapChanger5);
        assertEquals(2, phaseTapChanger5.getLowTapPosition());
        assertEquals(2, phaseTapChanger5.getTapPosition());
        assertEquals(2, twt5.getR(), 0);
        assertEquals(2.5, twt5.getB(), 0);
        assertEquals(0.5, twt5.getRatedU2(), 0);

        TwoWindingsTransformer twt6 = getNetwork().getTwoWindingsTransformer(TWT_ID_6);
        PhaseTapChanger phaseTapChanger6 = twt6.getPhaseTapChanger();
        assertNotNull(phaseTapChanger6);
        assertEquals(47, phaseTapChanger6.getRegulationValue(), 0);
        assertEquals(36, phaseTapChanger6.getTargetDeadband(), 0);
        assertEquals(20, twt6.getX(), 0);
        assertEquals(25, twt6.getG(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        TwoWindingsTransformer twt1 = getNetwork().getTwoWindingsTransformer(TWT_ID_1);
        RatioTapChanger ratioTapChanger1 = twt1.getRatioTapChanger();
        assertNotNull(ratioTapChanger1);
        assertEquals(50, ratioTapChanger1.getTargetV(), 0);
        assertEquals(0, ratioTapChanger1.getLowTapPosition());
        assertEquals(1, ratioTapChanger1.getTapPosition());
        assertEquals(55, ratioTapChanger1.getTargetDeadband(), 0);
        assertEquals(40, twt1.getX(), 0);
        assertEquals(60, twt1.getB(), 0);
        assertEquals(30, twt1.getR(), 0);
        assertEquals(50, twt1.getG(), 0);
        assertEquals(10, twt1.getRatedU1(), 0);
        assertEquals(20, twt1.getRatedU2(), 0);
        assertEquals(11, twt1.getRatedS(), 0);

        TwoWindingsTransformer twt2 = getNetwork().getTwoWindingsTransformer(TWT_ID_2);
        RatioTapChanger ratioTapChanger2 = twt2.getRatioTapChanger();
        assertNotNull(ratioTapChanger2);
        assertEquals(53, ratioTapChanger2.getTargetV(), 0);
        assertEquals(3, ratioTapChanger2.getLowTapPosition());
        assertEquals(4, ratioTapChanger2.getTapPosition());
        assertEquals(58, ratioTapChanger2.getTargetDeadband(), 0);
        assertEquals(45, twt2.getX(), 0);
        assertEquals(65, twt2.getB(), 0);
        assertEquals(15, twt2.getRatedU1(), 0);
        assertEquals(10, twt2.getRatedS(), 0);

        TwoWindingsTransformer twt3 = getNetwork().getTwoWindingsTransformer(TWT_ID_3);
        RatioTapChanger ratioTapChanger3 = twt3.getRatioTapChanger();
        assertNotNull(ratioTapChanger3);
        assertEquals(0, ratioTapChanger3.getLowTapPosition());
        assertEquals(1, ratioTapChanger3.getTapPosition());
        assertEquals(40, twt3.getR(), 0);
        assertEquals(60, twt3.getG(), 0);
        assertEquals(20, twt3.getRatedU1(), 0);
        assertEquals(30, twt3.getRatedU2(), 0);
        assertEquals(25, twt3.getRatedS(), 0);

        TwoWindingsTransformer twt4 = getNetwork().getTwoWindingsTransformer(TWT_ID_4);
        PhaseTapChanger phaseTapChanger4 = twt4.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(45, phaseTapChanger4.getRegulationValue(), 0);
        assertEquals(1, phaseTapChanger4.getLowTapPosition());
        assertEquals(2, phaseTapChanger4.getTapPosition());
        assertEquals(34, phaseTapChanger4.getTargetDeadband(), 0);
        assertEquals(45, twt4.getR(), 0);
        assertEquals(55, twt4.getX(), 0);
        assertEquals(65, twt4.getG(), 0);
        assertEquals(75, twt4.getB(), 0);
        assertEquals(25, twt4.getRatedU1(), 0);
        assertEquals(35, twt4.getRatedU2(), 0);
        assertEquals(15, twt4.getRatedS(), 0);

        TwoWindingsTransformer twt5 = getNetwork().getTwoWindingsTransformer(TWT_ID_5);
        PhaseTapChanger phaseTapChanger5 = twt5.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(2, phaseTapChanger5.getLowTapPosition());
        assertEquals(2, phaseTapChanger5.getTapPosition());
        assertEquals(50, twt5.getR(), 0);
        assertEquals(80, twt5.getB(), 0);
        assertEquals(40, twt5.getRatedU2(), 0);

        TwoWindingsTransformer twt6 = getNetwork().getTwoWindingsTransformer(TWT_ID_6);
        PhaseTapChanger phaseTapChanger6 = twt6.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(47, phaseTapChanger6.getRegulationValue(), 0);
        assertEquals(36, phaseTapChanger6.getTargetDeadband(), 0);
        assertEquals(65, twt6.getX(), 0);
        assertEquals(75, twt6.getG(), 0);
    }

    private void addRatioTapChangerSteps(RatioTapChangerAdder ratioTapChangerAdder) {
        ratioTapChangerAdder.beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(5.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .add();
    }

    private void addPhaseTapChangerSteps(PhaseTapChangerAdder phaseTapChangerAdder) {
        phaseTapChangerAdder.beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.1)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.1)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.1)
                .endStep()
                .add();
    }
}
