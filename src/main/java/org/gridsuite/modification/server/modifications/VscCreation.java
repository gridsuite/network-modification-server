package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.VscConverterStation;
import com.powsybl.iidm.network.VscConverterStationAdder;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.VscCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.HVDC_LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.modifications.ModificationUtils.createReactiveLimits;

public class VscCreation extends AbstractModification {
    private final VscCreationInfos modificationInfos;

    public VscCreation(VscCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }

        checkConverterStation(network, modificationInfos.getConverterStation1());
        checkConverterStation(network, modificationInfos.getConverterStation2());
    }

    private void checkConverterStation(Network network,
                                       ConverterStationCreationInfos converterStation) {
        if (converterStation != null) {
            // check connectivity
            ModificationUtils.getInstance().controlConnectivity(network,
                    converterStation.getVoltageLevelId(),
                    converterStation.getBusOrBusbarSectionId(),
                    converterStation.getConnectionPosition());

            // check reactive limits
            ModificationUtils.getInstance().checkReactiveLimitsCreation(converterStation, "Vsc");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        VscConverterStation converterStation1 = modificationInfos.getConverterStation1() == null ?
                null :
                createConvertStation(network, modificationInfos.getConverterStation1(), subReporter);

        VscConverterStation converterStation2 = modificationInfos.getConverterStation1() == null ?
                null :
                createConvertStation(network, modificationInfos.getConverterStation2(), subReporter);

        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(modificationInfos.getEquipmentId())
                .setName(modificationInfos.getEquipmentName())
                .setNominalV(modificationInfos.getDcNominalVoltage())
                .setR(modificationInfos.getDcResistance())
                .setMaxP(modificationInfos.getMaximumActivePower())
                .setActivePowerSetpoint(modificationInfos.getActivePower())
                .setConvertersMode(modificationInfos.getConvertersMode())
                .setConverterStationId1(converterStation1 != null ? converterStation1.getId() : null)
                .setConverterStationId2(converterStation2 != null ? converterStation2.getId() : null)
                .add();

        if (modificationInfos.getOperatorActivePowerLimitSide1() != null ||
            modificationInfos.getOperatorActivePowerLimitSide2() != null) {
            hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                    .withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitSide1())
                    .withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitSide2())
                    .add();
        }

        if (modificationInfos.getDroop() != null ||
            modificationInfos.getP0() != null) {
            var activePowerControlExtension = hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class)
                    .withEnabled(modificationInfos.getAngleDroopActivePowerControl());
            if (modificationInfos.getP0() != null) {
                activePowerControlExtension.withP0(modificationInfos.getP0());
            }

            if (modificationInfos.getDroop() != null) {
                activePowerControlExtension.withDroop(modificationInfos.getDroop());
            }

            activePowerControlExtension.add();
        }
        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReporter, modificationInfos.getEquipmentName(), "Name");
        }
    }

    private VscConverterStation createConvertStation(Network network,
                                                     ConverterStationCreationInfos converterStationCreationInfos,
                                                     Reporter subReporter) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, converterStationCreationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            return createConvertStationInNodeBreaker(network, voltageLevel, converterStationCreationInfos, subReporter);
        } else {
            return createConvertStationInBusBreaker(network, voltageLevel, converterStationCreationInfos, subReporter);
        }
    }

    private VscConverterStation createConvertStationInNodeBreaker(Network network,
                                                                  VoltageLevel voltageLevel,
                                                                  ConverterStationCreationInfos converterStationCreationInfos,
                                                                  Reporter subReporter) {
        VscConverterStationAdder converterStationAdder = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationInfos.getEquipmentId())
                .setName(converterStationCreationInfos.getEquipmentName())
                .setVoltageRegulatorOn(converterStationCreationInfos.getVoltageRegulationOn())
                .setReactivePowerSetpoint(converterStationCreationInfos.getReactivePower())
                .setVoltageSetpoint(converterStationCreationInfos.getVoltage())
                .setLossFactor(converterStationCreationInfos.getLossFactor());

        int position = ModificationUtils.getInstance().getPosition(converterStationCreationInfos.getConnectionPosition(),
                converterStationCreationInfos.getBusOrBusbarSectionId(),
                network,
                voltageLevel);

        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(converterStationCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(converterStationCreationInfos.getConnectionDirection())
                .withInjectionFeederName(converterStationCreationInfos.getConnectionName() != null
                        ? converterStationCreationInfos.getConnectionName()
                        : converterStationCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(converterStationAdder)
                .build();

        algo.apply(network, true, subReporter);
        VscConverterStation vscConverterStation = ModificationUtils.getInstance()
                .getVscConverterStation(network, converterStationCreationInfos.getEquipmentId());

        addExtensions(vscConverterStation,
                voltageLevel,
                converterStationCreationInfos,
                subReporter);

        return vscConverterStation;

    }

    private VscConverterStation createConvertStationInBusBreaker(Network network,
                                                                 VoltageLevel voltageLevel,
                                                                 ConverterStationCreationInfos converterStationCreationInfos, Reporter subReporter) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, converterStationCreationInfos.getBusOrBusbarSectionId());
        VscConverterStation vscConverterStation = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationInfos.getEquipmentId())
                .setName(converterStationCreationInfos.getEquipmentName())
                .setVoltageRegulatorOn(converterStationCreationInfos.getVoltageRegulationOn())
                .setReactivePowerSetpoint(converterStationCreationInfos.getReactivePower())
                .setBus(bus.getId())
                .setLossFactor(converterStationCreationInfos.getLossFactor())
                .setVoltageSetpoint(converterStationCreationInfos.getVoltage())
                .add();

        addExtensions(vscConverterStation, voltageLevel, converterStationCreationInfos, subReporter);
        return vscConverterStation;
    }

    private void addExtensions(VscConverterStation vscConverterStation,
                               VoltageLevel voltageLevel,
                               ConverterStationCreationInfos converterStationCreationInfos,
                               Reporter subReporter) {
        createReactiveLimits(converterStationCreationInfos, vscConverterStation, subReporter);
    }
}
