/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.loadflow.LoadFlowParameters;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.BalancesAdjustmentModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.utils.CountriesUtils;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.dto.BalancesAdjustmentModificationInfos.*;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "balances_adjustment")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "balancesAdjustment_id_fk_constraint"))
public class BalancesAdjustmentEntity extends ModificationEntity {

    @Column(name = "max_number_iterations")
    private int maxNumberIterations = DEFAULT_MAX_NUMBER_ITERATIONS;

    @Column(name = "threshold_net_position")
    private double thresholdNetPosition = DEFAULT_THRESHOLD_NET_POSITION;

    @Column(name = "countries_to_balance")
    private String countriesToBalance = CountriesUtils.stringify(DEFAULT_COUNTRIES_TO_BALANCE);

    @Column(name = "balance_type")
    @Enumerated(EnumType.STRING)
    private LoadFlowParameters.BalanceType balanceType = DEFAULT_BALANCE_TYPE;

    @Column(name = "with_load_flow")
    private boolean withLoadFlow = DEFAULT_WITH_LOAD_FLOW;

    @Column(name = "load_flow_parameters_id")
    private UUID loadFlowParametersId;

    @Column(name = "with_ratio_tap_changers")
    private boolean withRatioTapChangers = DEFAULT_WITH_RATIO_TAP_CHANGERS;

    @Column(name = "subtract_load_flow_balancing")
    private boolean subtractLoadFlowBalancing = DEFAULT_SUBTRACT_LOAD_FLOW_BALANCING;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "balances_adjustment_id", foreignKey = @ForeignKey(name = "area_balances_adjustment_id_fk"))
    private List<BalancesAdjustmentAreaEntity> areas;

    public BalancesAdjustmentEntity(@NonNull BalancesAdjustmentModificationInfos balancesAdjustmentModificationInfos) {
        super(balancesAdjustmentModificationInfos);
        assignAttributes(balancesAdjustmentModificationInfos);
    }

    @Override
    public BalancesAdjustmentModificationInfos toModificationInfos() {
        return BalancesAdjustmentModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .activated(getActivated())
                .areas(areas.stream().map(BalancesAdjustmentAreaEntity::getAreaInfos).toList())
                .thresholdNetPosition(thresholdNetPosition)
                .maxNumberIterations(maxNumberIterations)
                .countriesToBalance(CountriesUtils.toList(countriesToBalance))
                .balanceType(balanceType)
                .withLoadFlow(withLoadFlow)
                .loadFlowParametersId(loadFlowParametersId)
                .withRatioTapChangers(withRatioTapChangers)
                .subtractLoadFlowBalancing(subtractLoadFlowBalancing)
                .build();
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BalancesAdjustmentModificationInfos) modificationInfos);
    }

    private void assignAttributes(BalancesAdjustmentModificationInfos balancesAdjustmentModificationInfos) {
        maxNumberIterations = balancesAdjustmentModificationInfos.getMaxNumberIterations();
        thresholdNetPosition = balancesAdjustmentModificationInfos.getThresholdNetPosition();
        countriesToBalance = CountriesUtils.stringify(balancesAdjustmentModificationInfos.getCountriesToBalance());
        balanceType = balancesAdjustmentModificationInfos.getBalanceType();
        withLoadFlow = balancesAdjustmentModificationInfos.isWithLoadFlow();
        loadFlowParametersId = balancesAdjustmentModificationInfos.getLoadFlowParametersId();
        withRatioTapChangers = balancesAdjustmentModificationInfos.isWithRatioTapChangers();
        subtractLoadFlowBalancing = balancesAdjustmentModificationInfos.isSubtractLoadFlowBalancing();
        List<BalancesAdjustmentAreaEntity> areaEntities = balancesAdjustmentModificationInfos
            .getAreas()
            .stream()
            .map(BalancesAdjustmentAreaEntity::from)
            .toList();
        if (areas == null) {
            areas = areaEntities;
        } else {
            areas.clear();
            areas.addAll(areaEntities);
        }
    }
}
