/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.TwoWindingsTransformerField;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.dto.byfilter.equipmentfield.FieldUtils.getFieldValue;
import static org.gridsuite.modification.server.dto.byfilter.equipmentfield.FieldUtils.setFieldValue;
import static org.gridsuite.modification.server.modifications.ModificationUtils.*;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public abstract class AbstractModificationByAssignment extends AbstractModification {
    public static final String VALUE_KEY_FILTER_NAME = "filterName";
    public static final String VALUE_KEY_FIELD_NAME = "fieldName";
    public static final String VALUE_KEY_EQUIPMENT_NAME = "equipmentName";
    public static final String VALUE_KEY_EQUIPMENT_TYPE = "equipmentType";
    public static final String VALUE_KEY_EQUIPMENT_COUNT = "equipmentCount";
    public static final String VALUE_KEY_EQUIPMENT_IDS = "equipmentIds";
    public static final String VALUE_KEY_NB_CHANGED = "nbChanged";
    public static final String VALUE_KEY_NB_UNCHANGED = "nbUnchanged";
    public static final String VALUE_KEY_OLD_VALUE = "oldValue";
    public static final String VALUE_KEY_NEW_VALUE = "newValue";
    public static final String VALUE_KEY_MODIFICATION_TYPE_LABEL = "modificationTypeLabel";
    public static final String VALUE_KEY_FILTERS_EACH_ASSIGNMENT = "filtersEachAssignment";
    public static final String VALUE_KEY_ERROR_MESSAGE = "errorMessage";
    public static final String REPORT_KEY_RATIO_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR = "ratioTapChangerEquipmentModifiedError";
    public static final String REPORT_KEY_PHASE_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR = "phaseTapChangerEquipmentModifiedError";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_ERROR = "equipmentModifiedError";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_SOME = "byFilterModificationSome";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_FAILED = "byFilterModificationFailed";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_SUCCESS = "byFilterModificationSuccess";
    public static final String REPORT_KEY_NUMBER_OF_VALID_EQUIPMENT = "numberOfValidEquipment";
    public static final String REPORT_KEY_NOT_EDITED_EQUIPMENTS_FILTER = "notEditedEquipmentsFilter";
    public static final String REPORT_KEY_EDITED_FIELD_FILTER = "editedFieldFilter";
    public static final String REPORT_KEY_FILTER_EQUIPMENTS_NOT_FOUND = "filterEquipmentsNotFound";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_REPORT = "equipmentModifiedReport";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION = "equipmentModifiedReportException";
    public static final String REPORT_KEY_APPLIED_BY_FILTER_MODIFICATIONS = "appliedByFilterModifications";
    public static final String REPORT_KEY_APPLIED_ASSIGNMENT = "appliedAssignment";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_ALL = "byFilterModificationAll";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_NONE = "byFilterModificationNone";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_NOT_FOUND = "byFilterModificationNotFound";

    protected FilterService filterService;
    protected int equipmentNotModifiedCount;
    protected long equipmentCount;
    protected long equipmentNotFoundCount;

    protected AbstractModificationByAssignment() {
        equipmentNotModifiedCount = 0;
        equipmentCount = 0;
        equipmentNotFoundCount = 0;
    }

    public abstract String getModificationTypeLabel();

    private String getEditedFieldLabel(AbstractAssignmentInfos modificationByFilterInfos) {
        return modificationByFilterInfos.getEditedFieldLabel();
    }

    public abstract ModificationInfos getModificationInfos();

    public abstract IdentifiableType getEquipmentType();

    public abstract NetworkModificationException.Type getExceptionType();

    public abstract List<AbstractAssignmentInfos> getAssignmentInfosList();

    protected abstract boolean preCheckValue(Identifiable<?> equipment,
                                             AbstractAssignmentInfos abstractAssignmentInfos,
                                             List<ReportNode> reports, List<String> notEditableEquipments);

    protected abstract String getNewValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos);

    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        return getFieldValue(equipment, abstractAssignmentInfos.getEditedField());
    }

    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        // get new value
        String newValue = getNewValue(equipment, abstractAssignmentInfos);

        // set new value for the equipment
        setFieldValue(equipment, abstractAssignmentInfos.getEditedField(), newValue);
        return newValue;
    }

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (getModificationInfos() == null) {
            throw new NetworkModificationException(getExceptionType(), "Missing required attributes to modify the equipment");
        }

        if (CollectionUtils.isEmpty(getAssignmentInfosList())) {
            throw new NetworkModificationException(getExceptionType(), String.format("At least one %s is required", getModificationTypeLabel()));
        }

        if (getAssignmentInfosList().stream().anyMatch(modificationByFilterInfos -> CollectionUtils.isEmpty(modificationByFilterInfos.getFilters()))) {
            throw new NetworkModificationException(getExceptionType(), String.format("Every %s must have at least one filter", getModificationTypeLabel()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        Map<UUID, String> filters = getFilters();

        Map<UUID, FilterEquipments> filterUuidEquipmentsMap =
            ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, getModificationInfos().getErrorType());

        if (filterUuidEquipmentsMap != null) {
            ReportNode subReporter = subReportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_APPLIED_BY_FILTER_MODIFICATIONS, "${" + VALUE_KEY_MODIFICATION_TYPE_LABEL + "}s on ${" + VALUE_KEY_EQUIPMENT_TYPE + "} type")
                .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, StringUtils.capitalize(getModificationTypeLabel()))
                .withUntypedValue(VALUE_KEY_EQUIPMENT_TYPE, getEquipmentType().name())
                .add();
            // perform modifications
            getAssignmentInfosList().forEach(abstractAssignmentInfos -> {
                List<ReportNode> reports = new ArrayList<>();
                ReportNode eachAssignmentReporter = subReporter.newReportNode()
                    .withMessageTemplate(REPORT_KEY_APPLIED_ASSIGNMENT, "${" + VALUE_KEY_MODIFICATION_TYPE_LABEL + "} on filters : ${" + VALUE_KEY_FILTERS_EACH_ASSIGNMENT + "}")
                    .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, StringUtils.capitalize(getModificationTypeLabel()))
                    .withUntypedValue(VALUE_KEY_FILTERS_EACH_ASSIGNMENT, abstractAssignmentInfos.getFilters().stream().map(FilterInfos::getName)
                    .collect(Collectors.joining(", ")))
                    .add();
                abstractAssignmentInfos.getFilters().forEach(filterInfos -> applyOnFilterEquipments(network, filterUuidEquipmentsMap, reports, abstractAssignmentInfos, filterInfos));
                reports.forEach(report -> insertReportNode(eachAssignmentReporter, report));
            });
            // reporting
            if (equipmentNotModifiedCount == 0 && equipmentNotFoundCount == 0) {
                subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_ALL,
                            "All equipments have been modified : ${" + VALUE_KEY_EQUIPMENT_COUNT + "} equipment(s)")
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, equipmentCount)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

            } else {
                if (equipmentNotModifiedCount == equipmentCount) {
                    createReport(subReportNode, REPORT_KEY_BY_FILTER_MODIFICATION_NONE,
                        "No equipment have been modified",
                        Map.of(), TypedValue.ERROR_SEVERITY);
                } else {
                    subReportNode.newReportNode()
                        .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_SOME,
                                "Some of the equipment have been modified : ${" + VALUE_KEY_NB_CHANGED + "} equipment(s) modified and ${" + VALUE_KEY_NB_UNCHANGED + "} equipment(s) not modified")
                        .withUntypedValue(VALUE_KEY_NB_CHANGED, equipmentCount - equipmentNotModifiedCount)
                        .withUntypedValue(VALUE_KEY_NB_UNCHANGED, equipmentNotModifiedCount + equipmentNotFoundCount)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
                }
            }
        }
    }

    protected boolean isEquipmentEditable(Identifiable<?> equipment,
                                          AbstractAssignmentInfos abstractAssignmentInfos,
                                          List<ReportNode> equipmentsReport) {
        if (abstractAssignmentInfos.getEditedField() == null) {
            return false;
        }

        if (equipment.getType() == IdentifiableType.TWO_WINDINGS_TRANSFORMER) {
            TwoWindingsTransformerField editedField = TwoWindingsTransformerField.valueOf(abstractAssignmentInfos.getEditedField());
            TwoWindingsTransformer twoWindingsTransformer = (TwoWindingsTransformer) equipment;
            return switch (editedField) {
                case TARGET_V, RATIO_LOW_TAP_POSITION, RATIO_TAP_POSITION, RATIO_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getRatioTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(ReportNode.newRootReportNode()
                            .withMessageTemplate(REPORT_KEY_RATIO_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR + equipmentsReport.size(),
                                    "        Cannot modify field ${" + VALUE_KEY_FIELD_NAME + "} of equipment ${" + VALUE_KEY_EQUIPMENT_NAME + "} : Ratio tab changer is null")
                            .withUntypedValue(VALUE_KEY_FIELD_NAME, editedField.name())
                            .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                            .withSeverity(TypedValue.TRACE_SEVERITY)
                            .build());
                    }
                    yield isEditable;
                }
                case REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION, PHASE_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getPhaseTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(ReportNode.newRootReportNode()
                            .withMessageTemplate(REPORT_KEY_PHASE_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR + equipmentsReport.size(),
                                    "        Cannot modify field ${" + VALUE_KEY_FIELD_NAME + "} of equipment ${" + VALUE_KEY_EQUIPMENT_NAME + "} : Phase tab changer is null")
                            .withUntypedValue(VALUE_KEY_FIELD_NAME, editedField.name())
                            .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                            .withSeverity(TypedValue.TRACE_SEVERITY)
                            .build());
                    }
                    yield isEditable;
                }
                default -> true;
            };
        }
        return true;
    }

    private void createAssignmentReports(List<ReportNode> reports, AbstractAssignmentInfos abstractAssignmentInfos,
                                         FilterInfos filterInfos, FilterEquipments filterEquipments, List<String> notEditableEquipments) {
        if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_FAILED,
                        "No equipment(s) have been modified on filter ${" + VALUE_KEY_FILTER_NAME + "}")
                .withUntypedValue(VALUE_KEY_FILTER_NAME, filterInfos.getName())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
        } else {
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_SUCCESS,
                        "Successful application of ${" + VALUE_KEY_MODIFICATION_TYPE_LABEL + "} on filter ${" + VALUE_KEY_FILTER_NAME + "}")
                .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, getModificationTypeLabel())
                .withUntypedValue(VALUE_KEY_FILTER_NAME, filterInfos.getName())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_NUMBER_OF_VALID_EQUIPMENT, "      Number of equipment modified : ${" + VALUE_KEY_NB_CHANGED + "}")
                .withUntypedValue(VALUE_KEY_NB_CHANGED, filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

            if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(REPORT_KEY_NOT_EDITED_EQUIPMENTS_FILTER,
                            "       ${" + VALUE_KEY_NB_UNCHANGED + "} equipment(s) were not modified")
                    .withUntypedValue(VALUE_KEY_NB_UNCHANGED, notEditableEquipments.size())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            }
        }

        reports.add(ReportNode.newRootReportNode()
            .withMessageTemplate(REPORT_KEY_EDITED_FIELD_FILTER, "      Edited field : ${" + VALUE_KEY_FIELD_NAME + "}")
            .withUntypedValue(VALUE_KEY_FIELD_NAME, getEditedFieldLabel(abstractAssignmentInfos))
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());

        if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
            String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_FILTER_EQUIPMENTS_NOT_FOUND, "      Equipment not found : ${" + VALUE_KEY_EQUIPMENT_IDS + "}")
                .withUntypedValue(VALUE_KEY_EQUIPMENT_IDS, equipmentIds)
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
        }
    }

    private void applyModification(Identifiable<?> equipment,
                                     AbstractAssignmentInfos abstractAssignmentInfos,
                                     List<ReportNode> reports,
                                     List<String> notEditableEquipments) {

        // check pre-conditions
        if (!preCheckValue(equipment, abstractAssignmentInfos, reports, notEditableEquipments)) {
            return;
        }

        // perform to apply new value
        try {
            final String oldValue = getOldValue(equipment, abstractAssignmentInfos);
            final String newValue = applyValue(equipment, abstractAssignmentInfos);
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_REPORT,
                    "        ${" + VALUE_KEY_EQUIPMENT_TYPE + "} id : ${" + VALUE_KEY_EQUIPMENT_NAME +
                    "}, ${" + VALUE_KEY_FIELD_NAME + "} : ${" + VALUE_KEY_OLD_VALUE + "} → ${" + VALUE_KEY_NEW_VALUE + "}")
                .withUntypedValue(VALUE_KEY_EQUIPMENT_TYPE, equipment.getType().name())
                .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                .withUntypedValue(VALUE_KEY_FIELD_NAME, getEditedFieldLabel(abstractAssignmentInfos))
                .withUntypedValue(VALUE_KEY_OLD_VALUE, oldValue == null ? NO_VALUE : oldValue)
                .withUntypedValue(VALUE_KEY_NEW_VALUE, newValue)
                .withSeverity(TypedValue.TRACE_SEVERITY)
                .build());
        } catch (Exception e) {
            notEditableEquipments.add(equipment.getId());
            equipmentNotModifiedCount += 1;
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION,
                        "        Cannot modify equipment ${" + VALUE_KEY_EQUIPMENT_NAME + "} : ${" + VALUE_KEY_ERROR_MESSAGE + "}")
                .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                .withUntypedValue(VALUE_KEY_ERROR_MESSAGE, e.getMessage())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
        }
    }

    private Map<UUID, String> getFilters() {
        return getAssignmentInfosList().stream()
            .flatMap(v -> v.getFilters().stream())
            .filter(distinctByKey(FilterInfos::getId))
            .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));
    }

    private void applyOnFilterEquipments(Network network,
                                        Map<UUID, FilterEquipments> filterUuidEquipmentsMap,
                                        List<ReportNode> reports,
                                        AbstractAssignmentInfos abstractAssignmentInfos,
                                        FilterInfos filterInfos) {
        FilterEquipments filterEquipments = filterUuidEquipmentsMap.get(filterInfos.getId());

        if (CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes())) {
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_NOT_FOUND, "No equipments were found for filter ${" + VALUE_KEY_FILTER_NAME + "}")
                .withUntypedValue(VALUE_KEY_FILTER_NAME, filterInfos.getName())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
        } else {
            equipmentCount += filterEquipments.getIdentifiableAttributes().size();
            if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
                equipmentNotFoundCount += filterEquipments.getNotFoundEquipments().size();
            }
            List<String> notEditableEquipments = new ArrayList<>();
            List<ReportNode> equipmentsReport = new ArrayList<>();
            filterEquipments.getIdentifiableAttributes()
                .stream()
                .map(attributes -> network.getIdentifiable(attributes.getId()))
                .filter(equipment -> {
                    boolean isEditableEquipment = isEquipmentEditable(equipment, abstractAssignmentInfos, equipmentsReport);
                    if (!isEditableEquipment) {
                        notEditableEquipments.add(equipment.getId());
                        equipmentNotModifiedCount += 1;
                    }
                    return isEditableEquipment;
                })
                .forEach(equipment -> applyModification(equipment, abstractAssignmentInfos, equipmentsReport, notEditableEquipments));

            createAssignmentReports(reports, abstractAssignmentInfos, filterInfos, filterEquipments, notEditableEquipments);

            reports.addAll(equipmentsReport);
        }
    }

}
