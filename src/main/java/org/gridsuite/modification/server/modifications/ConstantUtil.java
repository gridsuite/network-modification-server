package org.gridsuite.modification.server.modifications;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public final class ConstantUtil {

    private ConstantUtil() {

    }

    public static final String ACTIVE_LIMITS = "Active limits";
    public static final String ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG = "Angle droop active power control, Droop and P0 must be all provided or none";
    public static final String ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD = "AngleDroopActivePowerControl";
    public static final String APPLIED = "Applied";
    public static final String BREAKER = "breaker_";
    public static final String BUS_BAR_SECTION_ID = "busbarSectionId";
    public static final String CHARACTERISTICS = "Characteristics";
    public static final String CONNECTION_DIRECTION_FIELD_NAME = "Connection direction";
    public static final String CONNECTION_NAME_FIELD_NAME = "Connection name";
    public static final String CONNECTION_POSITION_FIELD_NAME = "Connection position";
    public static final String CONNECTIVITY = "Connectivity";
    public static final String COUNT = "count";
    public static final String DISCONNECTOR = "disconnector_";
    public static final String DOES_NOT_EXIST_IN_NETWORK = " does not exist in network";
    public static final String DROOP_FIELD = "Droop";
    public static final String DURATION = "duration";
    public static final String EQUIPMENT_DISCONNECTED = "equipmentDisconnected";
    public static final String GENERATOR = "generator";
    public static final String GENERATORS_WITH_FIXED_SUPPLY = "generatorsWithFixedSupply";
    public static final String GENERATORS_WITHOUT_OUTAGE = "generatorsWithoutOutage";
    public static final String GENERATORS_FREQUENCY_RESERVE = "generatorsFrequencyReserve";
    public static final String GENERATORS_KEY = "GeneratorsModifications";
    public static final String GENERATORS_NAME = "Generators";
    public static final String IS_PLURAL = "isPlural";
    public static final String LIMITS = "Limits";
    public static final String MAGNETIZING_CONDUCTANCE_FIELD_NAME = "Magnetizing conductance";
    public static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";
    public static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    public static final String NAME = "name";
    public static final String NO_VALUE = "No value";
    public static final String NOT_EXIST_IN_NETWORK = " does not exist in network";
    public static final String P0_FIELD = "P0";
    public static final String PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE = "Phase tap changer";
    public static final String POWER_TO_DISPATCH = "PowerToDispatch";
    public static final String PROPERTIES = "Properties";
    public static final String RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE = "Ratio tap changer";
    public static final String REACTIVE_LIMITS = "Reactive limits";
    public static final String REACTIVE_POWER_SET_POINT = "Reactive power set point";
    public static final String REGION_CVG = "regionCvg";
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
    public static final String RESULT = "Result";
    public static final String SECTION_COUNT = "Section count";
    public static final String SETPOINTS = "Setpoints";
    public static final String SHUNT_COMPENSATORS_KEY = "ShuntCompensatorsModifications";
    public static final String SHUNT_COMPENSATORS_NAME = "Shunt compensators";
    public static final String STACKING = "Stacking";
    public static final String STATIC_VAR_COMPENSATORS_KEY = "StaticVarCompensatorsModifications";
    public static final String STATIC_VAR_COMPENSATORS_NAME = "Static var compensators";
    public static final String SUBSTATION = "substation";
    public static final String SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE = "Switched-on Q at nominal voltage";
    public static final String SYNCHRONOUS_COMPONENT = "SC";
    public static final String TABULAR_MODIFICATION_REPORT_KEY_PREFIX = "tabular";
    public static final String THREE_WINDINGS_TRANSFORMERS_KEY = "3WindingsTransformersModifications";
    public static final String THREE_WINDINGS_TRANSFORMERS_NAME = "3 windings transformers";
    public static final String TWO_WINDINGS_TRANSFORMERS_KEY = "2WindingsTransformersModifications";
    public static final String TWO_WINDINGS_TRANSFORMERS_NAME = "2 windings transformers";
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
    public static final String VOLTAGE_SET_POINT = "Voltage set point";
    public static final String VOLTAGE_MAGNITUDE = "Voltage magnitude";
    public static final String VOLTAGE_ANGLE = "Voltage angle";
    public static final String VSC_CONVERTER_STATIONS_KEY = "VscConverterStationsModifications";
    public static final String VSC_CONVERTER_STATIONS_NAME = "Vsc converter stations";
    public static final String VSC_SETPOINTS = "vscSetPoints";
    public static final String VSC_CHARACTERISTICS = "vscCharacteristics";
}
