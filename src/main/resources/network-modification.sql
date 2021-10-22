
    create table booleanEquipmentAttributeModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue boolean,
        id uuid not null,
        primary key (id)
    );

    create table doubleEquipmentAttributeModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue float8,
        id uuid not null,
        primary key (id)
    );

    create table equipmentDeletion (
       equipmentId varchar(255),
        equipmentType varchar(255),
        id uuid not null,
        primary key (id)
    );

    create table floatEquipmentAttributeModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue float4,
        id uuid not null,
        primary key (id)
    );

    create table generatorCreation (
       equipmentId varchar(255),
        equipmentName varchar(255),
        busOrBusbarSectionId varchar(255),
        voltageLevelId varchar(255),
        activePowerSetpoint float8,
        energySource int4,
        maxActivePower float8,
        minActivePower float8,
        ratedNominalPower float8,
        reactivePowerSetpoint float8,
        voltageRegulationOn boolean,
        voltageSetpoint float8,
        id uuid not null,
        primary key (id)
    );

    create table integerEquipmentAttributeModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue int4,
        id uuid not null,
        primary key (id)
    );

    create table loadCreation (
       equipmentId varchar(255),
        equipmentName varchar(255),
        busOrBusbarSectionId varchar(255),
        voltageLevelId varchar(255),
        activePower float8,
        loadType int4,
        reactivePower float8,
        id uuid not null,
        primary key (id)
    );

    create table lineCreation (
       equipmentId varchar(255),
        equipmentName varchar(255),
        seriesResistance float8,
        seriesReactance float8,
        shuntConductance1 float8,
        shuntSusceptance1 float8,
        shuntConductance2 float8,
        shuntSusceptance2 float8,
        voltageLevelId1 varchar(255),
        busOrBusbarSectionId1 varchar(255),
        voltageLevelId2 varchar(255),
        busOrBusbarSectionId2 varchar(255),
        id uuid not null,
        primary key (id)
    );

    create table modification (
       id uuid not null,
        date timestamp,
        type varchar(255),
        group_id uuid not null,
        primary key (id)
    );

    create table modificationGroup (
       id uuid not null,
        primary key (id)
    );

    create table stringEquipmentAttributeModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue varchar(255),
        id uuid not null,
        primary key (id)
    );
create index modificationEntity_group_id_index on modification (group_id);

    alter table if exists booleanEquipmentAttributeModification 
       add constraint boolean_equipment_attribute_modification_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists doubleEquipmentAttributeModification 
       add constraint double_equipment_attribute_modification_id_fk_constraint 
       foreign key (id)
       references modification;

    alter table if exists equipmentDeletion 
       add constraint equipmentDeletion_id_fk_constraint 
       foreign key (id)
       references modification;

    alter table if exists floatEquipmentAttributeModification 
       add constraint float_equipment_attribute_modification_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists generatorCreation 
       add constraint generatorCreation_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists integerEquipmentAttributeModification 
       add constraint integer_equipment_attribute_modification_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists loadCreation 
       add constraint loadCreation_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists lineCreation 
       add constraint lineCreation_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists modification 
       add constraint group_id_fk_constraint 
       foreign key (group_id) 
       references modificationGroup;

    alter table if exists stringEquipmentAttributeModification 
       add constraint string_equipment_attribute_modification_id_fk_constraint 
       foreign key (id) 
       references modification;
