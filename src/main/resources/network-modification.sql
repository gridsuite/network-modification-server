
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

    create table floatEquipmentAttributeModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue float4,
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
        busId varchar(255),
        voltageLevelId varchar(255),
        activePower float8,
        loadType int4,
        reactivePower float8,
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

    alter table if exists floatEquipmentAttributeModification 
       add constraint float_equipment_attribute_modification_id_fk_constraint 
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

    alter table if exists modification 
       add constraint group_id_fk_constraint 
       foreign key (group_id) 
       references modificationGroup;

    alter table if exists stringEquipmentAttributeModification 
       add constraint string_equipment_attribute_modification_id_fk_constraint 
       foreign key (id) 
       references modification;
