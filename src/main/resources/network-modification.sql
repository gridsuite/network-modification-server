
    create table booleanelementaryModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue boolean,
        id uuid not null,
        primary key (id)
    );

    create table doubleelementaryModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue float8,
        id uuid not null,
        primary key (id)
    );

    create table floatelementaryModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue float4,
        id uuid not null,
        primary key (id)
    );

    create table integerelementaryModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue int4,
        id uuid not null,
        primary key (id)
    );

    create table loadCreation (
       equipmentId varchar(255),
        busId varchar(255),
        equipmentName varchar(255),
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

    create table stringelementaryModification (
       equipmentId varchar(255),
        attributeName varchar(255),
        attributeValue varchar(255),
        id uuid not null,
        primary key (id)
    );
create index modificationEntity_group_id_index on modification (group_id);

    alter table if exists booleanelementaryModification 
       add constraint boolean_modification_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists doubleelementaryModification 
       add constraint double_modification_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists floatelementaryModification 
       add constraint float_modification_id_fk_constraint 
       foreign key (id) 
       references modification;

    alter table if exists integerelementaryModification 
       add constraint integer_modification_id_fk_constraint 
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

    alter table if exists stringelementaryModification 
       add constraint string_modification_id_fk_constraint 
       foreign key (id) 
       references modification;
