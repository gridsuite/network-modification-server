
    create table booleanelementaryModification (
       attributeName varchar(255),
        attributeValue boolean,
        equipmentId varchar(255),
        id uuid not null,
        primary key (id)
    );

    create table doubleelementaryModification (
       attributeName varchar(255),
        attributeValue float8,
        equipmentId varchar(255),
        id uuid not null,
        primary key (id)
    );

    create table floatelementaryModification (
       attributeName varchar(255),
        attributeValue float4,
        equipmentId varchar(255),
        id uuid not null,
        primary key (id)
    );

    create table integerelementaryModification (
       attributeName varchar(255),
        attributeValue int4,
        equipmentId varchar(255),
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
       attributeName varchar(255),
        attributeValue varchar(255),
        equipmentId varchar(255),
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

    alter table if exists modification 
       add constraint group_id_fk_constraint 
       foreign key (group_id) 
       references modificationGroup;

    alter table if exists stringelementaryModification 
       add constraint string_modification_id_fk_constraint 
       foreign key (id) 
       references modification;
