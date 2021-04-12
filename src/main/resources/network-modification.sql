
    create table booleanelementaryModification (
       attributeName varchar(255),
        attributeValue boolean,
        equipmentId varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );

    create table doubleelementaryModification (
       attributeName varchar(255),
        attributeValue float8,
        equipmentId varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );

    create table floatelementaryModification (
       attributeName varchar(255),
        attributeValue float4,
        equipmentId varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );

    create table integerelementaryModification (
       attributeName varchar(255),
        attributeValue int4,
        equipmentId varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );

    create table modification (
       uuid uuid not null,
        date timestamp,
        type varchar(255),
        group_uuid uuid not null,
        primary key (uuid)
    );

    create table modificationGroup (
       uuid uuid not null,
        primary key (uuid)
    );

    create table stringelementaryModification (
       attributeName varchar(255),
        attributeValue varchar(255),
        equipmentId varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );
create index modificationEntity_group_uuid_index on modification (group_uuid);

    alter table if exists booleanelementaryModification 
       add constraint boolean_modification_uuid_fk_constraint 
       foreign key (uuid) 
       references modification;

    alter table if exists doubleelementaryModification 
       add constraint double_modification_uuid_fk_constraint 
       foreign key (uuid) 
       references modification;

    alter table if exists floatelementaryModification 
       add constraint float_modification_uuid_fk_constraint 
       foreign key (uuid) 
       references modification;

    alter table if exists integerelementaryModification 
       add constraint integer_modification_uuid_fk_constraint 
       foreign key (uuid) 
       references modification;

    alter table if exists modification 
       add constraint group_uuid_fk_constraint 
       foreign key (group_uuid) 
       references modificationGroup;

    alter table if exists stringelementaryModification 
       add constraint string_modification_uuid_fk_constraint 
       foreign key (uuid) 
       references modification;
