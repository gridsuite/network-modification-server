
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
       add constraint FKa3hu3fx48om8vorcdyig31t78 
       foreign key (uuid) 
       references modification;

    alter table if exists doubleelementaryModification 
       add constraint FKeuf54j3isrkrm3j8gaiq43wd9 
       foreign key (uuid) 
       references modification;

    alter table if exists floatelementaryModification 
       add constraint FKdkaccp8q3hj71mtrlv0b2ddgm 
       foreign key (uuid) 
       references modification;

    alter table if exists integerelementaryModification 
       add constraint FKdr0bq6jl508feg74kd3ygp8o6 
       foreign key (uuid) 
       references modification;

    alter table if exists modification 
       add constraint group_uuid_fk_constraint 
       foreign key (group_uuid) 
       references modificationGroup;

    alter table if exists stringelementaryModification 
       add constraint FK7p1ijl4b1gd18psfy49iua05v 
       foreign key (uuid) 
       references modification;
