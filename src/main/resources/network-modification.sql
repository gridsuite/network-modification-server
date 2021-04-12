
    create table booleanelementaryModification (
       attributeValue boolean,
        uuid uuid not null,
        primary key (uuid)
    );

    create table doubleelementaryModification (
       attributeValue float8,
        uuid uuid not null,
        primary key (uuid)
    );

    create table elementaryModification (
       attributeName varchar(255),
        equipmentId varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );

    create table floatelementaryModification (
       attributeValue float4,
        uuid uuid not null,
        primary key (uuid)
    );

    create table integerelementaryModification (
       attributeValue int4,
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
       attributeValue varchar(255),
        uuid uuid not null,
        primary key (uuid)
    );
create index modificationEntity_group_uuid_index on modification (group_uuid);

    alter table if exists booleanelementaryModification 
       add constraint FKlsb35wcreerkuopydsaid8prq 
       foreign key (uuid) 
       references elementaryModification;

    alter table if exists doubleelementaryModification 
       add constraint FK22qc525p4dkeiat98vif28aa6 
       foreign key (uuid) 
       references elementaryModification;

    alter table if exists elementaryModification 
       add constraint FKl6dl2lad2w81g3umcypxw4uge 
       foreign key (uuid) 
       references modification;

    alter table if exists floatelementaryModification 
       add constraint FKhp1mqjh2et0jh9ybukq6i53r5 
       foreign key (uuid) 
       references elementaryModification;

    alter table if exists integerelementaryModification 
       add constraint FK2gs6yvdh2sv9a1rkmtwrh6h1 
       foreign key (uuid) 
       references elementaryModification;

    alter table if exists modification 
       add constraint group_uuid_fk_constraint 
       foreign key (group_uuid) 
       references modificationGroup;

    alter table if exists stringelementaryModification 
       add constraint FK81i1gqvwpk66p1kkw9qlobi03 
       foreign key (uuid) 
       references elementaryModification;
