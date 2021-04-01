create sequence hibernate_sequence start 1 increment 1;

    create table attribute (
       id int8 not null,
        attributeName varchar(255),
        primary key (id)
    );

    create table booleanattribute (
       attributeValue boolean,
        id int8 not null,
        primary key (id)
    );

    create table doubleAttribute (
       attributeValue float8,
        id int8 not null,
        primary key (id)
    );

    create table elementaryModification (
       equipmentId varchar(255),
        id uuid not null,
        attribute_id int8 not null,
        primary key (id)
    );

    create table floatAttribute (
       attributeValue float4,
        id int8 not null,
        primary key (id)
    );

    create table integerAttribute (
       attributeValue int4,
        id int8 not null,
        primary key (id)
    );

    create table modification (
       id uuid not null,
        date timestamp,
        type varchar(255),
        groupUuid uuid,
        primary key (id)
    );

    create table modificationGroup (
       id uuid not null,
        primary key (id)
    );

    create table stringAttribute (
       attributeValue varchar(255),
        id int8 not null,
        primary key (id)
    );

    alter table if exists elementaryModification 
       add constraint UK_h6jbw69dgdfmvhpykehgjym5u unique (attribute_id);

    alter table if exists booleanattribute 
       add constraint FKt68a7i3obnfar1y5827wowkyg 
       foreign key (id) 
       references attribute;

    alter table if exists doubleAttribute 
       add constraint FKdaadvjxcvn4n2n9mchpsitpwy 
       foreign key (id) 
       references attribute;

    alter table if exists elementaryModification 
       add constraint FKc3xgi6lsoaejnn84f40wlt9gk 
       foreign key (attribute_id) 
       references attribute;

    alter table if exists elementaryModification 
       add constraint FK2rn4rrsm6nkbiae0uote6yipn 
       foreign key (id) 
       references modification;

    alter table if exists floatAttribute 
       add constraint FKlc471hypvnwhev6qmhwcrkb7e 
       foreign key (id) 
       references attribute;

    alter table if exists integerAttribute 
       add constraint FKkrwg36kd74duaeo8ijuo6qtmm 
       foreign key (id) 
       references attribute;

    alter table if exists modification 
       add constraint FKoagcutli4rusr7rkumnmsu1jy 
       foreign key (groupUuid) 
       references modificationGroup;

    alter table if exists stringAttribute 
       add constraint FKib9wv30mqmp4n9t71yiggcs5f 
       foreign key (id) 
       references attribute;
