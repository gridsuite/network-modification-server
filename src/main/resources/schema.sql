create sequence hibernate_sequence;

alter sequence hibernate_sequence owner to postgres;

create table if not exists attribute
(
    id bigint not null
        constraint attribute_pkey
            primary key,
    attributename varchar(255)
);

alter table attribute owner to postgres;

create table if not exists booleanattribute
(
    attributevalue boolean,
    id bigint not null
        constraint booleanattribute_pkey
            primary key
        constraint fkt68a7i3obnfar1y5827wowkyg
            references attribute
);

alter table booleanattribute owner to postgres;

create table if not exists doubleattribute
(
    attributevalue double precision,
    id bigint not null
        constraint doubleattribute_pkey
            primary key
        constraint fkdaadvjxcvn4n2n9mchpsitpwy
            references attribute
);

alter table doubleattribute owner to postgres;

create table if not exists floatattribute
(
    attributevalue real,
    id bigint not null
        constraint floatattribute_pkey
            primary key
        constraint fklc471hypvnwhev6qmhwcrkb7e
            references attribute
);

alter table floatattribute owner to postgres;

create table if not exists integerattribute
(
    attributevalue integer,
    id bigint not null
        constraint integerattribute_pkey
            primary key
        constraint fkkrwg36kd74duaeo8ijuo6qtmm
            references attribute
);

alter table integerattribute owner to postgres;

create table if not exists modification
(
    id uuid not null
        constraint modification_pkey
            primary key,
    date timestamp,
    type varchar(255)
);

alter table modification owner to postgres;

create table if not exists elementarymodification
(
    equipmentid varchar(255),
    id uuid not null
        constraint elementarymodification_pkey
            primary key
        constraint fk2rn4rrsm6nkbiae0uote6yipn
            references modification,
    attribute_id bigint not null
        constraint uk_h6jbw69dgdfmvhpykehgjym5u
            unique
        constraint fkc3xgi6lsoaejnn84f40wlt9gk
            references attribute
);

alter table elementarymodification owner to postgres;

create table if not exists stringattribute
(
    attributevalue varchar(255),
    id bigint not null
        constraint stringattribute_pkey
            primary key
        constraint fkib9wv30mqmp4n9t71yiggcs5f
            references attribute
);

alter table stringattribute owner to postgres;
