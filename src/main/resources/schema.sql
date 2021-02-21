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
    equipmentname varchar(255),
    equipmentattributename varchar(255),
    equipmentattributevalue varchar(255),
    constraint elementarymodification_pkey
        primary key (id)
)
    inherits (modification);

alter table elementarymodification owner to postgres;
