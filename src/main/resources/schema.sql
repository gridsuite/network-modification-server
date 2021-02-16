CREATE TABLE IF NOT EXISTS elementaryModification
(
    id                      uuid not null unique,
    date                    timestamp,
    equipmentId             varchar(255),
    equipmentName           varchar(255),
    equipmentAttributeName  varchar(255),
    equipmentAttributeValue varchar(255),
    primary key (id)
);
