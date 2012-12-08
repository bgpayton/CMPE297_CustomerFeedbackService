# --- First database schema

# --- !Ups

create table user (
  email                     varchar(255) not null primary key,
  name                      varchar(255) not null,
  password                  varchar(255) not null,
  country                   varchar(255),
  address                   varchar(255),
  age                       int
);


# --- !Downs

drop table if exists user;
