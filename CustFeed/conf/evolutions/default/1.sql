# --- First database schema

# --- !Ups

create table user (
  clientID                  bigint not null primary key,
  email                     varchar(255) not null unique,
  name                      varchar(255) not null,
  password                  varchar(255) not null,
  country                   varchar(255),
  address                   varchar(255),
  template                  varchar(255),
  age                       int
);

create sequence clientID_seq start with 1000;

# --- !Downs

drop table if exists user;
drop sequence if exists clientID_seq;
