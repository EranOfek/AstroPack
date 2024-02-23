create table sensors.arduino_in
(
    presence       boolean,
    temp_in        double precision,
    pressure_in    double precision,
    visible_lux_in double precision,
    flame          double precision,
    co2            double precision,
    voc            double precision,
    raw_h2         double precision,
    raw_ethanol    double precision,
    tstamp         timestamp,
    pk             bigint generated always as identity
        constraint pk
            primary key
);

alter table sensors.arduino_in
    owner to postgres;

create index idx_arduino_in_tstamp
    on sensors.arduino_in (tstamp) include (tstamp);

create table sensors.arduino_out
(
    temp_out        double precision,
    humidity_out    double precision,
    pressure_out    double precision,
    dew_point       double precision,
    visible_lux_out double precision,
    ir_luminosity   double precision,
    wind_speed      double precision,
    wind_direction  double precision,
    tstamp          timestamp,
    pk              bigint generated always as identity
        constraint pk_arduino_out
            primary key
);

alter table sensors.arduino_out
    owner to postgres;

create index idx_arduino_out_tstamp
    on sensors.arduino_out (tstamp) include (tstamp);

create table sensors.davis
(
    temp_in         double precision,
    humidity_in     double precision,
    pressure_out    double precision,
    temp_out        double precision,
    humidity_out    double precision,
    wind_speed      double precision,
    wind_direction  double precision,
    rain            double precision,
    solar_radiation double precision,
    tstamp          timestamp,
    pk              bigint generated always as identity
        constraint pk_davis
            primary key
);

alter table sensors.davis
    owner to postgres;

create index idx_davis_tstamp
    on sensors.davis (tstamp) include (tstamp);

create table sensors.arduino_out2
(
    pk              bigint generated always as identity
        primary key,
    temp_out        double precision,
    humidity_out    double precision,
    pressure_out    double precision,
    dew_point       double precision,
    visible_lux_out double precision,
    ir_luminosity   double precision,
    wind_speed      double precision,
    wind_direction  double precision,
    t               timestamp
);

alter table sensors.arduino_out2
    owner to postgres;

create table sensors.arduino_in2
(
    pk             bigint generated always as identity
        primary key,
    presence       boolean,
    temp_in        double precision,
    pressure_in    double precision,
    visible_lux_in double precision,
    flame          double precision,
    co2            double precision,
    voc            double precision,
    raw_h2         double precision,
    raw_ethanol    double precision,
    t              timestamp,
    t2             varchar
);

alter table sensors.arduino_in2
    owner to postgres;

create table sensors.enclosure
(
    south_wall_status varchar,
    control_enable    varchar,
    emo               boolean,
    ups               boolean,
    air_conditioner_1 boolean,
    tstamp            timestamp,
    roof_status       varchar,
    north_wall_status varchar,
    east_wall_status  varchar,
    pk                bigint generated always as identity
);

comment on table sensors.enclosure is 'not really "sensors" but however. Includes also what marked as AC in our original Weather Dictionary';

alter table sensors.enclosure
    owner to postgres;

