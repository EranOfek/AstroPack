

select * from table_f_part2 where pk1 >= 32000001 and pk1 <= 3200000100

explain select * from table_f_part2 where f_dec = 0.0018

select count(*) from table_f_part2 where f_ra = 0.0009


COPY table_f_part2 (f_ra,f_dec,f_time,fdouble001,fdouble002,fdouble003,fdouble004,fdouble005,fdouble006,fdouble007,fdouble008,fdouble009,fdouble010,fdouble011,fdouble012,fdouble013,fdouble014,fdouble015,fdouble016,fdouble017,fdouble018,fdouble019,fdouble020,fdouble021,fdouble022,fdouble023,fdouble024,fdouble025,fdouble026,fdouble027,fdouble028,fdouble029,fdouble030,fdouble031,fdouble032,fdouble033,fdouble034,fdouble035,fdouble036,fdouble037,fdouble038,fdouble039,fdouble040,fdouble041,fdouble042,fdouble043,fdouble044,fdouble045,fdouble046,fdouble047,fdouble048,fdouble049,fdouble050) FROM '/var/samba/pgshare/BigDbTestPart1_45357.csv' DELIMITER ',' CSV HEADER;

CREATE TABLE table_f_part1_1801 PARTITION OF table_f_part2 FOR VALUES FROM (1801.000000) TO (3800.000000)

COPY table_f_part2 (f_ra,f_dec,f_time,fdouble001,fdouble002,fdouble003,fdouble004,fdouble005,fdouble006,fdouble007,fdouble008,fdouble009,fdouble010,fdouble011,fdouble012,fdouble013,fdouble014,fdouble015,fdouble016,fdouble017,fdouble018,fdouble019,fdouble020,fdouble021,fdouble022,fdouble023,fdouble024,fdouble025,fdouble026,fdouble027,fdouble028,fdouble029,fdouble030,fdouble031,fdouble032,fdouble033,fdouble034,fdouble035,fdouble036,fdouble037,fdouble038,fdouble039,fdouble040,fdouble041,fdouble042,fdouble043,fdouble044,fdouble045,fdouble046,fdouble047,fdouble048,fdouble049,fdouble050) FROM '/var/samba/pgshare/BigDbTestPart1_45357.csv' DELIMITER ',' CSV HEADER;

CREATE TABLE table_f_part1_201 PARTITION OF table_f_part2 FOR VALUES FROM (202.000000) TO (2200.000000)

select max(pk1) from table_f_part2


select * from table_f_radec1

delete from table_f_radec1
SELECT COUNT(*) FROM table_f_part2


SELECT reltuples AS estimate FROM pg_class WHERE relname = 'table_f_part2'

select  * from table_f_part2


CREATE TABLE public.table_f_radec1 (
pk1 BIGINT GENERATED ALWAYS AS IDENTITY,

f_ra DOUBLE PRECISION DEFAULT 0,
f_dec DOUBLE PRECISION DEFAULT 0,
p_ra_dec point default point(0, 0),

fdouble001 REAL DEFAULT 0,
fdouble002 REAL DEFAULT 0,
fdouble003 REAL DEFAULT 0,
fdouble004 REAL DEFAULT 0,
fdouble005 REAL DEFAULT 0,
fdouble006 REAL DEFAULT 0,
fdouble007 REAL DEFAULT 0,
fdouble008 REAL DEFAULT 0,
fdouble009 REAL DEFAULT 0,
fdouble010 REAL DEFAULT 0,
fdouble011 REAL DEFAULT 0,
fdouble012 REAL DEFAULT 0,
fdouble013 REAL DEFAULT 0,
fdouble014 REAL DEFAULT 0,
fdouble015 REAL DEFAULT 0,
fdouble016 REAL DEFAULT 0,
fdouble017 REAL DEFAULT 0,
fdouble018 REAL DEFAULT 0,
fdouble019 REAL DEFAULT 0,
fdouble020 REAL DEFAULT 0,
fdouble021 REAL DEFAULT 0,
fdouble022 REAL DEFAULT 0,
fdouble023 REAL DEFAULT 0,
fdouble024 REAL DEFAULT 0,
fdouble025 REAL DEFAULT 0,
fdouble026 REAL DEFAULT 0,
fdouble027 REAL DEFAULT 0,
fdouble028 REAL DEFAULT 0,
fdouble029 REAL DEFAULT 0,
fdouble030 REAL DEFAULT 0,
fdouble031 REAL DEFAULT 0,
fdouble032 REAL DEFAULT 0,
fdouble033 REAL DEFAULT 0,
fdouble034 REAL DEFAULT 0,
fdouble035 REAL DEFAULT 0,
fdouble036 REAL DEFAULT 0,
fdouble037 REAL DEFAULT 0,
fdouble038 REAL DEFAULT 0,
fdouble039 REAL DEFAULT 0,
fdouble040 REAL DEFAULT 0,
fdouble041 REAL DEFAULT 0,
fdouble042 REAL DEFAULT 0,
fdouble043 REAL DEFAULT 0,
fdouble044 REAL DEFAULT 0,
fdouble045 REAL DEFAULT 0,
fdouble046 REAL DEFAULT 0,
fdouble047 REAL DEFAULT 0,
fdouble048 REAL DEFAULT 0,
fdouble049 REAL DEFAULT 0,
fdouble050 REAL DEFAULT 0

);

create index table_f_radec1_idx_p_ra_dec on table_f_radec1 using spgist(p_ra_dec);

CREATE INDEX table_f_radec1_idx_ra ON public.table_f_radec1
  USING btree (f_ra);

CREATE INDEX table_f_radec1_idx_dec ON public.table_f_radec1
  USING btree (f_dec);







CREATE TABLE public.table_f_part2 (

pk1 BIGINT GENERATED ALWAYS AS IDENTITY,
f_time DOUBLE PRECISION DEFAULT 0,
f_ra DOUBLE PRECISION DEFAULT 0,
f_dec DOUBLE PRECISION DEFAULT 0,

fdouble001 DOUBLE PRECISION DEFAULT 0,
fdouble002 DOUBLE PRECISION DEFAULT 0,
fdouble003 DOUBLE PRECISION DEFAULT 0,
fdouble004 DOUBLE PRECISION DEFAULT 0,
fdouble005 DOUBLE PRECISION DEFAULT 0,
fdouble006 DOUBLE PRECISION DEFAULT 0,
fdouble007 DOUBLE PRECISION DEFAULT 0,
fdouble008 DOUBLE PRECISION DEFAULT 0,
fdouble009 DOUBLE PRECISION DEFAULT 0,
fdouble010 DOUBLE PRECISION DEFAULT 0,
fdouble011 DOUBLE PRECISION DEFAULT 0,
fdouble012 DOUBLE PRECISION DEFAULT 0,
fdouble013 DOUBLE PRECISION DEFAULT 0,
fdouble014 DOUBLE PRECISION DEFAULT 0,
fdouble015 DOUBLE PRECISION DEFAULT 0,
fdouble016 DOUBLE PRECISION DEFAULT 0,
fdouble017 DOUBLE PRECISION DEFAULT 0,
fdouble018 DOUBLE PRECISION DEFAULT 0,
fdouble019 DOUBLE PRECISION DEFAULT 0,
fdouble020 DOUBLE PRECISION DEFAULT 0,
fdouble021 DOUBLE PRECISION DEFAULT 0,
fdouble022 DOUBLE PRECISION DEFAULT 0,
fdouble023 DOUBLE PRECISION DEFAULT 0,
fdouble024 DOUBLE PRECISION DEFAULT 0,
fdouble025 DOUBLE PRECISION DEFAULT 0,
fdouble026 DOUBLE PRECISION DEFAULT 0,
fdouble027 DOUBLE PRECISION DEFAULT 0,
fdouble028 DOUBLE PRECISION DEFAULT 0,
fdouble029 DOUBLE PRECISION DEFAULT 0,
fdouble030 DOUBLE PRECISION DEFAULT 0,
fdouble031 DOUBLE PRECISION DEFAULT 0,
fdouble032 DOUBLE PRECISION DEFAULT 0,
fdouble033 DOUBLE PRECISION DEFAULT 0,
fdouble034 DOUBLE PRECISION DEFAULT 0,
fdouble035 DOUBLE PRECISION DEFAULT 0,
fdouble036 DOUBLE PRECISION DEFAULT 0,
fdouble037 DOUBLE PRECISION DEFAULT 0,
fdouble038 DOUBLE PRECISION DEFAULT 0,
fdouble039 DOUBLE PRECISION DEFAULT 0,
fdouble040 DOUBLE PRECISION DEFAULT 0,
fdouble041 DOUBLE PRECISION DEFAULT 0,
fdouble042 DOUBLE PRECISION DEFAULT 0,
fdouble043 DOUBLE PRECISION DEFAULT 0,
fdouble044 DOUBLE PRECISION DEFAULT 0,
fdouble045 DOUBLE PRECISION DEFAULT 0,
fdouble046 DOUBLE PRECISION DEFAULT 0,
fdouble047 DOUBLE PRECISION DEFAULT 0,
fdouble048 DOUBLE PRECISION DEFAULT 0,
fdouble049 DOUBLE PRECISION DEFAULT 0,
fdouble050 DOUBLE PRECISION DEFAULT 0

) PARTITION BY RANGE (pk1);

CREATE INDEX table_f_part2_idx_f_time ON public.table_f_part2
  USING btree (f_time);

CREATE INDEX table_f_part2_idx_f_ra ON public.table_f_part2
  USING btree (f_ra);

CREATE INDEX table_f_part2_idx_f_dec ON public.table_f_part2
  USING btree (f_dec);

