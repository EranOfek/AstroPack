
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

) PARTITION BY RANGE (f_time);

create index table_f_radec1_idx_p_ra_dec on table_f_radec1 using spgist(p_ra_dec);

CREATE INDEX table_f_radec1_idx_ra ON public.table_f_radec1
  USING btree (f_ra);
  
CREATE INDEX table_f_radec1_idx_dec ON public.table_f_radec1
  USING btree (f_dec);



