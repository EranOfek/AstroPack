

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

