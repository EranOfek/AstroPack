
CREATE TABLE public.table_f_part1 (
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

) PARTITION BY RANGE (f_time);

CREATE INDEX table_f_part1_idx_f_time ON public.table_f_part1
  USING btree (f_time);
  
CREATE INDEX table_f_part1_idx_fdouble001 ON public.table_f_part1
  USING btree (fdouble001);

CREATE INDEX table_f_part1_idx_fdouble002 ON public.table_f_part1
  USING btree (fdouble002);

CREATE INDEX table_f_part1_idx_fdouble003 ON public.table_f_part1
  USING btree (fdouble003);

CREATE INDEX table_f_part1_idx_fdouble004 ON public.table_f_part1
  USING btree (fdouble004);

CREATE INDEX table_f_part1_idx_fdouble005 ON public.table_f_part1
  USING btree (fdouble005);

CREATE INDEX table_f_part1_idx_fdouble006 ON public.table_f_part1
  USING btree (fdouble006);

CREATE INDEX table_f_part1_idx_fdouble007 ON public.table_f_part1
  USING btree (fdouble007);

CREATE INDEX table_f_part1_idx_fdouble008 ON public.table_f_part1
  USING btree (fdouble008);

CREATE INDEX table_f_part1_idx_fdouble009 ON public.table_f_part1
  USING btree (fdouble009);

CREATE INDEX table_f_part1_idx_fdouble010 ON public.table_f_part1
  USING btree (fdouble010);

CREATE INDEX table_f_part1_idx_fdouble011 ON public.table_f_part1
  USING btree (fdouble011);

CREATE INDEX table_f_part1_idx_fdouble012 ON public.table_f_part1
  USING btree (fdouble012);

CREATE INDEX table_f_part1_idx_fdouble013 ON public.table_f_part1
  USING btree (fdouble013);

CREATE INDEX table_f_part1_idx_fdouble014 ON public.table_f_part1
  USING btree (fdouble014);

CREATE INDEX table_f_part1_idx_fdouble015 ON public.table_f_part1
  USING btree (fdouble015);

CREATE INDEX table_f_part1_idx_fdouble016 ON public.table_f_part1
  USING btree (fdouble016);

CREATE INDEX table_f_part1_idx_fdouble017 ON public.table_f_part1
  USING btree (fdouble017);

CREATE INDEX table_f_part1_idx_fdouble018 ON public.table_f_part1
  USING btree (fdouble018);

CREATE INDEX table_f_part1_idx_fdouble019 ON public.table_f_part1
  USING btree (fdouble019);

CREATE INDEX table_f_part1_idx_fdouble020 ON public.table_f_part1
  USING btree (fdouble020);

CREATE INDEX table_f_part1_idx_fdouble021 ON public.table_f_part1
  USING btree (fdouble021);

CREATE INDEX table_f_part1_idx_fdouble022 ON public.table_f_part1
  USING btree (fdouble022);

CREATE INDEX table_f_part1_idx_fdouble023 ON public.table_f_part1
  USING btree (fdouble023);

CREATE INDEX table_f_part1_idx_fdouble024 ON public.table_f_part1
  USING btree (fdouble024);

CREATE INDEX table_f_part1_idx_fdouble025 ON public.table_f_part1
  USING btree (fdouble025);

CREATE INDEX table_f_part1_idx_fdouble026 ON public.table_f_part1
  USING btree (fdouble026);

CREATE INDEX table_f_part1_idx_fdouble027 ON public.table_f_part1
  USING btree (fdouble027);

CREATE INDEX table_f_part1_idx_fdouble028 ON public.table_f_part1
  USING btree (fdouble028);

CREATE INDEX table_f_part1_idx_fdouble029 ON public.table_f_part1
  USING btree (fdouble029);

CREATE INDEX table_f_part1_idx_fdouble030 ON public.table_f_part1
  USING btree (fdouble030);

CREATE INDEX table_f_part1_idx_fdouble031 ON public.table_f_part1
  USING btree (fdouble031);

CREATE INDEX table_f_part1_idx_fdouble032 ON public.table_f_part1
  USING btree (fdouble032);

CREATE INDEX table_f_part1_idx_fdouble033 ON public.table_f_part1
  USING btree (fdouble033);

CREATE INDEX table_f_part1_idx_fdouble034 ON public.table_f_part1
  USING btree (fdouble034);

CREATE INDEX table_f_part1_idx_fdouble035 ON public.table_f_part1
  USING btree (fdouble035);

CREATE INDEX table_f_part1_idx_fdouble036 ON public.table_f_part1
  USING btree (fdouble036);

CREATE INDEX table_f_part1_idx_fdouble037 ON public.table_f_part1
  USING btree (fdouble037);

CREATE INDEX table_f_part1_idx_fdouble038 ON public.table_f_part1
  USING btree (fdouble038);

CREATE INDEX table_f_part1_idx_fdouble039 ON public.table_f_part1
  USING btree (fdouble039);

CREATE INDEX table_f_part1_idx_fdouble040 ON public.table_f_part1
  USING btree (fdouble040);

CREATE INDEX table_f_part1_idx_fdouble041 ON public.table_f_part1
  USING btree (fdouble041);

CREATE INDEX table_f_part1_idx_fdouble042 ON public.table_f_part1
  USING btree (fdouble042);

CREATE INDEX table_f_part1_idx_fdouble043 ON public.table_f_part1
  USING btree (fdouble043);

CREATE INDEX table_f_part1_idx_fdouble044 ON public.table_f_part1
  USING btree (fdouble044);

CREATE INDEX table_f_part1_idx_fdouble045 ON public.table_f_part1
  USING btree (fdouble045);

CREATE INDEX table_f_part1_idx_fdouble046 ON public.table_f_part1
  USING btree (fdouble046);

CREATE INDEX table_f_part1_idx_fdouble047 ON public.table_f_part1
  USING btree (fdouble047);

CREATE INDEX table_f_part1_idx_fdouble048 ON public.table_f_part1
  USING btree (fdouble048);

CREATE INDEX table_f_part1_idx_fdouble049 ON public.table_f_part1
  USING btree (fdouble049);

CREATE INDEX table_f_part1_idx_fdouble050 ON public.table_f_part1
  USING btree (fdouble050);

