
CREATE TABLE public.table_f_radec0 (
pk1 BIGINT GENERATED ALWAYS AS IDENTITY,

f_ra DOUBLE PRECISION DEFAULT 0,
f_dec DOUBLE PRECISION DEFAULT 0,
p_ra_dec point default point(0, 0),

fdouble001 REAL DEFAULT 0,
fdouble002 REAL DEFAULT 0

);

create index table_f_radec0_idx_p_ra_dec on table_f_radec0 using spgist(p_ra_dec);

CREATE INDEX table_f_radec0_idx_ra ON public.table_f_radec0
  USING btree (f_ra);
  
CREATE INDEX table_f_radec0_idx_dec ON public.table_f_radec0
  USING btree (f_dec);



INSERT INTO table_f_radec0 (f_ra, f_dec, fdouble001, fdouble001) values(1, 2, 3, 4)

