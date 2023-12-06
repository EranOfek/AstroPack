-- Table: public.table_f4

-- DROP TABLE IF EXISTS public.table_f4;

CREATE TABLE IF NOT EXISTS public.table_f4
(
    pk1 bigint NOT NULL GENERATED ALWAYS AS IDENTITY,
    fdouble001 double precision DEFAULT 0,
    fdouble002 double precision DEFAULT 0,
    fdouble003 double precision DEFAULT 0,
    fdouble004 double precision DEFAULT 0,
    fdouble005 double precision DEFAULT 0,
    fdouble006 double precision DEFAULT 0,
    fdouble007 double precision DEFAULT 0,
    fdouble008 double precision DEFAULT 0,
    fdouble009 double precision DEFAULT 0,
    fdouble010 double precision DEFAULT 0,
    fdouble011 double precision DEFAULT 0,
    fdouble012 double precision DEFAULT 0,
    fdouble013 double precision DEFAULT 0,
    fdouble014 double precision DEFAULT 0,
    fdouble015 double precision DEFAULT 0,
    fdouble016 double precision DEFAULT 0,
    fdouble017 double precision DEFAULT 0,
    fdouble018 double precision DEFAULT 0,
    fdouble019 double precision DEFAULT 0,
    fdouble020 double precision DEFAULT 0,
    fdouble021 double precision DEFAULT 0,
    fdouble022 double precision DEFAULT 0,
    fdouble023 double precision DEFAULT 0,
    fdouble024 double precision DEFAULT 0,
    fdouble025 double precision DEFAULT 0,
    fdouble026 double precision DEFAULT 0,
    fdouble027 double precision DEFAULT 0,
    fdouble028 double precision DEFAULT 0,
    fdouble029 double precision DEFAULT 0,
    fdouble030 double precision DEFAULT 0,
    fdouble031 double precision DEFAULT 0,
    fdouble032 double precision DEFAULT 0,
    fdouble033 double precision DEFAULT 0,
    fdouble034 double precision DEFAULT 0,
    fdouble035 double precision DEFAULT 0,
    fdouble036 double precision DEFAULT 0,
    fdouble037 double precision DEFAULT 0,
    fdouble038 double precision DEFAULT 0,
    fdouble039 double precision DEFAULT 0,
    fdouble040 double precision DEFAULT 0,
    fdouble041 double precision DEFAULT 0,
    fdouble042 double precision DEFAULT 0,
    fdouble043 double precision DEFAULT 0,
    fdouble044 double precision DEFAULT 0,
    fdouble045 double precision DEFAULT 0,
    fdouble046 double precision DEFAULT 0,
    fdouble047 double precision DEFAULT 0,
    fdouble048 double precision DEFAULT 0,
    fdouble049 double precision DEFAULT 0,
    fdouble050 double precision DEFAULT 0
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.table_f4
    OWNER to postgres;
-- Index: table_f4_idx_fdouble001

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble001;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble001
    ON public.table_f4 USING btree
    (fdouble001 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble002

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble002;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble002
    ON public.table_f4 USING btree
    (fdouble002 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble003

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble003;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble003
    ON public.table_f4 USING btree
    (fdouble003 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble004

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble004;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble004
    ON public.table_f4 USING btree
    (fdouble004 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble005

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble005;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble005
    ON public.table_f4 USING btree
    (fdouble005 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble006

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble006;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble006
    ON public.table_f4 USING btree
    (fdouble006 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble007

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble007;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble007
    ON public.table_f4 USING btree
    (fdouble007 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble008

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble008;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble008
    ON public.table_f4 USING btree
    (fdouble008 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble009

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble009;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble009
    ON public.table_f4 USING btree
    (fdouble009 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble010

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble010;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble010
    ON public.table_f4 USING btree
    (fdouble010 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble011

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble011;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble011
    ON public.table_f4 USING btree
    (fdouble011 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble012

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble012;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble012
    ON public.table_f4 USING btree
    (fdouble012 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble013

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble013;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble013
    ON public.table_f4 USING btree
    (fdouble013 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble014

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble014;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble014
    ON public.table_f4 USING btree
    (fdouble014 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble015

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble015;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble015
    ON public.table_f4 USING btree
    (fdouble015 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble016

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble016;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble016
    ON public.table_f4 USING btree
    (fdouble016 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble017

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble017;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble017
    ON public.table_f4 USING btree
    (fdouble017 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble018

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble018;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble018
    ON public.table_f4 USING btree
    (fdouble018 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble019

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble019;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble019
    ON public.table_f4 USING btree
    (fdouble019 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble020

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble020;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble020
    ON public.table_f4 USING btree
    (fdouble020 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble021

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble021;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble021
    ON public.table_f4 USING btree
    (fdouble021 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble022

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble022;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble022
    ON public.table_f4 USING btree
    (fdouble022 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble023

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble023;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble023
    ON public.table_f4 USING btree
    (fdouble023 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble024

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble024;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble024
    ON public.table_f4 USING btree
    (fdouble024 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble025

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble025;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble025
    ON public.table_f4 USING btree
    (fdouble025 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble026

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble026;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble026
    ON public.table_f4 USING btree
    (fdouble026 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble027

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble027;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble027
    ON public.table_f4 USING btree
    (fdouble027 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble028

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble028;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble028
    ON public.table_f4 USING btree
    (fdouble028 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble029

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble029;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble029
    ON public.table_f4 USING btree
    (fdouble029 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble030

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble030;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble030
    ON public.table_f4 USING btree
    (fdouble030 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble031

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble031;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble031
    ON public.table_f4 USING btree
    (fdouble031 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble032

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble032;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble032
    ON public.table_f4 USING btree
    (fdouble032 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble033

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble033;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble033
    ON public.table_f4 USING btree
    (fdouble033 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble034

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble034;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble034
    ON public.table_f4 USING btree
    (fdouble034 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble035

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble035;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble035
    ON public.table_f4 USING btree
    (fdouble035 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble036

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble036;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble036
    ON public.table_f4 USING btree
    (fdouble036 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble037

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble037;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble037
    ON public.table_f4 USING btree
    (fdouble037 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble038

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble038;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble038
    ON public.table_f4 USING btree
    (fdouble038 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble039

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble039;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble039
    ON public.table_f4 USING btree
    (fdouble039 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble040

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble040;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble040
    ON public.table_f4 USING btree
    (fdouble040 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble041

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble041;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble041
    ON public.table_f4 USING btree
    (fdouble041 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble042

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble042;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble042
    ON public.table_f4 USING btree
    (fdouble042 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble043

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble043;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble043
    ON public.table_f4 USING btree
    (fdouble043 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble044

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble044;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble044
    ON public.table_f4 USING btree
    (fdouble044 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble045

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble045;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble045
    ON public.table_f4 USING btree
    (fdouble045 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble046

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble046;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble046
    ON public.table_f4 USING btree
    (fdouble046 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble047

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble047;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble047
    ON public.table_f4 USING btree
    (fdouble047 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble048

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble048;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble048
    ON public.table_f4 USING btree
    (fdouble048 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble049

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble049;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble049
    ON public.table_f4 USING btree
    (fdouble049 ASC NULLS LAST)
    TABLESPACE pg_default;
-- Index: table_f4_idx_fdouble050

-- DROP INDEX IF EXISTS public.table_f4_idx_fdouble050;

CREATE INDEX IF NOT EXISTS table_f4_idx_fdouble050
    ON public.table_f4 USING btree
    (fdouble050 ASC NULLS LAST)
    TABLESPACE pg_default;
	