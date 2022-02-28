--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\perftest.xlsx
--

-- To create the database, run from command line:
-- psql -U postgres -f <dbname>.sql

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.1
-- Dumped by pg_dump version 13.1

-- Started on 2021-04-27 11:16:52

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 2998 (class 1262 OID 16394)
-- Name: pipeline; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE perftest WITH TEMPLATE = template0 ENCODING = 'UTF8';


ALTER DATABASE perftest OWNER TO postgres;

\connect perftest

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;



-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\perftest\csv\perftest - big_table.csv
CREATE TABLE public.big_table (
RecID VARCHAR NOT NULL,
FDouble01 DOUBLE PRECISION DEFAULT 0,
FDouble02 DOUBLE PRECISION DEFAULT 0,
FDouble03 DOUBLE PRECISION DEFAULT 0,
FDouble04 DOUBLE PRECISION DEFAULT 0,
FDouble05 DOUBLE PRECISION DEFAULT 0,
FDouble06 DOUBLE PRECISION DEFAULT 0,
FDouble07 DOUBLE PRECISION DEFAULT 0,
FDouble08 DOUBLE PRECISION DEFAULT 0,
FDouble09 DOUBLE PRECISION DEFAULT 0,
FDouble10 DOUBLE PRECISION DEFAULT 0,
FDouble11 DOUBLE PRECISION DEFAULT 0,
FDouble12 DOUBLE PRECISION DEFAULT 0,
FDouble13 DOUBLE PRECISION DEFAULT 0,
FDouble14 DOUBLE PRECISION DEFAULT 0,
FDouble15 DOUBLE PRECISION DEFAULT 0,
FDouble16 DOUBLE PRECISION DEFAULT 0,
FDouble17 DOUBLE PRECISION DEFAULT 0,
FDouble18 DOUBLE PRECISION DEFAULT 0,
FDouble19 DOUBLE PRECISION DEFAULT 0,
FDouble20 DOUBLE PRECISION DEFAULT 0,
FDouble21 DOUBLE PRECISION DEFAULT 0,
FDouble22 DOUBLE PRECISION DEFAULT 0,
FDouble23 DOUBLE PRECISION DEFAULT 0,
FDouble24 DOUBLE PRECISION DEFAULT 0,
FDouble25 DOUBLE PRECISION DEFAULT 0,
FDouble26 DOUBLE PRECISION DEFAULT 0,
FDouble27 DOUBLE PRECISION DEFAULT 0,
FDouble28 DOUBLE PRECISION DEFAULT 0,
FDouble29 DOUBLE PRECISION DEFAULT 0,
FDouble30 DOUBLE PRECISION DEFAULT 0,
FDouble31 DOUBLE PRECISION DEFAULT 0,
FDouble32 DOUBLE PRECISION DEFAULT 0,
FDouble33 DOUBLE PRECISION DEFAULT 0,
FDouble34 DOUBLE PRECISION DEFAULT 0,
FDouble35 DOUBLE PRECISION DEFAULT 0,
FDouble36 DOUBLE PRECISION DEFAULT 0,
FDouble37 DOUBLE PRECISION DEFAULT 0,
FDouble38 DOUBLE PRECISION DEFAULT 0,
FDouble39 DOUBLE PRECISION DEFAULT 0,
FDouble40 DOUBLE PRECISION DEFAULT 0,
FDouble41 DOUBLE PRECISION DEFAULT 0,
FDouble42 DOUBLE PRECISION DEFAULT 0,
FDouble43 DOUBLE PRECISION DEFAULT 0,
FDouble44 DOUBLE PRECISION DEFAULT 0,
FDouble45 DOUBLE PRECISION DEFAULT 0,
FDouble46 DOUBLE PRECISION DEFAULT 0,
FDouble47 DOUBLE PRECISION DEFAULT 0,
FDouble48 DOUBLE PRECISION DEFAULT 0,
FDouble49 DOUBLE PRECISION DEFAULT 0,
FDouble50 DOUBLE PRECISION DEFAULT 0,

CONSTRAINT big_table_pkey PRIMARY KEY(RecID)
);

CREATE INDEX big_table_idx_FDouble01 ON public.big_table
  USING btree (FDouble01);

CREATE INDEX big_table_idx_FDouble02 ON public.big_table
  USING btree (FDouble02);

CREATE INDEX big_table_idx_FDouble03 ON public.big_table
  USING btree (FDouble03);

CREATE INDEX big_table_idx_FDouble04 ON public.big_table
  USING btree (FDouble04);

CREATE INDEX big_table_idx_FDouble05 ON public.big_table
  USING btree (FDouble05);

CREATE INDEX big_table_idx_FDouble06 ON public.big_table
  USING btree (FDouble06);

CREATE INDEX big_table_idx_FDouble07 ON public.big_table
  USING btree (FDouble07);

CREATE INDEX big_table_idx_FDouble08 ON public.big_table
  USING btree (FDouble08);

CREATE INDEX big_table_idx_FDouble09 ON public.big_table
  USING btree (FDouble09);

CREATE INDEX big_table_idx_FDouble10 ON public.big_table
  USING btree (FDouble10);

CREATE INDEX big_table_idx_FDouble11 ON public.big_table
  USING btree (FDouble11);

CREATE INDEX big_table_idx_FDouble12 ON public.big_table
  USING btree (FDouble12);

CREATE INDEX big_table_idx_FDouble13 ON public.big_table
  USING btree (FDouble13);

CREATE INDEX big_table_idx_FDouble14 ON public.big_table
  USING btree (FDouble14);

CREATE INDEX big_table_idx_FDouble15 ON public.big_table
  USING btree (FDouble15);

CREATE INDEX big_table_idx_FDouble16 ON public.big_table
  USING btree (FDouble16);

CREATE INDEX big_table_idx_FDouble17 ON public.big_table
  USING btree (FDouble17);

CREATE INDEX big_table_idx_FDouble18 ON public.big_table
  USING btree (FDouble18);

CREATE INDEX big_table_idx_FDouble19 ON public.big_table
  USING btree (FDouble19);

CREATE INDEX big_table_idx_FDouble20 ON public.big_table
  USING btree (FDouble20);

CREATE INDEX big_table_idx_FDouble21 ON public.big_table
  USING btree (FDouble21);

CREATE INDEX big_table_idx_FDouble22 ON public.big_table
  USING btree (FDouble22);

CREATE INDEX big_table_idx_FDouble23 ON public.big_table
  USING btree (FDouble23);

CREATE INDEX big_table_idx_FDouble24 ON public.big_table
  USING btree (FDouble24);

CREATE INDEX big_table_idx_FDouble25 ON public.big_table
  USING btree (FDouble25);

CREATE INDEX big_table_idx_FDouble26 ON public.big_table
  USING btree (FDouble26);

CREATE INDEX big_table_idx_FDouble27 ON public.big_table
  USING btree (FDouble27);

CREATE INDEX big_table_idx_FDouble28 ON public.big_table
  USING btree (FDouble28);

CREATE INDEX big_table_idx_FDouble29 ON public.big_table
  USING btree (FDouble29);

CREATE INDEX big_table_idx_FDouble30 ON public.big_table
  USING btree (FDouble30);

CREATE INDEX big_table_idx_FDouble31 ON public.big_table
  USING btree (FDouble31);

CREATE INDEX big_table_idx_FDouble32 ON public.big_table
  USING btree (FDouble32);

CREATE INDEX big_table_idx_FDouble33 ON public.big_table
  USING btree (FDouble33);

CREATE INDEX big_table_idx_FDouble34 ON public.big_table
  USING btree (FDouble34);

CREATE INDEX big_table_idx_FDouble35 ON public.big_table
  USING btree (FDouble35);

CREATE INDEX big_table_idx_FDouble36 ON public.big_table
  USING btree (FDouble36);

CREATE INDEX big_table_idx_FDouble37 ON public.big_table
  USING btree (FDouble37);

CREATE INDEX big_table_idx_FDouble38 ON public.big_table
  USING btree (FDouble38);

CREATE INDEX big_table_idx_FDouble39 ON public.big_table
  USING btree (FDouble39);

CREATE INDEX big_table_idx_FDouble40 ON public.big_table
  USING btree (FDouble40);

CREATE INDEX big_table_idx_FDouble41 ON public.big_table
  USING btree (FDouble41);

CREATE INDEX big_table_idx_FDouble42 ON public.big_table
  USING btree (FDouble42);

CREATE INDEX big_table_idx_FDouble43 ON public.big_table
  USING btree (FDouble43);

CREATE INDEX big_table_idx_FDouble44 ON public.big_table
  USING btree (FDouble44);

CREATE INDEX big_table_idx_FDouble45 ON public.big_table
  USING btree (FDouble45);

CREATE INDEX big_table_idx_FDouble46 ON public.big_table
  USING btree (FDouble46);

CREATE INDEX big_table_idx_FDouble47 ON public.big_table
  USING btree (FDouble47);

CREATE INDEX big_table_idx_FDouble48 ON public.big_table
  USING btree (FDouble48);

CREATE INDEX big_table_idx_FDouble49 ON public.big_table
  USING btree (FDouble49);

CREATE INDEX big_table_idx_FDouble50 ON public.big_table
  USING btree (FDouble50);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\perftest\csv\perftest - table_a.csv
CREATE TABLE public.table_a (
pk1 INTEGER NOT NULL,
pk2 INTEGER DEFAULT 0,
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
fdouble050 DOUBLE PRECISION DEFAULT 0,

CONSTRAINT table_a_pkey PRIMARY KEY(pk1)
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\perftest\csv\perftest - table_b.csv
CREATE TABLE public.table_b (
pk1 INTEGER NOT NULL,
pk2 INTEGER NOT NULL,
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
fdouble050 DOUBLE PRECISION DEFAULT 0,

CONSTRAINT table_b_pkey PRIMARY KEY(pk1, pk2)
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\perftest\csv\perftest - table_c.csv
CREATE TABLE public.table_c (
pk1 BIGINT NOT NULL,
pk2 BIGINT NOT NULL,
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
fdouble050 DOUBLE PRECISION DEFAULT 0,

CONSTRAINT table_c_pkey PRIMARY KEY(pk1, pk2)
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\perftest\csv\perftest - table_d.csv
CREATE TABLE public.table_d (
pk1 INTEGER NOT NULL,
pk2 INTEGER NOT NULL,
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
fdouble050 DOUBLE PRECISION DEFAULT 0,

CONSTRAINT table_d_pkey PRIMARY KEY(pk1, pk2)
);

CREATE INDEX table_d_idx_fdouble001 ON public.table_d
  USING btree (fdouble001);

CREATE INDEX table_d_idx_fdouble002 ON public.table_d
  USING btree (fdouble002);

CREATE INDEX table_d_idx_fdouble003 ON public.table_d
  USING btree (fdouble003);

CREATE INDEX table_d_idx_fdouble004 ON public.table_d
  USING btree (fdouble004);

CREATE INDEX table_d_idx_fdouble005 ON public.table_d
  USING btree (fdouble005);

CREATE INDEX table_d_idx_fdouble006 ON public.table_d
  USING btree (fdouble006);

CREATE INDEX table_d_idx_fdouble007 ON public.table_d
  USING btree (fdouble007);

CREATE INDEX table_d_idx_fdouble008 ON public.table_d
  USING btree (fdouble008);

CREATE INDEX table_d_idx_fdouble009 ON public.table_d
  USING btree (fdouble009);

CREATE INDEX table_d_idx_fdouble010 ON public.table_d
  USING btree (fdouble010);

CREATE INDEX table_d_idx_fdouble011 ON public.table_d
  USING btree (fdouble011);

CREATE INDEX table_d_idx_fdouble012 ON public.table_d
  USING btree (fdouble012);

CREATE INDEX table_d_idx_fdouble013 ON public.table_d
  USING btree (fdouble013);

CREATE INDEX table_d_idx_fdouble014 ON public.table_d
  USING btree (fdouble014);

CREATE INDEX table_d_idx_fdouble015 ON public.table_d
  USING btree (fdouble015);

CREATE INDEX table_d_idx_fdouble016 ON public.table_d
  USING btree (fdouble016);

CREATE INDEX table_d_idx_fdouble017 ON public.table_d
  USING btree (fdouble017);

CREATE INDEX table_d_idx_fdouble018 ON public.table_d
  USING btree (fdouble018);

CREATE INDEX table_d_idx_fdouble019 ON public.table_d
  USING btree (fdouble019);

CREATE INDEX table_d_idx_fdouble020 ON public.table_d
  USING btree (fdouble020);

CREATE INDEX table_d_idx_fdouble021 ON public.table_d
  USING btree (fdouble021);

CREATE INDEX table_d_idx_fdouble022 ON public.table_d
  USING btree (fdouble022);

CREATE INDEX table_d_idx_fdouble023 ON public.table_d
  USING btree (fdouble023);

CREATE INDEX table_d_idx_fdouble024 ON public.table_d
  USING btree (fdouble024);

CREATE INDEX table_d_idx_fdouble025 ON public.table_d
  USING btree (fdouble025);

CREATE INDEX table_d_idx_fdouble026 ON public.table_d
  USING btree (fdouble026);

CREATE INDEX table_d_idx_fdouble027 ON public.table_d
  USING btree (fdouble027);

CREATE INDEX table_d_idx_fdouble028 ON public.table_d
  USING btree (fdouble028);

CREATE INDEX table_d_idx_fdouble029 ON public.table_d
  USING btree (fdouble029);

CREATE INDEX table_d_idx_fdouble030 ON public.table_d
  USING btree (fdouble030);

CREATE INDEX table_d_idx_fdouble031 ON public.table_d
  USING btree (fdouble031);

CREATE INDEX table_d_idx_fdouble032 ON public.table_d
  USING btree (fdouble032);

CREATE INDEX table_d_idx_fdouble033 ON public.table_d
  USING btree (fdouble033);

CREATE INDEX table_d_idx_fdouble034 ON public.table_d
  USING btree (fdouble034);

CREATE INDEX table_d_idx_fdouble035 ON public.table_d
  USING btree (fdouble035);

CREATE INDEX table_d_idx_fdouble036 ON public.table_d
  USING btree (fdouble036);

CREATE INDEX table_d_idx_fdouble037 ON public.table_d
  USING btree (fdouble037);

CREATE INDEX table_d_idx_fdouble038 ON public.table_d
  USING btree (fdouble038);

CREATE INDEX table_d_idx_fdouble039 ON public.table_d
  USING btree (fdouble039);

CREATE INDEX table_d_idx_fdouble040 ON public.table_d
  USING btree (fdouble040);

CREATE INDEX table_d_idx_fdouble041 ON public.table_d
  USING btree (fdouble041);

CREATE INDEX table_d_idx_fdouble042 ON public.table_d
  USING btree (fdouble042);

CREATE INDEX table_d_idx_fdouble043 ON public.table_d
  USING btree (fdouble043);

CREATE INDEX table_d_idx_fdouble044 ON public.table_d
  USING btree (fdouble044);

CREATE INDEX table_d_idx_fdouble045 ON public.table_d
  USING btree (fdouble045);

CREATE INDEX table_d_idx_fdouble046 ON public.table_d
  USING btree (fdouble046);

CREATE INDEX table_d_idx_fdouble047 ON public.table_d
  USING btree (fdouble047);

CREATE INDEX table_d_idx_fdouble048 ON public.table_d
  USING btree (fdouble048);

CREATE INDEX table_d_idx_fdouble049 ON public.table_d
  USING btree (fdouble049);

CREATE INDEX table_d_idx_fdouble050 ON public.table_d
  USING btree (fdouble050);

