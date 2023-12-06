--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest.xlsx
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

CREATE DATABASE unittest WITH TEMPLATE = template0 ENCODING = 'UTF8';


ALTER DATABASE unittest OWNER TO postgres;

\connect unittest

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



-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - big_table.csv
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

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - big_table2.csv
CREATE TABLE public.big_table2 (
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

CONSTRAINT big_table2_pkey PRIMARY KEY(RecID)
);

CREATE INDEX big_table2_idx_FDouble01 ON public.big_table2
  USING btree (FDouble01);

CREATE INDEX big_table2_idx_FDouble02 ON public.big_table2
  USING btree (FDouble02);

CREATE INDEX big_table2_idx_FDouble03 ON public.big_table2
  USING btree (FDouble03);

CREATE INDEX big_table2_idx_FDouble04 ON public.big_table2
  USING btree (FDouble04);

CREATE INDEX big_table2_idx_FDouble05 ON public.big_table2
  USING btree (FDouble05);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - details_table.csv
CREATE TABLE public.details_table (
RecID VARCHAR NOT NULL,
InsertTime TIMESTAMP,
UpdateTime TIMESTAMP,
FInt INTEGER DEFAULT 0,
FBigInt BIGINT DEFAULT 0,
FBool BOOLEAN,
FDouble DOUBLE PRECISION DEFAULT 0,
FTimestamp TIMESTAMP,
FString VARCHAR,
FDouble1 DOUBLE PRECISION DEFAULT 0,
FDouble2 DOUBLE PRECISION DEFAULT 0,
FDouble3 DOUBLE PRECISION DEFAULT 0,
FDouble4 DOUBLE PRECISION DEFAULT 0,
FDouble5 DOUBLE PRECISION DEFAULT 0,
FDouble6 DOUBLE PRECISION DEFAULT 0,
FDouble7 DOUBLE PRECISION DEFAULT 0,
FDouble8 DOUBLE PRECISION DEFAULT 0,
FDouble9 DOUBLE PRECISION DEFAULT 0,
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
FInt1 INTEGER DEFAULT 0,
FInt2 INTEGER DEFAULT 0,
FInt3 INTEGER DEFAULT 0,
FInt4 INTEGER DEFAULT 0,
FInt5 INTEGER DEFAULT 0,
FInt6 INTEGER DEFAULT 0,
FInt7 INTEGER DEFAULT 0,
FInt8 INTEGER DEFAULT 0,
FInt9 INTEGER DEFAULT 0,
FInt10 INTEGER DEFAULT 0,
FString1 VARCHAR,
FString2 VARCHAR,
FString3 VARCHAR,
FString4 VARCHAR,
FString5 VARCHAR,
FString6 VARCHAR,
FString7 VARCHAR,
FString8 VARCHAR,
FString9 VARCHAR,
FString10 VARCHAR,
dstring1 VARCHAR,
dstring2 VARCHAR,
dstring3 VARCHAR,

CONSTRAINT details_table_pkey PRIMARY KEY(RecID)
);

CREATE INDEX details_table_idx_InsertTime ON public.details_table
  USING btree (InsertTime);

CREATE INDEX details_table_idx_UpdateTime ON public.details_table
  USING btree (UpdateTime);

CREATE INDEX details_table_idx_FInt ON public.details_table
  USING btree (FInt);

CREATE INDEX details_table_idx_FBigInt ON public.details_table
  USING btree (FBigInt);

CREATE INDEX details_table_idx_FBool ON public.details_table
  USING btree (FBool);

CREATE INDEX details_table_idx_FDouble ON public.details_table
  USING btree (FDouble);

CREATE INDEX details_table_idx_FTimestamp ON public.details_table
  USING btree (FTimestamp);

CREATE INDEX details_table_idx_FString ON public.details_table
  USING btree (FString);

CREATE INDEX details_table_idx_FDouble1 ON public.details_table
  USING btree (FDouble1);

CREATE INDEX details_table_idx_FDouble2 ON public.details_table
  USING btree (FDouble2);

CREATE INDEX details_table_idx_FDouble3 ON public.details_table
  USING btree (FDouble3);

CREATE INDEX details_table_idx_FDouble4 ON public.details_table
  USING btree (FDouble4);

CREATE INDEX details_table_idx_FInt1 ON public.details_table
  USING btree (FInt1);

CREATE INDEX details_table_idx_FInt2 ON public.details_table
  USING btree (FInt2);

CREATE INDEX details_table_idx_FInt3 ON public.details_table
  USING btree (FInt3);

CREATE INDEX details_table_idx_FInt4 ON public.details_table
  USING btree (FInt4);

CREATE INDEX details_table_idx_FString1 ON public.details_table
  USING btree (FString1);

CREATE INDEX details_table_idx_FString2 ON public.details_table
  USING btree (FString2);

CREATE INDEX details_table_idx_FString3 ON public.details_table
  USING btree (FString3);

CREATE INDEX details_table_idx_FString4 ON public.details_table
  USING btree (FString4);

CREATE INDEX details_table_idx_dstring1 ON public.details_table
  USING btree (dstring1);

CREATE INDEX details_table_idx_dstring2 ON public.details_table
  USING btree (dstring2);

CREATE INDEX details_table_idx_dstring3 ON public.details_table
  USING btree (dstring3);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - master_table.csv
CREATE TABLE public.master_table (
RecID VARCHAR NOT NULL,
InsertTime TIMESTAMP,
UpdateTime TIMESTAMP,
FInt INTEGER DEFAULT 0,
FBigInt BIGINT DEFAULT 0,
FBool BOOLEAN,
FDouble DOUBLE PRECISION DEFAULT 0,
FTimestamp TIMESTAMP,
FString VARCHAR,
FDouble1 DOUBLE PRECISION DEFAULT 0,
FDouble2 DOUBLE PRECISION DEFAULT 0,
FDouble3 DOUBLE PRECISION DEFAULT 0,
FDouble4 DOUBLE PRECISION DEFAULT 0,
FDouble5 DOUBLE PRECISION DEFAULT 0,
FDouble6 DOUBLE PRECISION DEFAULT 0,
FDouble7 DOUBLE PRECISION DEFAULT 0,
FDouble8 DOUBLE PRECISION DEFAULT 0,
FDouble9 DOUBLE PRECISION DEFAULT 0,
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
FInt1 INTEGER DEFAULT 0,
FInt2 INTEGER DEFAULT 0,
FInt3 INTEGER DEFAULT 0,
FInt4 INTEGER DEFAULT 0,
FInt5 INTEGER DEFAULT 0,
FInt6 INTEGER DEFAULT 0,
FInt7 INTEGER DEFAULT 0,
FInt8 INTEGER DEFAULT 0,
FInt9 INTEGER DEFAULT 0,
FInt10 INTEGER DEFAULT 0,
FString1 VARCHAR,
FString2 VARCHAR,
FString3 VARCHAR,
FString4 VARCHAR,
FString5 VARCHAR,
FString6 VARCHAR,
FString7 VARCHAR,
FString8 VARCHAR,
FString9 VARCHAR,
FString10 VARCHAR,
mstring1 VARCHAR,
mstring2 VARCHAR,
mstring3 VARCHAR,

CONSTRAINT master_table_pkey PRIMARY KEY(RecID)
);

CREATE INDEX master_table_idx_InsertTime ON public.master_table
  USING btree (InsertTime);

CREATE INDEX master_table_idx_UpdateTime ON public.master_table
  USING btree (UpdateTime);

CREATE INDEX master_table_idx_FInt ON public.master_table
  USING btree (FInt);

CREATE INDEX master_table_idx_FBigInt ON public.master_table
  USING btree (FBigInt);

CREATE INDEX master_table_idx_FBool ON public.master_table
  USING btree (FBool);

CREATE INDEX master_table_idx_FDouble ON public.master_table
  USING btree (FDouble);

CREATE INDEX master_table_idx_FTimestamp ON public.master_table
  USING btree (FTimestamp);

CREATE INDEX master_table_idx_FString ON public.master_table
  USING btree (FString);

CREATE INDEX master_table_idx_FDouble1 ON public.master_table
  USING btree (FDouble1);

CREATE INDEX master_table_idx_FDouble2 ON public.master_table
  USING btree (FDouble2);

CREATE INDEX master_table_idx_FDouble3 ON public.master_table
  USING btree (FDouble3);

CREATE INDEX master_table_idx_FDouble4 ON public.master_table
  USING btree (FDouble4);

CREATE INDEX master_table_idx_FInt1 ON public.master_table
  USING btree (FInt1);

CREATE INDEX master_table_idx_FInt2 ON public.master_table
  USING btree (FInt2);

CREATE INDEX master_table_idx_FInt3 ON public.master_table
  USING btree (FInt3);

CREATE INDEX master_table_idx_FInt4 ON public.master_table
  USING btree (FInt4);

CREATE INDEX master_table_idx_FString1 ON public.master_table
  USING btree (FString1);

CREATE INDEX master_table_idx_FString2 ON public.master_table
  USING btree (FString2);

CREATE INDEX master_table_idx_FString3 ON public.master_table
  USING btree (FString3);

CREATE INDEX master_table_idx_FString4 ON public.master_table
  USING btree (FString4);

CREATE INDEX master_table_idx_mstring1 ON public.master_table
  USING btree (mstring1);

CREATE INDEX master_table_idx_mstring2 ON public.master_table
  USING btree (mstring2);

CREATE INDEX master_table_idx_mstring3 ON public.master_table
  USING btree (mstring3);

