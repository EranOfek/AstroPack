--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest5.xlsx
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

CREATE DATABASE unittest5 WITH TEMPLATE = template0 ENCODING = 'UTF8';


ALTER DATABASE unittest5 OWNER TO postgres;

\connect unittest5

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



-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest5\csv\unittest5 - details_table.csv
CREATE TABLE public.details_table (
RecID VARCHAR(256) NOT NULL,
InsertTime TIMESTAMP,
UpdateTime TIMESTAMP,
FInt INTEGER DEFAULT 0,
FBigInt BIGINT DEFAULT 0,
FBool BOOLEAN,
FDouble DOUBLE PRECISION DEFAULT 0,
FTimestamp TIMESTAMP,
FString VARCHAR(256),
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
FString1 VARCHAR(256),
FString2 VARCHAR(256),
FString3 VARCHAR(256),
FString4 VARCHAR(256),
FString5 VARCHAR(256),
FString6 VARCHAR(256),
FString7 VARCHAR(256),
FString8 VARCHAR(256),
FString9 VARCHAR(256),
FString10 VARCHAR(256),
dstring1 VARCHAR(256),
dstring2 VARCHAR(256),
dstring3 VARCHAR(256),

CONSTRAINT details_table_pkey PRIMARY KEY(RecID)
);

CREATE INDEX details_table_idx_RecID ON public.details_table
  USING btree (RecID);

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

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest5\csv\unittest5 - master_table.csv
CREATE TABLE public.master_table (
RecID VARCHAR(256) NOT NULL,
InsertTime TIMESTAMP,
UpdateTime TIMESTAMP,
FInt INTEGER DEFAULT 0,
FBigInt BIGINT DEFAULT 0,
FBool BOOLEAN,
FDouble DOUBLE PRECISION DEFAULT 0,
FTimestamp TIMESTAMP,
FString VARCHAR(256),
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
FString1 VARCHAR(256),
FString2 VARCHAR(256),
FString3 VARCHAR(256),
FString4 VARCHAR(256),
FString5 VARCHAR(256),
FString6 VARCHAR(256),
FString7 VARCHAR(256),
FString8 VARCHAR(256),
FString9 VARCHAR(256),
FString10 VARCHAR(256),
mstring1 VARCHAR(256),
mstring2 VARCHAR(256),
mstring3 VARCHAR(256),

CONSTRAINT master_table_pkey PRIMARY KEY(RecID)
);

CREATE INDEX master_table_idx_RecID ON public.master_table
  USING btree (RecID);

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

