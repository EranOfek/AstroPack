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



-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - details_table.csv
CREATE TABLE public.details_table (
RecID VARCHAR NOT NULL,
InsertTime TIMESTAMP,
UpdateTime TIMESTAMP,
FInt INTEGER,
FBigInt BIGINT,
FBool BOOLEAN,
FDouble DOUBLE PRECISION,
FTimestamp TIMESTAMP,
FString VARCHAR,
FDouble1 DOUBLE PRECISION,
FDouble2 DOUBLE PRECISION,
FDouble3 DOUBLE PRECISION,
FDouble4 DOUBLE PRECISION,
FDouble5 DOUBLE PRECISION,
FDouble6 DOUBLE PRECISION,
FDouble7 DOUBLE PRECISION,
FDouble8 DOUBLE PRECISION,
FDouble9 DOUBLE PRECISION,
FDouble10 DOUBLE PRECISION,
FDouble11 DOUBLE PRECISION,
FDouble12 DOUBLE PRECISION,
FDouble13 DOUBLE PRECISION,
FDouble14 DOUBLE PRECISION,
FDouble15 DOUBLE PRECISION,
FDouble16 DOUBLE PRECISION,
FDouble17 DOUBLE PRECISION,
FDouble18 DOUBLE PRECISION,
FDouble19 DOUBLE PRECISION,
FDouble20 DOUBLE PRECISION,
FInt1 INTEGER,
FInt2 INTEGER,
FInt3 INTEGER,
FInt4 INTEGER,
FInt5 INTEGER,
FInt6 INTEGER,
FInt7 INTEGER,
FInt8 INTEGER,
FInt9 INTEGER,
FInt10 INTEGER,
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

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - master_table.csv
CREATE TABLE public.master_table (
RecID VARCHAR NOT NULL,
InsertTime TIMESTAMP,
UpdateTime TIMESTAMP,
FInt INTEGER,
FBigInt BIGINT,
FBool BOOLEAN,
FDouble DOUBLE PRECISION,
FTimestamp TIMESTAMP,
FString VARCHAR,
FDouble1 DOUBLE PRECISION,
FDouble2 DOUBLE PRECISION,
FDouble3 DOUBLE PRECISION,
FDouble4 DOUBLE PRECISION,
FDouble5 DOUBLE PRECISION,
FDouble6 DOUBLE PRECISION,
FDouble7 DOUBLE PRECISION,
FDouble8 DOUBLE PRECISION,
FDouble9 DOUBLE PRECISION,
FDouble10 DOUBLE PRECISION,
FDouble11 DOUBLE PRECISION,
FDouble12 DOUBLE PRECISION,
FDouble13 DOUBLE PRECISION,
FDouble14 DOUBLE PRECISION,
FDouble15 DOUBLE PRECISION,
FDouble16 DOUBLE PRECISION,
FDouble17 DOUBLE PRECISION,
FDouble18 DOUBLE PRECISION,
FDouble19 DOUBLE PRECISION,
FDouble20 DOUBLE PRECISION,
FInt1 INTEGER,
FInt2 INTEGER,
FInt3 INTEGER,
FInt4 INTEGER,
FInt5 INTEGER,
FInt6 INTEGER,
FInt7 INTEGER,
FInt8 INTEGER,
FInt9 INTEGER,
FInt10 INTEGER,
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

