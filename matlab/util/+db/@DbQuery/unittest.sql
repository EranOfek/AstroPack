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

