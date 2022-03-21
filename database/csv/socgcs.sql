--
-- Automatic generated file by xlsx2sql.py
-- Origin file: 
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

CREATE DATABASE socgcs WITH TEMPLATE = template0 ENCODING = 'UTF8';


ALTER DATABASE socgcs OWNER TO postgres;

\connect socgcs

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



-- Source file: D:\Ultrasat\AstroPack.git\database\csv\socgcs - gcs_telemetry.csv
CREATE TABLE public.gcs_telemetry (
pk BIGINT GENERATED ALWAYS AS IDENTITY,
rcv_time DOUBLE PRECISION DEFAULT 0,
param_name VARCHAR,
param_index INTEGER DEFAULT 0,
param_time DOUBLE PRECISION DEFAULT 0,
param_tick BIGINT DEFAULT 0,
value_numeric DOUBLE PRECISION DEFAULT 0,
value_text VARCHAR,
metadata VARCHAR
);

CREATE INDEX gcs_telemetry_idx_rcv_time ON public.gcs_telemetry
  USING btree (rcv_time);

CREATE INDEX gcs_telemetry_idx_param_name ON public.gcs_telemetry
  USING btree (param_name);

CREATE INDEX gcs_telemetry_idx_param_time ON public.gcs_telemetry
  USING btree (param_time);

CREATE INDEX gcs_telemetry_idx_value_numeric ON public.gcs_telemetry
  USING btree (value_numeric);

CREATE INDEX gcs_telemetry_idx_value_text ON public.gcs_telemetry
  USING btree (value_text);

