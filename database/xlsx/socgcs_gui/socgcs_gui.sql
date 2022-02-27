--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui.xlsx
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

CREATE DATABASE socgcs_gui WITH TEMPLATE = template0 ENCODING = 'UTF8';


ALTER DATABASE socgcs_gui OWNER TO postgres;

\connect socgcs_gui

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



-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui\csv\socgcs_gui - gcs_imaging_targets.csv
CREATE TABLE public.gcs_imaging_targets (
pk VARCHAR NOT NULL,
idx INTEGER DEFAULT 0,
task_id VARCHAR,
target_id VARCHAR,
start_time TIMESTAMP,
coord_ra DOUBLE PRECISION DEFAULT 0,
coord_dec DOUBLE PRECISION DEFAULT 0,
coord_roll DOUBLE PRECISION DEFAULT 0,
exposure DOUBLE PRECISION DEFAULT 0,
image_count INTEGER DEFAULT 0,
tiles VARCHAR,

CONSTRAINT gcs_imaging_targets_pkey PRIMARY KEY(pk)
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui\csv\socgcs_gui - gcs_imaging_tasks.csv
CREATE TABLE public.gcs_imaging_tasks (
pk VARCHAR NOT NULL,
idx INTEGER DEFAULT 0,
task_id VARCHAR,
title VARCHAR,
start_time VARCHAR,
target_count INTEGER DEFAULT 0,

CONSTRAINT gcs_imaging_tasks_pkey PRIMARY KEY(pk)
);

CREATE INDEX gcs_imaging_tasks_idx_task_id ON public.gcs_imaging_tasks
  USING btree (task_id);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui\csv\socgcs_gui - gcs_msg_header.csv
CREATE TABLE public.gcs_msg_header (
pk VARCHAR NOT NULL,
msg_id VARCHAR,
msg_time TIMESTAMP,
msg_type VARCHAR,
source VARCHAR,
org_msg_id VARCHAR,
task_id VARCHAR,

CONSTRAINT gcs_msg_header_pkey PRIMARY KEY(pk)
);

CREATE INDEX gcs_msg_header_idx_msg_id ON public.gcs_msg_header
  USING btree (msg_id);

