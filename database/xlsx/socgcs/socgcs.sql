--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs.xlsx
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



-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_events.csv
CREATE TABLE public.gcs_events (
uuid VARCHAR,
rcv_time TIMESTAMP,
FileName VARCHAR,
ImageTime TIMESTAMP,
Valid BOOLEAN
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_images.csv
CREATE TABLE public.gcs_images (
uuid VARCHAR NOT NULL,
image_id VARCHAR,
task_id VARCHAR,
image_time TIMESTAMP,
rcv_time TIMESTAMP,
file_name VARCHAR,
ImageTime TIMESTAMP,
valid BOOLEAN,

CONSTRAINT gcs_images_pkey PRIMARY KEY(uuid)
);

CREATE INDEX gcs_images_idx_image_id ON public.gcs_images
  USING btree (image_id);

CREATE INDEX gcs_images_idx_task_id ON public.gcs_images
  USING btree (task_id);

CREATE INDEX gcs_images_idx_image_time ON public.gcs_images
  USING btree (image_time);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_log.csv
CREATE TABLE public.gcs_log (
id ,
time TIMESTAMP,
type VARCHAR,
text VARCHAR
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_maintenance.csv
CREATE TABLE public.gcs_maintenance (
pk_id VARCHAR,
serial INTEGER DEFAULT 0,
rcv_time TIMESTAMP,
maint_type VARCHAR,
window_start TIMESTAMP,
window_end TIMESTAMP,
duration TIMESTAMP,
final_start TIMESTAMP,
approval_pending BOOLEAN,
approval_time TIMESTAMP,
approval_user VARCHAR
);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_msgs.csv
CREATE TABLE public.gcs_msgs (
uuid VARCHAR NOT NULL,
create_time TIMESTAMP,
send_time TIMESTAMP,
rcv_time TIMESTAMP,
FileName VARCHAR,
ImageTime TIMESTAMP,
Valid BOOLEAN,
ack_received BOOLEAN,
try_num ,
max_try ,
pending BOOLEAN,

CONSTRAINT gcs_msgs_pkey PRIMARY KEY(uuid)
);

CREATE INDEX gcs_msgs_idx_create_time ON public.gcs_msgs
  USING btree (create_time);

CREATE INDEX gcs_msgs_idx_send_time ON public.gcs_msgs
  USING btree (send_time);

CREATE INDEX gcs_msgs_idx_rcv_time ON public.gcs_msgs
  USING btree (rcv_time);

CREATE INDEX gcs_msgs_idx_ack_received ON public.gcs_msgs
  USING btree (ack_received);

CREATE INDEX gcs_msgs_idx_pending ON public.gcs_msgs
  USING btree (pending);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_state.csv
CREATE TABLE public.gcs_state (
uuid VARCHAR NOT NULL,
key VARCHAR,
value VARCHAR,
update_time TIMESTAMP,

CONSTRAINT gcs_state_pkey PRIMARY KEY(uuid)
);

CREATE INDEX gcs_state_idx_key ON public.gcs_state
  USING btree (key);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_tasks.csv
CREATE TABLE public.gcs_tasks (
uuid VARCHAR NOT NULL,
task_id VARCHAR NOT NULL,
task_time DOUBLE PRECISION DEFAULT 0,
rcv_time TIMESTAMP,
FileName VARCHAR,
ImageTime TIMESTAMP,
Valid BOOLEAN,
task_yml VARCHAR,
response_yml VARCHAR,
new_flag BOOLEAN,

CONSTRAINT gcs_tasks_pkey PRIMARY KEY(uuid, task_id)
);

CREATE INDEX gcs_tasks_idx_new_flag ON public.gcs_tasks
  USING btree (new_flag);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_telemetry.csv
CREATE TABLE public.gcs_telemetry (
uuid VARCHAR NOT NULL,
time TIMESTAMP,
text VARCHAR,
metadata VARCHAR,

CONSTRAINT gcs_telemetry_pkey PRIMARY KEY(uuid)
);

CREATE INDEX gcs_telemetry_idx_time ON public.gcs_telemetry
  USING btree (time);

