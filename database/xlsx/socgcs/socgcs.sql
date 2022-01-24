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
uuid VARCHAR NOT NULL,
object_name VARCHAR,
event_type VARCHAR,
description VARCHAR,
start_time TIMESTAMP,
end_time TIMESTAMP,
ack_time TIMESTAMP,
reset_time TIMESTAMP,
color INTEGER DEFAULT 0,
bkg INTEGER DEFAULT 0,
new_event BOOLEAN,
params VARCHAR,

CONSTRAINT gcs_events_pkey PRIMARY KEY(uuid)
);

CREATE INDEX gcs_events_idx_object_name ON public.gcs_events
  USING btree (object_name);

CREATE INDEX gcs_events_idx_event_type ON public.gcs_events
  USING btree (event_type);

CREATE INDEX gcs_events_idx_start_time ON public.gcs_events
  USING btree (start_time);

CREATE INDEX gcs_events_idx_new_event ON public.gcs_events
  USING btree (new_event);

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
id SERIAL NOT NULL,
time TIMESTAMP,
type VARCHAR,
text VARCHAR,
color INTEGER DEFAULT 0,
bkg INTEGER DEFAULT 0,

CONSTRAINT gcs_log_pkey PRIMARY KEY(id)
);

CREATE INDEX gcs_log_idx_time ON public.gcs_log
  USING btree (time);

CREATE INDEX gcs_log_idx_type ON public.gcs_log
  USING btree (type);

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
msg_id VARCHAR,
msg_time TIMESTAMP,
msg_type VARCHAR,
source VARCHAR,
task_id VARCHAR,
org_msg_id VARCHAR,
create_time TIMESTAMP,
send_time TIMESTAMP,
rcv_time TIMESTAMP,
expected_ack_time TIMESTAMP,
expected_response_time TIMESTAMP,
ack_time TIMESTAMP,
response_time TIMESTAMP,
image_time TIMESTAMP,
send_pending BOOLEAN,
try_num ,
max_try ,
sim BOOLEAN,
ack_ok BOOLEAN,
ack_received BOOLEAN,
response_received BOOLEAN,
response_status ,
xml_text VARCHAR,
yml_text VARCHAR,
FileName VARCHAR,

CONSTRAINT gcs_msgs_pkey PRIMARY KEY(uuid)
);

CREATE INDEX gcs_msgs_idx_msg_id ON public.gcs_msgs
  USING btree (msg_id);

CREATE INDEX gcs_msgs_idx_msg_time ON public.gcs_msgs
  USING btree (msg_time);

CREATE INDEX gcs_msgs_idx_msg_type ON public.gcs_msgs
  USING btree (msg_type);

CREATE INDEX gcs_msgs_idx_source ON public.gcs_msgs
  USING btree (source);

CREATE INDEX gcs_msgs_idx_task_id ON public.gcs_msgs
  USING btree (task_id);

CREATE INDEX gcs_msgs_idx_org_msg_id ON public.gcs_msgs
  USING btree (org_msg_id);

CREATE INDEX gcs_msgs_idx_create_time ON public.gcs_msgs
  USING btree (create_time);

CREATE INDEX gcs_msgs_idx_send_time ON public.gcs_msgs
  USING btree (send_time);

CREATE INDEX gcs_msgs_idx_rcv_time ON public.gcs_msgs
  USING btree (rcv_time);

CREATE INDEX gcs_msgs_idx_expected_ack_time ON public.gcs_msgs
  USING btree (expected_ack_time);

CREATE INDEX gcs_msgs_idx_expected_response_time ON public.gcs_msgs
  USING btree (expected_response_time);

CREATE INDEX gcs_msgs_idx_ack_time ON public.gcs_msgs
  USING btree (ack_time);

CREATE INDEX gcs_msgs_idx_response_time ON public.gcs_msgs
  USING btree (response_time);

CREATE INDEX gcs_msgs_idx_image_time ON public.gcs_msgs
  USING btree (image_time);

CREATE INDEX gcs_msgs_idx_send_pending ON public.gcs_msgs
  USING btree (send_pending);

CREATE INDEX gcs_msgs_idx_sim ON public.gcs_msgs
  USING btree (sim);

CREATE INDEX gcs_msgs_idx_ack_ok ON public.gcs_msgs
  USING btree (ack_ok);

CREATE INDEX gcs_msgs_idx_ack_received ON public.gcs_msgs
  USING btree (ack_received);

CREATE INDEX gcs_msgs_idx_response_received ON public.gcs_msgs
  USING btree (response_received);

CREATE INDEX gcs_msgs_idx_response_status ON public.gcs_msgs
  USING btree (response_status);

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
file_name VARCHAR,
image_time TIMESTAMP,
valid BOOLEAN,
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

