--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui.xlsx
--

--
-- SQLite database
--
-- Execute by command line:
--      sqlite3 db_file_name.sqlite < script.sql

-- When executing the script on existing database, remove this 'CREATE DATABASE' statement
-- Use 'DB Browser for SQLite' as GUI for SQLite

-- CREATE DATABASE socgcs_gui USER 'SYSDBA'
--    PAGE_SIZE 4096
--    DEFAULT CHARACTER SET UTF8;


-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui\csv\socgcs_gui - gcs_imaging_targets.csv
CREATE TABLE gcs_imaging_targets (
pk TEXT NOT NULL,
idx INTEGER,
task_id TEXT,
target_id TEXT,
start_time REAL,
coord_ra REAL,
coord_dec REAL,
coord_roll REAL,
exposure REAL,
image_count INTEGER,
tiles TEXT,

PRIMARY KEY(pk)
);


-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui\csv\socgcs_gui - gcs_imaging_tasks.csv
CREATE TABLE gcs_imaging_tasks (
pk TEXT NOT NULL,
idx INTEGER,
task_id TEXT,
title TEXT,
start_time TEXT,
target_count INTEGER,

PRIMARY KEY(pk)
);

CREATE INDEX gcs_imaging_tasks_idx_task_id ON gcs_imaging_tasks(task_id);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs_gui\csv\socgcs_gui - gcs_msg_header.csv
CREATE TABLE gcs_msg_header (
pk TEXT NOT NULL,
msg_id TEXT,
msg_time REAL,
msg_type TEXT,
source TEXT,
org_msg_id TEXT,
task_id TEXT,

PRIMARY KEY(pk)
);

CREATE INDEX gcs_msg_header_idx_msg_id ON gcs_msg_header(msg_id);

