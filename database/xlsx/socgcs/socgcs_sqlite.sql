--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs.xlsx
--

--
-- FirebirdSQL database
--

-- When executing the script on existing database, remove this 'CREATE DATABASE' statement
-- Use 'DB Browser for SQLite' as GUI for SQLite
CREATE DATABASE socgcs USER 'SYSDBA'
   PAGE_SIZE 4096
   DEFAULT CHARACTER SET UTF8;


-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_events.csv
CREATE TABLE gcs_events (
uuid TEXT NOT NULL,
object_name TEXT,
event_type TEXT,
description TEXT,
start_time REAL,
end_time REAL,
ack_time REAL,
reset_time REAL,
color INTEGER,
bkg INTEGER,
new_event INTEGER,
params TEXT,

PRIMARY KEY(uuid)
);

CREATE INDEX gcs_events_idx_object_name ON gcs_events(object_name);
CREATE INDEX gcs_events_idx_event_type ON gcs_events(event_type);
CREATE INDEX gcs_events_idx_start_time ON gcs_events(start_time);
CREATE INDEX gcs_events_idx_new_event ON gcs_events(new_event);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_images.csv
CREATE TABLE gcs_images (
uuid TEXT NOT NULL,
image_id TEXT,
task_id TEXT,
image_time REAL,
rcv_time REAL,
file_name TEXT,
ImageTime REAL,
valid INTEGER,

PRIMARY KEY(uuid)
);

CREATE INDEX gcs_images_idx_image_id ON gcs_images(image_id);
CREATE INDEX gcs_images_idx_task_id ON gcs_images(task_id);
CREATE INDEX gcs_images_idx_image_time ON gcs_images(image_time);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_log.csv
CREATE TABLE gcs_log (
id ROWID NOT NULL,
time REAL,
type TEXT,
text TEXT,
color INTEGER,
bkg INTEGER,

PRIMARY KEY(id)
);

CREATE INDEX gcs_log_idx_time ON gcs_log(time);
CREATE INDEX gcs_log_idx_type ON gcs_log(type);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_maintenance.csv
CREATE TABLE gcs_maintenance (
pk_id TEXT,
serial INTEGER,
rcv_time REAL,
maint_type TEXT,
window_start REAL,
window_end REAL,
duration REAL,
final_start REAL,
approval_pending INTEGER,
approval_time REAL,
approval_user TEXT
);


-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_msgs.csv
CREATE TABLE gcs_msgs (
uuid TEXT NOT NULL,
msg_id TEXT,
msg_time TEXT,
msg_type TEXT,
source TEXT,
task_id TEXT,
org_msg_id TEXT,
create_time REAL,
send_time REAL,
rcv_time REAL,
expected_ack_time REAL,
expected_response_time REAL,
ack_time REAL,
response_time REAL,
image_time REAL,
send_pending INTEGER,
try_num ,
max_try ,
sim INTEGER,
ack_ok INTEGER,
ack_received INTEGER,
response_received INTEGER,
response_status ,
xml_text TEXT,
yml_text TEXT,

PRIMARY KEY(uuid)
);

CREATE INDEX gcs_msgs_idx_msg_id ON gcs_msgs(msg_id);
CREATE INDEX gcs_msgs_idx_msg_time ON gcs_msgs(msg_time);
CREATE INDEX gcs_msgs_idx_msg_type ON gcs_msgs(msg_type);
CREATE INDEX gcs_msgs_idx_source ON gcs_msgs(source);
CREATE INDEX gcs_msgs_idx_task_id ON gcs_msgs(task_id);
CREATE INDEX gcs_msgs_idx_org_msg_id ON gcs_msgs(org_msg_id);
CREATE INDEX gcs_msgs_idx_create_time ON gcs_msgs(create_time);
CREATE INDEX gcs_msgs_idx_send_time ON gcs_msgs(send_time);
CREATE INDEX gcs_msgs_idx_rcv_time ON gcs_msgs(rcv_time);
CREATE INDEX gcs_msgs_idx_expected_ack_time ON gcs_msgs(expected_ack_time);
CREATE INDEX gcs_msgs_idx_expected_response_time ON gcs_msgs(expected_response_time);
CREATE INDEX gcs_msgs_idx_ack_time ON gcs_msgs(ack_time);
CREATE INDEX gcs_msgs_idx_response_time ON gcs_msgs(response_time);
CREATE INDEX gcs_msgs_idx_image_time ON gcs_msgs(image_time);
CREATE INDEX gcs_msgs_idx_send_pending ON gcs_msgs(send_pending);
CREATE INDEX gcs_msgs_idx_sim ON gcs_msgs(sim);
CREATE INDEX gcs_msgs_idx_ack_ok ON gcs_msgs(ack_ok);
CREATE INDEX gcs_msgs_idx_ack_received ON gcs_msgs(ack_received);
CREATE INDEX gcs_msgs_idx_response_received ON gcs_msgs(response_received);
CREATE INDEX gcs_msgs_idx_response_status ON gcs_msgs(response_status);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_state.csv
CREATE TABLE gcs_state (
uuid TEXT NOT NULL,
key TEXT,
value TEXT,
update_time REAL,

PRIMARY KEY(uuid)
);

CREATE INDEX gcs_state_idx_key ON gcs_state(key);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_tasks.csv
CREATE TABLE gcs_tasks (
uuid TEXT NOT NULL,
task_id TEXT NOT NULL,
task_time REAL,
rcv_time REAL,
file_name TEXT,
image_time REAL,
valid INTEGER,
task_yml TEXT,
response_yml TEXT,
new_flag INTEGER,

PRIMARY KEY(uuid, task_id)
);

CREATE INDEX gcs_tasks_idx_new_flag ON gcs_tasks(new_flag);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\socgcs\csv\socgcs - gcs_telemetry.csv
CREATE TABLE gcs_telemetry (
uuid TEXT NOT NULL,
time REAL,
text TEXT,
metadata TEXT,

PRIMARY KEY(uuid)
);

CREATE INDEX gcs_telemetry_idx_time ON gcs_telemetry(time);

