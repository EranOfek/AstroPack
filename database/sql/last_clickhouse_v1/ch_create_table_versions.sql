/*

  Title: ClickHouse SQL Script - Create Table: Versions Manager
  Description: This script creates a new table in the ClickHouse database.
  Use this script with preprocessor.py to replace macros.

  Author:  Chen Tishler
  Created: 22/02/2024
  Updated: 22/02/2024

  Usage:   Execute this script using ClickHouse client, database management tool, or our clickhouse_util.py.
  Example: clickhouse-client --multiquery < this_script.sql

  Notes:
	- Ensure the ClickHouse server is running and accessible.
	- Review and adjust the column data types and table settings according to project requirements.
	- This script assumes [any assumptions, e.g., necessary databases or user permissions are already set up].

  Changelog:
	- 2024-02-22: Initial version. [Chen Tishler]


*/


--- Use database_name.table_name (ClickHouse does not have schemas)
CREATE TABLE IF NOT EXISTS $db_name.versions
(
	ver_id			Int16,			--- Unique version ID, generated in Python code
	pipeline_ver	String,
	processing_num	Int16,
	comment		 	String
)


--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree
ENGINE = MergeTree()

--- A tuple of column names or arbitrary expressions.
--- ClickHouse uses the sorting key as a primary key if the primary key
--- is not defined explicitly by the PRIMARY KEY clause
ORDER BY (ver_id)


--- Policy includes what disk values to use (see /etc/clickhouse-server/config.xml)
SETTINGS storage_policy = 'pipeline_policy';


--- Insert initial data
INSERT INTO $db_name.versions (ver_id, pipeline_ver, processing_num) VALUES (0, '0.0.0', 0);

--- END OF FILE ---
