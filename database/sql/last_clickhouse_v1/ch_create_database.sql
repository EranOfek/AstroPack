/*

  Title: ClickHouse SQL Script - Create New Database.
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

CREATE DATABASE last_pipeline ENGINE = Atomic;


--- END OF FILE ---
