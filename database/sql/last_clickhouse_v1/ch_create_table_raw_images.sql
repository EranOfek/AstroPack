/*

  Title: ClickHouse SQL Script - Create Table: Raw Images
  Description: This script creates a new table in the ClickHouse database.
  Use this script with preprocessor.py to replace macros.

  Author:  Chen Tishler
  Created: 21/02/2024
  Updated: 22/02/2024

  Usage:   Execute this script using ClickHouse client, database management tool, or our clickhouse_util.py.
  Example: clickhouse-client --multiquery < this_script.sql

  Notes:
	- Ensure the ClickHouse server is running and accessible.
	- Review and adjust the column data types and table settings according to project requirements.
	- This script assumes [any assumptions, e.g., necessary databases or user permissions are already set up].

  Changelog:
	- 2024-02-21: Initial version. [Chen Tishler]


*/


--- Use database_name.table_name (ClickHouse does not have schemas)
CREATE TABLE IF NOT EXISTS $db_name.raw_images
(
	--- Primary key columns
	raw_img_id		UInt64,		--- Unique image ID, generated in bitfields.py

	--- File
	filename		String,
	xxhash			String,

	--- Coordinates & time
	ra				Float64		default 0,
	dec				Float64		default 0,
	jd				Float64		default 0,
	img_time		DateTime64(3, 'UTC'),		--- Image timestamp
	rcv_time		DateTime64(3, 'UTC'),		--- Received timestamp

	--- Telescope
	mount			Int16		default 0,
	camnum			Int16		default 0,
	imtype			String,
	bitpix			Int16		default 0,
	naxis1			Int16		default 0,
	naxis2			Int16		default 0,
	object			String,
	expmode			String,
	counter			Int32		default 0,
	exptime			Float32		default 0,
	gain			Float32		default 0,
	readnoi			Float32		default 0,
	darkcur			Float32		default 0,
	saturval		Float32		default 0,
	nonlin			Float32		default 0,
	binx			Int16		default 0,
	biny			Int16		default 0,
	camname			String,
	camtemp			Float32		default 0,
	camcool			Float32		default 0,
	cammode			Int16		default 0,
	camgain			Int16		default 0,
	camoffs			Int16		default 0,
	projname		String,
	obslon			Float32		default 0,
	obslat			Float32		default 0,
	obsalt			Float32		default 0,
	lst				Float32		default 0,
	date_obs		String,
	m_ra			Float64		default 0,
	m_dec			Float64		default 0,
	m_ha			Float64		default 0,
	m_jra			Float64		default 0,
	m_jdec			Float64		default 0,
	m_jha			Float64		default 0,
	ha				Float64		default 0,
	equinox			Float32		default 0,
	m_az			Float32		default 0,
	m_alt			Float32		default 0,
	az				Float32		default 0,
	alt				Float32		default 0,
	airmass			Float32		default 0,
	trk_ra			Float32		default 0,
	trk_dec			Float32		default 0,
	mnttemp			Float32		default 0,
	focus			Float32		default 0,
	prvfocus		Float32		default 0,
	procstat		String,
	procversion		Int16		default 0,


	--- Create indexes
	--- See: https://clickhouse.com/docs/en/optimize/sparse-primary-indexes
	--- See: https://clickhouse.com/docs/en/optimize/skipping-indexes
	--- Index name needs to be unique only within the scope of the table it
	--- belongs to, not across the entire database or namespace.
	--- You can have indexes with the same name in different tables.
	INDEX idx_filename		(filename)		TYPE minmax	GRANULARITY 16,
	INDEX idx_xxhash		(xxhash)		TYPE minmax GRANULARITY 16,
	INDEX idx_ra_dec		(ra, dec)		TYPE minmax GRANULARITY 16,
	INDEX idx_jd			(jd)			TYPE minmax GRANULARITY 16,
	INDEX idx_img_time		(img_time)		TYPE minmax GRANULARITY 16
)


--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree
ENGINE = MergeTree()


--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/custom-partitioning-key
PARTITION BY toYear(img_time)


--- A tuple of column names or arbitrary expressions.
--- ClickHouse uses the sorting key as a primary key if the primary key
--- is not defined explicitly by the PRIMARY KEY clause
ORDER BY (raw_img_id)


--- Policy includes what disk values to use (see /etc/clickhouse-server/config.xml)
SETTINGS storage_policy = 'pipeline_policy',


--- Maximum number of data rows between the marks of an index. default value: 8192
--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree#mergetree-data-storage
index_granularity = 8192;


--- END OF FILE ---
