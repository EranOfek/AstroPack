/*

  Title: ClickHouse SQL Script - Create Table
  Description: This script creates a new table in the ClickHouse database.

  Author:  Chen Tishler
  Created: 16/02/2024
  Updated: 19/02/2024

  Usage:   Execute this script using ClickHouse client, database management tool, or our clickhouse_util.py.
  Example: clickhouse-client --multiquery < this_script.sql

  Notes:
	- Ensure the ClickHouse server is running and accessible.
	- Review and adjust the column data types and table settings according to project requirements.
	- This script assumes [any assumptions, e.g., necessary databases or user permissions are already set up].

  Changelog:
	- 2024-02-16: Initial version. [Chen Tishler]
	- 2024-02-19: Modifications accroding do Eran's email [Chen Tishler]

*/


--- Use database_name.table_name (ClickHouse does not have schemas)
CREATE TABLE IF NOT EXISTS $db_name.coadd_sources
(
	--- Primary key columns
	img_id			UInt64,		--- Unique image id (primary key from Postgres images table, or composed from image type, timestamp, etc.)
	src_idx			Int32,		--- Running index for source in this image

	--- Coordnates and timestamp
	ra				Float64,
	dec				Float64,
	jd				Float64		default null,	--- Julian date/time
	subimg			Int32,
	proj_id			Int8		default 0,		--- Project ID (ULTRASAT, LAST, etc.)
	cam_id			Int16		default 0,		--- Camera ID in project, can be mapped to mount/camera, etc.

	--- HEALPix index for partition, low resolution search, high resolution search
	hp_part			Int16,
	hp_low			Int32,
	hp_high			Int64,


	--- Telescope
	flags			Int32		default 0,
	exptime			Float32		default 0,
	
	--- XY
	xpeak			Int16		default 0,
	ypeak			Int16		default 0,
	x1				Float32		default 0,
	y1				Float32		default 0,
	x2				Float32		default 0,
	y2				Float32		default 0,
	xy				Float32		default 0,
	x				Float32		default 0,
	y				Float32		default 0,
	
	--- SN
	sn				Float64		default null,	
	sn_1			Float32		default null,
	sn_2			Float32		default null,
	sn_3			Float32		default null,
	sn_4			Float32		default null,
	sn_5			Float32		default null,
	
	--- Image
	back_im			Float32		default null,
	var_im			Float32		default null,
	back_annulus	Float32		default null,
	std_annulus		Float32		default null,
	
	--- Flux
	flux_aper_1		Float64		default 0,
	flux_aper_2		Float64		default 0,
	flux_aper_3		Float64		default 0,
	fluxerr_aper_1	Float64		default 0,
	fluxerr_aper_2	Float64		default 0,
	fluxerr_aper_3	Float64		default 0,
	
	--- Mag
	mag_aper_1		Float64		default 0,
	mag_aper_2		Float64		default 0,
	mag_aper_3		Float64		default 0,
	magerr_aper_1	Float64		default 0,
	magerr_aper_2	Float64		default 0,
	magerr_aper_3	Float64		default 0,
	
	--- PSF
	flux_psf		Float64		default null,
	fluxerr_psf		Float64		default null,
	mag_psf			Float64		default null,
	magerr_psf		Float64		default null,
	psf_chi2dof		Float32		default null,

	--- Other
	mergedcatmask	Int32		default 0,
	nobs			Float32		default 0,
	distmp			Float32		default 0,


	--- Create indexes
	--- See: https://clickhouse.com/docs/en/optimize/sparse-primary-indexes
	--- See: https://clickhouse.com/docs/en/optimize/skipping-indexes
	--- Index name needs to be unique only within the scope of the table it
	--- belongs to, not across the entire database or namespace.
	--- You can have indexes with the same name in different tables.
	INDEX idx_hp_part	(hp_part)	TYPE minmax	GRANULARITY 16,
	INDEX idx_hp_low	(hp_low)	TYPE minmax GRANULARITY 16,
	INDEX idx_hp_high	(hp_high)	TYPE minmax GRANULARITY 16,
	INDEX idx_ra		(ra)		TYPE minmax GRANULARITY 16,
	INDEX idx_dec		(dec)		TYPE minmax GRANULARITY 16,
	INDEX idx_jd		(jd)		TYPE minmax GRANULARITY 16,
	INDEX idx_img_jd	(img_id)	TYPE minmax GRANULARITY 16,
	INDEX idx_proj_cam_id (proj_id, cam_id)	TYPE minmax GRANULARITY 16,
)


--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree
ENGINE = MergeTree()


--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/custom-partitioning-key
PARTITION BY hp_part


--- A tuple of column names or arbitrary expressions.
--- ClickHouse uses the sorting key as a primary key if the primary key
--- is not defined explicitly by the PRIMARY KEY clause
ORDER BY (hp_high, img_id)


--- Policy includes what disk values to use (see /etc/clickhouse-server/config.xml)
SETTINGS storage_policy = 'pipeline_policy',


--- Maximum number of data rows between the marks of an index. default value: 8192
--- See: https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree#mergetree-data-storage
index_granularity = 8192;


--- END OF FILE ---
