# SQL Scripts to create databases

Put here current stable versions


To create database or update existing database structure,
run from command line:

psql -U postgres -f filename.sql

For example:

psql -U postgres -f lastdb.sql

If password is required, use "pass"


Use DataGrip (JetBrains) to browse database structure and data.

Database tree:

postgres@localhost
	lastdb
		public
			tables
				processed_cropped_images
				raw_images
				sources_proc_cropped


