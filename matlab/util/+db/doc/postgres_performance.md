# Postgres v14 Performance Testing

This file contains information related to performance optimizations.

Updated: 02/2022


### Insert Performance

https://stackoverflow.com/questions/12206600/how-to-speed-up-insertion-performance-in-postgresql


### Disable indexing

https://fle.github.io/temporarily-disable-all-indexes-of-a-postgresql-table.html


Disable all table indexes

	UPDATE pg_index
	SET indisready=false
	WHERE indrelid = (
		SELECT oid
		FROM pg_class
		WHERE relname='<TABLE_NAME>'
	);


Reenable all table indexes

	UPDATE pg_index
	SET indisready=true
	WHERE indrelid = (
		SELECT oid
		FROM pg_class
		WHERE relname='<TABLE_NAME>'
	);


Reindex table

	REINDEX TABLE <TABLE_NAME>;
	
	

### Disable Indexing

https://dba.stackexchange.com/questions/190436/postgresql-how-to-disable-all-indexes-of-a-table


### Optimizations and Tunning Tools

https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server

https://www.enterprisedb.com/postgres-tutorials/how-tune-postgresql-memory


### Configuration

Edit Postgres configuration file

	sudo nano /etc/postgresql/14/main/postgresql.conf


### Tunning Performance - Improtant Parameters

https://www.enterprisedb.com/postgres-tutorials/how-tune-postgresql-memory


#### shared_buffers (integer)

The shared_buffers parameter determines how much memory is dedicated 
to the server for caching data. 

The default value for this parameter, which is set in postgresql.conf, is:

	#shared_buffers = 128MB

 

The value should be set to 15% to 25% of the machine’s total RAM. 
For example: if your machine’s RAM size is 32 GB, then the recommended 
value for shared_buffers is 8 GB. Please note that the database server 
needs to be restarted after this change.

 
#### work_mem (integer)

The work_mem parameter basically provides the amount of memory 
to be used by internal sort operations and hash tables before writing 
to temporary disk files. Sort operations are used for order by, distinct, 
and merge join operations. Hash tables are used in hash joins and hash based aggregation.

The default value for this parameter, which is set in postgresql.conf, is:

	#work_mem = 4MB

 

Setting the correct value of work_mem parameter can result in 
less disk-swapping, and therefore far quicker queries. 

We can use the formula below to calculate the optimal work_mem 
value for the database server:

    Total RAM * 0.25 / max_connections

 

The max_connections parameter is one of the GUC parameters to 
specify the maximum number of concurrent connections to the database server. 
By default it is set to 100 connections. 

We can also directly assign work_mem to a role from psql: 

    alter user test set work_mem='4GB';

    ALTER ROLE

 
#### maintenance_work_mem (integer)

The maintenance_work_mem parameter basically provides the maximum 
amount of memory to be used by maintenance operations like vacuum, 
create index, and alter table add foreign key operations. 

The default value for this parameter, which is set in postgresql.conf, is:

	#maintenance_work_mem = 64MB


It’s recommended to set this value higher than work_mem; 
this can improve performance for vacuuming. 
In general it should be: 

    Total RAM * 0.05

 
#### effective_cache_size (integer)

The effective_cache_size parameter estimates how much memory is available 
for disk caching by the operating system and within the database itself. 
The PostgreSQL query planner decides whether it’s fixed in RAM or not. 
Index scans are most likely to be used against higher values; otherwise, 
sequential scans will be used if the value is low. Recommendations are to set 
Effective_cache_size at 50% of the machine’s total RAM.

For more details and other parameters, please refer to the PostgreSQL 
documentation: https://www.postgresql.org/docs/12/runtime-config-resource.html.

	
### Configuration
	
https://www.postgresql.org/docs/12/runtime-config-resource.html


### Architecture and Tuning of Memory in PostgreSQL Databases

https://severalnines.com/database-blog/architecture-and-tuning-memory-postgresql-databases


### Reload Configuration

https://www.heatware.net/databases/postgresql-reload-config-without-restarting/


Option 1: From the command-line shell

	su - postgres
	/usr/bin/pg_ctl reload


Option 2: Using SQL statement

	SELECT pg_reload_conf();
	
	
Restart

	sudo /etc/init.d/postgresql restart	
	
	
Restart and enable PostgreSQL for the changes to take effect

    sudo systemctl restart postgresql
    sudo systemctl enable postgresql

	
### Fast Alternative to SELECT COUNT(*)

https://wiki.postgresql.org/wiki/Count_estimate

If you don't need an exact count, the current statistic from the catalog 
table pg_class might be good enough and is much faster to retrieve for 
big tables.

    SELECT reltuples AS estimate FROM pg_class WHERE relname = 'table_f_radec0';


## Partitioning

https://www.postgresql.org/docs/current/ddl-partitioning.html

https://hevodata.com/learn/postgresql-partitions/

#### Partitioning - Example 1

https://hevodata.com/learn/postgresql-partitions/


	CREATE TABLE sales (id int, p_name text, amount int, sale_date date)
	PARTITION BY RANGE (sale_date);

	CREATE TABLE sales_2019_Q4 PARTITION OF sales FOR VALUES FROM ('2019-10-01') TO ('2020-01-01');


#### Sub-Partitioning

https://blog.dbi-services.com/postgresql-partitioning-8-sub-partitioning/


