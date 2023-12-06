# PostgreSQL V14 - Installation Instructions for Windows

This documents contains installation instructions of PostgreSQL
database engine in Windows, along with utilities such as pgAdmin and
JetBrains DataGrip.


## Install Postgres

Installer download:

https://www.enterprisedb.com/downloads/postgres-postgresql-downloads

Select and download version 14.2


## Install pgAdmin

Download page:

https://www.pgadmin.org/download/


## Database GUI (Windows) - SQL Manager Lite - by EMS Software

On Windows, use **SQL Manager Lite for PostgreSQL** by EMS Software (or DataGrip):

https://www.sqlmanager.net/products/postgresql/manager

Download page:

https://www.sqlmanager.net/products/postgresql/manager/download


### Database GUI - DataGrip by JetBrains - License is required


https://www.jetbrains.com/datagrip/

Download page:

https://www.jetbrains.com/datagrip/download/


### Create Database on Server
	
	psql -h ubuntu -p 5432 -U admin -W -d postgres -f unittest_postgres.sql
	

To avoid typing the password, you can set the PGPASSWORD environment var.

	set PGPASSWORD=...
	psql -h gauss -p 5432 -U postgres -w -d postgres -f <script_postgres>.sql


