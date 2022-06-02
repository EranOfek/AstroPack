# Postgres TableSpace

### General


https://www.postgresql.org/docs/current/manage-ag-tablespaces.html


### Define tablespace

To define a tablespace, use the CREATE TABLESPACE command, for example::

	CREATE TABLESPACE fastspace LOCATION '/ssd1/postgresql/data';


Tables, indexes, and entire databases can be assigned to particular tablespaces. To do so, a user with the CREATE privilege on a given tablespace must pass the tablespace name as a parameter to the relevant command. For example, the following creates a table in the tablespace space1:

	CREATE TABLE foo(i int) TABLESPACE space1;

Alternatively, use the default_tablespace parameter:

	SET default_tablespace = space1;
	CREATE TABLE foo(i int);


To determine the set of existing tablespaces, examine the pg_tablespace system catalog, for example

	SELECT spcname FROM pg_tablespace;


To create an index on the column code in the table films and have the index reside in the tablespace indexspace:

	CREATE INDEX code_idx ON films (code) TABLESPACE indexspace;


