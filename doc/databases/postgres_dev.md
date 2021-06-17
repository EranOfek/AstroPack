


https://dba.stackexchange.com/questions/14740/how-to-use-psql-with-no-password-prompt/14741




You have four choices regarding the password prompt:

    set the PGPASSWORD environment variable. For details see the manual:
    http://www.postgresql.org/docs/current/static/libpq-envars.html
    use a .pgpass file to store the password. For details see the manual:
    http://www.postgresql.org/docs/current/static/libpq-pgpass.html
    use "trust authentication" for that specific user:
    http://www.postgresql.org/docs/current/static/auth-methods.html#AUTH-TRUST
    use a connection URI that contains everything:
    http://www.postgresql.org/docs/current/static/libpq-connect.html#AEN42532


	
	
set PGPASSWORD=pass
	
# Postgres - Windows

set PGDATA=D:\Ultrasat\PostgreSQL\data

pg_ctl start

pg_ctl status

psql -U postgres
password: pass

	\list



pgAdmin is written Python

"C:\Program Files\PostgreSQL\13\pgAdmin 4"


### Things to learn

Partitioning

Indexes



## Matlab 

https://www.mathworks.com/help/database/ug/database.html


# Postgres - Linux


pgAdmin4 password: pass

