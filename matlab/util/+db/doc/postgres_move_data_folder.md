# Moving Postgres Data Folder

This procedure explains how to move the data folder from the default
location.

https://fitodic.github.io/how-to-change-postgresql-data-directory-on-linux

https://www.joe0.com/2020/06/16/postgres-12-how-to-change-data-directory/

https://www.tutorialspoint.com/how-to-change-postgresql-data-folder-location-on-ubuntu-16-04


Default data folder:

    /var/lib/postgresql/14


Before you start anything, locate the Postgresql’s configuration file 
and its data directory:

    sudo su - postgres
    [postgres@host ~]$ psql
    Password for user postgres:
    psql (12.6)
    Type "help" for help.
    
    postgres=# SHOW config_file;
                config_file
    -----------------------------------
     /var/lib/pgsql/data/postgresql.conf
    (1 row)
    
    postgres=# SHOW data_directory;
      data_directory
    -------------------
     /var/lib/pgsql/data
    (1 row)


Type
    
    sudo su - postgres
    psql

Type

    SHOW config_file;

Output

    /etc/postgresql/14/main/postgresql.conf 

Type

    SHOW data_directory;

Output

    /var/lib/postgresql/14/main

    
Stop the systemd service

    sudo systemctl stop postgresql.service

Create new location

    sudo mkdir /data/pgdata
    sudo chown postgres:postgres /data/pgdata
    sudo chmod 700 /data/pgdata

Move files using mv


Move files using rsync

    sudo rsync -av /var/lib/postgresql /data/pgdata/
    

Edit configuration file

    sudo vim /etc/postgresql/14/main/postgresql.conf

Modify

    data_directory = '/data/pgdata/postgresql/14/main' 

Restarting the PostgreSQL Server and Verify the Data folder Location

    sudo systemctl start postgresql

    sudo systemctl status postgresql

Once the service is restarted we will now verify the data folder location.

    sudo su - postgres

    psql

    SHOW data_directory;


