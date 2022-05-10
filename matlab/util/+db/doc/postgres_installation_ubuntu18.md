# PostgreSQL V14 - Installation Instructions for Ubuntu 18.04

This documents contains installation instructions of PostgreSQL
database engine in Ubuntu 18.04, along with utilities such as
pgAdmin and JetBrains DataGrip.

Note that for other versions of Ubuntu, these instructions does not work!

See postgres_installation_ubuntu22.md for Ubuntu 22.04.

-----------------------------------------------------------------------------

### PostgreSQL v14 - Installation on Linux

Reference:

https://techviewleo.com/how-to-install-postgresql-database-on-ubuntu/

- Step 1: Check requirements and update system
- Step 2: Install PostgreSQL 14 on Ubuntu 20.04|18.04
- Step 3: Connect to PostgreSQL 14 database
- Step 4: Configure PostgreSQL 14 instance for Remote Access
- Step 5: User management in PostgreSQL 14 Database


### Steps 1,2 - Installation

	sudo apt update && sudo apt upgrade
	sudo apt -y install gnupg2 wget vim
	sudo apt-cache search postgresql | grep postgresql
	sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
	wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc
	sudo apt -y update
	sudo apt -y install postgresql-14


Verify the installed PostgreSQL version

	sudo -u postgres psql -c "SELECT version();"


### Set password for 'postgres' default user

FOLLOW THE INSTRUCTION IN THIS LINK

https://stackoverflow.com/questions/27107557/what-is-the-default-password-for-postgres


After installing postgres follow following steps in order to setup password 
for default system account of Linux execute following in terminal:

	sudo -i -u postgres
	psql

after executing above two commands you will get into postgres shell

Execute this query in postgres shell:

	ALTER USER postgres PASSWORD 'PassRoot';

your new password is 'PassRoot' without quotes and now you can connect 
with external GUI tools like pgAdmin or DBeaver.

Connect with password 'PassRoot'

	psql -U postgres -W


### Connect to server with 'psql' (after creating new user 'Admin', see below)

	Password: Passw0rd

	psql -h ubuntu -p 5432 -U admin -W -d template1


### Step 4: Configure PostgreSQL 14 instance for Remote Access

FOLLOW THE INSTRUCTION IN THIS LINK

https://stackoverflow.com/questions/27107557/what-is-the-default-password-for-postgres

    sudo sed -i '/^host/s/ident/md5/' /etc/postgresql/14/main/pg_hba.conf
    sudo sed -i '/^local/s/peer/trust/' /etc/postgresql/14/main/pg_hba.conf

Edit /etc/postgresql/14/main/pg_hba.conf

    sudo vim /etc/postgresql/14/main/pg_hba.conf

in the file, add the lines below, so the file will be like this:

Look for this line:

    # IPv4 local connections:
    host    all             all             127.0.0.1/32            scram-sha-256

Add below:

    host    all             all             0.0.0.0/0               md5
    

Look for this line:

    # IPv6 local connections:
    host    all             all             ::1/128                 scram-sha-256

Add below:
    host    all             all             0.0.0.0/0               md5


Edit /etc/postgresql/14/main/postgresql.conf

    sudo vim /etc/postgresql/14/main/postgresql.conf


In the file, uncomment and edit the line with 'listen_addresses', as below.

    #--------------------------------------
    # CONNECTIONS AND AUTHENTICATION
    #--------------------------------------
    .......
    listen_addresses='*'

    
Now restart and enable PostgreSQL for the changes to take effect

    sudo systemctl restart postgresql
    sudo systemctl enable postgresql

	
### Step 5: Create a superuser with the name 'admin'

FOLLOW THE INSTRUCTION IN THIS LINK

https://stackoverflow.com/questions/27107557/what-is-the-default-password-for-postgres


Create a superuser with the name admin as below. 
You can edit Passw0rd to your preferred password.

    sudo -u postgres psql

Type

    CREATE ROLE admin WITH LOGIN SUPERUSER CREATEDB CREATEROLE PASSWORD 'Passw0rd';

Verify the user has been created with the required privileges.

At the prompt, enter command '\du':

    postgres=# \du

You should get this output

     Role name |                         Attributes                         | Member of 
    -----------+------------------------------------------------------------+-----------
     admin     | Superuser, Create role, Create DB                          | {}
     postgres  | Superuser, Create role, Create DB, Replication, Bypass RLS | {}


-----------------------------------------------------------------------------

### Create Database on Server
	
	psql -h ubuntu -p 5432 -U admin -W -d postgres -f unittest_postgres.sql

	
### Set Password in Envoronment

Note that there are differences between bash and tcsh.

Linux (bash):

	export PGPASSWORD='...'
	psql -h gauss -p 5432 -U postgres -w -d postgres -f <script_postgres>.sql


Linux (tcsh):

	setenv PGPASSWORD '...'
	psql -h gauss -p 5432 -U postgres -w -d postgres -f <script_postgres>.sql



### JDBC Driver

https://jdbc.postgresql.org/download.html



### Database GUI - DataGrip by JetBrains (License is required)

On Linux, use **DataGrip** by JetBrains

https://www.jetbrains.com/datagrip/

Download page:

https://www.jetbrains.com/datagrip/download/


### Install psql on client Linux

On Linux machines that does not run Postgres server, it is enough to install
only the client utility.

    sudo apt install postgresql-client


### Install pgAdmin on Ubuntu 18.04 (DOES NOT WORK ON Ubuntu 22.04)

https://computingforgeeks.com/how-to-install-pgadmin-4-on-ubuntu/


    sudo apt-get install curl
    sudo curl https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo apt-key add
    sudo sh -c 'echo "deb https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list'
    sudo apt update
    sudo apt install pgadmin4

