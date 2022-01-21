# PostgreSQL V14 - Installation Instructions

This documents contains installation instructions of PostgreSQL
database engine in Windows and Linux, along with utilities such as
JetBrains DataGrip.



### PostgreSQL V14 - Installation on Linux:

https://techviewleo.com/how-to-install-postgresql-database-on-ubuntu/


	sudo apt update && sudo apt upgrade
	sudo apt -y install gnupg2 wget vim
	sudo apt-cache search postgresql | grep postgresql
	sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
	wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc
	sudo apt -y update
	sudo apt -y install postgresql-14


Verify the installed PostgreSQL version


	sudo -u postgres psql -c "SELECT version();"


### Set password

https://stackoverflow.com/questions/27107557/what-is-the-default-password-for-postgres


After installing postgres follow following steps in order to setup password 
for default system account of Linux execute following in terminal:

	user:~$ sudo -i -u postgres
	postgres@user:~$ psql

after executing above two commands you will get into postgres shell

Execute this query in postgres shell:

	postgres=# ALTER USER postgres PASSWORD 'PassRoot';

your new password is 'mynewpassword' without quotes and now you can connect 
with external GUI tools like DBeaver.

Connect with password 'PassRoot'

	psql -U postgres -W


### Connect

	Password: Passw0rd


	psql -h ubuntu -p 5432 -U admin -W -d template1
	
	
### Create database on remote server

	
	psql -h ubuntu -p 5432 -U admin -W -d postgres -f unittest.sql
	

### JDBC Driver

https://jdbc.postgresql.org/download.html


## PostgresSQL Installation - Windows

Web: https://www.postgresql.org/download/

Download and install **postgresql-13.1-1-windows-x64.exe**.


## Database GUI (Windows) - SQL Manager Lite - by EMS Software

On Windows, use **SQL Manager Lite for PostgreSQL** by EMS Software (or DataGrip):

https://www.sqlmanager.net/products/postgresql/manager

Download page:

https://www.sqlmanager.net/products/postgresql/manager/download



### Database GUI (Linux/Windows) - DataGrip by JetBrains

On Linux, use **DataGrip** by JetBrains

https://www.jetbrains.com/datagrip/

Download page:

https://www.jetbrains.com/datagrip/download/




### Install psql on client Linux

    sudo apt install postgresql-client


### Install pgAdmin on Linux

https://computingforgeeks.com/how-to-install-pgadmin-4-on-ubuntu/


    sudo apt-get install curl
    sudo curl https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo apt-key add
    sudo sh -c 'echo "deb https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list'
    sudo apt update
    sudo apt install pgadmin4


