# PostgreSQL V14 - Installation Instructions

NOTE: This is was splitted into thress: Windows, Ubuntu 18, Ubuntu 22.

Keep it until these files are fully updated and tests.

-----------------------------------------------------------------------------

This documents contains installation instructions of PostgreSQL
database engine in Windows and Linux, along with utilities such as
JetBrains DataGrip.

-----------------------------------------------------------------------------

### PostgreSQL V14 - Installation on Linux:

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

	user:~$ sudo -i -u postgres
	postgres@user:~$ psql

after executing above two commands you will get into postgres shell

Execute this query in postgres shell:

	postgres=# ALTER USER postgres PASSWORD 'PassRoot';

your new password is 'mynewpassword' without quotes and now you can connect 
with external GUI tools like DBeaver.

Connect with password 'PassRoot'

	psql -U postgres -W


### Connect to server with 'psql'

	Password: Passw0rd


	psql -h ubuntu -p 5432 -U admin -W -d template1


### Step 4: Configure PostgreSQL 14 instance for Remote Access

FOLLOW THE INSTRUCTION IN THIS LINK

https://stackoverflow.com/questions/27107557/what-is-the-default-password-for-postgres

    sudo sed -i '/^host/s/ident/md5/' /etc/postgresql/14/main/pg_hba.conf
    sudo sed -i '/^local/s/peer/trust/' /etc/postgresql/14/main/pg_hba.conf

Edit /etc/postgresql/14/main/pg_hba.conf

    sudo vim /etc/postgresql/14/main/pg_hba.conf

in the file, add the lines below

    # IPv4 local connections:
    host    all             all             127.0.0.1/32            scram-sha-256
    host    all             all             0.0.0.0/0                md5
    
    # IPv6 local connections:
    host    all             all             ::1/128                 scram-sha-256
    host    all             all             0.0.0.0/0                md5


Edit /etc/postgresql/14/main/postgresql.conf

    sudo vim /etc/postgresql/14/main/postgresql.conf

In the file, uncomment and edit the line as below.

    #------------------------------------------------------------------------------
    # CONNECTIONS AND AUTHENTICATION
    #-----------------------------------------------------------------------------
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

    postgres=# \du

You should get this output

     Role name |                         Attributes                         | Member of 
    -----------+------------------------------------------------------------+-----------
     admin     | Superuser, Create role, Create DB                          | {}
     postgres  | Superuser, Create role, Create DB, Replication, Bypass RLS | {}


-----------------------------------------------------------------------------

### Create database on server

	
	psql -h ubuntu -p 5432 -U admin -W -d postgres -f unittest_postgres.sql
	

Windows:

	set PGPASSWORD=...
	psql -h gauss -p 5432 -U postgres -w -d postgres -f <script_postgres>.sql

Linux (bash):

	export PGPASSWORD='...'
	psql -h gauss -p 5432 -U postgres -w -d postgres -f <script_postgres>.sql


Linux (tcsh):

	setenv PGPASSWORD '...'
	psql -h gauss -p 5432 -U postgres -w -d postgres -f <script_postgres>.sql



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


-----------------------------------------------------------------------------

## Moving Postgres Data Folder

https://fitodic.github.io/how-to-change-postgresql-data-directory-on-linux

https://www.joe0.com/2020/06/16/postgres-12-how-to-change-data-directory/

https://www.tutorialspoint.com/how-to-change-postgresql-data-folder-location-on-ubuntu-16-04


Default data folder:

    /var/lib/postgresql/14


Before you start anything, locate the postgresql’s configuration file and its data directory:

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



-----------------------------------------------------------------------------

# Share Linux Folder

Use the following instructions to create a shared folder between Linux server running PostgreSQL, 
and a client (Linux/Windows).

It is required in order to use Postgre's COPY TO/FROM functionality for high performance INSERT/SELECT.


## Server - Linux - Share Folder using SAMBA

Serevr host name: gauss

    ServerSharePath : '/var/samba/pgshare'
    MountSharePath  : '/media/gauss_pgshare'
	
Reference:
	
[https://www.howtogeek.com/176471/how-to-share-files-between-windows-and-linux/](https://www.howtogeek.com/176471/how-to-share-files-between-windows-and-linux/)


On **server** machine:

    sudo apt-get install samba
    smbpasswd -a root
    pass
    pass

    sudo mkdir /var/samba/pgshare
    sudo chmod 777 /var/samba/pgshare

Edit /etc/samba/smb.conf

    sudo nano /etc/samba/smb.conf


Add at the end of smb.conf

	[pgshare]
	path = /var/samba/pgshare
	available = yes
	read only = yes
	browsable = yes
	public = yes
	writable = yes
	guest ok = yes


Restart the service.

    sudo service smbd restart


### Client - Linux - Mount Shared SAMBA Folder

On **client** machine:

Mount on Linux as guest.
No user/password is required, grant full access to folder.

Install cifs and create folder with correct permissions:

    sudo apt-get install cifs-utils

For host 'gauss' (Eran)

    sudo mkdir /media/gauss_pgshare
    sudo chown -R nobody:nogroup /media/gauss_pgshare
    sudo chmod -R 0777 /media/gauss_pgshare
    sudo nano /etc/fstab

For host 'scorpius' (Yossi)

    sudo mkdir /media/scorpius_pgshare
    sudo chown -R nobody:nogroup /media/scorpius_pgshare
    sudo chmod -R 0777 /media/scorpius_pgshare
    sudo nano /etc/fstab



Edit /etc/fstab:

    sudo nano /etc/fstab
    //gauss/pgshare /media/gauss_pgshare cifs rw,guest,uid=nobody,iocharset=utf8,file_mode=0777,dir_mode=0777,noperm 0 0
    //scorpius/pgshare /media/scorpius_pgshare cifs rw,guest,uid=nobody,iocharset=utf8,file_mode=0777,dir_mode=0777,noperm 0 0


After saving the modifications, reload mount table:

    sudo mount -a

Test client access

    ls -la /media/gauss_pgshare/
    ls -la /media/scorpius_pgshare/

Test server access

    ls -la /var/samba/pgshare/


### Client - Windows - Mount Shared SAMBA Folder

Open **cmd** window and type:

    net use S: \\gauss\pgshare


-----------------------------------------------------------------------------

## NFS - @TODO

	NOTE: This section is not completed yet, still need some experiments and testing.
	


NFS vs SAMBA

    NFS offers better performance and is unbeatable if the files are medium-sized or small.
    For larger files, the timings of both methods are almost the same.
    In the case of sequential read, the performance of NFS and SMB are almost
    the same when using plain text.



https://www.tecmint.com/install-nfs-server-on-ubuntu/





https://www.digitalocean.com/community/tutorials/how-to-set-up-an-nfs-mount-on-ubuntu-18-04


On the Host

    sudo apt update
    sudo apt install nfs-kernel-server


    sudo mkdir /var/nfs/pgshare -p
    sudo chown nobody:nogroup /var/nfs/pgshare

    sudo nano /etc/exports


    /var/nfs/pgshare *(rw,sync,no_subtree_check)

    sudo systemctl restart nfs-kernel-server



Creating Mount Points and Mounting Directories on the Client

    sudo mkdir -p /nfs/gauss_pgshare

    sudo mount gauss:/var/nfs/pgshare /nfs/gauss_pgshare



On the Client

    sudo apt update
    sudo apt install nfs-common



On server:


    sudo mkdir /pgshare
    sudo chmod a+rw /pgshare
    sudo chown chent pgshare



On client:


1. Create a directory to serve as the mount point for the remote filesystem:

    cd /media
    sudo mkdir gauss_share
    sudo chmod a+rw gauss_share



2. Generally, you will want to mount the remote NFS share automatically at boot.
   To do so open the /etc/fstab file with your text editor:

    sudo nano /etc/fstab



Add

    # <file system>    <dir>       <type>   <options>   <dump>  <pass>

    remote.server:/dir /media/gauss_share  nfs      defaults    0       0


3. Mount the NFS share by running the following command:

    sudo mount /media/gauss_share


https://linuxize.com/post/how-to-mount-and-unmount-file-systems-in-linux/

