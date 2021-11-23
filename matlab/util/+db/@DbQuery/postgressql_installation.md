# PostgresSQL - Installation Guide - Use V13

## General

This documents contains instructions about installation of PostgreSQL
database engine in Windows and Linux, along with utilities such as
JetBrains DataGrip.

## PostgresSQL Installation - Windows


Web: https://www.postgresql.org/download/

Download and install postgresql-13.1-1-windows-x64.exe.


## Database GUI

On Windows, use **SQL Manager Lite for PostgreSQL** by EMS Software (or DataGrip):

https://www.sqlmanager.net/products/postgresql/manager

Download page:

https://www.sqlmanager.net/products/postgresql/manager/download


On Linux, use **DataGrip** by JetBrains

https://www.jetbrains.com/datagrip/

Download page:

https://www.jetbrains.com/datagrip/download/



## PostgresSQL Installation - Linux

Check if Postgres is installed

    which psql
    psql --version


Output should be something like this

    psql --version
    psql (PostgreSQL) 13.3 (Ubuntu 13.3-1.pgdg18.04+1)


Installation

Source: https://computingforgeeks.com/how-to-install-postgresql-13-on-ubuntu/


Postgres V13 is not supported yet by apt-get, you need to perform this sequence:

    sudo apt update
    sudo apt -y upgrade
    sudo reboot

    sudo apt -y install vim bash-completion wget

    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -

    echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" |sudo tee  /etc/apt/sources.list.d/pgdg.list

    sudo apt update

    sudo apt install postgresql-13 postgresql-client-13

    systemctl status postgresql.service




Set Postgres Password

    chent@ubuntu:~$ sudo su - postgres
    [sudo] password for chent:
    postgres@ubuntu:~$
    postgres@ubuntu:~$
    postgres@ubuntu:~$ psql
    psql (13.2 (Ubuntu 13.2-1.pgdg18.04+1))
    Type "help" for help.

    postgres=#
    postgres-# \password
    Enter new password: (enter 'pass')
    Enter it again: (enter 'pass')
    postgres-#quit



Check login

    chent@ubuntu:~$ psql -h localhost -U postgres
    Password for user postgres:
    psql (13.2 (Ubuntu 13.2-1.pgdg18.04+1))
    SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, bits: 256, compression: off)
    Type "help" for help.

    postgres=#



Configure Postgresq to ask for password

https://djangocentral.com/how-to-fix-fatal-peer-authentication-failed-for-user-postgres-error/


