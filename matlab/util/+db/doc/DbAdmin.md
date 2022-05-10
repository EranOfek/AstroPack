# Overview

## General


## Database Configuration File

### Database.DbConnections.UnitTest

config/Database.DbConnections.UnitTest.yml

or (note that files in config/local/ override the same file in config/)

config/local/Database.DbConnections.UnitTest.yml


    # Edit this file and save it in config/local/

    DatabaseName        : 'unittest'                # Database name
    Host                : 'gauss'                   # Host name or IP address
    DriverName          : 'postgres'                # Driver name
    Port                : 5432                      # Port number
    UserName            : '?'                       # Login user - Replace ? with username
    Password            : '?'                       # Login password - Replace ? with password
    ServerSharePath     : '/var/samba/pgshare'      # Linux
    MountSharePath      : '/media/gauss_pgshare'    # Linux
    WinMountSharePath   : 'S:/'                     # Windows (run: net use S: \\gauss\pgshare)


## Creating Database from Google Sheets

Database Definitions Sheets

   https://docs.google.com/document/d/1_puwzIOCL3pqQ8byxmX_uSI054olIAVH3OZrKyVu9ys/edit?usp=sharing

UnitTest database definition in Google Sheets:

   https://docs.google.com/spreadsheets/d/1ZAjdFRKAJ72p6eRuuXivN3Be_1fCsNQd5WqiSIUJxJI/edit?usp=sharing


## Using xlsx2sql.py

Generate SQL scripts from CSV files downloaded from Google Sheets tabs


### Usage

     python xlsx2sql.py -f unittest.xlsx

If the specified file name does not contain path,
it looks for files in AstroPack repository by environment variable
ASTROPACK_PATH, in folder database/xlsx/.

Output files are created under the same folder in a subfolder, i.e.
database/xls/unittest/.

### Create database from the output SQL file

     psql -U postgres -f unittest.sql

### Requirements (Python v3)

    pip3 install pyyaml openpyxl

Check Postgres version from command line

     psql -V

Postgres Passwords

    Chen Windows: postgres/pass
    Linux default:

