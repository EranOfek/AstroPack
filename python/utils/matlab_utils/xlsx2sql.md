# xls2sql.py

Generate SQL scripts from CSV files downloaded from Google Sheets tabs

### Usage

	usage: xlsx2sql.py [-h] [-f XLSX] [-d DIR] [-subdirs] [-postgres] [-firebird]
					   [-sqlite] [-python] [-matlab] [-cpp] [-delphi] [-dart]

	optional arguments:
	  -h, --help  show this help message and exit
	  -f XLSX     Specify input .xlsx file name
	  -d DIR      Specify input folder, all .xlsx files will be processed
	  -subdirs    Process xlsx files in subfolders
	  -postgres   Generate PostgreSQL script
	  -firebird   Generate FirebirdSQL script
	  -sqlite     Generate SQLite script
	  -python     Generate Python code
	  -matlab     Generate MATLAB code
	  -cpp        Generate C++ code
	  -delphi     Generate Delphi code
	  -dart       Generate Dart (Flutter) code

### Basic usage

Generate SQL scripts for PostgreSQL and SQLite.

     python xlsx2sql.py -f unittest.xlsx
	 

If the specified file name does not contain path, 
it looks for files in AstroPack repository by environment variable 
ASTROPACK_PATH, in folder database/xlsx/.

Output files are created under the same folder in a subfolder, i.e. 
database/xls/unittest/.

### Create database from the output SQL file

     psql -U postgres -f unittest.sql

### Requirements

Python v3

    pip3 install pyyaml openpyxl

Check Postgres version from command line

     psql -V


Postgres Passwords

    Chen Windows: postgres/pass
    Linux default:
