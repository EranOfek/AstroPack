# Overview

### General

DbQuery - SQL Database Component

This class provides basic and advanced SQL functionality, along with DbRecord 
and Database.yml configuration file it is all you need to access databases. 

Currently, it works with **PostgreSQL v13**.

### Related Classes:
- DbRecord - Class with dynamic properties, used as struct to store values of database record.
- DbConnection - Internally used to connect to specific database on local or remote server.
- DbDriver - Internally used to load Java package for Postgres.


### References:

- https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example

 Unit-Test:
   Use unittest from GDrive to test


## Related Classes

These classes are used internally by DbQuery:

- DbDriver - Wrapper for the Java driver object
- DbConnection - Wrapper for Java database connection object

# Usage

### Select number of records in table (count)

    Q = db.DbQuery('unittest:master_table');
    Count = Q.selectCount();


### Select specified fields with 'where' filter

    Fields = 'fdouble1,fdouble2,fdouble3,fdouble4,fdouble5';
    Where = 'fdouble1 > fdouble2';
    Limit = 100000;
    Output = 'mat';
    Mat = Q.select(Fields, 'TableName', 'master_table', 'where', Where, 'Convert', Output, 'Limit', Limit);
    Size = size(Mat);
    disp(Size(1));


### Insert DbRecord with insert() and Callback function 

    function Result = makePK(Q, Rec, First, Last)
       % Make priamry key
       for i=First:Last
           Rec.Data(i).recid = sprintf('Callback_%05d_%s', i, Rec.newKey());
       end
       Result = true;
    end

    % 
    Iters = 5;
    Count = 1;
    Cols = 20;
    ColNames = {};
    for i=1:Cols
       ColNames{i} = sprintf('fdouble%d', i);
    end

    for Iter=1:Iters
       Mat = rand(Count, Cols);
       R = db.DbRecord(Mat, 'ColNames', ColNames);
       Result = Q.insert(R, 'PrimaryKeyFunc', @makePK, 'BatchSize', 10000);
       io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());
       Count = Count * 10;
    end


## Advanced Usage

### Insert single record with exec()

    % Insert using raw SQL by calling Q.exec() directly
    CountBeforeInsert = Q.selectCount();
    InsertCount = 10;
    for i = 1:InsertCount
        Q.SqlText = sprintf("INSERT INTO master_table (recid, fint, fdouble) VALUES('%s', %d, %f)",...
            Component.newUuid(), randi(100), randi(100000));
        Q.exec();
    end

    % Assume that we are the single process writing to this table at the moment
    CountAfterInsert = Q.selectCount();
    assert(CountBeforeInsert + InsertCount == CountAfterInsert);


### Insert batch with exec()

    % Insert batch using raw sql: Prepare multiple INSERT lines
    % See: https://www.tutorialspoint.com/how-to-generate-multiple-insert-queries-via-java
    TestBatch = true;
    if (TestBatch)
        CountBeforeInsert = Q.selectCount();
        InsertCount = 10;
        Sql = '';
        for i = 1:InsertCount
            % Prepare statement
            uuid = Component.newUuid();
            SqlLine = sprintf("INSERT INTO master_table(RecID,FInt,FString) VALUES ('%s',%d,'Batch_%03d');", uuid, i, i).char;
            Sql = [Sql, SqlLine];
        end
        Q.exec(Sql);
        assert(Q.ExecOk);
        CountAfterInsert = Q.selectCount();
        assert(CountBeforeInsert + InsertCount == CountAfterInsert);
    end


# Notes

## Database Configuration File

Database.yml

    # Database Configuration File
    #
    # Notes:
    #   1. Key names should match DbConnection's properties names
    #      (i.e. there is DbConnection.Host)
    #
    #
    #


    # Default values, override by adding values to per-database section under Items below
    Default:
    Host            : 'localhost'       # Host name or IP address
    DriverName      : 'postgres'        # Driver name
    UserName        : 'postgres'        # Login user
    Password        : 'pass'            # Login password
    Port            : 5432              # Post number, 5432 is Postgres default


    # Databases, items names are used by DbConnection
    Items:

        #
        UnitTest:
            DatabaseName    : 'unittest'        # Database name
            Host            : 'localhost'       # Host name or IP address

        #
        Pipeline:
            DatabaseName    : 'pipeline'        # Database name
            Host            : 'localhost'       # Host name or IP address

        #
        Last:
            DatabaseName    : 'last'            # Database name
            Host            : 'localhost'       # Host name or IP address

        #
        Observations:
            DatabaseName    : 'observations'    # Database name
            Host            : 'localhost'       # Host name or IP address


## PostgresSQL - Installation Guide

https://docs.google.com/document/d/117tA4l6Dv_DSMZbMsvRINbDntChJ-FIk-2bPP-6lC_M/edit?usp=sharing


## Database GUI

On Windows, use **SQL Manager Lite for PostgreSQL** by EMS Software (or DataGrip):

https://www.sqlmanager.net/products/postgresql/manager

Download page:

https://www.sqlmanager.net/products/postgresql/manager/download


On Linux, use **DataGrip** by JetBrains

https://www.jetbrains.com/datagrip/

Download page:

https://www.jetbrains.com/datagrip/download/


# Creating Database from Google Sheets

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

