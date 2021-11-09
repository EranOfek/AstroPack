# Overview

 DbQuery - SQL Database query

 This class provides SQL functionality, currently tested with PostgreSQL v13.

Functionality

 Related Classes:
   DbDriver - Internally used to load Java package for Postgres
   DbConnection - Used to connect to specific database on local or remote
   server.
   DbRecord - Class with dynamic properties, used as struct to store
   values of database record.

 References:
   https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example

 Unit-Test:
   Use unittest__tables from GDrive to test


## Database GUI

On Windows, use **SQL Manager Lite for PostgreSQL** by EMS Software (or DataGrip):

https://www.sqlmanager.net/products/postgresql/manager

Download page:

https://www.sqlmanager.net/products/postgresql/manager/download


On Linux, use **DataGrip** by JetBrains

https://www.jetbrains.com/datagrip/

Download page:

https://www.jetbrains.com/datagrip/download/


## Database Alias


## Working with Multiple Primary Keys


## Related Classes

These classes are used internally by DbQuery:

- DbDriver - Wrapper for the Java driver object
- DbConnection - Wrapper for Java database connection object

# Usage

## Basic Database Operations

### Constructor (connect)

    Q = db.DbQuery('UnitTest:master_table');

    % Create query with database:table, get number of records
    Q = db.DbQuery('unittest:master_table');
    Count = Q.selectCount();
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', Count);

    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

    % Switch to another table
    Q.TableName = 'master_table';

    % Select and load all records
    Record = Q.select('*');
    Table = Record.convert2table();


### Get Database Version

    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
    Q = db.DbQuery('UnitTest');
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

## Select

### Select number of records in table (count)

    % Create query with database:table, get number of records
    Q = db.DbQuery('unittest:master_table');
    Count = Q.selectCount();
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', Count);


### Select all records in table

    % Create query with database:table, get number of records
    Q = db.DbQuery('unittest:master_table');
    Result = Q.select();
    io.msgLog(LogLevel.Test, 'Number of records in result: %d', numel(Result.Data));
    % Convert to table
    Table = Result.convert2table();
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', numel(Result.Data));

### Select specified fields with 'where' filter

## Insert

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


### Insert DbRecord with insert()


### Generating Primary Key

    function Result = makePrimaryKeyForMat(Q, Rec, Index)
       % Make priamry key, called for every row of Rec.Data(Index)

       if Index == 0
           First = 1;
           Last = numel(Rec.Data);
       else
           First = Index;
           Last = Index;
       end

       %
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
       Result = Q.insert(R, 'PrimaryKeyFunc', @makePrimaryKeyForMat, 'BatchSize', 10000);
       io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());
       Count = Count * 10;
    end



#### Example



## Update


## Delete

## copyFrom - Batch Import from CSV file

Note: To improve performance we write data to CSV file using mex-writematrix
(AstroPack.git/matlab/external/mex-writematrix), replacement for (slow) dlmwrite in MATLAB.


## Create Database

    createDatabase


### Updating structure of existing database




## copyTo - Batch Export to CSV file


# Examples

### Example 1: Basic usage


    Q = db.DbQuery('unittest:master_table');
    Q.selectCount()


# Database Configuration File

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



# See Also

# Notes

## Create Database Scripts

### Add Test Fields

Add 20x double fields

    ALTER TABLE public.master_table
    ADD COLUMN fdouble1 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble2 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble3 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble4 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble5 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble6 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble7 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble8 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble9 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble10 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble11 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble12 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble13 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble14 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble15 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble16 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble17 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble18 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble19 DOUBLE PRECISION;

    ALTER TABLE public.master_table
    ADD COLUMN fdouble20 DOUBLE PRECISION;


## Installatoin & Creating Database from Google Sheets

#### PostgresSQL - Installation Guide

https://docs.google.com/document/d/117tA4l6Dv_DSMZbMsvRINbDntChJ-FIk-2bPP-6lC_M/edit?usp=sharing

#### Database Definitions Sheets

https://docs.google.com/document/d/1_puwzIOCL3pqQ8byxmX_uSI054olIAVH3OZrKyVu9ys/edit?usp=sharing

#### UnitTest database definition in Google Sheets:

https://docs.google.com/spreadsheets/d/1ZAjdFRKAJ72p6eRuuXivN3Be_1fCsNQd5WqiSIUJxJI/edit?usp=sharing


#### xlsx2sql.u Use Manual

See AstroPack.git/python/utils/database_utils/xlsx2sql.md

