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

On Windows, use SQL Manager Lite for PostgreSQL by EMS Software (or DataGrip):

https://www.sqlmanager.net/products/postgresql/manager

Download page:

https://www.sqlmanager.net/products/postgresql/manager/download


On Linux, use DataGrip by JetBrains 

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

    PrimaryKeyFunc  = [];       % function(DbQuery, DbRecord, Index)


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

## Creating Database from Google Sheets

See AstroPack.git/python/utils/database_utils/xlsx2sql.md

