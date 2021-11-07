# Overview

% DbQuery - SQL Database query
%
% This class provides SQL functionality, currently tested with PostgreSQL v13.
%
% Functionality
%
% Related Classes:
%   DbDriver - Internally used to load Java package for Postgres
%   DbConnection - Used to connect to specific database on local or remote
%   server.
%   DbRecord - Class with dynamic properties, used as struct to store
%   values of database record.
%
% References:
%   https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example
%
% Unit-Test:
%   Use unittest__tables from GDrive to test
%

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


### Get number of records in table


### Select

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
        
        
        #
        Soc:
            DatabaseName    : 'observations'    # Database name
            Host            : 'localhost'       # Host name or IP address
    


# See Also

# Notes

## Creating Database from Google Sheets

See AstroPack.git/python/utils/database_utils/xlsx2sql.md

