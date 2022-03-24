# Overview

    DbQuery - MATLAB components for PostgreSQL
    Author: Chen Tishler
    Created: Jun 2021


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



### Difference between Database vs schema

https://www.postgresqltutorial.com/postgresql-schema/

What is the use of schema in PostgreSQL?
Schemas allow you to organize database objects e.g., tables into logical
groups to make them more manageable. Schemas enable multiple users to use
one database without interfering with each other.


https://www.educba.com/postgresql-database-vs-schema/

Difference between PostgreSQL Database vs schema

PostgreSQL database is a container containing all the schemas, records, logs,
and constraints of the table. Databases are rigidly separated, which means that
a user cannot access two databases together.
In order to manipulate the data in the database of PostgreSQL,
DML (Data Manipulation Language) commands are used.

PostgreSQL Schema defines the outline of how the data is logically structured
and stored in the database. It contains all the tables, data types, indexes,
functions, stored procedures, everything related to it. One can define the
different Schema in a database for different people accessing the application
in order to avoid conflicts and unnecessary interference.
A diagram can be drawn in order to show the database schema of a table
(showing the columns it contains, data types, key constraints, etc.)
known as a schema diagram.


### Views

https://www.postgresqltutorial.com/postgresql-views/

A view is a named query that provides another way to present data in the
database tables. A view is defined based on one or more tables which are known
as base tables. When you create a view, you basically create a query and assign
a name to the query. Therefore, a view is useful for wrapping a commonly used
complex query.

Note that regular views do not store any data except the materialized views.
In PostgreSQL, you can create special views called materialized views that
store data physically and periodically refresh data from the base tables.
The materialized views are handy in many scenarios, such as faster data access
to a remote server and caching.


### Tablespaces

https://www.postgresql.org/docs/14/manage-ag-tablespaces.html

Tablespaces in PostgreSQL allow database administrators to define locations in the
file system where the files representing database objects can be stored.
Once created, a tablespace can be referred to by name when creating database objects.

By using tablespaces, an administrator can control the disk layout of a PostgreSQL
installation. This is useful in at least two ways. First, if the partition or volume
on which the cluster was initialized runs out of space and cannot be extended,
a tablespace can be created on a different partition and used until the system
can be reconfigured.

Second, tablespaces allow an administrator to use knowledge of the usage pattern
of database objects to optimize performance. For example, an index which is very
heavily used can be placed on a very fast, highly available disk, such as an
expensive solid state device. At the same time a table storing archived data which
is rarely used or not performance critical could be stored on a less expensive,
slower disk system.


