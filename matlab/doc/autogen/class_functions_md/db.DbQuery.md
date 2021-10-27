# Class: db.DbQuery



    
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
      
      
      
      

### Functions List

    DbQuery - Create new DbQuery obeject
    clear - Clear current statement and ResultSet
    clearResultSet - Clear current ResultSet and related data
    close - Close current query
    copyFrom - Import records from file to table Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
    copyTo - Export records from table to file Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
    createDatabase - Create database
    delete - 
    deleteRecord - Delete record by fields specified in Rec
    exec - Execute SQL statement (that does not return data) Example: exec('INSERT ...')
    getDbVersion - Query Postgres version, result should be similar to 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
    getField - Get field value from current ResultSet, when FieldName is numeric, it is used as column index Example: Value = getField('MyFieldName')
    getFieldIndex - Get field index by field name, search in ColNames{}
    getFieldList - Get fields list of current ResultSet as celarray
    getFieldNames - 
    getFieldNamesOfType - Get cell array field names that match the specified field type
    getFieldTable - Get fields as empty table
    getFieldType - Get field type
    getMetadata - Get metadata of the specified table or the current result-set
    getRecord - Get current record from ResultSet as DbRecord NOTE: Field names are loaded in lower-case (because Postgres creates field names lower-cased)
    getStruct - Get current record from ResultSet as struct NOTE: Field names are loaded in lower-case (because Postgres creates field names lower-cased)
    getTableFieldList - Get fields list of specified table as celarray
    getValidFieldName - Convert specified table field name to valid Matlab property/struct-field name, replace non-valid chars with '_' Example: getValidFieldName('') ->
    insert - Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;
    insertCell - 
    insertDbRecord - Simple insert, all arguments are char Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;
    insertRecord - Insert DbRecord or struct fields to specified table Todo: support struct array
    isField - Check if field exists by name
    loadAll - Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)
    loadResultSet - Load ResultSet to DbRecord array Might be time and memory consuming!
    loadTable - Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)
    makeInsertFieldsText - 
    makeUpdateFieldsText - Prepare SQL text from cell array "UPDATE master_table set RecID=?,FInt=? WHERE..."
    makeWhereFieldsText - Prepare SQL text from cell array "WHERE RecID=? AND FInt=?..."
    newRecord - Create new empty record associated with this query
    next - Move cursor to next record, return false if reached end of data
    openConn - Open connection, throw exception on failure
    perfTest - DbQuery.perfTest
    prev - Move cursor to previous record, return false if reached end of data
    query - Run SELECT query, for other statements use exec() If no char argument is specified, use the current Obj.SqlText @Todo: Support query with params Example: Result = query('SELECT COUNT(*) from master_table')
    select - Execute SELECT Fields FROM TableName and load results to memory Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')
    selectCount - Select number of records with optionally where clause
    selectTableCount - Select number of records with optionally where clause
    selectWhere - SELECT the specified fields from table, using where clause
    setConnection - Set connection Connection argument may be either: - Empty string: use default connetion() - Non empty string: used as connection key for DbConnection.getDbConnection - DbConnection object
    setStatementValues - Set statement values from specified DbRecord or struct
    stressTest - DbQuery.stressTest
    unitTest - Unit-Test On Windows, use SQL Manager Lite for PostgreSQL by EMS Software On Linux, use DataGrip by JetBrains
    unitTestDev - Unit-Test On Windows, use SQL Manager Lite for PostgreSQL by EMS Software On Linux, use DataGrip by JetBrains
    updateRecord - Update record

### DbQuery

Create new DbQuery obeject


    
    Create new DbQuery obeject  


### clear

Clear current statement and ResultSet


    
    Clear current statement and ResultSet  
      


### clearResultSet

Clear current ResultSet and related data


    
    Clear current ResultSet and related data  


### close

Close current query


    
    Close current query  


### copyFrom

Import records from file to table Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/


    
    Import records from file to table  
    Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html  
    https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/  


### copyTo

Export records from table to file Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/


    
    Export records from table to file  
    Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html  
    https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/  


### createDatabase

Create database


    
    Create database  


### delete




    


### deleteRecord

Delete record by fields specified in Rec


    
    Delete record by fields specified in Rec  


### exec

Execute SQL statement (that does not return data) Example: exec('INSERT ...')


    
    Execute SQL statement (that does not return data)  
    Example: exec('INSERT ...')  
      


### getDbVersion

Query Postgres version, result should be similar to 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'


    
    Query Postgres version, result should be similar to  
    'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'  


### getField

Get field value from current ResultSet, when FieldName is numeric, it is used as column index Example: Value = getField('MyFieldName')


    
    Get field value from current ResultSet, when FieldName is  
    numeric, it is used as column index  
    Example:  
    Value = getField('MyFieldName')  
      


### getFieldIndex

Get field index by field name, search in ColNames{}


    
    Get field index by field name, search in ColNames{}  


### getFieldList

Get fields list of current ResultSet as celarray


    
    Get fields list of current ResultSet as celarray  


### getFieldNames




    


### getFieldNamesOfType

Get cell array field names that match the specified field type


    
    Get cell array field names that match the specified field type  


### getFieldTable

Get fields as empty table


    
    Get fields as empty table  
      


### getFieldType

Get field type


    
    Get field type  


### getMetadata

Get metadata of the specified table or the current result-set


    
    Get metadata of the specified table or the current result-set  


### getRecord

Get current record from ResultSet as DbRecord NOTE: Field names are loaded in lower-case (because Postgres creates field names lower-cased)


    
    Get current record from ResultSet as DbRecord  
    NOTE: Field names are loaded in lower-case (because Postgres  
    creates field names lower-cased)  
      
    Create new record object  


### getStruct

Get current record from ResultSet as struct NOTE: Field names are loaded in lower-case (because Postgres creates field names lower-cased)


    
    Get current record from ResultSet as struct  
    NOTE: Field names are loaded in lower-case (because Postgres  
    creates field names lower-cased)  
      
    Create new record object  


### getTableFieldList

Get fields list of specified table as celarray


    
    Get fields list of specified table as celarray  
      
    Select single record from table  
    @Todo: Check how to get it without select, or what we do when  
    the table is empty?  


### getValidFieldName

Convert specified table field name to valid Matlab property/struct-field name, replace non-valid chars with '_' Example: getValidFieldName('') ->


    
    Convert specified table field name to valid Matlab  
    property/struct-field name, replace non-valid chars with '_'  
    Example: getValidFieldName('') ->  


### insert

Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;


    
    Insert new record to table, Keys and Values are celarray  
    sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;  
      


### insertCell




    
      
    Insert DbRecord or struct fields to specified table  


### insertDbRecord

Simple insert, all arguments are char Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;


    
    Simple insert, all arguments are char  
    Insert new record to table, Keys and Values are celarray  
    sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;  


### insertRecord

Insert DbRecord or struct fields to specified table Todo: support struct array


    
    Insert DbRecord or struct fields to specified table  
    Todo: support struct array  


### isField

Check if field exists by name


    
    Check if field exists by name  


### loadAll

Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)


    
    Load entire ResultSet to memory, might be time/memory consuming!  
    @Todo?: add arg max row  
    @Todo: load to Table (instead?)  
      
      


### loadResultSet

Load ResultSet to DbRecord array Might be time and memory consuming!


    
    Load ResultSet to DbRecord array  
    Might be time and memory consuming!  
      


### loadTable

Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)


    
    Load entire ResultSet to memory, might be time/memory consuming!  
    @Todo?: add arg max row  
    @Todo: load to Table (instead?)  
      


### makeInsertFieldsText




    


### makeUpdateFieldsText

Prepare SQL text from cell array "UPDATE master_table set RecID=?,FInt=? WHERE..."


    
    Prepare SQL text from cell array  
    "UPDATE master_table set RecID=?,FInt=? WHERE..."  


### makeWhereFieldsText

Prepare SQL text from cell array "WHERE RecID=? AND FInt=?..."


    
    Prepare SQL text from cell array  
    "WHERE RecID=? AND FInt=?..."  


### newRecord

Create new empty record associated with this query


    
    Create new empty record associated with this query  
      


### next

Move cursor to next record, return false if reached end of data


    
    Move cursor to next record, return false if reached end of data  


### openConn

Open connection, throw exception on failure


    
    Open connection, throw exception on failure  


### perfTest

DbQuery.perfTest


    
    DbQuery.perfTest  
      
    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software  
    On Linux, use DataGrip by JetBrains  


### prev

Move cursor to previous record, return false if reached end of data


    
    Move cursor to previous record, return false if reached end of data  


### query

Run SELECT query, for other statements use exec() If no char argument is specified, use the current Obj.SqlText @Todo: Support query with params Example: Result = query('SELECT COUNT(*) from master_table')


    
    Run SELECT query, for other statements use exec()  
    If no char argument is specified, use the current Obj.SqlText  
    @Todo: Support query with params  
    Example:  
    Result = query('SELECT COUNT(*) from master_table')  
      
    Run SELECT statement (using java calls)  


### select

Execute SELECT Fields FROM TableName and load results to memory Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')


    
    Execute SELECT Fields FROM TableName and load results to memory  
    Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')  


### selectCount

Select number of records with optionally where clause


    
    Select number of records with optionally where clause  


### selectTableCount

Select number of records with optionally where clause


    
    Select number of records with optionally where clause  


### selectWhere

SELECT the specified fields from table, using where clause


    
    SELECT the specified fields from table, using where clause  


### setConnection

Set connection Connection argument may be either: - Empty string: use default connetion() - Non empty string: used as connection key for DbConnection.getDbConnection - DbConnection object


    
    Set connection  
    Connection argument may be either:  
    - Empty string: use default connetion()  
    - Non empty string: used as connection key for DbConnection.getDbConnection  
    - DbConnection object  
      
      


### setStatementValues

Set statement values from specified DbRecord or struct


    
    Set statement values from specified DbRecord or struct  


### stressTest

DbQuery.stressTest


    
    DbQuery.stressTest  
      
    Stree Test @Todo  
    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software  
    On Linux, use DataGrip by JetBrains  


### unitTest

Unit-Test On Windows, use SQL Manager Lite for PostgreSQL by EMS Software On Linux, use DataGrip by JetBrains


    
    Unit-Test  
    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software  
    On Linux, use DataGrip by JetBrains  
      


### unitTestDev

Unit-Test On Windows, use SQL Manager Lite for PostgreSQL by EMS Software On Linux, use DataGrip by JetBrains


    
    Unit-Test  
    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software  
    On Linux, use DataGrip by JetBrains  


### updateRecord

Update record


    
    Update record  


