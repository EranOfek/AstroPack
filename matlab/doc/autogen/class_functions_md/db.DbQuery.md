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

Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/


    
    Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html  
    https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/  
### copyTo

Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/


    
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




    
      
### getFieldTable

Get fields as empty table


    
    Get fields as empty table  
      
### getFieldType

Get field type


    
    Get field type  
### getMetadata

Get metadata of current result-set or specified table


    
    Get metadata of current result-set or specified table  
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
    @Todo: Check how to get it without slect  
### getValidFieldName




    
### insert

Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;


    
    Insert new record to table, Keys and Values are celarray  
    sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;  
      
### insertCell




    
      
    Insert DbRecord or struct fields to specified table  
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
      
      
### loadTable

Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)


    
    Load entire ResultSet to memory, might be time/memory consuming!  
    @Todo?: add arg max row  
    @Todo: load to Table (instead?)  
      
### makeInsertFieldsText

Prepare SQL text from cell array "INSERT INTO master_table(RecID, FInt) VALUES (?,?)"


    
    Prepare SQL text from cell array  
    "INSERT INTO master_table(RecID, FInt) VALUES (?,?)"  
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


    
    Move cursor to next record, return false if reached end of  
    data  
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


    
    Move cursor to previous record, return false if reached end  
    of data  
### query

Run SELECT query, for other statements use exec() @Todo: Replace varargin with arguments block? Example: Result = query('SELECT COUNT(*) from master_table')


    
    Run SELECT query, for other statements use exec()  
    @Todo: Replace varargin with arguments block?  
    Example:  
    Result = query('SELECT COUNT(*) from master_table')  
      
    Run SELECT statement (using java calls)  
### select

Execute: SELECT Fields FROM TableName


    
    Execute: SELECT Fields FROM TableName  
      
### selectCount

Select number of records with optionally where clause


    
    Select number of records with optionally where clause  
### selectWhere

SELECT the specified fields from table, using where clause


    
    SELECT the specified fields from table, using where clause  
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
