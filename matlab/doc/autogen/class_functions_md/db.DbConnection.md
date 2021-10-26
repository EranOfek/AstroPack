# Class: db.DbConnection



    
    DbConnection Class, connection to database on host, as use  
    Wrapper for Java Connection class  
    'jdbc:postgresql://localhost:5432/pipeline'  
      
    Used internally by DbQuery  
      
      
      

### Functions List

    DbConnection - Constructor
    close - Disconnect from database,  @Todo
    delete - Destructor
    getConnectionKey - Create connection key f - @TBD Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
    getDbConnection - Search global (singleton) map of DbConnection for the specified connection key
    newQuery - Create new DbQuery instance linked to this connection
    open - Connect to database specified by Host:Port:Database as UserName/Password
    setupDefault - 
    unitTest - dbConnection.unitTest

### DbConnection

Constructor


    
    Constructor  


### close

Disconnect from database,  @Todo


    
    Disconnect from database,  @Todo  
      


### delete

Destructor


    
    Destructor  


### getConnectionKey

Create connection key f - @TBD Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];


    
    Create connection key f - @TBD  
    Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];  


### getDbConnection

Search global (singleton) map of DbConnection for the specified connection key


    
    Search global (singleton) map of DbConnection for the  
    specified connection key  


### newQuery

Create new DbQuery instance linked to this connection


    
    Create new DbQuery instance linked to this connection  


### open

Connect to database specified by Host:Port:Database as UserName/Password


    
    Connect to database specified by Host:Port:Database as UserName/Password  
      


### setupDefault




    
      


### unitTest

dbConnection.unitTest


    
    dbConnection.unitTest  
      


