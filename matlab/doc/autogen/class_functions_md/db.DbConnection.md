# Class: db.DbConnection



    
    DbConnection Class, connection to database on host, as use  
    Wrapper for Java Connection class  
    'jdbc:postgresql://localhost:5432/pipeline'  
      
    Used internally by DbQuery  
      
### DbConnection




    
### close

Disconnect from database,  @Todo


    
    Disconnect from database,  @Todo  
      
### delete




    
### getConnectionKey

Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];


    
    Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];  
### getDbConnection




    
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
      
