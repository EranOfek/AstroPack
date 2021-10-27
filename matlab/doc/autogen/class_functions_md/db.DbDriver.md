# Class: db.DbDriver



    
    Database Driver Class, currently supports PostgreSQL  
    DbDriver is used **internally** by DbConnection, and SHOULD NOT be  
    accessed by the user.  
    We currently use postgresql-42.2.19.jar  
      
    The native MATLAB database functions require installation of MATLAB  
    Database Toolbox. To avoid dependency on it, we implement our database  
    classes using Java packages. Some workaround is required to use the java interface.  
    See: https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database  
      
    PostgreSQL Java driver (jar file) must be installed, download page:  
    https://jdbc.postgresql.org/  
    https://jdbc.postgresql.org/download.html  
      
    There should be only one driver object for each database type.  
    For example, when working with Postgress, we need only one DbDriver for  
    it. We hold a persistent ComponentMap of all created DbDriver objects with  
    the database type (i.e. 'postgres') as the key.  
      
    Usage:  
    Drv = DbDriver.getDbDriver('postgres');  
    Drv.loadDriver();  
      
      
      
      

### Functions List

    DbDriver - Constructor
    copyDriverFile - Copy driver file from source folder to target path This is requried to call javaclasspath()
    delete - Destructor
    getDbDriver - Get singleton Map object that maps database type to DbDriver object
    loadDriver - Load database driver library See https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
    unitTest - DbDriver.unitTest
    unloadDriver - Unload database driver

### DbDriver

Constructor


    
    Constructor  


### copyDriverFile

Copy driver file from source folder to target path This is requried to call javaclasspath()


    
    Copy driver file from source folder to target path  
    This is requried to call javaclasspath()  


### delete

Destructor


    
    Destructor  


### getDbDriver

Get singleton Map object that maps database type to DbDriver object


    
    Get singleton Map object that maps database type to DbDriver object  


### loadDriver

Load database driver library See https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database


    
    Load database driver library  
    See https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database  


### unitTest

DbDriver.unitTest


    
    DbDriver.unitTest  


### unloadDriver

Unload database driver


    
    Unload database driver  


