# Class: db.AstroDb



    
    AstroImage database adaptor  
      
    Usage: Use the static functions  
      
    db.AstroDb.insertHeader  
    db.AstroDb.insertCatalog  
    db.AstroDb.getDefaultQuery  
      
      
    Threads - For optional future use  
      
    https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1  
    https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2  
      
### AstroDb




    
### get




    
### getDefaultQuery

Get default database query


    
    Get default database query  
### insertCatalog

Insert AstroCatalog / AstroTable to the specified database table Note that AstroCatalog is derived from AstroTable


    
    Insert AstroCatalog / AstroTable to the specified database table  
    Note that AstroCatalog is derived from AstroTable  
      
    Values in AstroCatalog.Catalog = [];  
    Field names in AstroCatalog.ColNames cell = {};  
    arguments  
    Input                    AstroHeader / AstroImage  
    TableName char  
    Args.Fields = {}         As  
    Args.Uuid = []           Empty uses AstroHeader.Uuid  
    Args.Query = []          db.DbQuery  
    end  
      
### insertCatalogImpl

Internal implementations Insert AstroCatalog / AstroTable to the specified database table Note that AstroCatalog is derived from AstroTable


    
    Internal implementations  
    Insert AstroCatalog / AstroTable to the specified database table  
    Note that AstroCatalog is derived from AstroTable  
      
    Values in AstroCatalog.Catalog = [];  
    Field names in AstroCatalog.ColNames cell = {};  
### insertHeader

Insert AstroHeader/AstroImage object to the specified database table arguments


    
    Insert AstroHeader/AstroImage object to the specified database table  
    arguments  
    Input                            AstroHeader / AstroImage  
    TableName char  
    Args.Fields = {}                 As  
    Args.Uuid = []                   Empty uses AstroHeader.Uuid  
    Args.Query = []                  db.DbQuery  
    end  
      
### insertHeaderImpl

Internal implementation Insert AstroHeader/AstroImage object to the specified database table


    
    Internal implementation  
    Insert AstroHeader/AstroImage object to the specified database table  
### manage

@Todo: Manage queue of pending operations


    
    @Todo: Manage queue of pending operations  
      
    Get next operation from queue  
### perfTest




    
### stressTest




    
### unitTest

AstroDb.unitTest


    
    AstroDb.unitTest  
      
