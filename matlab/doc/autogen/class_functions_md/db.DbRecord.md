# Class: db.DbRecord



    
    Database record with dynamic properties  
    Similar to struct, but based on dynamicprops class  
    Used by DbQuery with select and insert SQL operations.  
      
      
      

### Functions List

    DbRecord - Constructor DbRecord()          - Create new empty record object DbRecord(DbQuery)   - Create object linked to specified query
    Equal - Compare two records, return true if equal
    addProp - Add new property with value
    delete - 
    getFieldNames - Get list of field names, properties ending with '_' are excluded
    getStruct - Return new struct with field values Field names ending with '_' are ignored
    loadFile - Load specified file to property @Todo - not implemented yet
    loadStruct - Load all struct fields to properties
    unitTest - DbRecord.unitTest

### DbRecord

Constructor DbRecord()          - Create new empty record object DbRecord(DbQuery)   - Create object linked to specified query


    
    Constructor  
    DbRecord()          - Create new empty record object  
    DbRecord(DbQuery)   - Create object linked to specified query  
      
      
    Generate unique id, as Uuid or SerialStr (more compact and fast)  


### Equal

Compare two records, return true if equal


    
    Compare two records, return true if equal  
      


### addProp

Add new property with value


    
    Add new property with value  


### delete




    


### getFieldNames

Get list of field names, properties ending with '_' are excluded


    
    Get list of field names, properties ending with '_' are excluded  
      


### getStruct

Return new struct with field values Field names ending with '_' are ignored


    
    Return new struct with field values  
    Field names ending with '_' are ignored  
      


### loadFile

Load specified file to property @Todo - not implemented yet


    
    Load specified file to property  
    @Todo - not implemented yet  


### loadStruct

Load all struct fields to properties


    
    Load all struct fields to properties  
      
    Iterate all struct fields  


### unitTest

DbRecord.unitTest


    
    DbRecord.unitTest  
      


