# Class: db.DbRecord



    
    Database record with dynamic properties  
    Similar to struct, but based on dynamicprops class  
    Used by DbQuery with select and insert SQL operations.  
      
      
      

### Functions List

    DbRecord - Constructor - @Todo - discuss corret row,col order! Data: struct array, table, cell array, matrix
    Equal - Compare two records, return true if equal
    addProp - Add new property with value
    convert2AstroCatalog - Convert record(s) to AstroCatalog
    convert2AstroTable - Convert record(s) to AstroTable
    convert2cell - Convert record(s) to cell
    convert2mat - Convert record(s) to matrix, non-numeric fields are
    convert2table - Convert record(s) to table
    delete - 
    getFieldNames - Get list of field names, properties ending with '_' are excluded
    getStruct - Return new struct with field values Field names ending with '_' are ignored
    loadFile - Load specified file to property @Todo - not implemented yet
    loadStruct - Load all struct fields to properties
    merge - Merge struct array with current data Usefull when we constructed from matrix and need key fields
    newKey - Generate unique id, as Uuid or SerialStr (more compact and fast)
    unitTest - DbRecord.unitTest

### DbRecord

Constructor - @Todo - discuss corret row,col order! Data: struct array, table, cell array, matrix


    
    Constructor - @Todo - discuss corret row,col order!  
    Data: struct array, table, cell array, matrix  


### Equal

Compare two records, return true if equal


    
    Compare two records, return true if equal  
      


### addProp

Add new property with value


    
    Add new property with value  


### convert2AstroCatalog

Convert record(s) to AstroCatalog


    
    Convert record(s) to AstroCatalog  


### convert2AstroTable

Convert record(s) to AstroTable


    
    Convert record(s) to AstroTable  


### convert2cell

Convert record(s) to cell


    
    Convert record(s) to cell  


### convert2mat

Convert record(s) to matrix, non-numeric fields are


    
    Convert record(s) to matrix, non-numeric fields are  


### convert2table

Convert record(s) to table


    
    Convert record(s) to table  


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


### merge

Merge struct array with current data Usefull when we constructed from matrix and need key fields


    
    Merge struct array with current data  
    Usefull when we constructed from matrix and need key fields  


### newKey

Generate unique id, as Uuid or SerialStr (more compact and fast)


    
    Generate unique id, as Uuid or SerialStr (more compact and fast)  


### unitTest

DbRecord.unitTest


    
    DbRecord.unitTest  


