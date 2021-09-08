# Package: imUtil.headerCell


### imUtil.headerCell.cellhead2struct

convert a 3-column cell array containing header into a structure. Description: Remove duplicate keys, illegal characters and the 'END' key.


    
    convert a 3-column cell array containing header into a structure.  
    Description: Remove duplicate keys, illegal characters and the 'END' key.  
    Input  : - A 3-column cell array.  
    * ...,key,val,...  
    'Occurence' - If duplicate keys which one to choose:  
    ['first'] | 'last'.  
    'RemoveEnd' - Remove the 'END' keyword. Default is true.  
    'ColKey' - Column index of keyword name. Default is 1.  
    'ColVal' - Column index of value. Default is 2.  
    'ColComment' - Column index of comment. Default is 3.  
    Output : - A structure, in which the field names are the keys.  
    Author : Eran Ofek (Mar 2021)  
    Example: St=imUtil.headerCell.cellhead2struct({'A',1,'';'B',2,'';'A','a','';'C','aaa','';'END','',''})  
      
### imUtil.headerCell.deleteKey

delete lines with specific keys


    
    delete lines with specific keys  
    Input  : - A 2 or 3 columns cell array  
    - A key name or a cell array of key names.  
    Output : - The input cell in which the lines with specific keys in the  
    first column were removed.  
    Author: Eran Ofek  (Mar 2021)  
    Example:  
    imUtil.headerCell.deleteKey({'A','a','';'B','a','';'C',1,''},'A')  
    imUtil.headerCell.deleteKey({'A','a','';'B','a','';'C',1,''},{'A','C'})  
      
### imUtil.headerCell.getByKey

get keyword value from an header in a cell format Package: @headCl Description: Given a 3 column cell array [Key, Val, Comment] search for a keyword name and return the sub header that


    
    get keyword value from an header in a cell format  
    Package: @headCl  
    Description: Given a 3 column cell array [Key, Val, Comment]  
    search for a keyword name and return the sub header that  
    contains the keyword.  
    Input  : - A 2 or 3 columnn cell array [Key, Val, Comment].  
    - Keyword name, or a cell array of keyword names to serch.  
    * ...,key,val,... or ...,key=val',... list  
    'SearchAlgo' - search using: ['strcmp'] | 'regexp'  
    'CaseSens' - Default is true.  
    'Fill' - Fill value if not exist: [] - skip. Default is NaN.  
    'Col' - Column in cell in which to search. Default is 1.  
    'ReturnN' - What to do if more than one is found.  
    [1] - return first, Inf - return all; 2 - return  
    second,...  
    'Val2Num' - Attempt to convert value to number. Default is  
    true.  
    Output : - A 3 column cell array with only the lines that  
    satisfy the search, or the filled values.  
    - A vector of logicals (one per Key) that indicat if the key  
    exist in the Cell header.  
    - Line indices in the input cell that satisfy the  
    search criteria. Filled values are not included.  
    - A cell array (number of elements equal to the number of keys)  
    in which each cell contains the indices of the found keys in  
    the cell-header.  
    Author: Eran Ofek  (Mar 2021)  
    Example: [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'ExpTime')  
    [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},{'ExpTime','A'})  
    [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'a','Col',2)  
    [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'Exp*','SearchAlgo','regexp')  
    [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'TYPE','Fill',[])  
    [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'TYPE','Fill',NaN)  
    [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','a',''},'ExpTime')  
    [SC,FE,II,IK]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','NaN',''},{'ExpTime','A'})  
    [SC,FE,II,IK]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','NaN','';'A',1,''},{'ExpTime','A'},'ReturnN',Inf)  
    [SC,FE,II,IK]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','NaN','';'A',1,''},{'ExpTime','A'},'ReturnN',1)  
    Reliable: 2  
      
### imUtil.headerCell.getValBySynonym

Return the first key/val in the list of synonyms that appears in the cell-header.


    
    Return the first key/val in the list of synonyms that appears in the cell-header.  
    Input  : - A 3 column cell array of {Key, Val, Comment}  
    - A cell array of synonyms to search in the cell key column.  
    * ...,key,val,...  
    'CaseSens'     - Case sensetive search. Default is true.  
    'SearchAlgo'   - ['strcmp'] | 'regexp'  
    'Occur'        - For each synonym return the ['first']  
    or 'last' match.  
    'Fill' - Fill value for the keyword Val, in case that the  
    key is not found. Default is NaN (comment will be  
    '').  
    'Val2Num' - Attempt to convert the value to numeric.  
    Default is true.  
    'ColKey' - Keywords column. Default is 1.  
    'ColVal' - Keywords column. Default is 2.  
    'ColCommeny' - Keywords column. Default is 3.  
    Output : - Keyword value.  
    - Keyword name.  
    - Keyword comment.  
    - For the synonym found, number of matches.  
    Author : Eran Ofek (Apr 2021)  
    Example: [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, {'C','A'})  
    [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, {'A','C'})  
    [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''; 'A',2,''}, {'A','C'})  
    [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, {'A','C'},'SearchAlgo','regexp')  
    [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''; 'A',2,''}, {'Aaa','Caa'})  
    [Val, Key, Comment, Nfound] =  
    imUtil.headerCell.getValBySynonym({'A',1','';'B','2','';'C','aa',''}, [])  return Args.Fill  
      
### imUtil.headerCell.insertKey

Insert key/val into a header in cell array format


    
    Insert key/val into a header in cell array format  
    Input  : - A 2 or 3 columns cell array.  
    - Either a string or a cell array of key/vals to insert.  
    If a string or a cell-column of keys, than these will be  
    padded with '' and inserted to the cell.  
    Alternatively, this can be a sub cell-header.  
    - Position to insert the new block: ['end-1'], 'end' or number.  
    Author: Eran Ofek  (Mar 2021)  
    Example:  
    NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},'C','end-1')  
    NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},'C','end')  
    NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},'C',1)  
    NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},{'C';'D'},2)  
    NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},{'C',1;'D',2},'end-1')  
      
### imUtil.headerCell.replaceKey

Replace an cell-header keywords and values, or add if doesn't exist


    
    Replace an cell-header keywords and values, or add if doesn't exist  
    Input  : - A 3 column cell array  
    - A key name or a cell array of key names.  
    - A cell array of values, corresponding to the key  
    names. Alternatively, a vector of numbers corresponding to the  
    key names.  
    * ...,key,val,... or ...,key=val',... list  
    'SearchAlgo' - search using: ['strcmp'] | 'regexp'  
    'CaseSens' - Default is true.  
    'RepVal'   - Replace value. Default is true.  
    'Comment'  - A cell array of optional comments.  
    If empty, then do not replace comment. Default is [].  
    'NewKey' - A cell array of new keys to replace the old keys.  
    If empty, then do not replace keys.  
    Default is {}.  
    'AddKey' - Add key if doens't exist. Default is true.  
    'AddPos' - Position in which to add key if doesn't exist.  
    Default is 'end-1'.  
    'ColKey' - Column index of keys. Default is 1.  
    'ColVal' - Column index of values. Default is 2.  
    'ColComment' - Column index of comments. Default is 3.  
    Output : - A new cell array with the replaced keys/values.  
    Example:  
    Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'C','aaa','a'},'A',{'AA'})  
    Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{10 12})  
    Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{10 12},'RepVal',false,'NewKey',{'q','r'})  
    Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{'pp' 12},'RepVal',true,'NewKey',{'q','r'})  
    Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'C','aaa','a'},'D',{'AA'})  
      
      
### imUtil.headerCell.replace_illegal_char

replace illegal characters in keyword names (e.g. '-'). Description: Given a 3 column cell array. Replace illegal characters in the first column. List of illegal characters:


    
    replace illegal characters in keyword names (e.g. '-').  
    Description: Given a 3 column cell array. Replace illegal  
    characters in the first column.  
    List of illegal characters:  
    '-' replace with '_'  
    Input  : - A 3 columns cell array  
    * ...,key,val,... or ...,key=val',... list  
    'Col' - Column index on which to replace chars.  
    Default is 1.  
    Output : - A 3 column cell array.  
    Example: Cell=imUtil.headerCell.replace_illegal_char({'A-NB',1,'';'a','1',''})  
    Reliable: 2  
      
### imUtil.headerCell.uniqueKey

Remove non-unique keywords from a cell header


    
    Remove non-unique keywords from a cell header  
    Input  : - A 2 or 3 column cell array.  
    * ..,key,val,... or ...,key=val',... list  
    'Col' - Column on which to operate the unique function.  
    Default is 1.  
    'Occur' - Select occurence: ['first'] | 'last'.  
    Output : - A cell array with unique keys.  
    - Indices of the selected rows.  
    Author: Eran Ofek  (Mar 2021)  
    Example: [SC,UI]=imUtil.headerCell.uniqueKey({'ExpTime',2,'';'A','a','';'ExpTime','3',''})  
      
### imUtil.headerCell.updateCell_fromStruct

update cell header by adding elements in struct not in header Description: Given a 3 column cell array (i.e., cell header) and a structure with keywords and values. First make sure all the header keywords have no


    
    update cell header by adding elements in struct not in header  
    Description: Given a 3 column cell array (i.e., cell header)  
    and a structure with keywords and values.  
    First make sure all the header keywords have no  
    illegal characters. Then look for fields in the  
    structure that are not in the cell header and  
    add them to the cell header.  
    Input  : - A structure.  
    - A cell header.  
    Output : - An updated cell header.  
    Example: C=imUtil.headerCell.updateCellFromStruct(struct('A',1,'B',2'),{'D',1;'END',''})  
    C=imUtil.headerCell.updateCellFromStruct(struct('A',1,'B',2'),{'A',2;'D',1;'END',''})  
      
