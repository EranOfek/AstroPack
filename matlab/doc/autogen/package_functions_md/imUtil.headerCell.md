# Package: imUtil.headerCell


### imUtil.headerCell.cellhead2struct

convert a 3-column cell array containing header into a structure. Description: Remove duplicate keys, illegal characters and the 'END' key.


### imUtil.headerCell.deleteKey

delete lines with specific keys


### imUtil.headerCell.getByKey

get keyword value from an header in a cell format Package: @headCl Description: Given a 3 column cell array [Key, Val, Comment] search for a keyword name and return the sub header that


### imUtil.headerCell.getValBySynonym

Return the first key/val in the list of synonyms that appears in the cell-header.


### imUtil.headerCell.insertKey

Insert key/val into a header in cell array format


### imUtil.headerCell.replaceKey

Replace an cell-header keywords and values, or add if doesn't exist


### imUtil.headerCell.replace_illegal_char

replace illegal characters in keyword names (e.g. '-'). Description: Given a 3 column cell array. Replace illegal characters in the first column. List of illegal characters:


### imUtil.headerCell.uniqueKey

Remove non-unique keywords from a cell header


### imUtil.headerCell.updateCell_fromStruct

update cell header by adding elements in struct not in header Description: Given a 3 column cell array (i.e., cell header) and a structure with keywords and values. First make sure all the header keywords have no


