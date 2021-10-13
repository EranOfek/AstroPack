# Package: tools.cell


### tools.cell.cell2_equal

Check if two cell array have an identical content Package: Util.cell Description: Given two cell array containing string or numeric, compare the content of the two cells element by element.


    
    Check if two cell array have an identical content  
    Package: Util.cell  
    Description: Given two cell array containing string or numeric, compare  
    the content of the two cells element by element.  
    Input  : - First cell array.  
    - Secodn cell array.  
    - Delete spaces from strings prior to comparison {true|false}.  
    Default is true.  
    Output : - A flag indicating if the two cells are identical.  
    - A vector indicating if each element of the cells is identical.  
    If the cells have different size then return NaN.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [IsEqual,IsEqualEach]=Util.cell.cell2_equal({'2',1},{'2',1})  
    Reliable: 2  
      
      
### tools.cell.cell2mat_nan

Convert numeric cell to matrix. Replace empty cells with NaNs. Package: tools.cell Description: Convert a numeric only cell array to matrix. Replace empty cells by NaN.


    
    Convert numeric cell to matrix. Replace empty cells with NaNs.  
    Package: tools.cell  
    Description: Convert a numeric only cell array to matrix. Replace  
    empty cells by NaN.  
    Input  : - Cell array.  
    Output : - Matrix.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jan 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
### tools.cell.cell_equal

Compare the content of a cell array to its first cell. Package: Util.cell Description: Given a cell array of strings or numbers, compare the value of all the cells to the first cell and check if they are equal.


    
    Compare the content of a cell array to its first cell.  
    Package: Util.cell  
    Description: Given a cell array of strings or numbers, compare the value  
    of all the cells to the first cell and check if they are  
    equal.  
    Input  : - A cell array  
    - Delete spaces from strings prior to comparison {true|false}.  
    Default is true.  
    Output : - A flag indicating of all the cell elements are identical.  
    - A vector indicating if each cell element is equal to the  
    elelemnt in the first cell.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: cell_equal({'A','A','A'}); cell_equal({1,'2','E'});  
    Reliable: 2  
      
### tools.cell.cell_find_groups

Find all cell lines with identical values, and return indices of groups. Package: Util.cell Description: Given a cell array, find all lines with identical values. Each such line defines a group.


    
    Find all cell lines with identical values, and return indices of groups.  
    Package: Util.cell  
    Description: Given a cell array, find all lines with identical values.  
    Each such line defines a group.  
    Input  : - Cell array which element contains strings or numbers.  
    - Delete spaces from strings prior to comparison {true|false}.  
    Default is true.  
    Output : - Structure of groups. Each structure represent a group of rows  
    in the input cell array which have equal values.  
    The structure contains the following fields:  
    .Conntent  - The row of values defines the group.  
    .ptr       - The indices of the rows that belong to the  
    group.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Groups,Flag]=tools.cell.cell_find_groups({1 2;1 2;'E' 2;'R', 'V';'E',2})  
    Reliable: 2  
      
      
      
      
### tools.cell.cell_insert

Insert elements within a cell array. Package: Util.cell Description: Insert elements within a cell array.


    
    Insert elements within a cell array.  
    Package: Util.cell  
    Description: Insert elements within a cell array.  
    Input  : - A 1-D cell array.  
    - An element or a cell array of multiple elements to insert.  
    Note that if multiple elements that they will be inserted  
    together at the same position.  
    - Index in which to insert. Use 0 to insert in the begining.  
    If multiple indices, then the whole insert string will be  
    inserted in each requested position.  
    Output : - A 1-D cell array with the inserted elements.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: NewCell=cell_insert({'a',b','c','d',e'},{'b1','b2'},2)  
    NewCell=cell_insert({'a',b','c','d',e'},{'b1'},[2 2 2])  
    Reliable: 2  
      
      
      
### tools.cell.cell_sprintf

sprintf on content in cell array Pacakfe: Util.cell Description: sprintf into strings in a cell array.


    
    sprintf on content in cell array  
    Pacakfe: Util.cell  
    Description: sprintf into strings in a cell array.  
    Input  : - A cell array of format stings (e.g., 'My Name is s').  
    - A cell array of parameters to write into the format strings.  
    A line per format string. If only one line is provided than  
    will use it for all format strings.  
    Output : - A cell array of output strings.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: NewCell=cell_sprintf({'s_s','s_s'},{'A','a';'B','b'})  
    NewCell=cell_sprintf({'s_s','s_s'},{'A','a'})  
    Reliable: 2  
      
      
### tools.cell.cellstr2num_dictionary

Given a cell array of strings, repace unique strings by numeric index Package: Util.cell Description: Concert a cell array of strings to a vector of numeric indices. Each numeric index represent a unique string. Also returns a dictionary that translate the string name to


    
    Given a cell array of strings, repace unique strings by numeric index  
    Package: Util.cell  
    Description: Concert a cell array of strings to a vector of numeric  
    indices. Each numeric index represent a unique string.  
    Also returns a dictionary that translate the string name to  
    its index.  
    Input  : - Cell array of strings.  
    - Optional dictionary to which to append the new dictionary.  
    Output : - A vector of indices.  
    - A structure array containing the dictionary.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Vector,Dictionary]=Util.cell.cellstr2num_dictionary(C{3});  
    Reliable: 2  
      
      
      
### tools.cell.cellstr_prefix

Add a prefix to all the strings in a char array. Package: Util.cell Description: Add a prefix to all the strings in a char array.


    
    Add a prefix to all the strings in a char array.  
    Package: Util.cell  
    Description: Add a prefix to all the strings in a char array.  
    Input  : - A char array.  
    - A string prefix.  
    Output : - A new char array with prefix.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Array=Util.cell.cellstr_prefix({'a','b','c'},'A_')  
    Reliable: 2  
      
      
### tools.cell.group_cellstr

Find and group distinct values in a cell array. Package: Util.cell Description: Given a cell array of strings create a cell array of indices of distinct strings (see example).


    
    Find and group distinct values in a cell array.  
    Package: Util.cell  
    Description: Given a cell array of strings create a cell array of  
    indices of distinct strings (see example).  
    Input  : - Cell vector of strings.  
    - Case sensitive {'y' | 'n'}, default is 'y'.  
    Output : - Cell vector of distinctive groups.  
    - Cell vector of indices of distinct groups (see example).  
    - Cell vector of flags indicating if a member of the input  
    list belong to this group (1) or not (0).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                   May 2006  
    URL : http://weizmann.ac.il/home/eran/matlab/  
    Example: A={'try1';'try2';'hhh';'try2'}  
    [GroupStr,GroupInd]=Util.cell.group_cellstr(A);  
    Group contains:  
    GroupStr{1}='try1';  GroupInd{1}=[1];  
    GroupStr{2}='try2';  GroupInd{2}=[2 4];  
    GroupStr{3}='hhh';   GroupInd{3}=[3];  
    Reliable: 1  
      
### tools.cell.ind_cell

Select indices of vectors in cell array of vectors. Package: Util.cell Description: Given a cell vector in which each element contains a vector of the same length and a vecor of indices, return a new cell array of the same size in which each element


    
    Select indices of vectors in cell array of vectors.  
    Package: Util.cell  
    Description: Given a cell vector in which each element contains a  
    vector of the same length and a vecor of indices, return  
    a new cell array of the same size in which each element  
    contains a vecor of only the elements which indices are  
    specified (see example).  
    Input  : - A cell vecor.  
    - A vector of indices.  
    Output : - A new cell vector, in which each element  
    contains a vecor of only the elements which indices are  
    specified (see example).  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Feb 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: A{1} = rand(100,1); A{2} = rand(100,1);  
    I = find(A{1}<0.5);  
    B = ind_cell(A,I);   returns B{1}=A{1}(I); B{2}=A{2}(I);  
    Reliable: 1  
      
      
### tools.cell.isempty_cell

Check if each cell element is empty. Package: Util.cell Description: Given a cell array, return a matrix of flags indicating if each one of the cells is empty.


    
    Check if each cell element is empty.  
    Package: Util.cell  
    Description: Given a cell array, return a matrix of flags indicating  
    if each one of the cells is empty.  
    Input  : - Cell array.  
    Output : - Matrix of flags indicating if each one of the cells is  
    empty. 1 if empty, and 0 if not empty.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Flag=Util.cell.isempty_cell({1, [], 'a'})  
    Reliable: 2  
      
      
### tools.cell.isnan_cell

Check if cell elemt is NaN. Package: Util.cell Description: Given a cell array, return a matrix of flags indicating if each one of the cells is nan.


    
    Check if cell elemt is NaN.  
    Package: Util.cell  
    Description: Given a cell array, return a matrix of flags indicating  
    if each one of the cells is nan.  
    Input  : - Cell array.  
    Output : - Matrix of flags indicating if each one of the cells is  
    NaN. 1 if NaN, and 0 if not NaN.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Jun 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
      
### tools.cell.remove_cell_element

Remove a list of indices from a cell vector. Package: Util.cell Description: Remove a list of indices from a cell vector.


    
    Remove a list of indices from a cell vector.  
    Package: Util.cell  
    Description: Remove a list of indices from a cell vector.  
    Input  : - Cell vector.  
    - Indices of elements to remove from cell vector.  
    Output : - New cell vector..  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      June 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cell = {1;2;3}; NewCell=Util.cell.remove_cell_element(Cell,[1 2]);  
    Reliable: 1  
    -  
      
### tools.cell.sort_numeric_cell

sort each row or columns in a cell array of numbers. Package: Util.cell Description: sort each row or columns in a cell array of numbers. see also: sortrows_numeric_cell.m


    
    sort each row or columns in a cell array of numbers.  
    Package: Util.cell  
    Description: sort each row or columns in a cell array of numbers.  
    see also: sortrows_numeric_cell.m  
    Input  : - Cell array containing numbers.  
    - Dimension along which to sort (default is 1).  
    - Mode ('ascend' | 'descend'). Default is 'ascend'.  
    Output : - A sorted cell array.  
    - The indices such that SortedCell=Cell(SortedInd).  
    See also: sortrows_numeric_cell.m  
    Tested : Matlab 2012a  
    By : Eran O. Ofek                    Oct 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [SortedCell,SortedInd]=Util.cell.sort_numeric_cell(Cell)  
    Reliable: 2  
      
      
      
### tools.cell.sprintf2cell

Generate a cell array of string using sprintf. Package: Util.cell Description: Generate a cell array of strings using the sprintf function, where the sprintf arguments, per string in cell is taken from a row in a matrix.


    
    Generate a cell array of string using sprintf.  
    Package: Util.cell  
    Description: Generate a cell array of strings using the sprintf function,  
    where the sprintf arguments, per string in cell is  
    taken from a row in a matrix.  
    Input  : - Template string. E.g., 'Name05d_c02d.fits'.  
    - Matrix of arguments, where the number of rows is the  
    number of requested cells, and the number of columns  
    is the number of arguments.  
    Alternatively this can be a cell array of strings.  
    Output : - Cell array of strings.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: CellS=Util.cell.sprintf2cell('APASS_htm05d.mat',(1:1:10)');  
    CellS=Util.cell.sprintf2cell('APASS_htm05d_02d.mat',[1 2;3 4]);  
    CellS=Util.cell.sprintf2cell('02d:02d:06.3f',convertdms(rand(10,1),'r','H'));  
    CellS=Util.cell.sprintf2cell('APASS_htms_s.mat',{'a','b';'c','d'});  
    Reliable: 2  
      
      
### tools.cell.strNameDict2ind

Search the first appearance of string in dictionary in another cell.


    
    Search the first appearance of string in dictionary in another cell.  
    Input  : - A cell array of names.  
    - A cell array of dictionary names.  
    Outout : - Index of the dictionary first found name in the cell of  
    strings.  
    - String name at that index.  
    Author : Eran Ofek (Jul 2021)  
    Example: [Ind, Name] = tools.cell.strNameDict2ind({'d','b','a','c'},{'a','A','c'})  
      
### tools.cell.unique_cell_grouping

Find unique lines in the cell matrix. Package: Util.cell Description: Given a cell matrix containing eithr strings or numeric values, find unique lines in the cell matrix.


    
    Find unique lines in the cell matrix.  
    Package: Util.cell  
    Description: Given a cell matrix containing eithr strings or numeric  
    values, find unique lines in the cell matrix.  
    Input  : - 2-D cell array.  
    Output : - Structure array of unique lines.  
    Each element coresponds to a unique line and contains two fields:  
    .Line - The values in the unique line, where strings were converted  
    to numeric values.  
    .Ind - contains the indices of the unique lines.  
    - A structure array containing the unique  
    values in each column.  
    - The input matrix converted to numeric values.  
    Tested : matlab 7.11  
    By : Eran O. Ofek                    Jun 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: B={'a','b',[1];'a','b',[1];'c','c',[1]}  
    [Unique,UniqueCol,MatCell]=Util.cell.unique_cell_grouping(B);  
    Reliable: 2  
    -  
      
