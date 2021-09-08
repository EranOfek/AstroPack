# Package: tools.struct


### tools.struct.isfield_check

If field exist run a function of fiel. Package: Util.struct Description: Check if a field name exist in structure, run a function on the field content and return the function output.


    
    If field exist run a function of fiel.  
    Package: Util.struct  
    Description: Check if a field name exist in structure, run a function  
    on the field content and return the function output.  
      
    Input  : - Structure.  
    - String containing field name.  
    - Function handle. Default is @all, so it will check if all  
    the field values are true.  
    Output : - Return the function output. Retuern false, if the field  
    doesn't exist.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Answer=Util.struct.isfield_check(Cat,'isHTM');  
    Reliable:  
      
      
### tools.struct.isfield_notempty

Check if field exist and not empty Package: Util.struct Description: Check if a field exist in a structure and if it is not empty.


    
    Check if field exist and not empty  
    Package: Util.struct  
    Description: Check if a field exist in a structure and if it is not  
    empty.  
    Input  : - Structure array.  
    - String containing field name.  
    Output : - A flag indicating if the field exist and not empty (true),  
    or otherwise (false).  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Flag=Util.struct.isfield_notempty(Sim,'Mask');  
    Reliable: 2  
      
      
### tools.struct.mergeStruct

Merge two structures (unique fields)


    
    Merge two structures (unique fields)  
    Input  : - First structure  
    - Second structure  
    Output : - Structure with merged fields.  
    Eran Ofek      Mar 2021  
### tools.struct.mergeStructArray

Merge each field in a structure array into the same field in a single element structure. The merging is done along the first dimension.


    
    Merge each field in a structure array into the same field in a single element structure.  
    The merging is done along the first dimension.  
    Input  : - A structure array.  
    Output : - A single element structure, with the same fields as the  
    input structure array.  
    Author : Eran Ofek (Aug 2021)  
    Example: A.A=ones(2,1,3); A(2).A=2.*ones(2,1,3); A(3).A=3.*ones(3,1,3);  
    MergedStruct = tools.struct.mergeStructArray(A);  
      
### tools.struct.reshapeFields

Reshape all fields which have a consistent size Given a NewSize vector, the program will search fields in which their first dimension length is equal to prod(NewSize), and will reshape these fields intwo [NewSize, additionl_dim]


    
    Reshape all fields which have a consistent size  
    Given a NewSize vector, the program will search fields in which  
    their first dimension length is equal to prod(NewSize), and will  
    reshape these fields intwo [NewSize, additionl_dim]  
    Input  : - A structure array  
    - A vector of new size. (vector, or multiple arguments).  
    - Behavior:  
    'first' - compare only the first dim of the field to  
    prod(NewSize). Default.  
    'all' - compare the numel of the field to  
    prod(NewSize).  
    Output : - If a field contains a vector which number of elements is  
    consistent with the new size, then the fields will be  
    reshaped with this new size.  
    Author : Eran Ofek (Aug 2021)  
    Example: St.A = ones(20,1); St(1).B = 1; St(2).A=ones(20,1); St(2).B=ones(20,1);  
    St = tools.struct.reshapeFields(St, [5, 4]);  
      
### tools.struct.sort_struct

Sort all the elements in a structure by one fields in the structure. Package: Util.struct Description: Sort in ascending order all the elements in a structure by one fields in the structure.


    
    Sort all the elements in a structure by one fields in the structure.  
    Package: Util.struct  
    Description: Sort in ascending order all the elements in a  
    structure by one fields in the structure.  
    Input  : - Structure.  
    - String containing structure field which the structure  
    elements will be sorted according to.  
    - Column number[s] is the structure field, on which  
    the columns will be sorted.  
    Output : - Sorted structure.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                  January 2008  
    URL : http:/wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### tools.struct.struct2keyval

Convert a structure into a cell array of key,val,... Package: +tools.struct


    
    Convert a structure into a cell array of key,val,...  
    Package: +tools.struct  
    Input  : - A structure.  
    - A logical indicating if the output is a row cell vector  
    (false), or a two column cell array [key, val] (true).  
    Default is false.  
    Output : - A cell array of key,val, in which the even elements are field  
    names, and the corresponding odd elements are values.  
    Example : St.A = 1; St.B = 2; C=tools.struct.struct2keyval(St);  
      
### tools.struct.struct2keyvalcell

Structure field name and content to cell array of key,val pairs Package: Util.struct Description: Given a structure convert the field names and their values to a cell array of ...,key,val,... arguments.


    
    Structure field name and content to cell array of key,val pairs  
    Package: Util.struct  
    Description: Given a structure convert the field names and their values  
    to a cell array of ...,key,val,... arguments.  
    Input  : - Structure.  
    Output : - A cell array.  
    Tested : Matlab R2013a  
    By : Eran O. Ofek                    Sep 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: S.In=1; S.Out='a'; Cell=Util.struct.struct2keyvalcell(S);  
    Reliable: 2  
      
      
### tools.struct.struct2varargin

Structure field name and content to cell array of key,val pairs Package: Util.struct Description: Given a structure, prepare a cell array of all the field_names, field_values,...


    
    Structure field name and content to cell array of key,val pairs  
    Package: Util.struct  
    Description: Given a structure, prepare a cell array of all the  
    field_names, field_values,...  
    This is useful for converting InPar to varargin input.  
    Input  : - A structure.  
    Output : - A cell array of all the field_names, field_values,...  
    Tested : Matlab R2013a  
    By : Eran O. Ofek                    Aug 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VArg=Util.struct.struct2varargin(InPar);  
    Reliable: 2  
      
      
### tools.struct.struct_def

Define a structure array of a specific size with fields. Package: Util.struct Description: Define a structure array of a specific size with fields specified in a cell array of names.


    
    Define a structure array of a specific size with fields.  
    Package: Util.struct  
    Description: Define a structure array of a specific size with fields  
    specified in a cell array of names.  
    Input  : - A cell array of field names.  
    - Number of lines in array, or [lines, rows]. Default is 1.  
    - Number of Rows in array. Default is 1.  
    Output : - A structure array.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Struct=Util.struct.struct_def({'Field1','Field2'},2,1);  
    Reliable: 2  
      
      
### tools.struct.structcon

Concatenate two structures into one. Package: Util.struct Description: Concatenate two structures into one. Example: S1.A S1.B, and S2.A, S2.B:


    
    Concatenate two structures into one.  
    Package: Util.struct  
    Description: Concatenate two structures into one.  
    Example: S1.A S1.B, and S2.A, S2.B:  
    S=structcon(S1,S2,1); will return a structure S,  
    with fields A and B which contains the content  
    of [S1.A;S2.A] and [S1.B;S2.B], respectively.  
    The concatantion can be done along the 1st or 2nd dimensions.  
    Input  : - First structure.  
    - Second structure, containing the same fields as the  
    first structure.  
    - Dimension along to concatenate the fields.  
    1 - column concatenation;  
    2 - raw concatenation.  
    Output : - New structure  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                  December 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: A.a.a = 1; A.a.b=[2 3]; A.c='ab'; A.d{1}='a'; A.d{2}=1;  
    S=Util.struct.structcon(A,A,1);  
    Reliable: 2  
      
      
### tools.struct.structcut

Select elements in structure fields by indices. Pacakage: Util.struct - structcut function                                                General


    
    Select elements in structure fields by indices.  
    Pacakage: Util.struct  
    -  
    structcut function                                                General  
    Description: Given a structure and a vector of indices, select  
    from each field in the structure only the rows in each  
    field which are specified by the vector of indices.  
    Input  : - Structure.  
    - Vector of indices.  
    Output : - new structure, with the selected rows only.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jul 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
      
