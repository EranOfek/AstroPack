# Package: tools.array


### tools.array.and_mat

Perform logical and operation between all the columns or rows of a matrix Package: Util.array Description: Perform logical and operation between all the columns or rows of a matrix.


    
    Perform logical and operation between all the columns or rows of a matrix  
    Package: Util.array  
    Description: Perform logical and operation between all the columns or  
    rows of a matrix.  
    Input  : - Matrix of logicals.  
    - Diemension along to do the logical and operation.  
    Default is 1.  
    Output : - Vector of logical result.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Mat = [false true; true true; false false]; Vec=and_mat(Mat,2);  
    Reliable: 2  
      
      
### tools.array.and_nan

Logical function "and" for NaNs. Package: Util.array Description: Logical function "and" for NaNs. This function is similar to "and" logical function, but NaNs are regarded as no information using the following logical table:


    
    Logical function "and" for NaNs.  
    Package: Util.array  
    Description: Logical function "and" for NaNs. This function is similar  
    to "and" logical function, but NaNs are regarded as no  
    information using the following logical table:  
    M1   M2    Result  
    1    1     1  
    1    0     0  
    0    1     0  
    0    0     0  
    NaN  1     1  
    NaN  0     0  
    1    NaN   1  
    0    NaN   0  
    NaN  NaN   1  
    Input  : - Matrix 1  
    - Matrix 2  
    Output : - The result.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                  December 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### tools.array.array_select

Select lines in an array which columns satisfay some criteria. Package: Util.array Description: Given a matrix, select lines which their column fullfill a specific criteria. For example, the values in the second columns are in some range or follow a specific criterion.


    
    Select lines in an array which columns satisfay some criteria.  
    Package: Util.array  
    Description: Given a matrix, select lines which their column fullfill  
    a specific criteria. For example, the values in the second  
    columns are in some range or follow a specific criterion.  
    Input  : - A matrix.  
    - An operator, @all or @any, which to use in order to combine  
    all the crietria. For example, if @all then all the crietria  
    will combined using and and operator.  
    If empty, use @all.  
    * Arbitrary number of arguments containing cell arrays of  
    criteria information.  
    This can be:  
    {Col Min Max} - where Col is a column index, and this column  
    should be >Min and <Max.  
    {Col @Fun, [Pars]} -In this case the criteria is  
    Fun(Matrix(:,Col),Pars), which return either  
    false or true.  
    Output : - A vector of flags (per line) indicating if line was fullfiling  
    (and/or) the list of criteria.  
    - A matrix of flags (per line, and column per criteria),  
    indicating if line was fullfiling each one of the criteria.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [FlagCom,Flag]=array_select(rand(1000,5),[],{1 0.8 0.9},{2 @gt 0.5});  
    Reliable: 2  
      
      
### tools.array.assoc_range

Index of points in each bin. Package: Util.array Description: Given a vector of data points and a vector of edges, for each value in the vector, return the index of the bin (defined by the edges) to which it belongs.


    
    Index of points in each bin.  
    Package: Util.array  
    Description: Given a vector of data points and a vector of edges,  
    for each value in the vector, return the index of the  
    bin (defined by the edges) to which it belongs.  
      
    Input  : - Vector od data points.  
    - Column vector of sorted edges.  
    - Behaviour type:  
    0 - Return NaN if the value is below the first edge or above  
    the last edge (default).  
    1 - Return 1 if the value is below the first edge, 2 if  
    the value is above the first edge and below the second  
    edge, etc.  
    Output : - Indices of bins (defined by edges association for each  
    data point.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Vec = [-1;0;1.5;17]; Edges = [0 1 2 3].'; RangeInd=assoc_range(Vec,Edges)  
    Reliable: 2  
      
      
      
### tools.array.bitand_array

Perform a bitand operation along all elements in an array. Package: Util.array Description: Perform a bitand operation along all elements in an array along a specific dimension.


    
    Perform a bitand operation along all elements in an array.  
    Package: Util.array  
    Description: Perform a bitand operation along all elements in an array  
    along a specific dimension.  
    Input  : - An array of integers.  
    - Dimension along to perform the bitand operation. Default is 1.  
    Output : - The result of the bitand operation.  
    See also: sum_bitor.m (the same)  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Val=bitand_array(Array);  
    Reliable: 2  
      
      
### tools.array.bitor_array

Perform a bitor operation along all elements in an array. Package: Util.array Description: Perform a bitor operation along all elements in an array along a specific dimension.


    
    Perform a bitor operation along all elements in an array.  
    Package: Util.array  
    Description: Perform a bitor operation along all elements in an array  
    along a specific dimension.  
    Input  : - An array of integers.  
    - Dimension along to perform the bitor operation. Default is 1.  
    Output : - The result of the bitor operation.  
    See also: sum_bitor.m (the same)  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Val=tools.array.bitor_array(Array);  
    Reliable: 2  
      
      
### tools.array.bsx_nsize

bsx_nsize gets multiple of array sizes (i.e. result of the size function), verifies they are candidates for bsxfun operation, and the size of the result array of bsxfun operation (S). The function also padding the sizes of the input arrays with ones, in case one of the size arrays is shorter (i.e. its last dimensions are singleton).


    
    bsx_nsize gets multiple of array sizes (i.e. result of the size function),  
    verifies they are candidates for bsxfun operation, and the size of the  
    result array of bsxfun operation (S).  
    The function also padding the sizes of the input arrays with ones, in  
    case one of the size arrays is shorter (i.e. its last dimensions are singleton).  
      
    The function uses array sizes rather than the arrays themself in order to  
    reduce the data traffic between the functions.  
### tools.array.bsx_size

bsx_size gets two arrays S1,S2 and calculates the size of the result array after a bsxfun operation is performed. The function verifies that the sizes are either identical, or for dimensions where thtey are differs, the dimension of one of the array must be scalar.


    
    bsx_size gets two arrays S1,S2 and calculates the size of the result  
    array after a bsxfun operation is performed.  
    The function verifies that the sizes are either identical, or for  
    dimensions where thtey are differs, the dimension of one of the array  
    must be scalar.  
    The function also padding the size arrays with ones, in case one of the  
    size arrays is shorter (i.e. its last dimensions are singleton.  
### tools.array.check_range

Replace out of bound indices with bound indices. Package: Util.array Description: Given the size of an N-dimensional array, and minimum and maximum indices per each dimension, check if the indices are out of bounds. If the minimum or maximum indices are


    
    Replace out of bound indices with bound indices.  
    Package: Util.array  
    Description: Given the size of an N-dimensional array, and minimum and  
    maximum indices per each dimension, check if the indices  
    are out of bounds. If the minimum or maximum indices are  
    out of bounds return a new minimum and maximum indices  
    that are in bound and nearest to the original indices.  
    Input  : - Vector of array size in each dimension (e.g., this can be  
    the output of size.m).  
    - Vector of minimum indices to check (one per dimension).  
    - Vector of maximum indices to check (one per dimension).  
    Output : - Vector of new minimum indices.  
    - Vector of new maximum indices.  
    - Vector of flags indicating of the minimum indices were  
    changed (1) or not (0).  
    - Vector of flags indicating of the maximum indices were  
    changed (1) or not (0).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [NMin,NMax,MinCh,MaxCh]=check_range(size(rand(5,3,2)),[1 0 -1],[6 2 2]);  
    Reliable: 2  
      
### tools.array.delete_ind

Delete a column/s or row/s from a matrix. Package: Util.array Description: Delete a column/s or row/s from a matrix.


    
    Delete a column/s or row/s from a matrix.  
    Package: Util.array  
    Description: Delete a column/s or row/s from a matrix.  
    Input  : - Matrix.  
    - Indices of rows or columns to remove.  
    - Dimension: 1 - Delete rows; 2 - Delete columns, default is 1.  
    Output : - A new matrix with selected column/s or row/s deleted.  
    See also: insert_ind.m  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                  December 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: NewMat=delete_ind(zeros(4,4),[2 3],1)  
    Reliable: 1  
    -  
### tools.array.find_ranges

Indices of vector values found within one of several ranges. Package: Util.array Description: Given a vector and several ranges, return the indices of values in the vector which are found within one of the ranges.


    
    Indices of vector values found within one of several ranges.  
    Package: Util.array  
    Description: Given a vector and several ranges, return the indices of  
    values in the vector which are found within one of the  
    ranges.  
    Input  : - Column vector of values.  
    - Matrix of ranges, in which each row specify a range.  
    The first column is for the range lower bound and the  
    second colum for the upper bound.  
    If the first column is NaN than look for NaN  
    Output : - List of indices.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek               Feb 2008  
    URL : http://weizmann.ac.il/eofek/matlab/  
    Example: [Ind]=Util.array.find_ranges([1 2 3 4 5 NaN],[0 1.5; NaN NaN]);  
    Reliable: 1  
      
      
### tools.array.find_ranges_flag

Check if values are within some ranges. Package: Util.array Description: Given a vector and several ranges, return the a vector indicating if a given position in the input vector is included in one of the ranges.


    
    Check if values are within some ranges.  
    Package: Util.array  
    Description: Given a vector and several ranges, return the a  
    vector indicating if a given position in the input vector  
    is included in one of the ranges.  
    Input  : - Column vector of values.  
    - Matrix of ranges, in which each row specify a range.  
    The first column is for the range lower bound and the  
    second colum for the upper bound.  
    If the first column is NaN than look for NaN  
    Output : - A vector (the same length as the input vector) in which  
    zero indicates that the position is not included in one  
    of the ranges, and other numbers indicate the index of  
    the range.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Flag]=find_ranges_flag((1:1:10),[1 3;6 7]);  
    [Flag]=find_ranges_flag([(1:1:10),NaN],[1 3;6 7; NaN NaN])  
    Reliable: 1  
      
      
### tools.array.findmany

Find all values in a vector in another vector or matrix. Package: Util.array Description: Find all values in a vector in another vector or matrix.


    
    Find all values in a vector in another vector or matrix.  
    Package: Util.array  
    Description: Find all values in a vector in another vector or matrix.  
    Input  : - Matrix.  
    - Vector of values. These values will be searched in the matrix.  
    Output : - Indices of all the values found in the matrix.  
    - Cell array in which the i-th element is a vector of indices in  
    the matrix that are equal to one value of the i-th element  
    in the vector.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Nov 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Mat = [1;2;3;3;4;4;5];  
    [I,Ifound]=findmany(Mat,[1;4]);  
    Reliable: 1  
      
      
### tools.array.flag2regions

Identify continous ranges of true values. Package: Util.array Description: Given a column vector flags (true/false) returns pairs of indices of the positions of continuus regions in which the flag is true.


    
    Identify continous ranges of true values.  
    Package: Util.array  
    Description: Given a column vector flags (true/false) returns pairs of  
    indices of the positions of continuus regions in which the  
    flag is true.  
    Input  : - Vector of flags (true/false).  
    Output : - Two column matrix in which the eachj line indicates the  
    start and end index of a continuus region of true in the  
    input Flags vector.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Regions=flag2regions([0;1;1;1;0;1;1;1;0]);  
    Reliable: 2  
      
      
### tools.array.hist2d_fast

A fast version of histcounts2 (without all the overheads) This is faster than imUtil.patternMatch.hist2d BUT unlike, imUtil.patternMatch.hist2d, the returned matrix X/Y directions I like those of histcounts2.


    
    A fast version of histcounts2 (without all the overheads)  
    This is faster than imUtil.patternMatch.hist2d  
    BUT unlike, imUtil.patternMatch.hist2d, the returned matrix X/Y  
    directions I like those of histcounts2.  
    Input  : - A vector of X coordinates.  
    - A vector of Y coordinates.  
    - A vector of X edges.  
    - A vector of Y edges.  
    - Flag indicating if i/j or x/y output:  
    true (default) - i,j output (like histcounts2)  
    X axis is along i-dimension...  
    false - output like imUtil.patternMatch.hist2.  
    Output : - A matrix of 2-D histogram (X vs Y).  
    - Vector of X coordinate of center of X bins. (unlike  
    histcounts2).  
    - Vector of Y coordinate of center of Y bins.  
    Author : Eran Ofek (Sep 2021)  
    Example: Xv = rand(1e4, 1); Yv=rand(1e4, 1);  
    RangeX = [0 1]; RangeY = [0 1.1]; StepX = 0.001; StepY = 0.001;  
    Xed = (0:0.001:1); Yed = (0:0.001:1.1);  
    Mat = tools.array.hist2d_fast(Xv,Yv, Xed, Yed);  
    Mat0 = tools.array.hist2d_fast(Xv,Yv, RangeX, RangeY, StepX, StepY);  
    Mat1 = histcounts2(Xv,Yv, Xed, Yed);  like Mat  
    compare all methods  
    assert(all(MatMat0,'all'))  
    assert(all(MatMat1,'all'))  
    simple test  
    Xv = 0.7; Yv =0.4; Xed=(0:0.333:1); Yed=(0:0.333:1.333);  
    Mat = tools.array.hist2d_fast(Xv,Yv, Xed, Yed)  
    speed  
    tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = tools.array.hist2d_fast(Xv,Yv, RangeX, RangeY, StepX, StepY); end, toc  
    tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = tools.array.hist2d_fast(Xv,Yv, Xed, Yed); end, toc  
    tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = histcounts2(Xv,Yv, Xed, Yed); end, toc  
      
### tools.array.ind2sub_array

ind2sub_array Multiple subscripts from linear index. IND2SUB is used to determine the equivalent subscript values corresponding to a given single index into an array. ind2sub_array is identical to matlab function IND2SUB except for the


    
    ind2sub_array Multiple subscripts from linear index.  
    IND2SUB is used to determine the equivalent subscript values  
    corresponding to a given single index into an array.  
      
    ind2sub_array is identical to matlab function IND2SUB except for the  
    fact its output is an array contains the subindices rather than  
    independent variables as the original function does.  
      
    Class support for input IND:  
    float: double, single  
    integer: uint8, int8, uint16, int16, uint32, int32, uint64, int64  
      
    See also IND2SUB.  
      
      
### tools.array.index_outofbound

Remove from vector of indices valu8es which are out of bounds. Package: Util.array Description: Given a vector of indices and the allowed size of the array will remove from the vector of indices any indices which are out of bound.


    
    Remove from vector of indices valu8es which are out of bounds.  
    Package: Util.array  
    Description: Given a vector of indices and the allowed size of the array  
    will remove from the vector of indices any indices which  
    are out of bound.  
    Input  : * Arbitrary number of pairs of arguments in which the first  
    of each pair is the vector of indices, and the second is  
    the allowed length.  
    Output : * The number of output is equal to the number of pairs of  
    input arguments. Each output argument contains the new  
    vector of indices.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: I=index_outofbound((-1:1:7),6);  
    [I,J]=index_outofbound((-1:1:7),6,(-1:2:8),4);  
    Reliable: 2  
      
      
      
### tools.array.insert_ind

Insert a column/s or row/s to a specific position in a matrix. Package: Util.array Description: Insert a column/s or row/s to a specific position in a matrix.


    
    Insert a column/s or row/s to a specific position in a matrix.  
    Package: Util.array  
    Description: Insert a column/s or row/s to a specific position in  
    a matrix.  
    Input  : - Matrix.  
    - Index in which, afterwards, to insert the new column/s row/s.  
    - Column/s or row/s to insert.  
    - Dimension: 1 - Insert rows; 2 - Insert columns, default is 1.  
    Output : - A new matrix with new column/s or row/s inserted.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Dec 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: delete_ind.m  
    Example: NewMat=insert_ind(zeros(3,2),2,[1 1; 2 2],1)  
    Reliable: 1  
    -  
### tools.array.is_evenint

Check for each integer number in array if even. Package: Util.array Description: Check for each integer number in array if even.


    
    Check for each integer number in array if even.  
    Package: Util.array  
    Description: Check for each integer number in array if even.  
    Input  : - Array of integers.  
    Output : - Array of boolean flags indicating if each elemnt is even.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Apr 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Flag=is_evenint([-1 0 1 2 3]);  
    Reliable: 2  
      
      
### tools.array.list2vec

Concatenate all vectors to a single vector. Package: Util.array Description: Given an arbitrary number of arguments, each containing a vector, concatenate all vectors to a single vector.


    
    Concatenate all vectors to a single vector.  
    Package: Util.array  
    Description: Given an arbitrary number of arguments, each containing  
    a vector, concatenate all vectors to a single vector.  
    Input  : * Arbirtary number of arguments, each containing a vector.  
    The vectors can be either row or column vectors.  
    Output : - A column vector, of all the vectors.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Vec=list2vec(Out(I).SrcEnergyList);  
    Reliable: 2  
      
      
### tools.array.maskflag_check

maskflag_check function                                          General Description: Given a matrix or vector of bit masks and a list of bits to test, return true for indices in the matrix in which one of the bits specified in the list of bits is open.


    
      
    maskflag_check function                                          General  
    Description: Given a matrix or vector of bit masks and a list of bits  
    to test, return true for indices in the matrix in which  
    one of the bits specified in the list of bits is open.  
    Input  : - Matrix or vector of bit masks (in decimal representation).  
    - Either a string containing a binary number (e.g., '0101'),  
    or a decimal number representing the binary number  
    (e.g., 5 is equivalent to '0101'), or a vector of bits  
    (e.g., [0 1 0 1]).  
    - Operator {'or'|'and'} between the bits. Default is 'and'.  
    Output : - Matrix or vector of logicals of the same size as the first  
    input argument in which the value is true if there are common  
    open bits between the bit mask and the binary number.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Oct 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: FlagOn=maskflag_check([0 3 4],'100')  
    FlagOn=maskflag_check([0 3 4],[1 0 0])  
    FlagOn=maskflag_check([0 3 4],4)  
    Reliable: 2  
      
      
### tools.array.maskflag_set

maskflag_set function                                            General Description: Given a matrix or vector of bit masks, set specific bits of specific indices.


    
      
    maskflag_set function                                            General  
    Description: Given a matrix or vector of bit masks, set specific  
    bits of specific indices.  
    Input  : - Matrix of vector of bit masks. If empty will create a  
    new matrix which size is the size of the first Flag matrix.  
    - Matrix type to create (if not exist). Default is 'uint16'.  
    If empty use default.  
    * Arbitrary number of pairs of: ...,BitNumber,FlagBit,...  
    BitNumber is the index of the bit to set (e.g., 1),  
    while FlagBit is a matrix or vector of logicals (true|false)  
    of the indices in which to set the speciic bits.  
    Output : - Output mask matrix.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: maskflag_check.m  
    Example: Flag1=zeros(5,5,'uint16'); Flag2=ones(5,5,'uint16');  
    Flag3=zeros(5,5,'uint16'); Flag3(1,[2:3])=true;  
    Mask=maskflag_set([],[],1,Flag1,2,Flag2,4,Flag3);  
    Reliable: 2  
      
      
### tools.array.mat2vec

Convert matrix to vector. Use (:) instead. Package: Util.array Description: Convert matrix to vector.


    
    Convert matrix to vector. Use (:) instead.  
    Package: Util.array  
    Description: Convert matrix to vector.  
    Input  : - Matrix.  
    Output : - Column Vector.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                       May 2000  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 1  
    -  
### tools.array.nan2val

Replace NaNs in an array with a specific values Package: Util.array Description: Replace NaNs in an array with a specific values


    
    Replace NaNs in an array with a specific values  
    Package: Util.array  
    Description: Replace NaNs in an array with a specific values  
    Input  : - An array.  
    - A value which will replace all the NaN. Default is 0.  
    If empty then use default.  
    Output : - The array with NaNs replaced.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Mat=nan2va([1 NaN;NaN 2],0);  
    Reliable: 2  
      
      
### tools.array.nangetind

Replace value with NaNs when index is out of bound. Package: Util.array Description: Get elements from an array by its indices. However, unlike A(I,J) operation in matlab if I or J are out of bound then return NaN.


    
    Replace value with NaNs when index is out of bound.  
    Package: Util.array  
    Description: Get elements from an array by its indices.  
    However, unlike A(I,J) operation in matlab if I or J are  
    out of bound then return NaN.  
    Input  : - Column vector, matrix or higher dimension (non-singulation)  
    array.  
    * Arbitrary number of input argument equal to the number  
    of dimensions in the input array.  
    Each contains the indices to retrieve.  
    Output : - Retrived array.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Jan 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: A=[1 2 3;4 5 6;7 8 9];  
    OutMat=Util.array.nangetind(A,[-1 2;3 4],[1 2;3 5]);  
    -1,1 and 4,5 are oput of bounds therefore return: [NaN 5;9 NaN]  
    OutMat=Util.array.nangetind(A,-1,3)  
    Reliable: 2  
    -  
      
      
### tools.array.nearest_unflag

Nearest coordinate to an unflagged point. Package Util.array Description: Given a coordinate X0, a list of X-coordinates and a flag (0 | 1) for each coordinate, find the nearest X coordinate in the list to X0 that is Flag0.


    
    Nearest coordinate to an unflagged point.  
    Package Util.array  
    Description: Given a coordinate X0, a list of X-coordinates and  
    a flag (0 | 1) for each coordinate, find the nearest X  
    coordinate in the list to X0 that is Flag0.  
    Input  : - X0 coordinate for which to search the nearest coordinate.  
    - List of X coordinates.  
    - List of flags (0 | 1) which corresponds to the list of  
    coordinates, default is [0].  
    Output : - Nearest coordinate tabulated in the list X for which  
    Flag0 and its nearest to X0.  
    - Index of nearest coordinate.  
    - Absolute value difference between nearest coordinate and X0.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Mar 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [NearestX,Ind]=Util.array.nearest_unflag(0.74,rand(100,1),floor(rand(100,1).*2));  
    -  
### tools.array.or_nan

Logical function "or" for NaNs. Package: Util.array Description: Logical function "or" for NaNs. This function is similar to "or" logical function, but NaNs are regarded as no information using the following logical table:


    
    Logical function "or" for NaNs.  
    Package: Util.array  
    Description: Logical function "or" for NaNs. This function is similar  
    to "or" logical function, but NaNs are regarded as no  
    information using the following logical table:  
    M1   M2    Result  
    1    1     1  
    1    0     1  
    0    1     1  
    0    0     0  
    NaN  1     1  
    NaN  0     0  
    1    NaN   1  
    0    NaN   0  
    NaN  NaN   1  
    Input  : - Matrix 1  
    - Matrix 2  
    Output : - The result.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Dec 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### tools.array.replace

Replace values in specific range, or equal NaN, with another value. Package: Util.array Description: Given an array replace any value in the array within some specific range, or equal NaN, with another value.


    
    Replace values in specific range, or equal NaN, with another value.  
    Package: Util.array  
    Description: Given an array replace any value in the array within some  
    specific range, or equal NaN, with another value.  
    Input  : - An array.  
    - List of ranges. This is a two columns matrix. Each row is  
    for a specific range, while the first column is for the lower  
    bound and the second column for the upper bound. If the first  
    column is NaN, then will search for NaN.  
    - A scalar. or a vector of the same length as the range matrix,  
    which values to use for the replacement.  
    Output : - The array with the replaced elements.  
    See also: replace_with_noise.m  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Util.array.replace([1 2; NaN 2;17 19],[1 3],0);  
    Util.array.replace([1 2; NaN 2;17 19],[1 3;18 Inf],[0;NaN]);  
    Reliable: 2  
      
      
      
### tools.array.select_by_ind

Select lines by index, return NaN when index is NaN. Package: Util.array select_by_ind function                                               General Description: Select lines from a matrix, given the indices of the lines contains NaNs. Return lines with NaNs from NaNs indices.


    
    Select lines by index, return NaN when index is NaN.  
    Package: Util.array  
    select_by_ind function                                               General  
    Description: Select lines from a matrix, given the indices of the lines  
    contains NaNs. Return lines with NaNs from NaNs indices.  
    Input  : - Matrix  
    - Vector of indices.  
    Output : - Matrix of selected indices. If index is NaN, than the matrix  
    will contain line of NaNs.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                     March 2010  
    URL: http://wise-obs.tau.ac.il/~eran/matlab.html  
      
      
### tools.array.sub2ind_array

sub2ind_array Linear index from multiple subscripts. sub2ind_array is used to determine the equivalent single index corresponding to a given set of subscript values. sub2ind_array is identical to matlab function sub2ind except for the


    
    sub2ind_array Linear index from multiple subscripts.  
    sub2ind_array is used to determine the equivalent single index  
    corresponding to a given set of subscript values.  
      
    sub2ind_array is identical to matlab function sub2ind except for the  
    fact its input is an array contains the subindices rather than  
    independent variables as for the original function.  
      
      
    Class support for inputs I,J:  
    float: double, single  
    integer: uint8, int8, uint16, int16, uint32, int32, uint64, int64  
      
    See also IND2SUB, sub2ind_array.  
      
### tools.array.sub2ind_fast

sub2ind fast version for 2D matrices Description: A fast version of sub2ind for 2D arrays.


    
    sub2ind fast version for 2D matrices  
    Description: A fast version of sub2ind for 2D arrays.  
    Input  : - Array size [Y,X].  
    - Y positions (whole pixels)  
    - X positions (whole pixels)  
    Output : - Linear index of position in array.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ind=sub2ind_fast([3 3],2,2)  
    Reliable: 2  
      
      
### tools.array.sum_bitor

A bitor operation on all lines or rows and return a vector ofbit-wise or. Package: Util.array Description: Given a 2D array of integers, perform a bitor operation on all lines or rows and return a vector ofbit-wise or.


    
    A bitor operation on all lines or rows and return a vector ofbit-wise or.  
    Package: Util.array  
    Description: Given a 2D array of integers, perform a bitor operation  
    on all lines or rows and return a vector ofbit-wise or.  
    Input  : - A unsigned integer matrix.  
    - Dimension of bitor summation. Default is 1.  
    Output : - A vector of bit wise or between the lines or rows.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Bit=zeros(5,3,'uint16'); Bit(4,2)=1; Bit(4,3)=2; Bit(3,3)=12;  
    SumBit=Util.array.sum_bitor(Bit,2);  
    Reliable: 2  
    -  
      
      
### tools.array.unique_count

Unique values and count the number of apperances of each value. Package: Util.array Description: Select unique values in numeric vector and count the number of apperances of each value.


    
    Unique values and count the number of apperances of each value.  
    Package: Util.array  
    Description: Select unique values in numeric vector and count the  
    number of apperances of each value.  
    Input  : - Numeric vector.  
    Output : - Unique values  
    - Count of appearances per unique value.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [UnVal,Count]=Util.array.unique_count(Vec)  
    Reliable: 2  
      
      
