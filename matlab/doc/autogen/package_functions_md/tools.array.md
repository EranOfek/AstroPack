# Package: tools.array


### tools.array.and_mat

Perform logical and operation between all the columns or rows of a matrix Package: Util.array Description: Perform logical and operation between all the columns or rows of a matrix.


### tools.array.and_nan

Logical function "and" for NaNs. Package: Util.array Description: Logical function "and" for NaNs. This function is similar to "and" logical function, but NaNs are regarded as no


### tools.array.array_select

Select lines in an array which columns satisfay some criteria. Package: Util.array Description: Given a matrix, select lines which their column fullfill a specific criteria. For example, the values in the second


### tools.array.assoc_range

Index of points in each bin. Package: Util.array Description: Given a vector of data points and a vector of edges, for each value in the vector, return the index of the


### tools.array.bitand_array

Perform a bitand operation along all elements in an array. Package: Util.array Description: Perform a bitand operation along all elements in an array along a specific dimension.


### tools.array.bitor_array

Perform a bitor operation along all elements in an array. Package: Util.array Description: Perform a bitor operation along all elements in an array along a specific dimension.


### tools.array.bsx_nsize

bsx_nsize gets multiple of array sizes (i.e. result of the size function), verifies they are candidates for bsxfun operation, and the size of the result array of bsxfun operation (S). The function also padding the sizes of the input arrays with ones, in


### tools.array.bsx_size

bsx_size gets two arrays S1,S2 and calculates the size of the result array after a bsxfun operation is performed. The function verifies that the sizes are either identical, or for dimensions where thtey are differs, the dimension of one of the array


### tools.array.check_range

Replace out of bound indices with bound indices. Package: Util.array Description: Given the size of an N-dimensional array, and minimum and maximum indices per each dimension, check if the indices


### tools.array.delete_ind

Delete a column/s or row/s from a matrix. Package: Util.array Description: Delete a column/s or row/s from a matrix.


### tools.array.find_ranges

Indices of vector values found within one of several ranges. Package: Util.array Description: Given a vector and several ranges, return the indices of values in the vector which are found within one of the


### tools.array.find_ranges_flag

Check if values are within some ranges. Package: Util.array Description: Given a vector and several ranges, return the a vector indicating if a given position in the input vector


### tools.array.findmany

Find all values in a vector in another vector or matrix. Package: Util.array Description: Find all values in a vector in another vector or matrix.


### tools.array.flag2regions

Identify continous ranges of true values. Package: Util.array Description: Given a column vector flags (true/false) returns pairs of indices of the positions of continuus regions in which the


### tools.array.ind2sub_array

ind2sub_array Multiple subscripts from linear index. IND2SUB is used to determine the equivalent subscript values corresponding to a given single index into an array.


### tools.array.index_outofbound

Remove from vector of indices valu8es which are out of bounds. Package: Util.array Description: Given a vector of indices and the allowed size of the array will remove from the vector of indices any indices which


### tools.array.insert_ind

Insert a column/s or row/s to a specific position in a matrix. Package: Util.array Description: Insert a column/s or row/s to a specific position in a matrix.


### tools.array.is_evenint

Check for each integer number in array if even. Package: Util.array Description: Check for each integer number in array if even.


### tools.array.list2vec

Concatenate all vectors to a single vector. Package: Util.array Description: Given an arbitrary number of arguments, each containing a vector, concatenate all vectors to a single vector.


### tools.array.maskflag_check

maskflag_check function                                          General Description: Given a matrix or vector of bit masks and a list of bits to test, return true for indices in the matrix in which


### tools.array.maskflag_set

maskflag_set function                                            General Description: Given a matrix or vector of bit masks, set specific bits of specific indices.


### tools.array.mat2vec

Convert matrix to vector. Use (:) instead. Package: Util.array Description: Convert matrix to vector.


### tools.array.nan2val

Replace NaNs in an array with a specific values Package: Util.array Description: Replace NaNs in an array with a specific values


### tools.array.nangetind

Replace value with NaNs when index is out of bound. Package: Util.array Description: Get elements from an array by its indices. However, unlike A(I,J) operation in matlab if I or J are


### tools.array.nearest_unflag

Nearest coordinate to an unflagged point. Package Util.array Description: Given a coordinate X0, a list of X-coordinates and a flag (0 | 1) for each coordinate, find the nearest X


### tools.array.or_nan

Logical function "or" for NaNs. Package: Util.array Description: Logical function "or" for NaNs. This function is similar to "or" logical function, but NaNs are regarded as no


### tools.array.replace

Replace values in specific range, or equal NaN, with another value. Package: Util.array Description: Given an array replace any value in the array within some specific range, or equal NaN, with another value.


### tools.array.select_by_ind

Select lines by index, return NaN when index is NaN. Package: Util.array select_by_ind function                                               General Description: Select lines from a matrix, given the indices of the lines


### tools.array.sub2ind_array

sub2ind_array Linear index from multiple subscripts. sub2ind_array is used to determine the equivalent single index corresponding to a given set of subscript values.


### tools.array.sub2ind_fast

sub2ind fast version for 2D matrices Description: A fast version of sub2ind for 2D arrays.


### tools.array.sum_bitor

A bitor operation on all lines or rows and return a vector ofbit-wise or. Package: Util.array Description: Given a 2D array of integers, perform a bitor operation on all lines or rows and return a vector ofbit-wise or.


### tools.array.unique_count

Unique values and count the number of apperances of each value. Package: Util.array Description: Select unique values in numeric vector and count the number of apperances of each value.


