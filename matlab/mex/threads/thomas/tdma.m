% y = TDMA(main, lower, upper, f, direction);
%  TDMA  MEX implementation of Thomas' Algorithm for REAL and 
%  COMPLEX data in memory for single- or multi-dimensional problems.
%  
%    Use the following command to build this MEX file:
%        mex -R2018a 'CFLAGS=-mavx' tdma.c
% 
%    Used to solve tridiagonal matrix problems of the form
%  
%    |main   upper    0     ...    ...     0  |  
%    |lower  main   upper    0     ...    ... |
%    |  0    lower  main   upper   ...    ... | * y = f 
%    | ...     0    lower   ...    ...     0  |
%    | ...    ...    ...    ...    ...   upper|
%    |  0     ...    ...     0    lower  main |
%    
%    Where main, upper, lower, f and y are all the same size (either one-
%    two-, or three-dimensional). However, the first element of lower and 
%    the last element of upper are not used--in each row, column or shaft.
% 
%    y = TDMA(main, lower, upper, f) solves the one-dimensional
%    problem, with main, lower and upper as vectors representing the
%    appropriate matrix diagonal.
%
%    y = TDMA(main, lower, upper, f, direction) solves the multi- 
%    dimensional problem, but with main, lower, and upper as a matrix
%    or 3D array.  For example, each column in main, lower, upper, f, and 
%    y can represent a single tridiagonal matrix problem to solve.  
%    Similarly for each row or shaft.
%    
%    Direction is the direction you want to solve along.  A direction of 1
%    will solve each column (going down each row); a direction of 2 will
%    solve each row (going across each column); and, a direction of 3 will
%    solve each shaft (going through each page / layer).
%    
%    See also TDMA_TEST.
%    
%      The Reference Cube:
%    
%             shaft
%               x----x
%              /    /|
%         row x----x x
%             |    |/
%             x----x
%            col
%     
%     Performance Notes (Complex Data, > 10e6 Elements):
%         For 1D problems, this performs just as well as other MEX
%         implementations, and 3 - 4 times faster than a straight up
%         implementation in MATLAB.
%         
%         For 2D problems, this is actually slower than than reshaping the
%         problem into a 1D problem and then solving via a standard MEX
%         implementation, but only by a little bit, even though it  
%         requires some additional massaging of the data before solving.
%         
%         For 3D problems, this is can be > 33% faster than permuting and
%         reshaping the data as needed to get it into an appropriate 1D
%         state before solving.
%     
%     For REAL 1D problems, just use a straight up MATLAB implementation.  
%     Or if you're working through simple problems, sparse matrices work 
%     great!
%
%     Multi-Threading:
%         This supports multi-threading for multi-dimensional inputs, i.e.
%         2D or greater.  However, multiple threads will only be used if
%         the inputs are set up to solve when direction == 1; solving the
%         problem down each column.  As such, when solving multi-
%         dimensional problems in MATLAB, inputs need to be either reshaped
%         (for 2D problems) or permuted (for 3D and above) before this
%         function is called, otherwise it will just run in one thread.  
%         For the most part, 2D problems just need to be transposed 
%         properly.
%
%     Performance Notes (Multi-Threading):
%         This has only really been tested on a four core machine that's
%         almost ten years old, but for real problems that require multiple
%         steps, the need to transpose data (a 2D problem) eats up a big
%         chunk of the time saved by multi-threading.  Total speedup can be
%         > 40% compared to the single threaded case though.
%
%         For simpler 2D problems that don't require permutation or
%         reshaping of data, using four threads I've seen >3x speedup!
