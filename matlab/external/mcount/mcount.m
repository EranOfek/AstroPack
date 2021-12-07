% RES = mcount(MATRIX,X,COND)
%   Given a real-valued vector or matix MATRIX, counts the number of times
%   that the scalar X satisfy the (string) condition COND. The result is
%   returned in the variable RES. the COND operators allowed are:
%   '==', '>=', '<=', '<', '>', '!='.
%   The function is faster than sum(MATRIX'COND'X) or
%   length(FIND(MATRIX'COND'X)).
%   
%
% EXAMPLE:
% 
% >> mcount(1:10,2,'<=')
% 
% ans =
% 
%      2
% See also SUM, LENGTH.