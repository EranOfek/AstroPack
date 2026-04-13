% Fast mex for selecting rows from a 2D matrix, and insert NaN rows for NaN indices.
% Description: Given a 2D matrix and a vector/matrix of row indices, return
%              a new matrix containing the selected rows. If an element in
%              Ind is NaN, then the corresponding output row is filled with
%              NaNs.
%               This is analog to:
%               Ind0 = double(Ind(:));
%               Good = ~isnan(Ind0);
%               NewMatrix = NaN(numel(Ind0), size(Matrix,2));
%               NewMatrix(Good,:) = double(Matrix(Ind0(Good),:));
%
% Input  : - A 2D matrix of any numeric or logical type. Size: [Nrow, Ncol].
%          - Indices array. Can be single, double, or any integer/unsigned
%            integer type. Number of elements: M. Values must be between
%            1 and Nrow. NaN values are allowed only for floating-point Ind.
% Output : - Output matrix of size [M, Ncol], containing the selected rows.
%            If Matrix is double, the output is double.
%            If Matrix is single, the output is single.
%            If Matrix is integer or logical, the output is double, in order
%            to allow NaN rows.
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native' selectIndNaN.cpp
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example:
% Matrix = reshape(1:20,5,4);
% Ind    = [2; NaN; 5; 1];
% NewMatrix = selectIndNaN(Matrix, Ind);
%
% NewMatrix =
%      2     7    12    17
%    NaN   NaN   NaN   NaN
%      5    10    15    20
%      1     6    11    16