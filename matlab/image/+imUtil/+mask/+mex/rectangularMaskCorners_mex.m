% (MEX) Return the four corners of a single-filled rectangular region og true in a logical matrix.
%   Given a logical matrix of true/false, in which there is a
%   rectangular (maybe rotated) shape containing true. The function
%   return the four corners of the rectangular shape
% Input  : - A matrix of logicals.
% Output : - A two column matrix [Y, X] containing the four corners.
% Author : ChatGPT + Eran Ofek (2025 Oct) 
% Example: Corners=imUtil.mask.mex.rectangularMaskCorners_mex(Flag)