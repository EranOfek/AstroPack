function [Mean,Var]=filter2_var(Mat,Radius)
% Calculate the variance and mean filter of an image 
% Package: imUtil.filter
% Description: Calculate the variance and mean filter of an image in
%              circular apertures.
% Input  : - A matrix.
%          - Filter radius,
%            or [inner, outer] filter radii of annulus filter,
%            or [A, B, cov] for Gaussian filter.
% Output : - The variance filter.
%          - The mean filter.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [Mean,Var]=imUtil.filter.filter2_var(rand(100,100),3);
% Reliable: 2
%--------------------------------------------------------------------------


switch numel(Radius)
    case 1
        % circular filter
        CircFilter = imUtil.kernel2.circ(Radius,ceil([Radius.*2+1 Radius.*2+1]));
    case 2 
        % annulus filter
        CircFilter = imUtil.kernel2.annulus(Radius);
    case 3
        CircFilter = imUtil.kernel2.gauss(Radius);
    otherwise
        error('Unknown number of elements in the 2nd input argument');
end

Mean = imUtil.filter.filter2_fast(Mat,CircFilter);

if nargout>1
    Var = imUtil.filter.filter2_fast((Mat - Mean).^2 ,CircFilter);
end

