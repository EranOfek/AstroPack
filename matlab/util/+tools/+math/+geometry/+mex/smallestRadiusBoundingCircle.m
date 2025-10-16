% Fast MEX for calculating smallest radius bounding circle in 2D geometry
%     Using Welzl’s randomized incremental algorithm
%   See also: tools.math.geometry.boundingCircle
% Input  : - A vector of X coordinates.
%          - A vector of Y coordinates.
% Output : - Best X center.
%          - Best Y center.
%          - Best radius.
% Author : ChatGPT + Eran Ofek (2025 Oct) 
% Example: [Xc,Yc,Rc]=tools.math.geometry.mex.smallestRadiusBoundingCircle(X,Y)