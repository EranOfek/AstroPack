function [C,goodindices,tm] = sliceGaussianProfile(X1,X2,x,y,W,Args)
% divide the rasterized strip in slices, and fit gaussians to the intensity
%  values W in each slice
% Input:
%  X1: [x1,y1]; X2: [x2,y2] of the base segment
%  x,y,W: Nx1 vectors
%  x,y: coordinates in pixels of the pixels belonging to the streak strip
%  W:   intensity of the pixels
%
% Name-value pairs::
%    slice_width: length in pixel of each section of the strip to be analysed
%                 separately (default 10px)
%    rthreshold: minimal value of R^2 for accepting a fit. Usually sections
%                of streaks contaminated by a neighboring source lead to
%                poorer transverse fits than the rest. Default 0.7
%    medianclip: clipping factor for outlier removal (default 2). 
%                Values > 1 enable median-based clipping
%    testplot: show diagnostic plot (default false)
%
% Output:
%  C:           4xM for each slice, (A,sigma,mu_h,r). M is L/slice_width.
%  goodindices: logical vector 1xN, true for indices of elements of W which
%               lead to an acceptable fit (R-square>rthreshold)
%  tm:          vector of values of the intrinsic segment coordinate, at the
%               mid of each slice. To associate the photometry of each slice with
%               pixel coordinates, via 
%               [X,Y]=segmentParabolicOffset([x1,y1],[x2,y2],curve(i).parfit,tm)
%
%  To compile the mex core:
%     mex('OPTIMFLAGS=-O3','sliceGaussianProfile_mex.cpp')

arguments
    X1 (1,2) double
    X2 (1,2) double
    x (:,1) double
    y (:,1) double
    W (:,1) double
    Args.slice_width (1,1) double = 10  % pixel units
    Args.rthreshold (1,1) double = 0.7
    Args.medianclip (1,1) double = 2
    Args.testplot (1,1) logical = false
end

% Call MEX function for fitting (returns C and goodindices)
try
    [C, goodindices] = sliceGaussianProfile_mex(X1, X2, x, y, W, ...
        Args.slice_width, Args.rthreshold,Args.medianclip);
catch ME
    error('MEX function failed. Make sure sliceGaussianProfile_mex is compiled. Error: %s', ME.message);
end

% Compute slice center positions (light computation, kept in MATLAB for clarity)
M = size(C, 2);
tm = ((0.5:M)/M);  % 1xM vector of slice centers in intrinsic coordinate

% Only compute visualization if needed
if Args.testplot
    % transform to intrinsic coordinates for plotting
    L = sqrt((X2(1)-X1(1))^2 + (X2(2)-X1(2))^2);
    D = ((X2(1)-X1(1))*(y-X1(2)) - (X2(2)-X1(2))*(x-X1(1)))/L;
    T = ((X2(1)-X1(1))*(x-X1(1)) + (X2(2)-X1(2))*(y-X1(2)))/L^2;
    
    clf
    scatter(T,D,[],W,'filled')
    hold on
    H = C(2,:)';  % sigma values (width)
    H(C(4,:) < Args.rthreshold) = NaN;
    plot(tm, H, '-k','LineWidth',2)
    hold off
end

end