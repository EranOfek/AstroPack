
function V = vignettingRectFilter(X, Y, D, Args)
% Calculate geometrical vignetting due to a rectangular filter.
% Description:
%   Calculate the fractional vignetting of an f/N converging beam by a
%   rectangular filter located a distance D from the focal plane.
%
%   The beam footprint at the filter plane is assumed to be circular,
%   with radius:
%       A = D./(2.*FNumber)
%
%   Unlike a circular filter, the vignetting by a rectangular filter
%   depends separately on X and Y and not only on radial distance.
%
% Input  : - X - X coordinate on detector relative to optical axis [mm].
%                  Scalar, vector, or matrix.
%          - Y - Y coordinate on detector relative to optical axis [mm].
%                  Scalar, vector, or matrix.
%          - D - Distance of filter from focal plane [mm].
%                  Scalar, vector, or matrix.
%                  X, Y, and D must have compatible sizes for implicit
%                  expansion.
%          * ...,key,val,...
%            'FNumber'   - Telescope focal ratio.
%                          Default is 2.2.
%            'FilterSize'- [Width Height] of rectangular filter [mm].
%                          Default is [40 40].
%            'Nquad'     - Number of Gauss-Legendre integration points.
%                          Default is 64.
%
% Output : - V - Fractional vignetting in the range [0,1].
%                V=0 means no vignetting.
%                V=1 means complete vignetting.
%
% Author : Eran Ofek (2026 Aug)
%
% Example:
%   [X,Y] = meshgrid((1:36)-18.5,(1:24)-12.5);
%   V = telescope.geometry.vignettingFilterRect(X,Y,34,...
%                                               'FilterSize',[40 30]);
%   imagesc(V);
%   axis image;
%   colorbar;
%
% See also: telescope.geometry.vignettingFilter

arguments
    X
    Y
    D
    Args.FNumber   = 2.2
    Args.FilterSize = [40 40]
    Args.Nquad     = 64
end

N  = Args.FNumber;
FS = Args.FilterSize;

Hx = FS(1).*0.5;
Hy = FS(2).*0.5;

% Radius of beam footprint at filter plane:
A = D./(2.*N);

% Find common output size using implicit expansion:
Tmp = X + Y + A;

% Explicit expansion is required before logical indexing:
X = X + zeros(size(Tmp),'like',Tmp);
Y = Y + zeros(size(Tmp),'like',Tmp);
A = A + zeros(size(Tmp),'like',Tmp);

V = zeros(size(Tmp),'like',Tmp);

%----------------------------------------------------------
% Identify trivially unvignetted positions.
% The entire circular footprint is inside the rectangle.
%----------------------------------------------------------
FlagFullTransmission = ...
    abs(X) + A <= Hx & ...
    abs(Y) + A <= Hy;

%----------------------------------------------------------
% Identify positions with no intersection between beam
% footprint and rectangular filter.
%----------------------------------------------------------
Dx = max(abs(X) - Hx,0);
Dy = max(abs(Y) - Hy,0);

FlagNoTransmission = hypot(Dx,Dy) >= A;

V(FlagNoTransmission) = 1;

%----------------------------------------------------------
% Partial-overlap cases
%----------------------------------------------------------
FlagPart = ~FlagFullTransmission & ~FlagNoTransmission;

if any(FlagPart(:))

    Xc = X(FlagPart);
    Yc = Y(FlagPart);
    Ac = A(FlagPart);

    Np = numel(Xc);

    % Gauss-Legendre nodes and weights on [-1,1]:
    [Gn,Gw] = localGaussLegendre(Args.Nquad);

    % X limits of circle-rectangle intersection:
    X1 = max(-Hx, Xc - Ac);
    X2 = min( Hx, Xc + Ac);

    % Transform quadrature nodes to each integration interval:
    Xm = 0.5.*(X1 + X2);
    Xh = 0.5.*(X2 - X1);

    XX = Xm + Xh.*Gn.';

    % Circle half-height at each X:
    RR = Ac.^2 - (XX - Xc).^2;
    RR = max(RR,0);
    YY = sqrt(RR);

    % Vertical extent of the circular beam:
    YlowCircle  = Yc - YY;
    YhighCircle = Yc + YY;

    % Intersection with rectangular filter:
    Ylow  = max(YlowCircle, -Hy);
    Yhigh = min(YhighCircle, Hy);

    DY = max(Yhigh - Ylow,0);

    % Integrate intersection area:
    Aoverlap = Xh .* sum(DY .* Gw.',2);

    % Fractional vignetting:
    Vp = 1 - Aoverlap./(pi.*Ac.^2);

    V(FlagPart) = Vp;

end

% Protect against numerical round-off:
V = max(0,min(1,V));

end


%==========================================================================
function [X,W] = localGaussLegendre(N)
% Return Gauss-Legendre nodes and weights on [-1,1].

Beta = 0.5 ./ sqrt(1 - (2.*(1:N-1)).^(-2));

T = diag(Beta,1) + diag(Beta,-1);

[Vec,Val] = eig(T);

X = diag(Val);
[X,Ind] = sort(X);

Vec = Vec(:,Ind);
W = 2.*Vec(1,:).^2;

W = W.';

end