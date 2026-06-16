function [Low, High] = convertHealPix2highNsideNested(NSide, PixID, NewNSide)
% Convert low-NSide nested HEALPix pixel to a range of high-NSide nested pixels.
%
% Description:
%   Given a HEALPix pixel ID in NESTED ordering at NSide, return the
%   inclusive range [Low, High] of pixel IDs at NewNSide contained inside
%   the original low-resolution pixel.
%
%   If NSide is empty, PixID is assumed to be a full HEALPix ID using:
%
%       FullID = 4.*NSide.^2 + PixID
%
%   In this case, the returned Low and High are also full HEALPix IDs
%   at NewNSide.
%
% Input  : - (NSide) Original lower NSide. If empty, PixID is assumed to be FullID.
%          - (PixID) Pixel ID at NSide, or FullID if NSide=[].
%          - (NewNSide) Requested higher NSide.
%
% Output : - Lowest high-resolution nested pixel ID inside PixID.
%          - Highest high-resolution nested pixel ID inside PixID.
%
% Author : ChatGPT + Eran Ofek (Jun 2026)
% Example:
%   [Low, High] = celestial.healpix.convertHealPix2highNsideNested(2.^8, 0, 2.^16)
%
%   Low =
%        0
%
%   High =
%        65535
%
% Example with full ID:
%   FullID = 4.*256.^2 + 0;
%   [LowFull, HighFull] = celestial.healpix.convertHealPix2highNsideNested([], FullID, 2.^16);

arguments
    NSide
    PixID
    NewNSide (1,1) {mustBePositive, mustBeInteger}
end

if ~isPowerOfTwo(NewNSide)
    error('NewNSide must be a positive power of 2.');
end

InputIsFullID = isempty(NSide);

if InputIsFullID
    FullID = PixID;

    % Decode NSide from full ID.
    % FullID = 4*NSide^2 + localPixID
    %
    % For NSide = 2^Order:
    %   first FullID of this order is 4*NSide^2 = 2^(2*Order + 2)
    %
    Order = floor(log2(double(FullID))./2) - 1;
    NSide = 2.^Order;

    LocalPixID = double(FullID) - 4.*NSide.^2;

else
    if ~all(isPowerOfTwo(NSide), 'all')
        error('NSide must be a positive power of 2.');
    end

    LocalPixID = PixID;
end

if any(NewNSide < NSide, 'all')
    error('NewNSide must be >= NSide. This function converts to higher NSide only.');
end

Factor = NewNSide ./ NSide;

if any(Factor ~= round(Factor), 'all') || ~all(isPowerOfTwo(Factor), 'all')
    error('NewNSide/NSide must be an integer power of 2.');
end

Nchild = double(Factor).^2;

LowLocal  = double(LocalPixID) .* Nchild;
HighLocal = LowLocal + Nchild - 1;

if InputIsFullID
    Low  = 4.*double(NewNSide).^2 + LowLocal;
    High = 4.*double(NewNSide).^2 + HighLocal;
else
    Low  = LowLocal;
    High = HighLocal;
end

end


function Flag = isPowerOfTwo(X)
% isPowerOfTwo  True for positive integer powers of two.
Flag = X > 0 & X == round(X) & 2.^round(log2(double(X))) == X;
end