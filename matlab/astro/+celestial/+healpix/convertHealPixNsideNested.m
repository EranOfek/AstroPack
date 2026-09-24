function [NewPixID, NewFullID, OldNSide] = convertHealPixNsideNested(NSide, PixID, NewNSide)
% Convert nested HEALPix pixel IDs to another NSide.
% Description:
%   Convert HEALPix NESTED pixel IDs from NSide to NewNSide.
%
%   If NSide is empty, then PixID is assumed to be a "full" HEALPix ID
%   incorporating NSide, using the convention:
%
%       FullID = 4.*NSide.^2 + PixID
%
%   In this case, the first output NewPixID is also returned as a full ID
%   at NewNSide.
%
% Input  : - (NSide) Original NSide. If empty, PixID is assumed to be FullID.
%          - (PixID) HEALPix pixel ID in NESTED ordering, or FullID if NSide=[].
%          - (NewNSide) Requested output NSide.
%
% Output : - (NewPixID) Converted pixel ID.
%            If NSide is empty, this is returned as FullID at NewNSide.
%            Otherwise, this is the ordinary pixel index at NewNSide.
%          - (NewFullID) FullID at NewNSide:
%            4.*NewNSide.^2 + local NewPixID
%          - (OldNSide) Decoded/input NSide for each input pixel.
%
% Authpr : ChatGPT + Eran Ofek (Jun 2026)
%
% Example:
%   % Ordinary nested pixel index:
%   NewPix = convertHealPixNsideNested(16, 1234, 4);
%
%   % Full ID input:
%   FullID = 4.*16.^2 + 1234;
%   NewFullID = convertHealPixNsideNested([], FullID, 4);

arguments
    NSide
    PixID
    NewNSide (1,1) {mustBePositive, mustBeInteger}
end

% Check NewNSide
if ~isPowerOfTwo(NewNSide)
    error('NewNSide must be a positive power of 2.');
end

InputIsFullID = isempty(NSide);

if InputIsFullID
    % PixID is actually FullID:
    FullID = PixID;

    % Decode order from FullID.
    % For the convention FullID = 4*NSide^2 + localPix,
    % order = log2(NSide), and:
    %   FullID range for order k is [2^(2k+2), 2^(2k+4)-1]
    Order = floor(log2(double(FullID))./2) - 1;

    OldNSide = 2.^Order;

    % Extract local pixel index
    LocalPixID = double(FullID) - 4.*OldNSide.^2;

else
    OldNSide = NSide;
    LocalPixID = PixID;

    if ~all(isPowerOfTwo(OldNSide), 'all')
        error('NSide must be a positive power of 2.');
    end
end

% NewNSide must not be larger than old NSide.
% Going to higher resolution is not unique: one parent has 4^k children.
if any(NewNSide > OldNSide, 'all')
    error('NewNSide must be <= NSide. Conversion to higher NSide is not unique.');
end

Ratio = OldNSide ./ NewNSide;

if any(abs(Ratio - round(Ratio)) > 0, 'all') || ~all(isPowerOfTwo(Ratio), 'all')
    error('NSide/NewNSide must be an integer power of 2.');
end

% NESTED hierarchy:
% each reduction by factor Ratio in NSide combines Ratio^2 pixels.
NewLocalPixID = floor(double(LocalPixID) ./ double(Ratio).^2);

% Full ID at the new NSide
NewFullID = 4.*double(NewNSide).^2 + NewLocalPixID;

% Preserve the user-facing convention:
% - If input was full ID, return full ID as first output.
% - Otherwise return ordinary pixel index as first output.
if InputIsFullID
    NewPixID = NewFullID;
else
    NewPixID = NewLocalPixID;
end

end


function Flag = isPowerOfTwo(X)
% isPowerOfTwo  True for positive integer powers of two.
Flag = X > 0 & X == round(X) & 2.^round(log2(double(X))) == X;
end