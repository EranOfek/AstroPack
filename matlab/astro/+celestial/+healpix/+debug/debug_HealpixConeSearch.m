%==========================================================================
% Project     : AstroPack
% File        : +celestial/+healpix/+debug/debug_HealpixConeSearch.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Debug cone search functions in celestial.healpix:
%               coneSearch, coneSearchRecur, coneSearch2PixRanges,
%               mex.coneSearch.
%==========================================================================

function debug_HealpixConeSearch()
    % Smoke-test healpix cone search variants.
    fprintf('\n========== DEBUG HEALPIX CONE SEARCH ==========\n');

    NSide = 2^8;
    RA = 200.67;
    Dec = 50.4;
    Rad = 10;

    debug_coneSearch(NSide, RA, Dec, Rad);
    debug_coneSearchRecur(NSide, RA, Dec, Rad);
    debug_coneSearch2PixRanges();
    debug_mexConeSearch();

    fprintf('========== DEBUG HEALPIX CONE SEARCH DONE ==========\n');
end


function debug_coneSearch(NSide, RA, Dec, Rad)
    fprintf('\n--- coneSearch ---\n');
    try
        Result = celestial.healpix.coneSearch(NSide, RA, Dec, Rad, ...
            'RadiusUnits', 'deg', 'CooUnits', 'deg');
        fprintf('ok, numel(Result)=%d\n', numel(Result));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_coneSearchRecur(NSide, RA, Dec, Rad)
    fprintf('\n--- coneSearchRecur ---\n');
    try
        Result = celestial.healpix.coneSearchRecur(NSide, RA, Dec, Rad, ...
            'RadiusUnits', 'deg', 'CooUnits', 'deg');
        fprintf('ok, numel(Result)=%d\n', numel(Result));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_coneSearch2PixRanges()
    fprintf('\n--- coneSearch2PixRanges ---\n');

    RA = 1;
    Dec = 0.5;
    SearchRadius = 1./1024;
    NSideCat = 2^16;

    debug_coneSearch2PixRangesCase('neighb', RA, Dec, SearchRadius, NSideCat, 'Algo', 'neighb');
    debug_coneSearch2PixRangesCase('cone', RA, Dec, SearchRadius, NSideCat, 'Algo', 'cone');
end


function debug_coneSearch2PixRangesCase(Label, RA, Dec, SearchRadius, NSideCat, varargin)
    fprintf('  %s: ', Label);
    try
        PixRanges = celestial.healpix.coneSearch2PixRanges(RA, Dec, SearchRadius, NSideCat, varargin{:});
        fprintf('ok, size(PixRanges)=[%d %d]\n', size(PixRanges, 1), size(PixRanges, 2));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_mexConeSearch()
    fprintf('\n--- mex.coneSearch ---\n');

    NSide = 1024;
    RA = 1.;
    Dec = 1.;
    Rad = 0.1;

    debug_mexConeSearchCase('inclusive/NEST (default)', NSide, RA, Dec, Rad);
    debug_mexConeSearchCase('exclusive', NSide, RA, Dec, Rad, 'exclusive');
    debug_mexConeSearchCase('inclusive/RING', NSide, RA, Dec, Rad, 'inclusive', 'RING');
end


function debug_mexConeSearchCase(Label, NSide, RA, Dec, Rad, varargin)
    fprintf('  %s: ', Label);
    try
        if nargin > 5
            Ind = celestial.healpix.mex.coneSearch(NSide, RA, Dec, Rad, varargin{:});
        else
            Ind = celestial.healpix.mex.coneSearch(NSide, RA, Dec, Rad);
        end
        fprintf('ok, numel(Ind)=%d\n', numel(Ind));
    catch ME
        fprintf('failed (mex may be uncompiled): %s\n', ME.message);
    end
end
