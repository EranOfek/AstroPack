%==========================================================================
% Project     : AstroPack
% File        : +celestial/+healpix/+debug/debug_Healpix.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Master debug entry point for celestial.healpix package.
%               Calls grouped debug files for basics, indexing, ang
%               conversion, neighbors, cone search, and geometry.
%==========================================================================

function debug_Healpix()
    % Smoke-test all celestial.healpix debug groups.
    fprintf('========== DEBUG HEALPIX ==========\n');

    debug_HealpixBasics();
    debug_HealpixIndexing();
    debug_HealpixAngConversion();
    debug_HealpixNeighbors();
    debug_HealpixConeSearch();
    debug_HealpixGeometry();

    fprintf('========== DEBUG HEALPIX DONE ==========\n');
end
