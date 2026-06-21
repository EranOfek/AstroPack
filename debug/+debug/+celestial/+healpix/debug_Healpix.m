function debug_Healpix()
    % debug_Healpix  Master debug entry point for celestial.healpix package.
    % Package: celestial.healpix.debug
    % Description: Calls grouped debug files for basics, indexing, ang
    %              conversion, neighbors, cone search, and geometry.
    % Author : Chen Tishler (Jun 2026)
    % Run by: debug.celestial.healpix.debug_Healpix
    fprintf('========== DEBUG HEALPIX ==========\n');

    debug_HealpixBasics();
    debug_HealpixIndexing();
    debug_HealpixAngConversion();
    debug_HealpixNeighbors();
    debug_HealpixConeSearch();
    debug_HealpixGeometry();

    fprintf('========== DEBUG HEALPIX DONE ==========\n');
end
