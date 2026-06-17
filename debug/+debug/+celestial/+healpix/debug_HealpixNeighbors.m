function debug_HealpixNeighbors()
    % debug_HealpixNeighbors  Debug neighbor lookup in celestial.healpix.
    % Package: celestial.healpix.debug
    % Description: Smoke-test findNeighbors and mex.neighbors_nested.
    % Author : Chen Tishler (Jun 2026)
    % Run by: debug.celestial.healpix.debug_HealpixNeighbors
    fprintf('\n========== DEBUG HEALPIX NEIGHBORS ==========\n');

    NSide = 256;
    Pix = int64([20567; 100; 50000]);

    debug_findNeighbors(NSide, Pix, false);
    debug_findNeighbors(NSide, Pix, true);
    debug_mexNeighborsNested(NSide, Pix);

    fprintf('========== DEBUG HEALPIX NEIGHBORS DONE ==========\n');
end


function debug_findNeighbors(NSide, Pix, IncludeSelf)
    if IncludeSelf
        Label = 'findNeighbors (IncludeSelf=true)';
    else
        Label = 'findNeighbors (IncludeSelf=false)';
    end
    fprintf('\n--- %s ---\n', Label);
    try
        NeighPix = celestial.healpix.findNeighbors(NSide, Pix, 'IncludeSelf', IncludeSelf);
        fprintf('ok, size(NeighPix)=[%d %d]\n', size(NeighPix, 1), size(NeighPix, 2));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_mexNeighborsNested(NSide, Pix)
    fprintf('\n--- mex.neighbors_nested ---\n');
    try
        NeighPix = celestial.healpix.mex.neighbors_nested(NSide, Pix(1));
        fprintf('ok, numel(NeighPix)=%d\n', numel(NeighPix));
    catch ME
        fprintf('failed (mex may be uncompiled): %s\n', ME.message);
    end
end
