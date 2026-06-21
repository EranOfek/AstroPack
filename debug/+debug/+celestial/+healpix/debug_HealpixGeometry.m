function debug_HealpixGeometry()
    % debug_HealpixGeometry  Debug geometry, rasterize, and plot functions.
    % Package: celestial.healpix.debug
    % Description: Smoke-test healpixVertices, isInside, pixBoundries,
    %              rasterize_polygon, mex.rasterize_polygon, plot.
    % Author : Chen Tishler (Jun 2026)
    % Run by: debug.celestial.healpix.debug_HealpixGeometry
    fprintf('\n========== DEBUG HEALPIX GEOMETRY ==========\n');

    NSide = 16;
    Pix = [197; 31];
    Lon = 1;
    Lat = 0.5;
    Polygon = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];

    debug_healpixVertices(NSide, Pix);
    debug_isInside(NSide, Pix(1), Lon, Lat);
    debug_pixBoundries(NSide, Pix);
    debug_rasterizePolygon(Polygon);
    debug_mexRasterizePolygon(Polygon);
    debug_plot();

    closeFiguresSafely();

    fprintf('========== DEBUG HEALPIX GEOMETRY DONE ==========\n');
end


function debug_healpixVertices(NSide, Pix)
    fprintf('\n--- healpixVertices ---\n');
    try
        [CornerLon, CornerLat] = celestial.healpix.healpixVertices(NSide, Pix, 'nested');
        fprintf('ok, size(CornerLon)=[%d %d]\n', size(CornerLon, 1), size(CornerLon, 2));
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_isInside(NSide, Pix, Lon, Lat)
    fprintf('\n--- isInside ---\n');
    try
        IsInside = celestial.healpix.isInside(NSide, Pix, Lon, Lat, 'Type', 'nested');
        fprintf('ok, IsInside=%d\n', IsInside);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_pixBoundries(NSide, Pix)
    fprintf('\n--- pixBoundries (expected failure) ---\n');
    try
        [CornerLons, CornerLats] = celestial.healpix.pixBoundries(NSide, Pix, 'nested');
        fprintf('ok (unexpected), size(CornerLons)=[%d %d]\n', size(CornerLons, 1), size(CornerLons, 2));
    catch ME
        fprintf('ok (expected): %s\n', ME.message);
    end
end


function debug_rasterizePolygon(Polygon)
    fprintf('\n--- rasterize_polygon ---\n');

    debug_rasterizePolygonCase('UseMex=true', Polygon, 'Nside', 2^12, 'UseMex', true);
    debug_rasterizePolygonCase('UseMex=false', Polygon, 'Nside', 2^12, 'UseMex', false);
end


function debug_rasterizePolygonCase(Label, Polygon, varargin)
    fprintf('  %s: ', Label);
    try
        [Result, NsideOut] = celestial.healpix.rasterize_polygon(Polygon, varargin{:});
        fprintf('ok, numel(Result)=%d, Nside=%d\n', numel(Result), NsideOut);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_mexRasterizePolygon(Polygon)
    fprintf('\n--- mex.rasterize_polygon ---\n');
    try
        [Ind, NsideOut] = celestial.healpix.mex.rasterize_polygon(Polygon.', 3);
        fprintf('ok, numel(Ind)=%d, Nside=%d\n', numel(Ind), NsideOut);
    catch ME
        fprintf('failed (mex may be uncompiled): %s\n', ME.message);
    end
end


function debug_plot()
    fprintf('\n--- plot ---\n');
    try
        Pix = [181313; 181316; 133256];
        celestial.healpix.plot(Pix, 'Nside', 128, 'PlotOnMap', false);
        fprintf('ok\n');
    catch ME
        fprintf('failed (plot/Mapping Toolbox may be unavailable): %s\n', ME.message);
    end
end


function closeFiguresSafely()
    try
        close all;
    catch
    end
end
