function [UpixLow, Raster0] = pixCoversPolygon(Polygon, Args)
    % Find the healpix pixels covering a sky polygon at search and DB resolutions.
    %     Rasterizes the polygon at the raster resolution and returns the central
    %     pixel together with its neighbors (as UNIQ ids) at the low (DB)
    %     resolution, used to query images that may overlap the polygon footprint.
    % Input  : - A polygon as an [N x 2] matrix of [RA, Dec] vertices [deg].
    %          * ...,key,val,...
    %            'RA0' - Central RA of the polygon [deg]. If empty (or Dec0 is
    %                   empty), it is calculated from the polygon vertices.
    %                   Default is [].
    %            'Dec0' - Central Dec of the polygon [deg]. If empty (or RA0 is
    %                   empty), it is calculated from the polygon vertices.
    %                   Default is [].
    %            'RasterResolution' - Polygon rasterization resolution [arcsec].
    %                   Default is 3.
    %            'NsideSearch' - Nside at which the central pixel and its
    %                   neighbors are found. Default is 2^7.
    %            'NsideLow' - Low (DB image table) Nside to which the search
    %                   pixels are translated. Default is 2^8.
    % Output : - Column vector of the central and neighbor pixels UNIQ ids at NsideLow.
    %          - Rasterized healpix coverage of the polygon (pixel indices).
    % Author : A.M. Krassilchtchikov (2026 Jun)
    % Example: [UpixLow, Raster0] = celestial.healpix.pixCoversPolygon(P0, 'RA0',RA, 'Dec0',Dec);
    %
    arguments
        Polygon
        Args.RA0              = [];
        Args.Dec0             = [];
        Args.RasterResolution = 3;     % arcsec
        Args.NsideSearch      = 2^7;
        Args.NsideLow         = 2^8;
    end

    RAD = 180/pi;

    % central coordinates: use the supplied values or the mean of the vertices
    if isempty(Args.RA0) || isempty(Args.Dec0)
        RA0  = mean(Polygon(:,1));
        Dec0 = mean(Polygon(:,2));
    else
        RA0  = Args.RA0;
        Dec0 = Args.Dec0;
    end

    % rasterize the polygon and find the healpix coverage
    [Raster0, ~] = celestial.healpix.mex.rasterize_polygon(Polygon, Args.RasterResolution,'arcsec');

    % find the center and neighbors at the search resolution
    UpixCenter = celestial.healpix.ang2pix(Args.NsideSearch, RA0/RAD, Dec0/RAD);
    UpixNeighb = celestial.healpix.mex.neighbors_nested(Args.NsideSearch, UpixCenter);

    % translate the center and the neighbors to NsideLow (as in the image table of the DB)
    UpixCenterLow = celestial.healpix.increasePixelResolution(UpixCenter, Args.NsideSearch, Args.NsideLow);
    UpixNeighbLow = celestial.healpix.increasePixelResolution(UpixNeighb, Args.NsideSearch, Args.NsideLow);

    % convert to UNIQ and merge into a single list
    UpixCenterLow = celestial.healpix.pix2uniqueId(Args.NsideLow, UpixCenterLow);
    UpixNeighbLow = celestial.healpix.pix2uniqueId(Args.NsideLow, UpixNeighbLow);
    UpixLow       = [UpixCenterLow(:); UpixNeighbLow(:)];
end
