function [WhereClause] = queryConeSearch_Healpix(RA, Dec, SearchRadius, Args)
    % Given RA and Dec, construct a query to search a DB table by Healpix pixel indices.
    %     Given Long/Lat and search radius, find all the healpix indices
    %     that are in the cone. Construct an SQL query WHERE clause that
    %     searches for these healpix indices.
    %     The function uses celestial.convert.cooResolve to resolve the
    %     coordinates.
    % Input  : - R.A., [deg|rad|sex] or object name.
    %            If second input is provided and RA is not numeric, then
    %            will assume input is in sexagesinal coordinates.
    %          - Dec. [deg|rad|sex]. If empty, then will interpret the
    %            first input argument as an object name.
    %            Default is [].
    %          - searchRadius. Default is 3 [arcsec is default units].
    %          * ...,key,val,... 
    %            'NSide' - Healpix NSide to use. Default is 2.^16.
    %            'Type' - Healpix type: 'ring'|'nested'.
    %                   Default is 'nested'.
    %            'UniquePixID' - Logical indicating if to use unique pix
    %                   ID (or simple pix ID). Default is true.
    %
    %            'SearchRadiusUnits' - Default is 'arcsec'
    %            'InUnits' - Default is 'deg'.
    %            'OutUnits' - Coo units for the DB search. Default is 'deg'.
    %            'Server' - If input is object name, then this is the name
    %                   server that will be used: @VO.name.server_simbad|
    %                   @VO.name.server_ned.
    %                   Default is @VO.name.server_simbad
    %
    % Output : - A char array with healpix cone search SQL Where clause.
    % Author : Eran Ofek (2025 Jan) 
    % Example: WhereClause=db.search.queryConeSearch_Healpix(100,10)  % deg
    %          WhereClause=db.search.queryConeSearch_Healpix('10:10:10','-20:10:10',5,'NSide',2.^13, 'HP_ColName','upix_low')
    %          

    arguments
        RA
        Dec                    = [];
        SearchRadius           = 3;
        Args.NSide             = 2.^16;
        Args.Type              = 'nested';
        Args.UniquePixID       = true;
        Args.HP_ColName        = 'upix_high';
        
        Args.SearchRadiusUnits = 'arcsec';
        Args.InUnits           = 'deg';  % 'deg'|'rad'|'sex'|'ned'|'simbad'|
        Args.OutUnits          = 'deg';  % 'deg'|'rad'
        Args.Server            = @VO.name.server_simbad;
    end
    RAD = 180./pi;
    ARCSEC_DEG = 3600;
    
    if strcmp(Args.SearchRadiusUnits, 'arcsec')
        % quick conversion
        SearchRadius = SearchRadius./(RAD.*ARCSEC_DEG);  % [rad]
    else
        SearchRadius = convert.angular(Args.SearchradiusUnits, 'rad', SearchRadius);  % [rad]
    end
    
    % get Coo:
    [RA, Dec]=celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits',Args.OutUnits, 'Server',Args.Server);
    
    % convert coo to haelpix indices
    PixHP  = celestial.healpix.coneSearch(Args.NSide, RA./RAD, Dec./RAD, SearchRadius, 'Type',Args.Type);
    if Args.UniquePixID
        % convert to unique pix id:
        PixHP = celestial.healpix.pix2uniqueId(Args.NSide, PixHP);
    end
     
    % construct WHERE clause:
    Npix = numel(PixHP);
    WhereClause = sprintf('%s=%d',Args.HP_ColName, PixHP(1));
    for Ipix=2:1:Npix
        WhereClause = sprintf('%s OR %s=%d',WhereClause, Args.HP_ColName, PixHP(1));
    end    
    
end
