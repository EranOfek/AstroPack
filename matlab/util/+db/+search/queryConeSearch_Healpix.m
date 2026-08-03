function [WhereClause,PixHP] = queryConeSearch_Healpix(RA, Dec, SearchRadius, Args)
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
    %            'AddWhere' - Additional where clause to add.
    %                   Default is ''.
    %
    % Output : - A char array with healpix cone search SQL Where clause.
    %          - A vector of healpix pixels that covers the cone search.
    % Author : Eran Ofek (2025 Jan) 
    % Example: WhereClause=db.search.queryConeSearch_Healpix(100,10)  % deg
    %          WhereClause=db.search.queryConeSearch_Healpix('10:10:10','-20:10:10',5,'NSide',2.^8, 'HP_ColName','upix_low')
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

        Args.AddWhere          = '';  % should in clude AND or OR
    end
    ARCSEC_DEG = 3600;

    % the healpix cone search works in degrees:
    if strcmp(Args.SearchRadiusUnits, 'arcsec')
        % quick conversion
        SearchRadius = SearchRadius./ARCSEC_DEG;  % [deg]
    else
        SearchRadius = convert.angular(Args.SearchRadiusUnits, 'deg', SearchRadius);  % [deg]
    end

    % get Coo:
    [RA, Dec]=celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits',Args.OutUnits, 'Server',Args.Server);
    Factor = convert.angular(Args.OutUnits, 'deg');

    switch lower(Args.Type)
        case 'nested'
            Scheme = 'NEST';
        case 'ring'
            Scheme = 'RING';
        otherwise
            error('Unknown Type option');
    end

    % convert coo to healpix indices.
    % The inclusive disc query of the healpix library is used: it returns all the
    % pixels whose borders overlap the cone. The sampling-based
    % celestial.healpix.coneSearch misses some of these pixels (issue #579).
    PixHP  = celestial.healpix.mex.coneSearch(Args.NSide, RA.*Factor, Dec.*Factor, SearchRadius, 'inclusive', Scheme);
    if Args.UniquePixID
        % convert to unique pix id:
        PixHP = celestial.healpix.pix2uniqueId(Args.NSide, PixHP);
    end
     
    % construct WHERE clause:
    Npix = numel(PixHP);
    WhereClause = sprintf('(%s=%d',Args.HP_ColName, PixHP(1));
    for Ipix=2:1:Npix
        WhereClause = sprintf('%s OR %s=%d',WhereClause, Args.HP_ColName, PixHP(Ipix));
    end    
    WhereClause = sprintf('%s)',WhereClause);

    if ~isempty(Args.AddWhere)
        WhereClause = sprintf('%s %s', WhereClause, Args.AddWhere);
    end
    
end
