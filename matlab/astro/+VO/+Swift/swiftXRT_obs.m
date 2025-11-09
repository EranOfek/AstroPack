function [T,DataURL]=swiftXRT_obs(RAdeg, Decdeg, SearchRadiusDeg, Args)
    % Search for Swift XRT observations around a position using the VO API or TAP service.
    %   Query HEASARC's SWIFTMASTR catalog for all Swift observations within a
    %   circular region centered on (RAdeg, Decdeg) with radius SearchRadiusDeg (deg).
    %   This version uses the VO Cone endpoint by default (returns VOTable TABLEDATA),
    %   which is reliable across deployments and avoids VOTable BINARY/BINARY2 parsing.
    %
    % Input  : - J2000 RA [deg]. See celestial.convert.cooResolve for
    %            options.
    %          - J2000 Dec [deg]. Default is [].
    %          - Search radius [deg]. Default is 1.
    %          * ...,key,val,...
    %            'cooResolveArgs' - A cell array of additional argumnets to
    %                   pass to celestial.convert.cooResolve.
    %                   Default is {}.
    %            'UseTap' - A logical indicating if to use Tap.
    %                   If false use: VO.Swift.swiftXRT_obs_API
    %                   Default is false.
    %            'MaxRows' - double, default 0 (unlimited)
    %                    Maximum number of rows to return (0 = no server-side limit).
    %            'Timeout' - double > 0, default 120
    %                    Web request timeout in seconds.
    %            'RequireXRT' - logical, default true
    %                    If true, keep only rows with XRT_Exposure > 0 (if that
    %                    column exists in the returned table).
    %            'UseVOCone' - logical, default true
    %                    If true (default), use the VO Cone endpoint:
    %                      https://heasarc.gsfc.nasa.gov/xamin/vo/cone
    %                    If set to false, switches to the Xamin endpoint:
    %                      https://heasarc.gsfc.nasa.gov/xamin/query
    %                    Note: the Xamin path may return VOTable in BINARY/BINARY2,
    %                    which this lightweight parser does not decode. VO Cone is
    %                    recommended unless you add a BINARY2 parser.
    %
    % Output : - table
    %            All columns returned by SWIFTMASTR for entries within the cone
    %            (the service may evolve; the function preserves whatever it returns),
    %            plus two convenience columns appended by this function:
    %            Notes on specific columns:
    %            ObsID_str          (string)
    %               ObsID normalized to string (useful when the catalog field appears as
    %               numeric or with a variant name).
    %            AWS_S3_ArchivePath (string)
    %               Best-effort S3 archive path for each observation:
    %                   s3://nasa-heasarc/swift/data/obs/YYYY_MM/OBSID
    %               where YYYY_MM is derived from Start_Time when available.
    %               If Start_Time is missing/invalid, the path is left empty ("").
    %           Typical SWIFTMASTR columns you may see (names can evolve):
    %               ObsID, Name, RA, Dec, Start_Time, Stop_Time, Exposure,
    %               XRT_Exposure, XRT_Expo_Pc/Wt/Im, UVOT_Exposure, BAT_*,
    %               Proposal/PI fields, Processing/Pipeline fields.
    %          - A string array of XRT/event data urls for each line in the table.   
    % Author : ChatGPT + Eran Ofek (Nov 2025)
    % Example:
    %           [T,DU]  = VO.Swift.swiftXRT_obs(210.75, 54.3, 2);


    arguments
        RAdeg                 
        Decdeg                = [];
        SearchRadiusDeg       (1,1) double {mustBePositive} = 1;
        Args.CooResolveArgs   = {};
        Args.UseTap           = false;
        Args.MaxRows          (1,1) double {mustBeNonnegative} = 0
        Args.Timeout          (1,1) double {mustBePositive} = 120
        Args.RequireXRT       (1,1) logical = true
        Args.UseVOCone        (1,1) logical = true

        Args.AddPathXRT  = true;
    end

    [RA, Dec]=celestial.convert.cooResolve(RAdeg, Decdeg);


    if Args.UseTap
    
        Q = sprintf("SELECT * FROM swiftmastr WHERE 1 = CONTAINS( POINT('ICRS', RA, Dec), CIRCLE('ICRS', %9.5f, %9.5f, %7.4f) )", RAdeg, Decdeg, SearchRadiusDeg);

        %Q = 'SELECT TOP 10 * FROM swiftmastr';
        T = VO.TopCat.queryStilts(Q, ...
                            'TapUrl','https://heasarc.gsfc.nasa.gov/xamin/vo/tap', ...
                            'Ofmt','csv','TimeoutSec',120);

    else
        T = VO.Swift.swiftXRT_obs_API(RAdeg, Decdeg, SearchRadiusDeg,...
                                    'MaxRows',Args.MaxRows,...
                                    'Timeout',Args.Timeout,...
                                    'RequireXRT',Args.RequireXRT,...
                                    'UseVOCone',Args.UseVOCone);
    end


    if nargout>1
        DataURL = VO.Swift.swiftXRT_table2link(T, Args.AddPathXRT);
    end       

end

