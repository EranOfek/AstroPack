function [T,DataURL]=swiftXRT_obs(RAdeg, Decdeg, SearchRadiusDeg, Args)
    % Search for Swift XRT observations around a position using the VO API or TAP service.
    %   Query HEASARC's SWIFTMASTR catalog for all Swift observations within a
    %   circular region centered on (RAdeg, Decdeg) with radius SearchRadiusDeg (deg).
    %   This version uses the VO Cone endpoint by default (returns VOTable TABLEDATA),
    %   which is reliable across deployments and avoids VOTable BINARY/BINARY2 parsing.
    %
    % Input  : - J2000 RA [deg].
    %          - J2000 Dec [deg].
    %          - Search radius [deg]. Default is 1.
    %          * ...,key,val,...
    %            'UseTap' - A logical indicating if to use Tap.
    %                   If false use: VO.Swift.swiftXRT_obs_API
    %                   Default is true.
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
        RAdeg                 (1,1) double
        Decdeg                (1,1) double
        SearchRadiusDeg       (1,1) double {mustBePositive} = 1;
        Args.UseTap           = true;
        Args.MaxRows          (1,1) double {mustBeNonnegative} = 0
        Args.Timeout          (1,1) double {mustBePositive} = 120
        Args.RequireXRT       (1,1) logical = true
        Args.UseVOCone        (1,1) logical = true

        Args.AddPathXRT  = true;
    end

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
        Nt = size(T,1);
        DataURL = strings(Nt,1);
        for It=1:1:Nt
            Row   = T(It,:);                                % your selected line
            Obsid = string(Row.obsid);                     % or string(row.ObsID_str)
            %St    = datetime(string(Row.start_time),'InputFormat','yyyy-MM-dd''T''HH:mm:ss','TimeZone','UTC');
            St = local_to_datetime(Row.start_time);   % robust ISO/MJD/JD → datetime(UTC)
    
            Yyyy_mm = sprintf('%04d_%02d', year(St), month(St));
            
            S3    = "s3://nasa-heasarc/swift/data/obs/" + Yyyy_mm + "/" + Obsid + "/";
            DataURL(It) = "https://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/" + Yyyy_mm + "/" + Obsid + "/";
            %disp(S3), disp(DataURL)
        end

        if Args.AddPathXRT
            DataURL = DataURL + "xrt/event";
        end
    end

end


function Dt = local_to_datetime(V)
% Robust converter: ISO string(s) or JD/MJD number(s) → datetime(UTC)
    if isa(V,'datetime')
        Dt = V; 
        if isempty(Dt.TimeZone), Dt.TimeZone = 'UTC'; end
        return
    end

    if isstring(V) || ischar(V)
        S = string(V);
        % Try ISO-8601 with milliseconds, then seconds
        try
            Dt = datetime(S,'InputFormat','yyyy-MM-dd''T''HH:mm:ss.SSS','TimeZone','UTC');
            return
        catch, end
        try
            Dt = datetime(S,'InputFormat','yyyy-MM-dd''T''HH:mm:ss','TimeZone','UTC');
            return
        catch, end
        % Maybe numeric-in-string (JD/MJD)
        D = str2double(S);
        if all(~isnan(D))
            Dt = local_from_mjd_or_jd(D);
            return
        end
        % Fallback
        Dt = NaT(size(S)); Dt.TimeZone = 'UTC';
        return
    end

    if isnumeric(V)
        Dt = local_from_mjd_or_jd(V);
        return
    end

    % Final fallback
    Dt = NaT(size(V)); Dt.TimeZone = 'UTC';
end

function Dt = local_from_mjd_or_jd(X)
% Accepts scalar or array. Treat values > 2.4e6 as JD, else MJD.
    Epoch = datetime(1858,11,17,'TimeZone','UTC');  % MJD epoch
    X = double(X);
    IsJD = X > 2400000;                % JD if huge; else MJD
    MJD = X; 
    MJD(IsJD) = X(IsJD) - 2400000.5;   % JD → MJD
    Dt = Epoch + days(MJD);
end
