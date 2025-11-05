function T = swiftXRT_obs_API(RAdeg, Decdeg, SearchRadiusDeg, Args)
    % Search for Swift XRT observations around a position (VO Cone) using the VO API.
    %   Query HEASARC's SWIFTMASTR catalog for all Swift observations within a
    %   circular region centered on (RAdeg, Decdeg) with radius SearchRadiusDeg (deg).
    %   This version uses the VO Cone endpoint by default (returns VOTable TABLEDATA),
    %   which is reliable across deployments and avoids VOTable BINARY/BINARY2 parsing.
    %
    % Input  : - J2000 RA [deg].
    %          - J2000 Dec [deg].
    %          - Search radius [deg]. Default is 1.
    %          * ...,key,val,...
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
    %
    % Author : ChatGPT + Eran Ofek (Nov 2025)
    % Example :
    %           % Your case (VO Cone default):
    %           T  = VO.Swift.swiftXRT_obs_API(210.75, 54.3, 2);
    
    arguments
        RAdeg                 (1,1) double
        Decdeg                (1,1) double
        SearchRadiusDeg       (1,1) double {mustBePositive} = 1;
        Args.MaxRows          (1,1) double {mustBeNonnegative} = 0
        Args.Timeout          (1,1) double {mustBePositive} = 120
        Args.RequireXRT       (1,1) logical = true
        Args.UseVOCone        (1,1) logical = true
    end
    
    VOConeURL = 'https://heasarc.gsfc.nasa.gov/xamin/vo/cone';
    XaminURL  = 'https://heasarc.gsfc.nasa.gov/xamin/query';
    
    try
        if Args.UseVOCone
            % VO Cone (radius in degrees)
            Query = sprintf('%s?table=swiftmastr&RA=%.8f&DEC=%.8f&SR=%.8f', ...
                            VOConeURL, RAdeg, Decdeg, SearchRadiusDeg);
            if Args.MaxRows > 0
                Query = sprintf('%s&VERB=2&MAXREC=%d', Query, round(Args.MaxRows));
            end
        else
            % Xamin (radius in arcminutes; may return BINARY/BINARY2)
            Query = sprintf('%s?table=%s&position=%s&radius=%g&format=votable&messages=none', ...
                            XaminURL, 'swiftmastr', urlencode(sprintf('%.8f,%.8f',RAdeg,Decdeg)), ...
                            SearchRadiusDeg*60.0);
            if Args.MaxRows > 0
                Query = sprintf('%s&resultmax=%d', Query, round(Args.MaxRows));
            end
        end
    
        XmlText = webread(Query, weboptions('Timeout',Args.Timeout,'ContentType','text'));
        T = local_votable_to_table(XmlText);
    catch ME
        warning('Primary query failed (%s). Returning empty table.', ME.message);
        T = table();
    end
    
    % Optional: filter to XRT_Exposure>0
    if Args.RequireXRT && ~isempty(T)
        Vn = T.Properties.VariableNames;
        Xcol = Vn(strcmpi(Vn,'xrt_exposure'));
        if ~isempty(Xcol)
            T = T(T.(Xcol{1})>0, :);
        end
    end
    
    % Normalize ObsID and add S3 path
    if ~isempty(T)
        Vn = T.Properties.VariableNames;
    
        % ObsID as string (handles numeric/text variants)
        OidName = Vn(strcmpi(Vn,'obsid'));
        if ~isempty(OidName)
            ObsIDStr = string(T.(OidName{1}));
        else
            ObsIDStr = repmat("", height(T), 1);
        end
    
        % Extract Start_Time -> YYYY_MM
        StartDT = [];
        StName = Vn(strcmpi(Vn,'start_time'));
        if ~isempty(StName)
            StartDT = T.(StName{1});
            if ~isa(StartDT,'datetime')
                try
                    StartDT = datetime(string(StartDT), ...
                        'InputFormat','yyyy-MM-dd''T''HH:mm:ss','TimeZone','UTC');
                catch
                    StartDT = NaT(size(StartDT));
                end
            end
        end
    
        YYYY_MM = strings(height(T),1);
        for I = 1:height(T)
            if ~isempty(StartDT) && ~isnat(StartDT(I))
                YYYY_MM(I) = sprintf('%04d_%02d', year(StartDT(I)), month(StartDT(I)));
            else
                YYYY_MM(I) = "";
            end
        end
    
        % Construct S3 path
        S3Base = "s3://nasa-heasarc/swift/data/obs";
        AWS_S3_ArchivePath = strings(height(T),1);
        for I = 1:height(T)
            if strlength(ObsIDStr(I))>0 && strlength(YYYY_MM(I))>0
                AWS_S3_ArchivePath(I) = S3Base + "/" + YYYY_MM(I) + "/" + ObsIDStr(I);
            else
                AWS_S3_ArchivePath(I) = "";
            end
        end
    
        % Append convenience columns
        T.ObsID_str = ObsIDStr;
        T.AWS_S3_ArchivePath = AWS_S3_ArchivePath;
    end

end % function


% -------------------- Helpers (internal) --------------------
function T = local_votable_to_table(XmlText)
% Minimal VOTable TABLEDATA parser -> table (keeps all fields verbatim).
    Dom = xmlreadstring(XmlText);
    Tables = Dom.getElementsByTagName('TABLE');
    if Tables.getLength()==0, T = table(); return, end
    Tab = Tables.item(0);

    Fields = Tab.getElementsByTagName('FIELD');
    Ncol = Fields.getLength();
    VarNames = strings(1,Ncol);
    DataTypes = strings(1,Ncol);
    for I = 1:Ncol
        F = Fields.item(I-1);
        Nm = char(F.getAttribute('name'));
        if isempty(Nm), Nm = char(F.getAttribute('ID')); end
        if isempty(Nm), Nm = sprintf('col%d',I); end
        Nm = regexprep(Nm,'[^A-Za-z0-9_]','_');
        if isempty(Nm) || ~isletter(Nm(1)), Nm = ['V_' Nm]; end
        VarNames(I) = string(Nm);
        DataTypes(I) = string(lower(char(F.getAttribute('datatype'))));
    end

    TData = Tab.getElementsByTagName('TABLEDATA');
    if TData.getLength()==0, T = table(); return, end
    Rows = TData.item(0).getElementsByTagName('TR');
    Nrow = Rows.getLength();

    Cols = cell(1,Ncol); for C = 1:Ncol, Cols{C} = cell(Nrow,1); end
    for R = 1:Nrow
        TDs = Rows.item(R-1).getElementsByTagName('TD');
        for C = 1:min(Ncol, TDs.getLength())
            Node = TDs.item(C-1);
            if Node.hasChildNodes()
                Cols{C}{R} = char(Node.getFirstChild().getNodeValue());
            else
                Cols{C}{R} = '';
            end
        end
    end

    V = cell(1,Ncol);
    for C = 1:Ncol
        Raw = Cols{C};
        Dt  = DataTypes(C);
        if any(Dt == ["double","float","float8","float4","int","long","short"])
            Num = nan(Nrow,1);
            for K = 1:Nrow
                Vv = str2double(Raw{K});
                if ~isnan(Vv), Num(K) = Vv; end
            end
            V{C} = Num;
        else
            LooksISO = true;
            for K = 1:Nrow
                S = Raw{K};
                if ~(isempty(S) || ~isempty(regexp(S,'^\d{4}-\d{2}-\d{2}T', 'once')))
                    LooksISO = false; break
                end
            end
            if LooksISO
                try
                    V{C} = datetime(string(Raw), ...
                        'InputFormat','yyyy-MM-dd''T''HH:mm:ss','TimeZone','UTC');
                    continue
                catch
                end
            end
            V{C} = string(Raw);
        end
    end

    T = table(V{:}, 'VariableNames', cellstr(VarNames));
end

function Dom = xmlreadstring(S)
    import java.io.*; import org.xml.sax.InputSource
    Sr = java.io.StringReader(S);
    Is = org.xml.sax.InputSource(Sr);
    Dom = xmlread(Is);
end
