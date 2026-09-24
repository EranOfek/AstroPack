function [T, ObjectUrl, Raw] = queryTNS(RA, Dec, Args)
    % Query TNS by object name OR position, with constraints.
    % Input  : - J2000 R.A., [deg|sex] or object name.
    %            If second input is provided and RA is not numeric, then
    %            will assume input is in sexagesinal coordinates.
    %            The RA, Dec are resolved using:
    %            celestial.convert.cooResolve
    %          - J2000 Dec. [deg|sex]. If empty, then will interpret the
    %            first input argument as an object name.
    %            Default is [].
    %          * ...,key,val,...
    %            'cooResolveArgs' - A cell array of additional arguments to
    %                   pass to celestial.convert.cooResolve
    %            'Radius' - Search radius. Default is 5.
    %            'Units' - Search radius units. Default is 'arcsec'.
    %            See more options in code.
    % Output : - Table with selected results from the TNS DB.
    %          - URL of selected object.
    %          - Raw decoded JSON response (struct).
    % Author : ChatGPT, Eran Ofek (Sep 2025)
    % Example: [T, Url] = VO.TNS.queryTNS('AT2025vll', [], 'Radius',0.5, 'Units',"arcmin", 'Limit',50);    
    %          [T1, Url1] = VO.TNS.queryTNS(T.ra{1}, T.dec{1}, 'Radius',0.5, 'Units',"arcmin", 'Limit',50);
    %
    
    arguments
        RA
        Dec     = [];
    
        Args.cooResolveArgs   = {};
        Args.Radius            = 5;
        Args.Units             = "arcsec";

        % --- Auth / UA marker ---
        Args.TnsApiKey  = [];
        Args.TnsId      = [];
        Args.TnsName    = [];
        Args.TnsType    = "bot"
    
        % Query mode: if Name given -> GET endpoint; else -> SEARCH endpoint
        %Args.Name        (1,1) string = ""
    
        % /api/get/search filters
        
        Args.Page              = 1;
        Args.Limit             = 100;
        Args.Classification    = string.empty;
        Args.NameLike          = "";
        Args.InternalName      = "";
        Args.DiscDateStart     = "";      % YYYY-MM-DD
        Args.DiscDateEnd       = "";      % YYYY-MM-DD
        Args.PublicationStatus = "public";  % ["public","nonpublic","all"])}
    

        %--- Enrichment (Get-Object after Search) ---
        Args.ExpandDetails logical  = true;   % fetch full object info for each hit
        Args.WithPhotometry logical = false; % include photometry in Get-Object
        Args.WithSpectra logical    = false; % include spectra in Get-Object
        Args.MaxGet                 = 500;   % safety cap
        Args.GetDelaySec            = 0.25;  % pause between Get-Object calls


        % Endpoints & behavior (NOTE the /api/get base)
        Args.UrlBase         = "https://www.wis-tns.org/api/get";
        Args.Timeout         = 120;
        Args.Verbose logical = false;
    end
    
    if isempty(Args.TnsApiKey)
        % get from Configuration
        Conf = Configuration.getSingelton;
        Args.TnsApiKey = Conf.Data.TNS.ApiKey;
        Args.TnsId     = Conf.Data.TNS.TnsId;
        Args.TnsName   = Conf.Data.BotName;
    end


    % ---------- Guards ----------
    if strlength(strtrim(Args.TnsApiKey)) == 0
        error("TnsApiKey is empty. Pass it via 'TnsApiKey', <your key>.");
    end
    

    [NewRA, NewDec] = celestial.convert.cooResolve(RA, Dec, Args.cooResolveArgs{:});
    if isnan(NewRA) && (ischar(RA) || isstring(RA))
        Name = RA;
        RA   = [];
        Dec  = [];
    else
        RA   = NewRA;
        Dec  = NewDec;
        Name = [];
    end

    
    
    UrlSearch = Args.UrlBase + "/search";
    UrlGet    = Args.UrlBase + "/object";
    
    % ---------- TNS User-Agent header ----------
    Ua = sprintf('tns_marker{"tns_id":%d,"type":"%s","name":"%s"}', ...
                 Args.TnsId, char(Args.TnsType), char(Args.TnsName));
    
    Opts = weboptions( ...
        'HeaderFields', {'User-Agent', Ua; 'user-agent', Ua; 'Accept','application/json'}, ...
        'RequestMethod','post', ...
        'MediaType','application/x-www-form-urlencoded', ...  % form fields
        'Timeout', Args.Timeout);
    
    IsByName = ~isempty(Name); %strlength(strtrim(Name)) > 0;
    
    if IsByName
        % ======================= /api/get/object (single object) =======================
        Data = struct('objname', char(Name));
        if Args.WithPhotometry
            Data.photometry = "1";
        else
            Data.photometry = "0"; 
        end
        if Args.WithSpectra
            Data.spectra    = "1";
        else
            Data.spectra    = "0";
        end
    
        Raw = webwrite(UrlGet, 'api_key', char(Args.TnsApiKey), 'data', jsonencode(Data), Opts);
        ObjList = LocalExtractObjectsFromGet(Raw);
    
    else
        % ======================= /api/get/search (cone & filters) =====================
        % Build filters
        Data = struct();
        TriedDegrees = false;
    
        if isfinite(RA) && isfinite(Dec)
            % First try in DECIMAL DEGREES (as strings) — often works best
            Data.ra     = sprintf('%.10f', RA);
            Data.dec    = sprintf('%.10f', Dec);
            Data.radius = num2str(Args.Radius);
            Data.units  = char(Args.Units);
            TriedDegrees = true;
        end
    
        if strlength(Args.NameLike)     > 0
            Data.objname       = char(Args.NameLike);     
        end
        if strlength(Args.InternalName) > 0
            Data.internal_name = char(Args.InternalName);
        end
        if ~isempty(Args.Classification)
            Data.classification = strjoin(Args.Classification, ',');
        end
        if strlength(Args.DiscDateStart)>0 || strlength(Args.DiscDateEnd)>0
            Data.public_timestamp = struct('from',char(Args.DiscDateStart), 'to',char(Args.DiscDateEnd));
        end
        switch Args.PublicationStatus
            case "public"
                Data.public = "1";
            case "nonpublic"
                Data.public = "0";
            case "all"        
                % omit
        end
        Data.page     = Args.Page;
        Data.num_page = Args.Limit;
    
        Raw = webwrite(UrlSearch, 'api_key', char(Args.TnsApiKey), 'data', jsonencode(Data), Opts);
        ObjList = LocalExtractObjectsFromSearch(Raw);
    
        % If none returned, retry with sexagesimal RA/Dec
        if isempty(ObjList) && TriedDegrees && isfinite(RA) && isfinite(Dec)
            [RaHms, DecDms] = LocalRaDecToSexa(RA, Dec);
            Data.ra  = RaHms;   % "hh:mm:ss.s"
            Data.dec = DecDms;  % "±dd:mm:ss.s"
            Raw = webwrite(UrlSearch, 'api_key', char(Args.TnsApiKey), 'data', jsonencode(Data), Opts);
            ObjList = LocalExtractObjectsFromSearch(Raw);
        end
    
        % ---- Enrich with Get-Object (turn minimal search hits into full rows) ----
        if Args.ExpandDetails && ~isempty(ObjList)
            Names = LocalPickNames(ObjList);
            N = min(numel(Names), Args.MaxGet);
            Full = cell(1, N);
            for I = 1:N
                if Args.Verbose
                    fprintf("Get-Object %d/%d: %s\n", I, N, Names(I));
                end
                D2 = struct('objname', char(Names(I)));
                D2.photometry = string(double(Args.WithPhotometry));
                D2.spectra    = string(double(Args.WithSpectra));
                R2 = webwrite(UrlGet, 'api_key', char(Args.TnsApiKey), 'data', jsonencode(D2), Opts);
                L2 = LocalExtractObjectsFromGet(R2);
                if ~isempty(L2)
                    Full{I} = L2(1);
                else
                    Full{I} = struct();
                end
                if Args.GetDelaySec > 0
                    pause(Args.GetDelaySec);
                end
            end
            % Replace ObjList with enriched details where available; fallback to search stub
            Mask = cellfun(@(c) ~isempty(c), Full);
            ObjList = [Full{Mask}];  % rows with details
            if isempty(ObjList)
                % if for some reason all Get calls failed, keep the search stubs
                ObjList = LocalExtractObjectsFromSearch(Raw);
            end
        end
    end
    
    % ---------- Build table & URLs ----------
    if isempty(ObjList)
        if Args.Verbose, fprintf('No objects matched the query.\n'); end
        T = table(); ObjectUrl = strings(0,1);
        return
    end
    
    Flat = arrayfun(@(S) LocalFlattenStruct(S, ""), ObjList, 'UniformOutput', false);
    T = struct2table(cat(2, Flat{:}), 'AsArray', true);
    
    % Find a name-like field for URL building
    NameField = "";
    for Cand = ["name","objname","objName","object_name","objectName","name_prefix"]
        if any(strcmpi(T.Properties.VariableNames, Cand))
            NameField = Cand;
            break;
        end
    end
    Nm = strings(height(T),1);
    if NameField~=""
        Nm = string(T.(NameField));
    end
    
    % Correct URL construction
    BaseUrl  = "https://www.wis-tns.org/object/";
    ObjectUrl = strings(height(T),1);
    for I = 1:height(T)
        if strlength(Nm(I)) > 0
            U = matlab.net.URI(BaseUrl + Nm(I));
            ObjectUrl(I) = string(U.EncodedURI);
        end
    end
    T.ObjectURL = ObjectUrl;
    
    % ====================== Helpers ======================
    function L = LocalExtractObjectsFromGet(R)
        L = [];
        if isstruct(R) && isfield(R,'data') && ~isempty(R.data)
            D = R.data;
            if isstruct(D)
                if isfield(D,'object') && isstruct(D.object)
                    L = D.object;
                elseif isfield(D,'objname') || isfield(D,'name')
                    L = D;
                elseif numel(D) > 1
                    L = D;
                end
            elseif iscell(D)
                Tmp = [];
                for K = 1:numel(D)
                    X = D{K};
                    if isstruct(X)
                        if isfield(X,'object'), X = X.object; end
                        Tmp = [Tmp; X]; %#ok<AGROW>
                    end
                end
                L = Tmp;
            end
        end
        if isempty(L), L = []; end
    end
    
    function L = LocalExtractObjectsFromSearch(R)
        L = [];
        if isstruct(R) && isfield(R,'data') && ~isempty(R.data)
            D = R.data;
            if isfield(D,'reply') && isstruct(D.reply)
                Rr = D.reply;
                if isfield(Rr,'objects'), L = Rr.objects; else, L = Rr; end
            elseif isfield(D,'objects')
                L = D.objects;
            elseif isstruct(D)
                L = D;
            end
        end
        if isempty(L), L = []; end
    end
    
    function [RaHms, DecDms] = LocalRaDecToSexa(RaDeg, DecDeg)
        RaH  = RaDeg / 15;  Rah = floor(RaH);  Ram = floor((RaH-Rah)*60);
        Ras  = (RaH - Rah - Ram/60)*3600;
        RaHms = sprintf('%02d:%02d:%06.3f', Rah, Ram, Ras);
        Sgn = '+'; if DecDeg < 0, Sgn = '-'; end
        Ad = abs(DecDeg); Dd = floor(Ad); Dm = floor((Ad-Dd)*60);
        Ds = (Ad - Dd - Dm/60)*3600;
        DecDms = sprintf('%c%02d:%02d:%06.3f', Sgn, Dd, Dm, Ds);
    end
    
    function Flat = LocalFlattenStruct(S, Prefix)
        Flat = struct(); Fn = fieldnames(S);
        for Ii = 1:numel(Fn)
            K = Fn{Ii}; V = S.(K);
            Name = K; if strlength(Prefix)>0, Name = sprintf('%s_%s', Prefix, K); end
            if isstruct(V)
                if numel(V)==1
                    Sub = LocalFlattenStruct(V, Name);
                    Flat = LocalMerge(Flat, Sub);
                else
                    Flat.(Name) = string(jsonencode(V));
                end
            elseif iscell(V)
                try
                    Flat.(Name) = strjoin(string([V{:}]), '; ');
                catch
                    Flat.(Name) = string(jsonencode(V));
                end
            elseif isstring(V)
                if isscalar(V)
                    Flat.(Name)=V;
                else
                    Flat.(Name)=strjoin(V,'; ');
                end
            elseif ischar(V)
                Flat.(Name) = string(V);
            elseif isnumeric(V) || islogical(V)
                if isscalar(V)
                    Flat.(Name)=V;
                else, Flat.(Name)=string(mat2str(V));
                end
            else
                Flat.(Name) = string(jsonencode(V));
            end
        end
    end
    
    function Out = LocalMerge(A,B)
        Out = A; Fb = fieldnames(B);
        for J = 1:numel(Fb)
            Out.(Fb{J}) = B.(Fb{J});
        end
    end
    
    function Nms = LocalPickNames(ObjOrList)
        % Try to extract 'objname' from search rows or get-object rows
        if isstruct(ObjOrList) && numel(ObjOrList)==1
            ObjOrList = ObjOrList;
        end
        if isempty(ObjOrList)
            Nms = strings(0,1);
            return;
        end
        tmp = strings(1, numel(ObjOrList));
        for k = 1:numel(ObjOrList)
            s = ObjOrList(k);
            cand = "";
            for c = ["objname","name","object_name","objectName","name_prefix"]
                if isfield(s, c)
                    cand = string(s.(c));
                    break;
                end
            end
            tmp(k) = cand;
        end
        Nms = tmp(tmp~="");
    end
end

