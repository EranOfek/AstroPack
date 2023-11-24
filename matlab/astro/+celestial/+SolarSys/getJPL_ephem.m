function [OutputTable, Output, StrURL] = getJPL_ephem(Object, Args)
    % A general purpose access to JPL horizons ephemeris
    %   This is a basic function that can retrieve ephemeris, vectors, and
    %   ocultaing elements for Solar System bodies.
    % Input  : - Object name. Solar system body name.
    %            For asteroids (add ';' at the end of name - e.g., '9804;'.
    %            Selected bodies:
    %            '299' - Venus'
    %            '85' - Io
    %            See https://ssd.jpl.nasa.gov/horizons/manual.html#select
    %          * ...,key,val,...
    %            'FailMultiName' - A logical indicating on what to do if
    %                   the object name resulted in multiple possible objects.
    %                   If true, then will fail.
    %                   If false, then will attempt to read the name of the
    %                   barycenter of the likely object and re execute the
    %                   function.
    %                   Default is false.
    %            'AddToName' - Addittional condition to add to object name.
    %                   'CAP' - apparition solution closest to present date, including any fragments
    %                   'NOFRAG' -  excluding fragments
    %                    'CAP; NOFRAG'
    %                    '' - No additions. Default.
    %            'StartTime' - Start time [D M Y H M S], or JD, or ISO
    %                   string. If empty, then use now.
    %                   Default is [].
    %            'StopTime' - Like StartTime, but for the end time.
    %                   If empty, then use now + 1 day.
    %                   If smaller than StartTime, then will set this to
    %                   StartTime + 1 day.
    %                   Default is [].
    %            'TimeScale' - Options are:
    %                   'TDB' - Barycentric Dynamical Time.
    %                   'TT' - Terrestrial Time.
    %                   'UT' - For UTC 
    %                   Default is 'UT'.
    %            'EPHEM_TYPE' - Options are:
    %                   'OBSERVER' - Ephemeris for an observer specified by
    %                           'CENTER' and 'GeoCoo'.
    %                           'TimeScale' must be 'TT' or 'UT'.
    %                   'ELEMENTS' - Heliocentric orbital elements.
    %                           'TimeScale' must be 'TDB'.
    %                   'VECTORS' - J2000.0 Ecliptic Cartesian position.
    %                   Default is 'OBSERVER'.
    %            'CENTER' - Body center (observer or coordinate origin).
    %                   For 'EPHEM_TYPE'='ELEMENTS', the 'CENTER' is
    %                   switched to '500@10' (i.e., heliocentric).
    %                   Examples:
    %                       '500@10' - Heliocentric
    %                       '500@0' - Barycentric
    %                       '399' - Geocentric
    %                   Default is 'Geocentric'.
    %            'GeoCoo' - [Lon, Lat, Height] in [deg deg km] for a
    %                   topocentric observer. Default is [].
    %            'StepSize' - Ephemereis steop size. Default is 1.
    %            'StepUnits' - Ephemeris step units.
    %                   'm','h','d', for min, hour, day.
    %                   Default is 'd'.
    %            'OutType' - Default is 'table'.
    %                   'OrbitalEl' is possible for 'ELEMENTS' options.
    %            'OrbEl' - An optional celestial.OrbitalEl object.
    %                   If given then the result will be written into one
    %                   row in OrbEl (wile columns are for different
    %                   objects).
    %                   If empty, create a new object. Default is [].
    %            'LineInd' - An optional line index in which to insert the
    %                   orbital element. If empty, append to end.
    %                   Default is [].
    %            See code for additional arguments
    % Output : - A Table with requested ephmeris.
    %          - Output string from JPL horizons website.
    %          - Query URL.
    % Author : Eran Ofek (Nov 2023)
    % References: Horizons API description: https://ssd-api.jpl.nasa.gov/doc/horizons.html
    % Example: [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','VECTORS','TimeScale','TT');  
    %          [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB');
    %          [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','OBSERVER','TimeScale','TT'); 
    %          [T,~,U] = celestial.SolarSys.getJPL_ephem('1999 AN10','EPHEM_TYPE','OBSERVER','TimeScale','TT'); 

    
    arguments
        Object              = '9804';    % 'COMMAND'   % use ';' for asteroid
        
        % BE CARFUL - keyword enfing with capital letters will be used in
        % the query...
        Args.FailMultiName logical = false;
        Args.AddDes logical = false;
        Args.AddToName      = ''; % 'CAP' | 'NOFRAG'
        Args.StartTime  = [];  % START_TIME : JD%f'  % TT, TDB, UT
        Args.StopTime   = [];  % STOP_TIME
        Args.TimeScale  = 'UT'; %'TDB'; %'TT'; %'TDB'; %'UT';
        Args.EPHEM_TYPE  = 'OBSERVER'; %'ELEMENTS'; %'OBSERVER'; %'OBSERVER' | 'VECTORS' | 'ELEMENTS' | 'SPK' | 'APPROACH'
        Args.CENTER      = 'Geocentric';
        Args.GeoCoo      = []; %[35 30 0.415];   % [deg deg km]
        
        Args.STEP_SIZE   = '1 d';  % STEP_SIZE
        Args.StepSize    = [];  
        Args.StepUnits   = 'd';
        
        Args.FORMAT      = 'json';  % 'json'|'text'
        Args.CSV_FORMAT  = 'YES';
        Args.OBJ_DATA    = 'NO';
        Args.MAKE_EPHEM  = 'YES';
        
        %Args.SITE_COORD  = [];  % [deg deg km]
        
        %Args.TLIST_TYPE  = 'JD';  % 'JD' | 'MJD' | 'CAL'
        Args.QUANTITIES  = '1,9,10,13,19,20,23,24,47';  % https://ssd.jpl.nasa.gov/horizons.cgi?s_tset=1#top
        Args.REF_SYSTEM  = 'J2000';
        Args.OUT_UNITS   = 'AU-D';   % 'KM-S' | 'AU-D' | 'KM-D'
        Args.CAL_FORMAT  = 'JD'; %'CAL';   % 'CAL' | 'JD' | 'BOTH'  
        Args.ANG_FORMAT  = 'DEG';  % 'HMS' | 'DEG'
        Args.APPARENT    = 'AIRLESS';  % 'AIRLESS' | 'REFRACTED'
        Args.TIME_DIGITS = 'FRACSEC';
        Args.SUPPRESS_RANGE_RATE = 'YES';
        Args.ELEV_CUT    = '-90';
        Args.SKIP_DAYLT  = 'NO';
        Args.SOLAR_ELONG = '0,180';
        Args.EXTRA_PREC  = 'YES';
                
        Args.GetOnlyUrl logical        = false;
        Args.Convert2Numeric logical   = true;
        Args.BaseUrl     = 'https://ssd.jpl.nasa.gov/api/horizons.api?';
        Args.DoubleQoutesFields = {'SOLAR_ELONG','START_TIME','STOP_TIME','STEP_SIZE','QUANTITIES','SITE_COORD'};
        Args.OutType     = 'table';
        Args.OrbEl       = [];
        Args.LineInd     = [];
        
        % internal usage (for recursive calls)
        Args.START_TIME  = [];
        Args.STOP_TIME   = [];
        Args.SkipFormating logical = false;  
    end
    
    if ~Args.SkipFormating
        switch Args.EPHEM_TYPE
            case 'ELEMENTS'
                Args.CENTER      = '500@10';  % only heliocentric
                Args.QUANTITIES  = [];
                Args.APPARENT    = [];
                Args.ELEV_CUT    = [];
                Args.SKIP_DAYLT  = [];
                Args.SOLAR_ELONG = [];
                Args.SUPPRESS_RANGE_RATE = [];
                Args.MAKE_EPHEM  = [];
            case 'VECTORS'

            otherwise
                % do nothing
        end

        if ~isempty(Args.StepSize)
            % override STEP_SIZE
            Args.STEP_SIZE = sprintf('%d %s', Args.StepSize, Args.StepUnits);
        end
    
    
        if isnumeric(Args.StartTime)
            if numel(Args.StartTime)>1
                JD1 = celestial.time.julday(Args.StartTime);
            elseif numel(Args.StartTime)==1
                JD1 = Args.StartTime;
            else
                % empty
                % get now
                JD1 = celestial.time.julday;
            end
        else
            JD1 = celestial.time.julday(Args.StartTime);
        end

        if isnumeric(Args.StopTime)
            if numel(Args.StopTime)>1
                JD2 = celestial.time.julday(Args.StopTime);
            elseif numel(Args.StopTime)==1
                JD2 = Args.StopTime;
            else
                % empty
                % get now + 1
                JD2 = celestial.time.julday + 1;
            end
        else
            JD2 = celestial.time.julday(Args.StopTime);
        end

        if JD1>JD2
            JD2 = JD1 + 1;
        end

        Args.START_TIME = sprintf('JD %19.9f %s',JD1, Args.TimeScale);
        Args.STOP_TIME  = sprintf('JD %19.9f',JD2);


        if ~isempty(Args.GeoCoo)
            Args.SITE_COORD = sprintf('%9.5f,%9.5f,%9.5f',Args.GeoCoo);
        end
    end
    
    Args.COMMAND = Object;
    
    if Args.AddDes
        % add 'DES=' before the object name
        Args.COMMAND = sprintf('DES= %s',Args.COMMAND);
    end
    
    if ~isempty(Args.AddToName)
        Args.COMMAND = sprintf('%s; %s',Args.COMMAND, Args.AddToName);
    end
    
    % add double quoates
    
    if ~Args.SkipFormating
        Ndq = numel(Args.DoubleQoutesFields);
        for Idq=1:1:Ndq
            Field = Args.DoubleQoutesFields{Idq};
            if isfield(Args, Field)
                if ~isempty(Args.(Field))
                    Args.(Field) = sprintf('''%s''',Args.(Field));
                end
            end
        end
    end
        
    
    % construct API string
    FN  = fieldnames(Args);
    Nfn = numel(FN);

    % Examples:
    %https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='499'&OBJ_DATA='YES'&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='2006-01-01'&STOP_TIME='2006-01-20'&STEP_SIZE='1%20d'&QUANTITIES='1,9,20,23,24,29'
    %https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='499'&OBJ_DATA='YES'&MAKE_EPHEM='YES'&EPHEM_TYPE='ELEMENTS'&CENTER='500@10'&START_TIME='2006-01-01'&STOP_TIME='2006-01-20'&STEP_SIZE='1%20d'&QUANTITIES='1,9,20,23,24,29'
    %https://ssd.jpl.nasa.gov/horizons_batch.cgi?batch=1&COMMAND='9804'&CENTER='500'&MAKE_EPHEM='NO'&TABLE_TYPE='OBSERVER'&START_TIME='2018-06-14'&STOP_TIME='2018-06-20'&STEP_SIZE='1d'&QUANTITIES='1,9,10,13,19,20,23,24'&CSV_FORMAT='YES'&EXTRA_PREC='YES'
    
    StrURL = sprintf('%s',Args.BaseUrl);
    for Ifn=1:1:Nfn
        LastLetter = FN{Ifn};
        if ~isempty(LastLetter)
            LastLetter = LastLetter(end);
            
            if ~strcmp(lower(LastLetter),LastLetter)
                %FN{Ifn}
                % upper case - assume this is a valid command
                if ~isempty(Args.(FN{Ifn}))
                    %FN{Ifn}
                    %Args.(FN{Ifn})
                    StrURL = sprintf('%s%s=%s&',StrURL, FN{Ifn}, Args.(FN{Ifn}));
                end
            end
        end
    end
    if strcmp(StrURL(end),'&')
        StrURL = StrURL(1:end-1);
    end
    
    
    if Args.GetOnlyUrl
        Output      = [];
        OutputTable = [];
    else
        Output = webread(StrURL);        
        if isfield(Output, 'error')
            error('%s',Output.error);
        end
        
        % Inspect validity of output
        if contains(Output.result, 'No matches found')
            error('Object name - No matches found');
        end
        
        if contains(Output.result, 'Multiple major-bodies match string')
            MultiName = true;
        else
            MultiName = false;
        end
        
        if MultiName
            if Args.FailMultiName
                Output.result
                error('Multiple body name found');
            else
                % extract the body system barycenter
                Tmp = regexp(Output.result,'(?<N>\d+) .+ \(system barycenter\)','tokens');

                Args.SkipFormating = true;
                Args = rmfield(Args, {'COMMAND'});
                % 
                KeyVal = tools.struct.struct2keyval(Args);
                [OutputTable, Output, StrURL] = celestial.SolarSys.getJPL_ephem(Tmp{1}{1}, KeyVal{:}); 
            end
        else
                

            JDstr = sprintf('JD%s',Args.TimeScale);
            PosJD = strfind(Output.result, JDstr);
            PosCD = strfind(Output.result, 'Calendar Date');
            PosDT = strfind(Output.result, 'Date__');

            Pos = min([PosJD(1); PosCD; PosDT]);

            Tmp1 = regexp(Output.result(Pos:end), '.+\$\$SOE','match');

            ColNames = split(Tmp1{1},',');
            ColNames = ColNames(1:end-1);
            ColNames = tools.string.spacedel(ColNames);
            Ncol     = numel(ColNames);

            Tmp2 = regexp(Output.result, '\$\$SOE.+\$\$EOE','match');
            Table = strrep(Tmp2{1},'$$SOE','');
            Table = strrep(Table,'$$EOE','');
            TS    = split(Table, ',');
            TS    = TS(1:end-1);
            TS    = reshape(TS, [Ncol numel(TS)./Ncol]).';

            FlagGoodCol = ~cellfun(@isempty, ColNames);
            ColNames    = ColNames(FlagGoodCol);
            TS          = TS(:,FlagGoodCol);

            % rename columns:
            ColNames = regexprep(ColNames, 'Date\_+JDTT', 'JDTT');
            ColNames = regexprep(ColNames, 'Date\_\_.+', 'Date');
            ColNames = regexprep(ColNames, 'Date.+JDUT', 'JDUT');
            ColNames = regexprep(ColNames, 'Date.+JDTDB', 'JDTDB');
            ColNames = regexprep(ColNames, 'JDTDB.+', 'JDTDB');
            ColNames = regexprep(ColNames, 'JDUT.+', 'JDUT');
            ColNames = regexprep(ColNames, 'JDTT.+', 'JDTT');
            ColNames = regexprep(ColNames, 'R\.A\..+', 'RA');
            ColNames = regexprep(ColNames, 'DEC.+', 'Dec');
            ColNames = regexprep(ColNames, '/r', 'TrailLead');  % trail for evning sky... = 1
            ColNames = regexprep(ColNames, '-','');



            OutputTable = cell2table(TS, 'VariableNames',ColNames);
            % If TrailLead exist in output table:
            if any(strcmp(OutputTable.Properties.VariableNames, 'TrailLead'))
                OutputTable.TrailLead = strrep(OutputTable.TrailLead, '/T', '1');
                OutputTable.TrailLead = strrep(OutputTable.TrailLead, '/L', '0');
            end

            % attmpt to convert to numeric
            if Args.Convert2Numeric
                Ncol = numel(ColNames);
                for Icol=1:1:Ncol
                    Val = str2double(OutputTable.(ColNames{Icol}));
                    if ~any(isnan(Val))
                        OutputTable.(ColNames{Icol}) = Val;
                    end
                end
            end

            switch lower(Args.OutType)
                case 'table'
                    % do nonthing
                case 'orbitalel'
                    if strcmp(Args.EPHEM_TYPE, 'ELEMENTS')
                        if isempty(Args.OrbEl)
                            OrbEl = celestial.OrbitalEl;
                        else
                            OrbEl = Args.OrbEl;
                        end
                        if isempty(Args.LineInd)
                            LineInd = size(OrbEl.Eccen,1) + 1;
                        else
                            LineInd = Args.LineInd;
                        end

                        %error('not working yet');

                        OrbEl.A(LineInd,:)         = OutputTable.A(:).';
                        OrbEl.Eccen(LineInd,:)     = OutputTable.EC(:).';
                        OrbEl.PeriDist(LineInd,:)  = OutputTable.QR(:).';
                        OrbEl.W(LineInd,:)         = OutputTable.W(:).';
                        OrbEl.Node(LineInd,:)      = OutputTable.OM(:).';
                        OrbEl.Incl(LineInd,:)      = OutputTable.IN(:).';
                        OrbEl.Tp(LineInd,:)        = OutputTable.Tp(:).';  
                        OrbEl.Epoch(LineInd,:)     = OutputTable.JDTDB(:).';

                        CleanObject = strrep(Object, ';','');
                        if isnan(str2double(CleanObject))
                            % assume Designation is provided
                            OrbEl.Designation{LineInd,1} = CleanObject;
                        else
                            % assume number is provided
                            OrbEl.Number(LineInd,1) = str2double(CleanObject);
                        end
                        OutputTable     = OrbEl;
                    else
                        warning('OutType==OrbitalEl is possible only for EPHEM_TYPE==ELEMENTS - output will be table');
                    end

                otherwise
                    error('OutType option %s is not supported',Args.OutType);
            end

        end
    end
    
     
end

