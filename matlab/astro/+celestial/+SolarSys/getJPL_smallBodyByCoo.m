function [T,URL] = getJPL_smallBodyByCoo(RA, Dec, JD, Args)
    % Search for small bodies near position and time using JPL horizons
    %     See also imProc.match.match2SolarSystem
    % Input  : - Scalar J2000 RA. Sexagesimal string or number.
    %          - Scalar J2000 Dec. Sexagesimal string or number.
    %          - JD (scalar) or [D M Y H M S].
    %          * ...,key,val,... 
    %            'Kind' - kind of objects to search: 'a'-asteroid;
    %                   'c'-comet. Default is 'a'.
    %            'CooUnits' - Units of input coordinates.
    %                   Default is 'deg'.
    %            'ObsCode' - Observatory code.
    %                   Default is '097'
    %            'SearchRadius' - Search radius. Default is 1000.
    %            'SearchRadiusUnits' - Search radius units.
    %                   Default is 'arcsec'
    %            'Timeout' - webread timeout. Default is 120s.
    %            'Execute' - A logical indicating if to execute the search.
    %                   If false, then will just return the search URL.
    %                   Default is true.
    %            'ColNames' - Column names in out table.
    % Output : - A table of objects found near coordinates and time.
    %          - The URL for the request.
    % Author : Eran Ofek (2024 Nov) 
    % History: 2025-07-07 bug fix
    %          2025-07-07 change default obs code to 'M01'
    % Reference: https://ssd-api.jpl.nasa.gov/doc/sb_ident.html
    %            https://ssd.jpl.nasa.gov/tools/sb_ident.html#/
    % Example: T=celestial.SolarSys.getJPL_smallBodyByCoo(0,0)


    arguments
        RA(1,1)
        Dec(1,1)
        JD                     = [9 2 2021 0 0 0];
        Args.CooUnits          = 'deg';
        Args.ObsCode           = 'M01';  % LAST   %'097';  % Wise observatory
        Args.LimMag            = 22;
        Args.SearchRadius      = 1000;
        Args.SearchRadiusUnits = 'arcsec';
        Args.Timeout           = 120;  % [s]
        Args.Kind              = 'a';
        Args.Execute logical   = true;

        Args.ColNames          = {'Object','RA','Dec','DistRA','DistDec','Dist','Mag','RateRA','RateDec'};
    end

    if ischar(RA) || isstring(RA)
        RA_HMS = celestial.coo.convertdms(RA, 'SH', 'H');
    else
        RA     = convert.angular(Args.CooUnits, 'rad', RA);
        RA_HMS = celestial.coo.convertdms(RA, 'r', 'H');
    end
    StrRA = sprintf('%02d-%02d-%05.2f',RA_HMS);

    if ischar(Dec) || isstring(Dec)
        Dec_DMS = celestial.coo.convertdms(Dec, 'SD', 'D');
    else
        Dec     = convert.angular(Args.CooUnits, 'rad', Dec);
        Dec_DMS = celestial.coo.convertdms(Dec, 'r', 'D');
    end
    if Dec_DMS(1)<0
        DecSign = 'M';
    else
        DecSign = '';
    end
    StrDec = sprintf('%s%02d-%02d-%05.2f',DecSign, Dec_DMS(2:end));


    SearchRadius     = convert.angular(Args.SearchRadiusUnits,'deg', Args.SearchRadius);
    RA_HalfWidthDeg  = SearchRadius;
    Dec_HalfWidthDeg = SearchRadius;

    if numel(JD)==1
        Time = celestial.time.jd2date(JD,'H');
    else
        Time = JD;
    end
    TimeStr = sprintf('%04d-%02d-%02d_%02d:%02d:%02d', round(Time([3 2 1 4 5 6])));

    

    % https://ssd-api.jpl.nasa.gov/sb_ident.api?sb-kind=a&mpc-code=568&obs-time=2021-02-09_00:00:00&mag-required=true&two-pass=true&suppress-first-pass=true&req-elem=false&vmag-lim=20&fov-ra-lim=10-10-00%2C10-20-00&fov-dec-lim=10-00-00,10-30-00
    URL = sprintf('https://ssd-api.jpl.nasa.gov/sb_ident.api?sb-kind=%s&mpc-code=%s&obs-time=%s&mag-required=true&two-pass=true&suppress-first-pass=true&req-elem=false&vmag-lim=%4.1f&fov-ra-center=%s&fov-dec-center=%s&fov-ra-hwidth=%7.5f&fov-dec-hwidth=%7.5f',...
                Args.Kind, Args.ObsCode, TimeStr, Args.LimMag, StrRA, StrDec, RA_HalfWidthDeg, Dec_HalfWidthDeg);

    Cmd = sprintf('%s',(URL));
    WebOpt = weboptions('Timeout',Args.Timeout);

    if ~Args.Execute
        T = [];
    else
        [Output] = webread(Cmd, WebOpt);
    
        if isfield(Output, 'data_second_pass')
            Nsrc = numel(Output.data_second_pass);
            for Isrc=1:1:Nsrc
                %Output.data_secod_pass{Isrc}
                if Isrc==1
                    Cell = Output.data_second_pass{Isrc}.';
                else
                    Cell = [Cell; Output.data_second_pass{Isrc}.'];
                end
            end
            
            T = cell2table(Cell);
            if isempty(Args.ColNames)
                T.Properties.VariableNames = Output.fields_second.';
            else
                T.Properties.VariableNames = Args.ColNames;
                T.Dist     = str2double(T.Dist);
                T.DistRA   = str2double(T.DistRA);
                T.DistDec  = str2double(T.DistDec);
                T.Mag      = str2double(T.Mag);
                T.RateRA   = str2double(T.RateRA);
                T.RateDec  = str2double(T.RateDec);
                T.RA       = celestial.coo.convertdms(T.RA,'SH','d');
                Tmp=strrep(T.Dec, ' ', '');
                Tmp=strrep(Tmp, '''', ''); 
                Tmp=strrep(Tmp, '"', '');
                T.Dec = celestial.coo.convertdms(Tmp,'SDn','d');
                T.Object = string(T.Object);
            end
        else
            T = [];
        end
    end
    

end
