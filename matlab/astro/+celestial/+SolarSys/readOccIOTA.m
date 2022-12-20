function [PathD, InLim, InErr, Path]=readOccIOTA(URL, Args)
    % Read IOTA we-page asteroid occultation path into matlab
    % Input  : - URL page.
    %            E.g., https://www.asteroidoccultation.com/2022_12/1220_71_78282_Summary.txt
    %          * ...,key,val,...
    %            see code.
    % Output : - Structure array with occultation path.
    %            Lon and Lat in deg.
    % Reference: https://www.asteroidoccultation.com/
    % Author : Eran Ofek (Dec 2022)
    % Example: [PD,P]=celestial.SolarSys.readOccIOTA;
    %          L=www.find_urls('https://www.asteroidoccultation.com/','match','\d\d\d\d_\d\d');
    %          L1=strrep(L,'.htm','_Summary.txt');
    %          for I=1:1:numel(L1), [Path,InLim,InErr]=celestial.SolarSys.readOccIOTA(L1{I}); if InErr, Found(I)=true; end, end

    arguments
        URL          = 'https://www.asteroidoccultation.com/2022_12/1220_71_78282_Summary.txt';
        Args.strfind = [];
        Args.match   = [];
        Args.input   = 'url';
        Args.base    = [];
        Args.User    = '';
        Args.Pass    = '';
        Args.Method  = 'webread';
        Args.Plot    = false;
        
        Args.GeoPos  = [35 30];
    end

    Pos(1).Pos = [4 7];
    Pos(2).Pos = [9 10];

    switch lower(Args.input)
        case 'url'
            switch lower(Args.Method)
                case 'webread'
                    Options = weboptions('Username',Args.User,'Password',Args.Pass,'Timeout',15);
                    Str = webread(URL,Options);
                case 'urlread'
                    Str = urlread(URL);
                otherwise
                    error('Unknown Method option');
            end
        case 'file'
            Str = io.files.file2str(URL,'str');
        otherwise
            error('Unknown input option');
    end
    Lines = regexp(Str,'\n','split');
    Is = find(~tools.cell.isempty_cell(regexp(Lines,'Occultation of','match')));
    Ie = find(~tools.cell.isempty_cell(regexp(Lines,'Uncertainty in time','match')));
    Lines = Lines(Is+5:Ie-2);
    
    %IndStart = strfind(Str, 'Latitude   Longitude  Latitude');
    %IndStart = max(IndStart);
    %IndEnd = strfind(Str, 'Uncertainty in time');
    %TableData = Str(IndStart+31:IndEnd);
    %Lines = regexp(TableData,'\n','split');

    %error('doesnt work');
    Tmp = regexp(Str, '1 sigma uncertainty ellipse (major, minor, PA): (?<Err>\d\.\d\d\d)','names');
    Tmp = regexp(Str, 'approx. diameter [km]: (?<Diam>\d+)','names');
    
    Nl = numel(Lines);

    VecStart = [4 16];%; 58 69; 80 91; 102 113; 124 135];
    PropName = {'Lon','Lat'}; %'LonLim1','LatLim1';'LonLim2','LatLim2';'LonErrLim1','LatErrLim1';'LonErrLim2','LatErrLim2'};
    Nvs      = size(VecStart,1);
    K = 0;
    for Il=1:1:Nl
        Il
        if numel(Lines{Il})>100
            K = K + 1;
            for Ivs=1:1:Nvs
                Istart = VecStart(Ivs,1);
                if strcmp(Lines{Il}(Istart),'-')
                    Sign = -1;
                else
                    Sign = +1;
                end
                Path(K).(PropName{Ivs,1})      = [Sign str2double(Lines{Il}(Istart+1:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
                if ~any(isnan(Path(K).(PropName{Ivs,1})))
                    SDMS = Path(K).(PropName{Ivs,1});
                    PathD(K).(PropName{Ivs,1})     = SDMS(1).*(SDMS(2) + SDMS(3)./60 + SDMS(4)./3600);
                else
                    PathD(K).(PropName{Ivs,1})     = NaN;
                end
                Istart = VecStart(Ivs,2);
                if strcmp(Lines{Il}(Istart),'-')
                    Sign = -1;
                else
                    Sign = +1;
                end
                Path(K).(PropName{Ivs,2})      = [Sign str2double(Lines{Il}(Istart+1:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
                if ~any(isnan(Path(K).(PropName{Ivs,2})))
                    SDMS = Path(K).(PropName{Ivs,2});
                    PathD(K).(PropName{Ivs,2})     = SDMS(1).*(SDMS(2) + SDMS(3)./60 + SDMS(4)./3600);
                else
                    PathD(K).(PropName{Ivs,2})     = NaN;
                end
            end

            Path(K).Time    = [str2double(Lines{Il}(29:30)), str2double(Lines{Il}(32:33)), str2double(Lines{Il}(35:38))];

            Path(K).StarAlt = str2double(Lines{Il}(42:45));
            Path(K).StarAz  = str2double(Lines{Il}(47:50));
            Path(K).SunAlt  = str2double(Lines{Il}(54:56));

            %Istart = 58;
            %Path(Il).LonLim1 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
            %Istart = 69;
            %Path(Il).LatLim1 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];

            %  123456789 123456789 123456789 123456789 123456789 123456789 23456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 12
            % '   -103 18 31    31 32 23    9 22 26.7    10   312   -56 -101 45 44   30  4 18 -104 37 26   32 46 53 -100 39 37   29  1 20 -105 19 51   33 26 44   1.61'

            %Str
        end

    end
    
        
    if Args.Plot
        load coastlines.mat
        plot(coastlon,coastlat)
        hold on
        plot([PathD.Lon], [PathD.Lat],'-')
        plot([PathD.LonLim1], [PathD.LatLim1],'--')
        plot([PathD.LonLim2], [PathD.LatLim2],'--')
        plot([PathD.LonErrLim1], [PathD.LatErrLim1],'--')
        plot([PathD.LonErrLim2], [PathD.LatErrLim2],'--')

    end
    
    if nargout>1
        LonOut = [[PathD.LonErrLim1]'; flipud([PathD.LonErrLim2]')];
        LatOut = [[PathD.LatErrLim1]'; flipud([PathD.LatErrLim2]')];
        
        InErr = inpolygon(Args.GeoPos(:,1),Args.GeoPos(:,2),LonOut, LatOut);
        
        LonOut = [[PathD.LonLim1]'; flipud([PathD.LonLim2]')];
        LatOut = [[PathD.LatLim1]'; flipud([PathD.LatLim2]')];
        
        InLim = inpolygon(Args.GeoPos(:,1),Args.GeoPos(:,2),LonOut, LatOut);
        
    end
end