function readOccIOTA(URL, Args)
    %
    % https://www.asteroidoccultation.com/
    % https://www.asteroidoccultation.com/2022_12/1220_71_78282_Summary.txt


    arguments
        URL          = 'https://www.asteroidoccultation.com/2022_12/1220_71_78282_Summary.txt';
        Args.strfind = [];
        Args.match   = [];
        Args.input   = 'url';
        Args.base    = [];
        Args.User    = '';
        Args.Pass    = '';
        Args.Method  = 'webread';
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

    IndStart = strfind(Str, 'Latitude   Longitude  Latitude');
    IndStart = max(IndStart);

    IndEnd = strfind(Str, 'Uncertainty in time');

    TableData = Str(IndStart+31:IndEnd);

    Lines = regexp(TableData,'\n','split');

    Nl = numel(Lines);

    VecStart = [4 16; 58 69; 80 91; 102 113; 124 135];
    PropName = {'Lon','Lat';'LonLim1','LatLim1';'LonLim2','LatLim2';'LonErrLim1','LatErrLim1';'LonErrLim2','LatErrLim2'};
    Nvs      = size(VecStart,1);
    for Il=1:1:Nl
        
        for Ivs=1:1:Nvs
            Istart = VecStart(Ivs,1);
            if strcmp(Lines{Il}(Istart),'-')
                Sign = -1;
            else
                Sign = +1;
            end
            Path(Il).(PropName{Ivs,1})      = [Sign str2double(Lines{Il}(Istart+1:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];

            Istart = VecStart(Ivs,2);
            Path(Il).Lat      = [Sign str2double(Lines{Il}(Istart+1:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
            if strcmp(Lines{Il}(Istart),'-')
                Sign = -1;
            else
                Sign = +1;
            end
            Path(Il).(PropName{Ivs,2})      = [Sign str2double(Lines{Il}(Istart+1:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
        end


        Path(Il).Lon     = [str2double(Lines{Il}(4:7)), str2double(Lines{Il}(9:10)), str2double(Lines{Il}(12:13))];
        Path(Il).Lat     = [str2double(Lines{Il}(16:19)), str2double(Lines{Il}(21:22)), str2double(Lines{Il}(24:25))];
        Path(Il).Time    = [str2double(Lines{Il}(29:30)), str2double(Lines{Il}(32:33)), str2double(Lines{Il}(35:38))];

        Path(Il).StarAlt = str2doubel(Lines{Il}(42:45));
        Path(Il).StarAz  = str2doubel(Lines{Il}(47:50));
        Path(Il).SunAlt  = str2doubel(Lines{Il}(54:56));

        Istart = 58;
        Path(Il).LonLim1 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
        Istart = 69;
        Path(Il).LatLim1 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];

        Istart = 80;
        Path(Il).LonLim2 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
        Istart = 91;
        Path(Il).LatLim2 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];

        Istart = 102;
        Path(Il).LonErrLim1 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
        Istart = 113;
        Path(Il).LatErrLim1 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];

        Istart = 124;
        Path(Il).LonErrLim2 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];
        Istart = 135;
        Path(Il).LatErrLim2 = [str2double(Lines{Il}(Istart:Istart+3)), str2double(Lines{Il}(Istart+5:Istart+6)), str2double(Lines{Il}(Istart+8:Istart+9))];



    %  123456789 123456789 123456789 123456789 123456789 123456789 23456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 12
    % '   -103 18 31    31 32 23    9 22 26.7    10   312   -56 -101 45 44   30  4 18 -104 37 26   32 46 53 -100 39 37   29  1 20 -105 19 51   33 26 44   1.61'

    Str

    end

end