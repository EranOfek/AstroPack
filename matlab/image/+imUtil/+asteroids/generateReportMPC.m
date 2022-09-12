function Msg = generateReportMPC(Table, Args)
    % Generate an MPC asteroids/comet report
    % Input  : - Table or matrix with the following columns:
    %            [JD, RA, Dec, Mag, Filter, AstIndex].
    %            Alternatively, for different formats, use the 'ColRA',
    %            'ColDec', 'ColJD', 'ColRA', 'ColDec', 'ColMag',
    %            'ColFilter', 'ColAstIndex', to specify the column index.
    %            ColAstIndex is a some integer uniquly indicating asteroids
    %            in the same batch.
    %          * ...,key,val,...
    %            'Filter' - If not empty, will overrid the filter in the
    %                   table. Default is 'C'.
    %            'CooUnits' - Coordinates units. Default is 'deg'.
    %            'IsComet' - A logical indicating if a comet.
    %                   Default is false.
    %            ...
    %            See code for more options
    % Output : - A string containing the message. Use fprintf to print it.
    % Author : Eran Ofek (Sep 2022)
    % Example: Table = [2451545.16, 100.1, -12.1, 17.12, NaN, 1; 2451545.16, 100.1, -12.1, 17.12, NaN, 1; 2451545.16, 100.1, -12.1, 17.12, NaN, 2]
    %          Msg = imUtil.asteroids.generateReportMPC(Table, 'Filter','C');
    
    arguments
        Table                % [JD, RA, Dec, Mag, Filter, AstIndex]
        Args.Filter           = 'C';
        Args.ColJD            = 1;
        Args.ColRA            = 2;
        Args.ColDec           = 3;
        Args.ColMag           = 4;
        Args.ColFilter        = 5;
        Args.ColAstIndex      = 6;
        Args.CooUnits         = 'deg';
        
        Args.IsComet logical  = false;
        Args.IsPrecision logical = true;
        Args.ObsCode          = 'XXX';
        Args.Contributer      = 'D. Polishook';
        Args.EMail            = 'david.polishook@weizmann.ac.il';
        Args.Observer         = 'LAST';
        Args.Measurer         = 'E. O. Ofek';
        Args.ObsLon           = 35.0407331;
        Args.ObsLat           = 30.0529838;
        Args.ObsAlt           = 415.4;
        Args.ObsAltUnits      = 'm';
        Args.ObsSys           = 'WGS84';
        Args.ObsName          = 'Large Array Survey Telescope (LAST) Node 01 Mount 01';  %'Weizmann Institute Observatory at Neot Smadar';
        Args.ObsAddress       = 'Weizmann Institute of Science, 234 Herzl St. Rehovot 76100, Israel';
        Args.Telescope        = '11-inch f/2.2 Schmidt + 9K x 6K CMOS';
        Args.RefCatalog       = 'GAIA-DRE3';
        
        Args.InstType         = 'B';  % B for CMOS; C for CCD   % https://minorplanetcenter.net/iau/info/OpticalObs.html
        Args.SkyCond          = '';    % https://minorplanetcenter.net/iau/info/ObsNote.html
        Args.DesigPrefix      = 'L';   % single letter
        
        
    end
    
    switch sign(Args.ObsLon)
        case 1
            SignLon = 'E';
        case -1
            SignLon = 'W';
        otherwise
            error('Error with sign of longitude');
    end
    switch sign(Args.ObsLat)
        case 1
            SignLat = 'N';
        case -1
            SignLat = 'S';
        otherwise
            error('Error with sign of latitude');
    end
    
    
    Position = sprintf('Lon. %12.7f %1s, Lat. %12.7f %1s, Alt. %7.1f %s, %s',abs(Args.ObsLon), SignLon, abs(Args.ObsLat), SignLat, Args.ObsAlt, Args.ObsAltUnits, Args.ObsSys); 
    
    Msg = sprintf('COD %s\n',Args.ObsCode);
    Msg = sprintf('%sCON %s\n',Msg, Args.Contributer);
    Msg = sprintf('%sCON %s\n',Msg, Args.EMail);
    Msg = sprintf('%sOBS %s\n',Msg, Args.Observer);
    Msg = sprintf('%sMEA %s\n',Msg, Args.Measurer);
    Msg = sprintf('%sCOM %s\n',Msg, Position);
    Msg = sprintf('%sCOM %s\n',Msg, Args.ObsName);
    Msg = sprintf('%sCOM %s\n',Msg, Args.ObsAddress);
    if Args.IsComet
        Msg = sprintf('%sCOM %s\n',Msg, 'Probable comet');
    end
    Msg = sprintf('%sTEL %s\n',Msg, Args.Telescope);
    Msg = sprintf('%sNET %s\n',Msg, Args.RefCatalog);
    
    % sort table by asteroid index and JD
    Table = sortrows(Table, [6 1]);
    
    LastAstIndex = Table(1,Args.ColAstIndex);
    N = size(Table(:,1));
    DesigCounter = 1;
    for I=1:1:N
        if Table(I,Args.ColAstIndex)==LastAstIndex
            % keep DesigCounter as is
        else
            DesigCounter = DesigCounter + 1;
        end
        LastAstIndex = Table(I,Args.ColAstIndex);
        
        
        Desig   = sprintf('%s%05d', Args.DesigPrefix, DesigCounter);
        Date    = celestial.time.jd2date(Table(I, Args.ColJD), 'f','YMD');
        Date(3) = Date(3) + Date(4);
        DateStr = sprintf('%04d %02d %08.5f',Date(1:3));
        
        RAdeg   = convert.angular(Args.CooUnits,'deg',Table(I, Args.ColRA));
        Decdeg  = convert.angular(Args.CooUnits,'deg',Table(I, Args.ColDec));
        RA      = celestial.coo.convertdms(RAdeg, 'd','SHb');
        Dec     = celestial.coo.convertdms(Decdeg, 'd','SDb');

        if ~Args.IsPrecision
            RA(end)  = ' ';
            Dec(end) = ' ';
        end
        
        Mag = Table(I, Args.ColMag);
        if isempty(Args.Filter)
            Filter = Table(I,Args.ColFilter);
        else
            Filter = Args.Filter;
        end
        Msg = sprintf('%s%5s%6s%3s%1s%s %s%s%9s%5.1f %1s%6s%3s\n',Msg, '', Desig, '', Args.InstType, DateStr, RA, Dec, '',Mag, Filter, '', Args.ObsCode);
    
    
end
