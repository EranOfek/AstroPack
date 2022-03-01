function Data=sunAzAlt(Date, Args)
    % Returm/print Sun local Az/Alt data
    % Example: celestial.SolarSys.sunAzAlt([1 3 2022],'FileName','NeoSmadarSunAlt.txt');
    
    arguments
        Date
        Args.Time       = 2;    % [days]
        Args.Lon        = 35.02;
        Args.Lat        = 32.04;
        Args.TimeStep   = 5./1440;
        Args.MinAlt     = 15;   % [deg]
        Args.TimeZone   = 2;    % [h]
        Args.FileName   = '';
    end
    RAD = 180./pi;
    
    JD0 = celestial.time.julday(Date);
    JD  = JD0 + (0:Args.TimeStep:Args.Time).';
    
    [RA,Dec] = celestial.SolarSys.suncoo(JD);
    
    LST=celestial.time.lst(JD,Args.Lon./RAD,'a').*2.*pi;  % [rad]
    [Az,Alt]=celestial.coo.hadec2azalt(LST-RA, Dec, Args.Lat./RAD);
    Az  = Az.*RAD;
    Alt = Alt.*RAD;
    
    Flag = Alt>Args.MinAlt;
    
    Data = [JD(Flag) + Args.TimeZone./24, Az(Flag), Alt(Flag)];
    
    
    
    if ~isempty(Args.FileName)
        FID = fopen(Args.FileName,'w');
        fprintf(FID,'%02d-%02d-%04d %02d:%02d:%04.1f   %7.2f %7.2f\n',[celestial.time.jd2date(Data(:,1),'h'), Data(:,2:3)].');
        fclose(FID);
    end
    
end