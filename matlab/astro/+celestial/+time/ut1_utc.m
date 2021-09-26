function [UT1mUTC, TblEOP] = ut1_utc(JD, Args)
    % Return UT1-UTC (DUT1) - read from IERS EOP file (1992 to present day)
    % Package: celestial.time
    % Description: Return UT1-UTC (also known as DUT1).
    % Input  : - Vector of Julian days (valid only after 1 1 1961).
    %          * ...,key,val,...
    %            'WhereToGet' -
    %                   'get' - get the latest EOP data from the IERS website and
    %                           update the local version.
    %                   'use' - use local version of the EOP data (default).
    %            'FillVal' - Fill value if not available. Default is NaN. 
    %            'SourceFile' - Source file - options are:
    %                   '1992' - 1992 till now+3 month (default).
    %                   '1962' - 1962 till now.
    % Output : - UT1-UTC [seconds].
    %          - EOP full table (see Installer/readIERS_EOP)
    % Aujthor : Eran Ofek (Sep 2021)
    % Example: [UT1mUTC,EOP]=celestial.time.ut1_utc(2451545);
    %          [UT1mUTC,EOP]=celestial.time.ut1_utc(2451545,'WhereToGet','get','FillVal',0);
    %          [UT1mUTC]=celestial.time.ut1_utc(2451545,'SourceFile','1962')
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        JD
        Args.WhereToGet        = 'use';
        Args.FillVal           = NaN;
        Args.SourceFile        = '1992';  % '1992' | '1962'
    end

    Ins = Installer;
    switch lower(Args.WhereToGet)
        case 'get'
            Ins.install('Time');
        case 'use'
            % do nothing
        otherwise
            error('Unknown WhereToGet option');
    end

    % load table
    switch Args.SourceFile
        case '1992'
            TblEOP = Ins.readIERS_EOP(1);
        case '1962'
            [~,TblEOP] = Ins.readIERS_EOP(2);
        otherwise
            error('Unknown SourceFile option (1962 | 1992)');
    end
    
    MJD    = convert.time(JD, 'JD', 'MJD');

    UT1mUTC = interp1(TblEOP.MJD, TblEOP.UT1_UTC, MJD, 'linear',NaN);

    UT1mUTC(isnan(UT1mUTC)) = Args.FillVal;

    % old code (Jun 2014) - doens't work any more
    % Def.Read = 'use';
    % if (nargin==1)
    %    Read = Def.WhereToGet;
    % end
    % InterpMethod = 'linear';
    % 
    % EOP = celestial.time.wget_eop(Read);
    % 
    % UT1mUTC = interp1(EOP.Cat(:,EOP.Col.MJD)+2400000.5,EOP.Cat(:,EOP.Col.UT1_UTC),JD,InterpMethod);

end