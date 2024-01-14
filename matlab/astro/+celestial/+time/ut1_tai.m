function [UT1mTAI, TblEOP] = ut1_tai(JD, Args)
    % Return the UT1-TAI time (available from 1846 till now).
    % Input  : - A vector of JD
    %          * ...,key,val,...
    %            'WhereToGet' -
    %                   'get' - get the latest EOP data from the IERS website and
    %                           update the local version.
    %                   'use' - use local version of the EOP data (default).
    %            'FillVal' - Fill value if not available. Default is NaN. 
    % Output : - UT1-TAI [seconds].
    %          - EOP full table (see Installer/readIERS_EOP)
    % Aujthor : Eran Ofek (Sep 2021)
    % Example: [UT1mTAI,EOP]=celestial.time.ut1_tai(2451545);
    %          [UT1mTAI,EOP]=celestial.time.ut1_tai(2451545,'get',0);
    
    arguments
        JD
        Args.WhereToGet        = 'use';
        Args.FillVal           = NaN;
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
    
    [~, ~, TblEOP] = Ins.readIERS_EOP(3);
    
    MJD    = convert.time(JD, 'JD', 'MJD');

    FNN = ~isnan(TblEOP.UT1_TAI);
    UT1mTAI = interp1(TblEOP.MJD(FNN), TblEOP.UT1_TAI(FNN), MJD, 'linear',NaN);

    UT1mTAI(isnan(UT1mTAI)) = Args.FillVal;
    
end