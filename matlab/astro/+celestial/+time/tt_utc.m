function [TTmUTC, TTmUT1, UT1mTAI, UT1mUTC]=tt_utc(JD, Args)
    % Get TT-UTC and TT-UT1 time
    % Description: TT continues Terrestrial Dynamical Time (TDT or TD),
    %              which succeeded ephemeris time (ET). 
    %              \Delta{T} = TT - UT1
    %              TAI − GPS time = +19 seconds
    % Input  : - Vector of JDs.
    %            Default is now.
    %          * ...,key,val,...
    %            'WhereToGet' -
    %                   'get' - get the latest EOP data from the IERS website and
    %                           update the local version.
    %                   'use' - use local version of the EOP data (default).
    %            'FillVal' - Fill value if not available. Default is NaN. 
    %            'SourceFile' - Source file - options are:
    %                   '1992' - 1992 till now+3 month (default).
    %                   '1962' - 1962 till now.
    %            'NearFutureInterp' - A logical indicating if to perform
    %                   interpolation into the near future.
    %                   Default is true.
    %                   This should be used only if you need TT-UT1 in the
    %                   present or up to a few months into the future.
    % Output : - TT - UTC [s]
    %          - TT - UT1 [s] = \Delta{T}
    %          - UT1 - TAI [s]
    %          - UT1 - UTC [s]
    % Author : Eran Ofek (Sep 2021)
    % Example: [TTmUTC, TTmUT1, UT1mTAI, UT1mUTC]=celestial.time.tt_utc([0;2451545;celestial.time.julday+10]);

    arguments
        JD                             = celestial.time.julday;
        Args.WhereToGet                = 'use';   % 'use' | 'get'
        Args.FillVal                   = NaN;
        Args.SourceFile                = '1992';  % '1992' | '1962'
        Args.NearFutureInterp logical  = true;
    end
    TTmTAI = 32.184;   % TT = TAI + 32.184

    if Args.NearFutureInterp
        JDnow    = celestial.time.julday;
        JDnear   = JDnow - [30;60];
        AllJD    = [JDnear(:); JD(:)];
        [UT1mTAI, ~] = celestial.time.ut1_tai(AllJD, 'WhereToGet',Args.WhereToGet, 'FillVal',Args.FillVal);
        [UT1mUTC, ~] = celestial.time.ut1_utc(AllJD, 'WhereToGet',Args.WhereToGet, 'FillVal',Args.FillVal, 'SourceFile',Args.SourceFile);
        UT1mTAI =interp1(JDnear, UT1mTAI(1:2), JD, 'linear', 'extrap');
        UT1mUTC =interp1(JDnear, UT1mUTC(1:2), JD, 'linear', 'extrap');
        TTmUT1 = TTmTAI - UT1mTAI;
        TTmUTC = TTmUT1 + UT1mUTC;
        if (max(JD)-JDnow)>60
            warning('UT1-UTC interpolation is larger than 60 days');
        end
    else
        [UT1mTAI, ~] = celestial.time.ut1_tai(JD, 'WhereToGet',Args.WhereToGet, 'FillVal',Args.FillVal);
        [UT1mUTC, ~] = celestial.time.ut1_utc(JD, 'WhereToGet',Args.WhereToGet, 'FillVal',Args.FillVal, 'SourceFile',Args.SourceFile);
        TTmUT1 = TTmTAI - UT1mTAI;
        TTmUTC = TTmUT1 + UT1mUTC;
    end
    
        
    
    % OBSOLETE code (from Jun 2014)
    % Def.Read = 'use';
    % if (nargin==1)
    %    Read = Def.Read;
    % end
    % 
    % TAI_UTC = celestial.time.wget_tai_utc(Read);
    % Cat     = [[-Inf NaN NaN NaN]; TAI_UTC.Cat; [Inf NaN NaN NaN]];
    % Ind     = Util.array.assoc_range(JD,Cat(:,TAI_UTC.Col.JD));
    % 
    % TAImUTC = Cat(Ind,TAI_UTC.Col.DelTAU_UTC) + ...
    %           (JD - 2400000.5 - Cat(Ind,TAI_UTC.Col.T0)).*Cat(Ind,TAI_UTC.Col.S);
    % 
    % TTmUTC = 32.184 + TAImUTC;
end