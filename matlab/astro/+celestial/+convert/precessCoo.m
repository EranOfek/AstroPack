function [OutRA, OutDec] = precessCoo(InRA, InDec, CD3, Args)
    % Precess coordinates
    %     Allow to precess coordinates from and to some mean/true equinox
    %     of date.
    % Input  : - Array of R.A. If char, string or cell this is treated as
    %            an array of sexagesimal coordinates.
    %            Alternatively cosine dir 1.
    %          - Array of Dec (like R.A.).
    %            Alternatively cosine dir 1.
    %          - cosine dir 3. Default is [].
    %          * ...,key,val,... 
    %            'InEquinox' - date of input equinox.
    %                   Default is 2451545.5 (i.e., J2000).
    %            'InMean' - A logical indicating if input coordinates are
    %                   refered to mean equinox of date (true), or true
    %                   equinox of date (false).
    %                   Default is true.
    %            'InType' - Type of input date (e.g., 'J'-julian years, 'JD', 'MJD').
    %                   Default is 'JD'.
    %            'OutEquinox' - date of output equinox.
    %                   Default is celestial.time.julday (i.e., now).
    %            'OutMean' - A logical indicating if output coordinates are
    %                   refered to mean equinox of date (true), or true
    %                   equinox of date (false).
    %                   Default is false.
    %            'OutType' - Type of output date (e.g., 'J'-julian years, 'JD', 'MJD').
    %                   Default is 'JD'.
    %            'InUnits' - Input units ('rad'|'deg'). Default is 'deg'.
    %                   Note that if 3rd input arg. is not empty, then the
    %                   input is in cosine direction.
    %            'OutUnits' - Output units ('rad'|'deg'). Default is 'deg'.
    % Output : - An array of output R.A.
    %          - An array of output Dec.
    % Author : Eran Ofek (2024 Jan) 
    % Example: % J2000.0 to true equinox of today date [deg]
    %          [OutRA, OutDec] = celestial.convert.precessCoo(180,-20);
    %          % J2000.0 to mean equinox of 2050 [input: sex, output: deg]
    %          [OutRA, OutDec] = celestial.convert.precessCoo('12:00:00','-20:00:00','OutMean',true);

    arguments
        InRA
        InDec
        CD3                          = [];
        Args.InEquinox               = 2451545.5;
        Args.InMean logical          = true;
        Args.InType                  = 'JD';
        Args.OutEquinox              = celestial.time.julday;
        Args.OutMean logical         = false;
        Args.OutType                 = 'JD';
        Args.InUnits                 = 'deg';
        Args.OutUnits                = 'deg';
    end

    if ischar(InRA) || isstring(InRA) || iscell(InRA)
        Args.InUnits = 'sex';
    end
    
    if strcmp(Args.InUnits, 'sex')
        InRA  = celestial.coo.convertdms(InRA, 'gH','r');
        InDec = celestial.coo.convertdms(InDec, 'gD','r');
        
        Args.InUnits = 'rad';
    end
    
    % Convert coordinates to radians:
    SizeIn = size(InRA);
    if isempty(CD3)
        AngConv = convert.angular(Args.InUnits, 'rad');
        InRA    = AngConv .* InRA;
        InDec   = AngConv .* InDec;

        % convert to cosine direction
        [CD1,CD2,CD3]=celestial.coo.coo2cosined(InRA(:).', InDec(:).');
    else
        % 'cosined'
            
        CD1 = InRA(:).';
        CD2 = InDec(:).';
        %CD3 = CD3;
    end
        
    InEquinox  = convert.time(Args.InEquinox, Args.InType, 'JD');
    OutEquinox = convert.time(Args.OutEquinox, Args.OutType, 'JD');
    
    % precess to mean J2000.0
    if Args.InMean
        % mean equinox of date to mean J2000.0
        InMean = 'p';
    else
        % true equinox of date to mean J2000.0
        InMean = 'pd';
    end
    
    RotM1 = celestial.coo.rotm_coo(InMean, InEquinox);
    
    % precess from J2000.0
    % precess to mean J2000.0
    if Args.OutMean
        % Mean J2000.0 to mean equinox of date
        OutMean = 'P';
    else
        % Mean J2000.0 to true equinox of date
        OutMean = 'Pd';
    end
    
    RotM2 = celestial.coo.rotm_coo(OutMean, OutEquinox);
    
    
    % precesses
    OutCD = RotM2 * RotM1 * [CD1; CD2; CD3];
    
    [OutRA,OutDec] = celestial.coo.cosined2coo(OutCD(1,:), OutCD(2,:), OutCD(3,:));
    
    % convert to OutUnits
    AngConv = convert.angular('rad', Args.OutUnits);
    OutRA    = AngConv .* OutRA;
    OutDec   = AngConv .* OutDec;
        
    % reshape
    OutRA  = reshape(OutRA, SizeIn);
    OutDec = reshape(OutDec, SizeIn);
    
end
