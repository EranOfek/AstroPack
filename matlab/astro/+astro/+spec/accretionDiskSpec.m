function [IntegratedSpec, Wave, VecR, T, Ibb] = accretionDiskSpec(Args)
    % Calculate simplified optically thick accreation disk spectrum.
    % Input  : * ...,key,val,... 
    %            'M' - Mass of accreator.
    %            'Mdot' - Accreation rate.
    %            'MassUnits' - Default is 'SunM'.
    %            'TimeUnits' - Default is 'yr'.
    %            'Rin' - Inner radius.
    %            'Rout' - Outer radius.
    %            'RUnits' - radius units. If 'rs', then use Schwarzwald radius
    %                   Default is 'rs'.
    %            'Nstep' - Number of radius steps.
    %            'Wave' - Vector of wavelength at which to calculate
    %                   spectra [Ang].
    %                   Default is (100:100:1e4).'.
    %
    % Output : - Integrated spectrum emittance [erg/sec/cm^2/cm(lambda)]
    %          - Vector of wavelength [Ang].
    %          - Vector of Radii.
    %          - Effectove temperature at each radius.
    %          - Spectrum for each radius (spectra in columns).
    % Author : Eran Ofek (2024 Jan) 
    % Example: [IntegratedSpec, Wave, VecR, T, Ibb] = astro.spec.accretionDiskSpec;

    arguments
        Args.M         = 1e7;
        Args.Mdot      = 0.1
        Args.MassUnits = 'SunM'
        Args.TimeUnits = 'yr';
        Args.Rin       = 10;  % Or vector of R
        Args.Rout      = 300;
        Args.RUnits    = 'rs';
        Args.Nstep     = 100;
        Args.Wave      = (100:100:1e4).';  % Ang        
    end

    G = constant.G;
    c = constant.c;
    Sig = constant.sigma;
    
    % convert all to cgs
    MassFactor   = convert.mass(Args.MassUnits, 'gr');
    TimeFactor   = convert.timeUnits(Args.TimeUnits, 's');
    
    
    M            = Args.M.*MassFactor;
    Mdot         = Args.Mdot.*MassFactor./TimeFactor;
    
    switch lower(Args.RUnits)
        case 'rs'
            % Sh. radius
            
            Rs   = 2.*G.*M./(c.^2);
            % Rin/out in units of Rs
            LenFactor = Rs;
        otherwise            
            LenFactor    = convert.length(Args.RUnits, 'cm');
    end
    
    Rin          = Args.Rin.*LenFactor;
    Rout         = Args.Rout.*LenFactor;
    
    Wave         = Args.Wave(:);
    
    % VecR is a row vector
    if numel(Rin)>1
        VecR = Rin(:).';
    else
        StepR = (Rout - Rin)./Args.Nstep;
        VecR = (Rin:StepR:Rout);
    end
    
    
    

    % temperature (T) as function of radius (R)
    % T is a row vector
    T       = (3.*G.*M.*Mdot./(8.*pi.*Sig.*VecR.^3)  .* (1 - sqrt(Rin./VecR)) ).^0.25;
    
    % Emittance [erg/sec/cm^2/cm(lambda)]
    % R in columns, Wave in rows
    [Ibb] = astro.spec.black_body(T, Wave);
    
    IntegratedSpec = sum(Ibb, 2);
end