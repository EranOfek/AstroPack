function Mag = spec2ABmag(Spec, Tran, Args)
    % Convert spectrum + transmission to AB magnitude
    % Input  : - [Wave, Flambda], or [Nu, Fnu] spectrum.
    %          - [Wave, Tr], or [Nu, Tr] transmission.
    %          * ...,key,val,...
    %            'SpecWaveUnits' - Default is 'A'.
    %            'SpecFluxUnits' - 'cgs/A'|'cgs/Hz'. Default is 'cgs/A'
    %            'TranWaveUnits' - Default is 'A'.
    %            'InterpMethod' - Default is linear'.
    %            'InterpSpec' - Interp spec to transmission (true), or vice
    %                   verse. Defalt is true.
    % Example: W=(4000:10:9000)'; [~,Emit]=astro.spec.black_body(5700,W);
    %          F = AstFilter.get('SDSS','r');
    %          astro.mag.spec2ABmag([W, Emit], F, 'SpecWaveUnits','A','SpecFluxUnits','cgs/hz');
    %          astro.mag.spec2ABmag([W, Emit.*constant.SunR.^2./(100.*constant.pc.^2)], F, 'SpecWaveUnits','A','SpecFluxUnits','cgs/hz')

    
    arguments
        Spec
        Tran
        Args.SpecWaveUnits       = 'A';
        Args.SpecFluxUnits       = 'csg/A';   % 'cgs/A'|'cgs/Hz'
        Args.TranWaveUnits       = 'A';
        
        Args.InterpMethod        = 'linear';
        Args.InterpSpec logical  = true;
    end
    
    if isa(Tran, 'AstFilter')
        Transmission = Tran.nT;
    else
        Transmission = Tran;
    end
    
    % convert transmission to frequency
    TranNu  = convert.energy(Args.SpecWaveUnits, 'Hz', Transmission(:,1));
    TranTr  = Transmission(:,2);
    
    % convert spectra to [Freq, Fnu]
    switch lower(Args.SpecFluxUnits)
        case 'cgs/a'
            if strcmp(Args.SpecWaveUnits,'A')
                Fnu = fLambda2fNu(Spec(:,1), Spec(:,2), 'A');
            else
                error('SpecFluxUnits = cgs/a and SpecWaveUnits not A - not supported');
            end
        case 'cgs/hz'
            Fnu = Spec(:,2);
        otherwise
            error('Unknown SpecFluxUnits option');
    end
            
    SpecNu = convert.energy(Args.SpecWaveUnits, 'Hz', Spec(:,1));
    
    if Args.InterpSpec
        % interp spectrum on filter 
        Fnu    = interp1(SpecNu, Fnu, TranNu, Args.InterpMethod);
        SpecNu = TranNu;
    else
        % interp filter on spectrum
        TranTr = interp1(TranNu, TranTr, SpecNu, Args.InterpMethod);
        TranNu = SpecNu;
    end
    
    
    [MeanF,ErrMeanF] = astro.mag.meanPhotonWeightedFlux(SpecNu, Fnu, TranTr, 'Dim',1,...
                                                        'SpecificUnit','freq',...
                                                        'ErrF',[]);
        
    
    Mag = -2.5.*log10(MeanF) - 48.6;
    
end