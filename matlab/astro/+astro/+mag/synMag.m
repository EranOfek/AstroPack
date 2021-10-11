function Mag = synMag(Spec, Trans, System, Args)
    % Calculate the synthetic magnitude of a spectrum and transmission
    %   in the Vega, AB, or STMAG systems.
    % Input  : - Spectra: [Wave(Ang), Flux, Flux,...], where flux in
    %            erg/cm^2/s/A.
    %          - Transmission [Wave(Ang), RelTran]
    %          - Mag sys: ['AB'] | 'Vega' | 'STMAG'
    % Output : - Magnitude per column of flux.
    % Author : Eran Ofek (Oct 2021)
    % Example: Trans = [6000 0; 6001 1; 7000 1; 7001 0];
    %          W=(3000:1:1e4)'; [~,~,Fl] = astro.spec.black_body(5700,W);
    %          Spec  = [W, Fl.*(constant.SunR.^2)./(10.*constant.pc).^2];
    %          Mag = astro.mag.synMag(Spec, Trans)
    %          Mag = astro.mag.synMag([Spec, Spec(:,2)], Trans)
    %          Mag = astro.mag.synMag([Spec, Spec(:,2)], Trans, 'AB')
    %          Mag = astro.mag.synMag([Spec, Spec(:,2)], Trans, 'STMAG')
    
    arguments
        Spec       % [Wave(Ang), f_lambda] 
        Trans      % [Wave(Ang), Trans(relative)]
        System        = 'AB';   % 'Vega' | 'AB' | 'STMAG'
        Args.Method   = 'linear';
    end
   
    Wave = astro.spec.eqSampling(Spec(:,1), Trans(:,1));
    
    % interpolate - Spec/Trans now contains only the spectrum
    Flux  = interp1(Spec(:,1), Spec(:,2:end), Wave, Args.Method);
    Trans = interp1(Trans(:,1), Trans(:,2), Wave, Args.Method);
    
    %[Spec, Trans] = astro.spec.eq_sampling(Spec, Trans, [], Args.Method);
    %Wave = Spec(:,1);
    %Flux = Spec(:,2:end);
    
    switch lower(System)
        case 'vega'
            Msys = astro.mag.magVega;
            FlambdaSys = astro.mag.specVega;
            FlambdaSys = [Wave, interp1(FlambdaSys(:,1), FlambdaSys(:,2), Wave, Args.Method)];
        case 'ab'
            Msys = 0;
            AB0  = 10.^(-0.4.*48.6).*1e23;  % AB zero point in Jy
            FlambdaSys = [Wave, convert.flux(AB0.*1000, 'mJy', 'cgs/A', Wave, 'A')];
        case 'stmag'
            Msys = 0;
            STMAG0     = 10.^(-0.4.*21.10);
            FlambdaSys = [Wave, STMAG0 + zeros(size(Wave))];
        otherwise
            error('Unknown System option');
    end
    
    Mag = -2.5.*log10(trapz(Wave, Flux .* Trans .* Wave)./trapz(Wave, FlambdaSys(:,2) .* Trans .* Wave)) + Msys;
    
end