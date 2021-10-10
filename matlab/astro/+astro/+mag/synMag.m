function Mag = synMag(Spec, Trans, System, Args)
    % Calculate the synthetic magnitude of a spectrum and transmission
    %   in the Vega, AB, or STMAG systems.
    % Input  : - 
    % Example: Trans = [6000 0; 6001 1; 7000 1; 7001 0];
    %          W=(3000:1:1e4)'; [~,~,Fl] = astro.spec.black_body(5700,W);
    %          Spec  = [W, Fl.*(constant.SunR.^2)./(10.*constant.pc).^2];
    %          Mag = astro.mag.synMag(Spec, Trans)
    
    arguments
        Spec       % [Wave(Ang), f_lambda] 
        Trans      % [Wave(Ang), Trans(relative)]
        System        = 'Vega';   % 'Vega' | 'AB' | 'STMAG'
        Args.Method   = 'linear';
    end
   
    [Spec, Trans] = astro.spec.eq_sampling(Spec, Trans, [], Args.Method);
    Wave = Spec(:,1);
    Flux = Spec(:,2:end);
    
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
    
    Mag = -2.5.*log10(trapz(Flux .* Trans(:,2) .* Wave)./trapz(FlambdaSys(:,2) .* Trans(:,2) .* Wave)) + Msys;
    
    
end