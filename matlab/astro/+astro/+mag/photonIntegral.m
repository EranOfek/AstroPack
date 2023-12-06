function Nph = photonIntegral(Spec, Trans, Args)
    % Calculate the number of photons from a spectrum given transmission.
    % Input  : - Spectrum [Wave, F_lambda] in units of [Ang, erg/cm^2/s/A].
    %          - Transmission [Wave, T], in units of [Ang, cm^2]
    %          * ...,key,val,...
    %            'Method' 0 Interpolation method. Default is 'linear'.
    % Author : Eran Ofek (Oct 2021)
    % Example: Trans=[4000 0; 4001 1; 9100 1;9101 0];
    %          Spec = astro.mag.specVega;
    %          Nph = astro.mag.photonIntegral(Spec, Trans)
    
    arguments
        Spec                          % [Ang, erg/cm^2/s/A]
        Trans                         % [Ang, cm^2]
        Args.Method      = 'linear';
    end
    CmInAng = 1e-8;
    h = constant.h;
    c = constant.c;
    
        
    % equal sampling
    [Spec, Trans] = astro.spec.eq_sampling(Spec, Trans, [], Args.Method);

    % trapzoidial integartion
    Nph = trapz(Spec(:,2).*Trans(:,2).*Spec(:,1)).*CmInAng./(c.*h);
            
end
            