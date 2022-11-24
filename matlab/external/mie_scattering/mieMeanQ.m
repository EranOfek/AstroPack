function Result = mieMeanQ(R, Fun_dNdR, Par_dNdR, Theta, N, Lambda, Rho)
    % Calculate mean mie scattering properties for paarticle size distribution
    % Example: R = logspace(log10(10),log10(1000),20);
    %          dNdR = R.^-4;
    %          Result = mieMeanQ(R, dNdR);
   
    arguments
        R         = logspace(log10(10),log10(1000),20);
        Fun_dNdR  = []; %@(R,Alpha,A) A.*R.^(-Alpha);  % or vector
        Par_dNdR  = {5, 1};  % Alpha, A
        Theta     = 58.1;
        N         = 1.7 + 0.3.*1i;
        Lambda    = 5000;
        Rho       = 1;
    end
    
    if isa(Fun_dNdR, 'function_handle')
        dNdR = Fun_dNdR(R, Par_dNdR{:});
    else
        dNdR = Fun_dNdR;
    end
    
    R  = R(:);
    Nr = numel(R);
    dNdR = dNdR(:);
    
    dR               = zeros(Nr,1);
    dN               = zeros(Nr,1); 
    MassInBin        = zeros(Nr,1);
    
    Result.S_sca            = zeros(Nr,1);
    Result.S_abs            = zeros(Nr,1);
    Result.S_ext            = zeros(Nr,1);
    Result.S_scaT           = zeros(Nr,1);
    Result.Q_sca            = zeros(Nr,1);
    Result.Q_abs            = zeros(Nr,1);
    Result.Q_ext            = zeros(Nr,1);
    Result.Q_scaT           = zeros(Nr,1);
    Result.MR2Qabs          = zeros(Nr,1);
    
    
    GeomSigma = pi.*R.^2;
    MassPerParticle = 4./3.*pi.*Rho.*R.^3;
    
    for Ir=1:1:Nr-1
        [S, C, Ang] = calcmie(R(Ir), N, 1, Lambda,180);

        S1=squeeze(abs(S(1,1,:))).^2;
        S2=squeeze(abs(S(2,2,:))).^2;
        a      = 2.*pi.*R(Ir)./Lambda; % size parameter (alpha)

        
        % Note that there is an error in Eq. 8 in REF 1 - should be ^-1 instead of
        % ^-2
        %Itheta = (2.*k.^2).^-1 .*(S1 + S2);
        Itheta = (2.*pi.*a.^2).^-1 .*(S1 + S2);
        % to calculate the angular Mie cross section
        Q_IthetaT = interp1(Ang(:),Itheta(:), Theta);   % [1/sr]
        S_IthetaT = Q_IthetaT.*pi.*R(Ir).^2;   % [area/sr]

        Result.MR2Qabs(Ir) = dNdR(Ir) .* S_IthetaT;  % pi r^2 Q r^-alpha - replacing: m Qabs pi r^2
        Result.S_sca(Ir)   = dNdR(Ir) .* C.sca;
        Result.S_abs(Ir)   = dNdR(Ir) .* C.abs;
        Result.S_ext(Ir)   = dNdR(Ir) .* C.ext;
        Result.S_scaT(Ir)  = dNdR(Ir) .* S_IthetaT;
        Result.Q_sca(Ir)   = dNdR(Ir) .* C.sca./GeomSigma(Ir);
        Result.Q_abs(Ir)   = dNdR(Ir) .* C.abs./GeomSigma(Ir);
        Result.Q_ext(Ir)   = dNdR(Ir) .* C.ext./GeomSigma(Ir);
        Result.Q_scaT(Ir)  = dNdR(Ir) .* Q_IthetaT;
        
                
        
        dR(Ir)              = R(Ir+1)-R(Ir);
        dN(Ir)              = dNdR(Ir).*dR(Ir);
        
        MassInBin(Ir) = MassPerParticle(Ir).*dNdR(Ir);

    end
    
    Result.R       = R;
    Result.Mp      = MassPerParticle;
    Result.dM      = MassInBin;
    Result.dN      = dN;
    Result.dR      = dR;
    Result.CumMass = cumsum(MassInBin);
    Result.IntM        = trapz(R, MassInBin);
    Result.IntS_aca    = trapz(R, Result.S_sca);
    Result.IntS_abs    = trapz(R, Result.S_abs);
    Result.IntS_ext    = trapz(R, Result.S_ext);
    Result.IntS_scaT   = trapz(R, Result.S_scaT);
    Result.IntQ_aca    = trapz(R, Result.Q_sca);
    Result.IntQ_abs    = trapz(R, Result.Q_abs);
    Result.IntQ_ext    = trapz(R, Result.Q_ext);
    Result.IntQ_scaT   = trapz(R, Result.Q_scaT);
    Result.IntMR2Qabs  = trapz(R, Result.MR2Qabs);
    
    Result.Int_dNdR    = trapz(R, dNdR);
    Result.Int_dNdR_r3 = trapz(R, dNdR.*R.^3);
    
end
