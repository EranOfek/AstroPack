function Result = mieMeanQ(R, Fun_dNdR, Par_dNdR, Theta, N, Lambda, Rho)
    % Calculate mean mie scattering properties for paarticle size distribution
    % Input  : - Vector of particle radius at which dN/dR are given.
    %          - Vector of dN/dR, or function Fun(R, args)
    %          - Cell array of argumnets for Fun. Default is {}.
    %          - Theta [deg] for sigma(Theta). Default is 58.1
    %          - Refractive index, Default is 1.7 + 0.3.*1i
    %          - Wavelength, or [wave, weight] of filter.
    %            Default is 5000.
    %          - Particle density. Default is 1.
    % Output : - Structure of integration results.
    % Author : Eran Ofek (Nov 2022)
    % Example: R = logspace(log10(10),log10(1000),20);
    %          dNdR = R.^-4;
    %          Result = mieMeanQ(R, dNdR);
   
    arguments
        R         = logspace(log10(10),log10(1000),20);
        Fun_dNdR  = []; %@(R,Alpha,A) A.*R.^(-Alpha);  % or vector
        Par_dNdR  = {5, 1};  % Alpha, A
        Theta     = 58.1;
        N         = 1.7 + 0.3.*1i;
        Lambda    = 5000;  % or filter
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
    Result.NR2Sabs          = zeros(Nr,1);
    Result.dSdOsT           = zeros(Nr,1);
    
    GeomSigma = pi.*R.^2;
    MassPerParticle = 4./3.*pi.*Rho.*R.^3;
    
    if numel(Lambda)==1
        Lambda = [Lambda, 1];
    end
    Nlambda  = size(Lambda, 1);
    SumWeight = sum(Lambda(:,2));
    
    for Ir=1:1:Nr-1
        V_Q_IthetaT = zeros(Nlambda, 1);
        V_S_IthetaT = zeros(Nlambda, 1);
        C_sca       = zeros(Nlambda, 1);
        C_abs       = zeros(Nlambda, 1);
        C_ext       = zeros(Nlambda, 1);
        
        for Il=1:1:Nlambda
            [S, C, Ang] = calcmie(R(Ir), N, 1, Lambda(Il,1),180);

            S1=squeeze(abs(S(1,1,:))).^2;
            S2=squeeze(abs(S(2,2,:))).^2;
            a      = 2.*pi.*R(Ir)./Lambda(Il,1); % size parameter (alpha)

        
            % Note that there is an error in Eq. 8 in REF 1 - should be ^-1 instead of
            % ^-2
            %Itheta = (2.*k.^2).^-1 .*(S1 + S2);
            Itheta = (2.*pi.*a.^2).^-1 .*(S1 + S2);
            % to calculate the angular Mie cross section
            V_Q_IthetaT(Il) = interp1(Ang(:),Itheta(:), Theta);   % [1/sr]
            V_S_IthetaT(Il) = V_Q_IthetaT(Il).*pi.*R(Ir).^2;   % [area/sr]
            C_sca(Il) = C.sca;
            C_abs(Il) = C.abs;
            C_ext(Il) = C.ext;
        end
        Q_IthetaT = sum(V_Q_IthetaT.*Lambda(:,2))./SumWeight;
        S_IthetaT = sum(V_S_IthetaT.*Lambda(:,2))./SumWeight;
        C.sca     = sum(C_sca.*Lambda(:,2))./SumWeight;
        C.abs     = sum(C_abs.*Lambda(:,2))./SumWeight;
        C.ext     = sum(C_ext.*Lambda(:,2))./SumWeight;
        
        dR(Ir)              = R(Ir+1)-R(Ir);
        dN(Ir)              = dNdR(Ir).*dR(Ir);
        
        MassInBin(Ir) = MassPerParticle(Ir).*dNdR(Ir);
        
        Result.MR2Qabs(Ir) = MassInBin(Ir) .* S_IthetaT;  % pi r^2 Q r^-alpha - replacing: m Qabs pi r^2
        Result.NR2Sabs(Ir) = dNdR(Ir).*S_IthetaT;
        
        Result.S_sca(Ir)   = dNdR(Ir) .* C.sca;
        Result.S_abs(Ir)   = dNdR(Ir) .* C.abs;
        Result.S_ext(Ir)   = dNdR(Ir) .* C.ext;
        Result.S_scaT(Ir)  = dNdR(Ir) .* S_IthetaT;
        Result.Q_sca(Ir)   = dNdR(Ir) .* C.sca./GeomSigma(Ir);
        Result.Q_abs(Ir)   = dNdR(Ir) .* C.abs./GeomSigma(Ir);
        Result.Q_ext(Ir)   = dNdR(Ir) .* C.ext./GeomSigma(Ir);
        Result.Q_scaT(Ir)  = dNdR(Ir) .* Q_IthetaT;
        
        Result.dSdOsT(Ir)  = dNdR(Ir) .* S_IthetaT./C.ext;
    end
    
    Result.R       = R;
    Result.Mp      = MassPerParticle;
    Result.dM      = MassInBin;
    Result.dN      = dN;
    Result.dR      = dR;
    Result.CumMass = cumsum(MassInBin);
    Result.IntM        = trapz(R, MassInBin);
    Result.IntN        = trapz(R, dNdR);
    Result.IntS_sca    = trapz(R, Result.S_sca);
    Result.IntS_abs    = trapz(R, Result.S_abs);
    Result.IntS_ext    = trapz(R, Result.S_ext);
    Result.IntS_scaT   = trapz(R, Result.S_scaT);
    Result.IntQ_aca    = trapz(R, Result.Q_sca);
    Result.IntQ_abs    = trapz(R, Result.Q_abs);
    Result.IntQ_ext    = trapz(R, Result.Q_ext);
    Result.IntQ_scaT   = trapz(R, Result.Q_scaT);
    Result.IntMR2Qabs  = trapz(R, Result.MR2Qabs);
    Result.IntNR2Sabs  = trapz(R, Result.NR2Sabs);
    
    Result.Int_dSdOsT  = trapz(R, Result.dSdOsT);
    
    
    Result.Int_dNdR    = trapz(R, dNdR);
    Result.Int_dNdR_r3 = trapz(R, dNdR.*R.^3);
    
end
