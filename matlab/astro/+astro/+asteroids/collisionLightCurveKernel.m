function [S_comb, S_tot, VecT] = collisionLightCurveKernel(S_ej, Time, VecT, V_ej, AperSize, F_g, Alpha, Args)
    %
    % Example: [S_comb, S_tot, VecT] = astro.asteroids.collisionLightCurveKernel
   
    arguments
        S_ej       = [0.1e10; 0.5e10; 0.4e10; 0.4e10; 0.4e10; 0.6e10; 0.6e10; 0.6e10]   % cm^2
        Time       = logspace(-1,6,1000).';  % time at which to interpolate the LC
        VecT       = logspace(-1,6,1000).';  % vec of time for full LC
        V_ej       = [10 30; 30 100; 100 300; 300 1000; 1000 3000; 3000 10000; 10000 30000; 140000 170000]; %[1.2e5, 1.6e5];  % [min max] velocity in bin [cm/s]
        AperSize   = 2.7e7;  % cm
        F_g        = 1;      % geometrical factor
        Alpha      = 1;
        Args.Plot logical   = false;
        Args.ObsS           = [];
    end
    
    VecT = VecT(:);
    Nt = numel(VecT);
    Nvel = size(V_ej, 1);
    
    % calculate the time scale in which the ejecta
    % transitions from optically thick to optically thin
    MeanV = sqrt(mean(V_ej.^2,2));
    T_rise = sqrt(F_g.*S_ej./(4.*pi.*(1+2.*Alpha).*MeanV.^2));
    I_rise = zeros(Nvel,1);
    
    MaxV = max(V_ej,[],2);
    MinV = min(V_ej,[],2);
    
    S_tot = zeros(Nt, Nvel);
    for Ivel=1:1:Nvel
        I_rise(Ivel) = find(VecT>T_rise(Ivel),1,'first');
    
    
        S_rise = VecT.^2 .* (1+2.*Alpha).*MeanV(Ivel).^2;
        S_rise = S_ej(Ivel).*S_rise./S_rise(I_rise(Ivel));
        S_rise(I_rise(Ivel)+1:end) = 0;
    
        % The time it takes the ejecta to get out of the aperture.
        T_outMin  = AperSize./MaxV(Ivel);
        I_outMin  = find(VecT>T_outMin,1,'first');
        T_outMax  = AperSize./MinV(Ivel);
        I_outMax  = find(VecT>T_outMax,1,'first');

        S_out     = nan(Nt,1);
        S_out(I_outMin) = S_ej(Ivel);
        S_out(I_outMax) = 0;
        if sum(~isnan(S_out))>=2
            S_out = tools.interp.interp1_nan(VecT, S_out);
        end
        S_out(isnan(S_out)) = 0;

        % plareau time
        S_plat = zeros(Nt,1);
        S_plat(I_rise(Ivel)+1:I_outMin-1) = S_ej(Ivel);
    
        S_tot(:,Ivel) = S_rise + S_plat + S_out;
    
        if Args.Plot
            plot(VecT, S_rise);
            hold on
            plot(VecT, S_out);
            plot(VecT, S_plat);
            plot(VecT, S_tot);
            set(gca,'XS','log','YS','log');
        end
    
    end
    
    Flag0 = S_ej(:).'<0;
    
    S_tot(:,Flag0) = 0;
    S_comb = sum(S_tot,2);
    
    S_comb = interp1(VecT, S_comb, Time);
    
    if ~isempty(Args.ObsS)
        Ratio  = Args.ObsS\S_comb;
        S_comb = S_comb.*Ratio;
    end
    
end