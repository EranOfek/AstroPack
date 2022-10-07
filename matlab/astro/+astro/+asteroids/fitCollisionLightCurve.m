function fitCollisionLightCurve
    %%

    load B.mat
    LC = B{1}(:,1:3);
    LC = LC( LC(:,1)>0,:);

    Ft = ~(LC(:,1)>4.2e5 & LC(:,1)<4.5e5);
    
    %Ft = LC(:,1)<86400;
    LC = LC(Ft,:);
    
    DidymosArea = pi.*41000.^2;
    
    S_obs = 10.^(0.4.*(14.6 - LC(:,2) )) -1 ;
    S_obs = S_obs.*DidymosArea;
    S_err = S_obs.*0.3;
    

    VecT       = logspace(-1,6,1000).';
    Time       = LC(:,1); 
    V_ej       = [0.01 0.1;
                  0.1 0.5;
                  0.5 0.77;
                  0.77 1.9;
                  1.19 1.85;
                  1.85 2.8;
                  2.8 4.4;
                  4.4 5.8;
                  6.8 10.6;
                  10 100;
                  100 300;
                  300 1000;
                  1000 1300;
                  1300 2000;
                  2000 5000];
    %V_ej = [2 6;
    %       1500 2700];
        
              
    V_ej       = V_ej.*100;  % cm/s
    AperSize   = 2.7e7;  % cm
    F_g        = 1;
    S_ej       = ones(15,1).*0.4;
    %S_ej       = ones(2,1).*3;
    S_ej(7)  = 2;
    S_ej(14) = 1;
    S_ej     = S_ej.*DidymosArea;
    
    Alpha = 1;
    %

    [S_comb, S_tot, VecT] = astro.asteroids.collisionLightCurveKernel(S_ej, Time, VecT, V_ej, AperSize, F_g);
    Fun = @astro.asteroids.collisionLightCurveKernel;
    
    Op = optimset('fminsearch');
    Op.MaxFunEvals = 1e6;
    Op.MaxIter     = 1e6;
    Op.TolX        = 0.01;
    Op.TolFun      = 0.01;
    
    [BestS,Chi2,E,O] = tools.math.fit.fminsearch_chi2(Time,S_obs,S_err,{Fun, VecT, V_ej, AperSize, F_g, Alpha, 'ObsS',S_obs},[S_ej], Op);
    BestS(BestS<0) = 0;
    
    [S_comb, S_tot, VecT] = astro.asteroids.collisionLightCurveKernel(BestS, Time, VecT, V_ej, AperSize, F_g, Alpha, 'ObsS',S_obs);
    
    
    loglog(Time,S_obs,'o')
    hold on
    loglog(Time,S_comb)   

    %%
    
end