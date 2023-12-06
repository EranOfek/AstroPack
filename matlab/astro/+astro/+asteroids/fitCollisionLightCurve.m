function fitCollisionLightCurve
    %%

    load B.mat
    LC = B{1}(:,1:3);
    LC = LC( LC(:,1)>0,:);

    Ft = ~(LC(:,1)>4.2e5 & LC(:,1)<4.5e5);
    
    %Ft = LC(:,1)<86400;
    LC = LC(Ft,:);
    Ft = ~(LC(:,1)>1343 & LC(:,1)<10799);
    LC = LC(Ft,:);
    
    Time = LC(:,1);
    
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
    
    BinEye = [6.95e5 1e9;
            6.1e5  1.05e9;
            2.65e5 5.1e9;
            1.77e5 5.9e9;
            9.2e4  7.8e9;
            1.15e4 8.4e9;
            1300   9.3e9;
            600    1e10;
            300    1.15e10;
            200    1.2e10;
            150    1.28e10;
            100    1.55e10;
            85     1.82e10;
            61     1.87e10;
            50     2e10;
            40     2.17e10];
            
            
      TimeB = BinEye(:,1);
      Sobs = BinEye(:,2);
      
      Nt = numel(TimeB);
      
      
      DidymosArea = pi.*41000.^2;
      Aper5 = 8./206000 .*0.075.*constant.au;
            
      V = Aper5./TimeB;
      MeanV = 0.5.*(V(1:end-1) + V(2:end));
      MeanT = 0.5.*(TimeB(1:end-1) + TimeB(2:end));
      DiffS = diff(Sobs);
            
      MeanT = [Time(1);MeanT];
      MeanV = [V(1); MeanV];
      DiffS = [Sobs(1);DiffS];
      
      %for It=1:1:Nt
      %    [S_comb, S_tot, VecT] = astro.asteroids.collisionLightCurveKernel(DiffS(It), MeanT, VecT, MeanV(It), Aper5, F_g, Alpha);
      %end
      
      VecT       = logspace(0,6,1000).';
      
      F_g = 1;
      [S_comb, S_tot, VecT] = astro.asteroids.collisionLightCurveKernel(DiffS, VecT, VecT, MeanV, Aper5, F_g, Alpha);
      
      loglog(VecT, S_comb)
      hold on
      plot(Time, S_obs,'o')
      
    
end