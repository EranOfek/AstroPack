function [ResLC, ResG, ResSIm] = fit_astrometric_flux_UnevenlySpaced(t,F_t,x_t,y_t,sigma_F,sigma_x,Args)
    %
    %
    % Input  : - t : vector of times.
    %          - F_t: vector of total flux.
    %          - x_t: vector of x position.
    %          - y_t: vector of y position.
    %          - sigma_F: Error in flux.
    %          - sigma_x: Error in position.
    %          * Arbitrary number of pairs of ...,key,val,... arguments.
    %            The following keywords are available:
    %            'Solver' - Either @tools.math.fit.fminsearch_my | @tools.math.fit.fminunc_my
    %                       Default is @tools.math.fit.fminunc_my.
    %            'FitPar' - The list of parameters to fit.
    %                       [A0, A1, A2, x0, x1, x2, gamma]
    %                       If NaN, then will attempt to fit the parameter.
    %                       Default is 
    %                       [NaN NaN NaN  NaN NaN NaN  3]
    %            'DefPar' - The list of initial guess to use, or the parameter
    %                       value if not fitted. Default is
    %                       [2 1   0.66  0   1   -1   0   0   0    3]
    %            'Limits' - A two column matrix of [lower, upper] bounds on the
    %                       parameters. Default is
    %                       [0 5;0 2;0 2;  -1 1; -2.1 2.1; -2.1 2.1;    1.5 3.5]
    %            'VecInvTau' - A vector of 1/time_delay to attempt fitting.
    %                       Default is (1./100:0.5./100:1./10)
    %            'EndMatching' - Apply end-matching. Default is true.
    %            'Min_w'   - Minimum w. Default is 2.*pi./100.
    %            'Verbose' - Default is true.
    
    arguments
        t
        F_t
        x_t
        y_t
        sigma_F
        sigma_x
        Args.Step_t                = 1;
        Args.VecRot                = (-90:30:90).';
        Args.VecInvTau             = (1./100:0.5./100:1./10);
        Args.Solver                = @tools.math.fit.fminunc_my;
        Args.FitPar                = [NaN NaN NaN  NaN NaN NaN  3];
        Args.DefPar                = [2 1   0.66  0   1   -1   0   0   0    3];
        Args.Limits                = [0 5;0 2;0 2;  -1 1; -2.1 2.1; -2.1 2.1;    1.5 3.5];
        Args.EndMatching logical   = true;
        Args.Min_w                 = 2.*pi./100;
        Args.Verbose logical       = true;
        
        Args.Nsim                  = 100;
    end
    
    % make sute t start with 0
    t = t - min(t);
    
    % interpolate into an equally spaced time series
    [F_t, Newt] = timeSeries.interp.interpGP(t, F_t, Args.Step_t);
    [x_t, ~] = timeSeries.interp.interpGP(t, x_t, Newt);
    [y_t, ~] = timeSeries.interp.interpGP(t, y_t, Newt);
    
    Nrot = numel(Args.VecRot);
    TotTime = range(t);
    
    for Irot=1:1:Nrot
    
        RotMat = [cosd(Args.VecRot(Irot)), -sind(Args.VecRot(Irot)); sind(Args.VecRot(Irot)), cosd(Args.VecRot(Irot))];
        XY = RotMat*[x_t(:).'; y_t(:).'];
    
        [Res(Irot)]=timeSeries.timeDelay.fit_astrometric_flux(Newt,F_t,XY(1,:).',XY(2,:).',sigma_F,sigma_x, 'Solver',Args.Solver,...
                                                              'FitPar',Args.FitPar,...
                                                              'DefPar',Args.DefPar,...
                                                              'Limits',Args.Limits,...
                                                              'VecInvTau',Args.VecInvTau,...
                                                              'EndMatching',Args.EndMatching,...
                                                              'Min_w',Args.Min_w,...
                                                              'Verbose',Args.Verbose);
    end
    
    % select the best rotation
    % for the best rotation run simulations
    
    InPar.Cyclic = false;
    InPar.x0  = 0;
    InPar.y0  = 0;
    InPar.y   = [0.0  0.0];   
    InPar.f_dc = 50;
    InPar.DeltaT  = 0.1;
    InPar.StdMeanRange = [0.1 0.15];
    InPar.AliasFactor  = 10;
    InPar.EndMatching  = true;
    
    InPar.Tau = 30;
    InPar.A0  = 0;
    InPar.A   = [1 0.5];
    InPar.x   = [0.1 -0.4];
    InPar.Gamma = 2.0;
    InPar.TotTime = TotTime;
    InPar.sigma_x = sigma_x;
    
    InPar.sigma_F_rel = mean((sigma_F./F_t ))./sum(InPar.A); %0.02.*sum(InPar.A);    

    InPar.Slope   = 0;
    InPar.EndMatching = false;
    InPar.AliasFactor = 10;
        
    for Isim=1:1:Nsim
        [ResLC,ResG]=timeSeries.timeDelay.rand_lensed('A0',InPar.A0,'A',InPar.A,'Tau',InPar.Tau,...
                                    'x0',InPar.x0,'y0',InPar.y0,'x',InPar.x,'y',InPar.y,...
                                    'f_dc',InPar.f_dc,'Gamma',InPar.Gamma,...
                                    'TotTime',InPar.TotTime,...
                                    'DeltaT',InPar.DeltaT,...
                                    'sigma_x',InPar.sigma_x,...
                                    'sigma_F_rel',InPar.sigma_F_rel,...
                                    'Cyclic',InPar.Cyclic,...
                                    'Validate',true,...
                                    'Slope',InPar.Slope,...
                                    'StdMeanRange',InPar.StdMeanRange,...
                                    'AliasFactor',InPar.AliasFactor,...
                                    'EndMatching',InPar.EndMatching);
                                
       
        % interpolate
        Sim_F_t = interp1(ResLC.T, ResLC.F_t, Newt);
        Sim_x_t = interp1(ResLC.T, ResLC.x_t, Newt);
        Sim_y_t = zeros(size(Sim_x_t));
        
        [ResSim(Isim)]=timeSeries.timeDelay.fit_astrometric_flux(Newt,Sim_F_t,Sim_x_t,Sim_y_t,sigma_F,sigma_x, 'Solver',Args.Solver,...
                                                              'FitPar',Args.FitPar,...
                                                              'DefPar',Args.DefPar,...
                                                              'Limits',Args.Limits,...
                                                              'VecInvTau',Args.VecInvTau,...
                                                              'EndMatching',Args.EndMatching,...
                                                              'Min_w',Args.Min_w,...
                                                              'Verbose',Args.Verbose);
                                
    end
            
    
    

end
