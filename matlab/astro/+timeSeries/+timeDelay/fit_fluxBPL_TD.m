function fit_fluxBPL_TD
    %
   
    
    arguments
        Args.Solver     = @tools.math.fit.fminunc_my;
        Args.FitPar     = [NaN   NaN      1./1000        1];  % [A1, gamma1 w_br gamma2]
        Args.DefPar     = [1     2        1./1000        1];  % [A1, gamma1 w_br gamma2]
        Args.Limits     = [1e-5 1000;  1 4.5; 1./1000 2; 1 5]; %  without Tau
        Args.Min_w      = 2.*pi./100;
        Args.Verbose    = false;
    end
    
    
    

    FitParH0  = [NaN, NaN, 1./1000, 1];
    FitParH1  = InPar.FitPar;
    ParH0     = InPar.DefPar(isnan(FitParH0));
    ParH1     = InPar.DefPar(isnan(InPar.FitPar));


    Limits    = InPar.Limits;

    % Pars, FitPar, Limits, Min_w, w, F_w, sigma_F_hat
    [Res.BestPar_H0,Res.LL_H0,Res.ExitFlag_H0]=InPar.Solver({@timeSeries.timeDelay.logl_BPL, ...
                                             FitParH0, Limits, InPar.Min_w, w, F_w, ...
                                             sigma_F},...
                                            ParH0,AddPars{:});




    [Res.BestPar_H1,Res.LL_H1,Res.ExitFlag_H1]=InPar.Solver({@timeSeries.timeDelay.logl_BPL, ...
                                             FitParH1, Limits, InPar.Min_w, w, F_w, ...
                                             sigma_F},...
                                            ParH1,AddPars{:});

    
    
end
