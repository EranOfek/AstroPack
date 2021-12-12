function Res=fit_fluxBPL(t,F_t,sigma_F,varargin)
% Fit the power spectrum with broken power-law model
% FOR TESTING PURPOSES - DO NOT USE
% Example: Res=TimeDelay.fit_fluxBPL(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat)


InPar = inputParser;
addOptional(InPar,'Solver',@Util.fit.fminunc_my);
addOptional(InPar,'InputFT',false);  % input is t,F_t (false) or w,F_w (true)

addOptional(InPar,'FitPar',[NaN   NaN      1./1000        1]);  % [A1, gamma1 w_br gamma2]
addOptional(InPar,'DefPar',[1     2        1./1000        1]);  % [A1, gamma1 w_br gamma2]
addOptional(InPar,'Limits',[1e-5 1000;  1 4.5; 1./1000 2; 1 5]); %  without Tau
addOptional(InPar,'Min_w',2.*pi./100);
addOptional(InPar,'Verbose',false);
parse(InPar,varargin{:});
InPar = InPar.Results;

InPar.FitPar = [InPar.FitPar];
InPar.DefPar = [InPar.DefPar];

options = optimoptions(@fminunc,'Display','off','Diagnostic','off');
AddPars = {options};



if InPar.InputFT
    w   = t(:);
    F_w = F_t;  % assume F_w is already with ortho normalization
    N   = numel(w);
else
    N     = numel(F_t);
    Tstep = unique(diff(t));
    if numel(Tstep)>1 && range(Tstep)>(10000.*eps)
        error('fit_flux works on evenly spaced data - interpolate data');
    end

    freqs = TimeDelay.fft_freq(N, Tstep(1));
    w = 2.*pi*freqs;
    w = w(:);

    F_w = fft(F_t)./sqrt(N);
end



FitParH0  = [NaN, NaN, 1./1000, 1];
FitParH1  = InPar.FitPar;
ParH0     = InPar.DefPar(isnan(FitParH0));
ParH1     = InPar.DefPar(isnan(InPar.FitPar));


Limits    = InPar.Limits;

% Pars, FitPar, Limits, Min_w, w, F_w, sigma_F_hat
[Res.BestPar_H0,Res.LL_H0,Res.ExitFlag_H0]=InPar.Solver({@TimeDelay.logl_BPL, ...
                                         FitParH0, Limits, InPar.Min_w, w, F_w, ...
                                         sigma_F},...
                                        ParH0,AddPars{:});
                                    


    
[Res.BestPar_H1,Res.LL_H1,Res.ExitFlag_H1]=InPar.Solver({@TimeDelay.logl_BPL, ...
                                         FitParH1, Limits, InPar.Min_w, w, F_w, ...
                                         sigma_F},...
                                        ParH1,AddPars{:});
    
