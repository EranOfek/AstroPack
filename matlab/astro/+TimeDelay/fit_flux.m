function Res=fit_flux(t,F_t,sigma_F,varargin)
% Fit the 2 images time delay using the flux-only method
% Package: +TimeDelay
% Description: Fit a combined light curve using the flux-only method.
%              Can work with both time domain and frequency domain methods.
% Input  : - Vector of times.
%          - Vector of combined fluxes F(t).
%          - scalar/vector of errors in F(t).
%          * Arbitrary number of ...,key,val,...
%            'Solver' - Default is @Util.fit.fminunc_my (Requires MAAT)
%            'TimeDomain' - Method to use. Default is false.
%            'InputFT' - If true, then, t, F_t are in frequency domain -
%                       i.e., w, F_w.
%            'FitPar'  - Vector of [A1, A2, gamma]. Values which are NaN
%                       will be fitted. Default is [NaN NaN 2].
%            'DefPar' - Defualt parameters for [A1, A2, gamma].
%                       Default is [1 0.5 2].
%            'Limits' - A two column matrix of lower/upper limits on the
%                       parameters. Default is [1e-5 1000;0 1000; 1.5 3.5].
%            'VecInvTau' - Vector of 1/tau to fit.
%                       Default is (1./100:0.5./240:1./10).
%            'Min_w' - Reject frequency [below above] this frequency.
%                       Default is [2.*pi./100 Inf].
%            'w0'    - w0 for lower bound of cross-correlation integration.
%                       If empty, use the 'infinitity removal' method.
%                       Default is [].
%            'Verbose' - Default is false.
%            'AbsTol' - Abs. tolerance for integral soltion.
%                   Default is 1e-2.
%            'RelTol' - Rel. tolerance for integral solution.
%                   Default is 1e-6.
% Output : A structure with the best fit solution data.
% Dependency: requires MAAT
% Reference: Springer & Ofek 2021a
% Example: Res=TimeDelay.fit_flux(ResLC.T,ResLC.F_t,ResLC.sigma_F_hat)


InPar = inputParser;
addOptional(InPar,'Solver',@Util.fit.fminunc_my);
addOptional(InPar,'TimeDomain',false);  % work in time domain
addOptional(InPar,'InputFT',false);  % input is t,F_t (false) or w,F_w (true)
addOptional(InPar,'FitPar',[NaN   NaN              2]);  % [A1, A2, gamma]
addOptional(InPar,'DefPar',[1     0.5              2]);  % [A1, A2, gamma]
addOptional(InPar,'Limits',[1e-5 1000;0 1000;       1.5 3.5]); %  without Tau
addOptional(InPar,'VecInvTau',(1./100:0.5./240:1./10)); %1./(20:1:33)); %1./(10.7:1:18.7)); %(2./100:0.5./100:1./10));  % sign has meaning!
addOptional(InPar,'Min_w',2.*pi./100);
addOptional(InPar,'w0',[]);
addOptional(InPar,'Verbose',false);
addOptional(InPar,'AbsTol',1e-2);
addOptional(InPar,'RelTol',1e-6);
parse(InPar,varargin{:});
InPar = InPar.Results;

InPar.FitPar = [InPar.FitPar];
InPar.DefPar = [InPar.DefPar];

options = optimoptions(@fminunc,'Display','off','Diagnostic','off');
AddPars = {options};




VecTau = 1./InPar.VecInvTau; 
Ntau   = numel(VecTau);


if InPar.InputFT
    w   = t(:);
    F_w = F_t;  % assume F_w is already with ortho normalization
    N   = numel(w);
else
    N     = numel(F_t);
    Tstep = unique(diff(t));
    if numel(Tstep)>1 && range(Tstep)>(10000.*eps) && ~InPar.TimeDomain
        error('fit_flux works on evenly spaced data - interpolate data');
    end

    freqs = TimeDelay.fft_freq(N, Tstep(1));
    w = 2.*pi*freqs;
    w = w(:);

    F_w = fft(F_t)./sqrt(N);
end


% check if evenly spaced (for time domain solution)
EvenlySpaced = true;
if InPar.TimeDomain
    DiffT = diff(t);
    MeanDT = mean(DiffT);
    if all(abs(DiffT - MeanDT)<1e-5)
        % evenly spaced
        EvenlySpaced = true;
        
    else
        % unevenly spaced
        EvenlySpaced = false;
        
    end
end
    



FitParH0  = [0, NaN, 0, InPar.FitPar(end)];
ParH0     = InPar.DefPar(1);
if isnan(InPar.FitPar(end))
    ParH0 = [ParH0, InPar.DefPar(end)];
end
ParH1     = InPar.DefPar(isnan(InPar.FitPar));

Res.Tau    = VecTau(:);
Res.LL_H0  = nan(Ntau,1);
Res.LL_H1  = nan(Ntau,1);
Res.BestPar_H0 = nan(Ntau,numel(ParH0));
Res.BestPar_H1 = nan(Ntau,numel(ParH1));
Res.ExitFlag_H1 = nan(Ntau,1);
Res.ExitFlag_H0 = nan(Ntau,1);


%Res.LL_H0 = TimeDelay.logl_F(ParH0, FitParH0, InPar.Limits, InPar.Min_w, w, F_w, sigma_F);
Limits    = [0 0;InPar.Limits];

if InPar.TimeDomain
    
    Tau   = FitParH0(1);
    gamma = FitParH0(4);
    
    if EvenlySpaced
        % evenly spaced
        DT = (0:MeanDT:N-1);
    else
        % unevenly spaced
        % DT is a matrix
        DT = t - t.';
    end
    
    FunG1  = @(Omega) (cos(Omega.*DT)  - 1).*Omega.^(-gamma);
    FunG1t = @(Omega) (cos(Omega.*Tau) - 1).*Omega.^(-gamma);
    FunG2  = @(Omega) (cos(Omega.*DT)  - 1).*cos(Omega.*Tau).*Omega.^(-gamma);
     
    if isempty(InPar.w0)
        
        G1  = 2.*integral(FunG1,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G1t = 2.*integral(FunG1t,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G2  = 2.*integral(FunG2,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G0  = 0;
    else
        % w0 is the integral lower bound
        
        FunG0 = @(Omega) Omega.^(-gamma);
        
        G1  = 2.*integral(FunG1,InPar.w0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G1t = 2.*integral(FunG1t,InPar.w0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G2  = 2.*integral(FunG2,InPar.w0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        % G0 analytic solution (for gamma>1 & w0>0):
        %G0  = 2.*integral(FunG0,InPar.w0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G0 = 2.*(InPar.w0.^(1-gamma))./(gamma-1);
    end
    
    % Pars, FitPar, Limits, Min_w, w, F_w, sigma_F_hat
    [Res.BestPar_H0,Res.LL_H0,Res.ExitFlag_H0]=InPar.Solver({@TimeDelay.logl_F, ...
                                             'FitPar',FitParH0,...
                                             'Limits',Limits,...
                                             'Min_w',InPar.Min_w,...
                                             't',t,...
                                             'F_t',F_t,...
                                             'sigma_F',sigma_F,...
                                             'G0',G1,'G1',G1,'G1t',G1t,'G2',G2,'DT',DT},...
                                            ParH0,AddPars{:});

    for Itau=1:1:Ntau
        if InPar.Verbose
            fprintf('Fitting time delay %d of %d   -  Tau=%f\n',Itau,Ntau,VecTau(Itau));
        end

        Limits = [VecTau(Itau), VecTau(Itau); InPar.Limits];

        FitParH1 = [VecTau(Itau), InPar.FitPar(1:end)];

        
        Tau   = FitParH1(1);
        gamma = FitParH1(4);
        
        % only Tau changes from sim to sim:
        FunG1t = @(Omega) (cos(Omega.*Tau) - 1).*Omega.^(-gamma);
        FunG2 = @(Omega) (cos(Omega.*DT) - 1).*cos(Omega.*Tau).*Omega.^(-gamma);

        %G1  = 2.*integral(FunG1,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G1t = 2.*integral(FunG1t,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        G2  = 2.*integral(FunG2,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);

        
        [Res.BestPar_H1(Itau,:),Res.LL_H1(Itau),Res.ExitFlag_H1(Itau)]=InPar.Solver({@TimeDelay.logl_F, ...
                                            'FitPar',FitParH1,...
                                             'Limits',Limits,...
                                             'Min_w',InPar.Min_w,...
                                             't',t,...
                                             'F_t',F_t,...
                                             'sigma_F',sigma_F,...
                                             'G0',G0,'G1',G1,'G1t',G1t,'G2',G2,'DT',DT},...
                                            ParH1,AddPars{:});

    end
    
    
else

    % Pars, FitPar, Limits, Min_w, w, F_w, sigma_F_hat
    [Res.BestPar_H0,Res.LL_H0,Res.ExitFlag_H0]=InPar.Solver({@TimeDelay.logl_F, ...
                                             'FitPar',FitParH0,...
                                             'Limits',Limits,...
                                             'Min_w',InPar.Min_w,...
                                             'w',w,...
                                             'F_w',F_w,...
                                             'sigma_F',sigma_F},...
                                            ParH0,AddPars{:});

    for Itau=1:1:Ntau
        if InPar.Verbose
            fprintf('Fitting time delay %d of %d   -  Tau=%f\n',Itau,Ntau,VecTau(Itau));
        end

        Limits = [VecTau(Itau), VecTau(Itau); InPar.Limits];

        FitParH1 = [VecTau(Itau), InPar.FitPar(1:end)];

        [Res.BestPar_H1(Itau,:),Res.LL_H1(Itau),Res.ExitFlag_H1(Itau)]=InPar.Solver({@TimeDelay.logl_F, ...
                                            'FitPar',FitParH1,...
                                             'Limits',Limits,...
                                             'Min_w',InPar.Min_w,...
                                             'w',w,...
                                             'F_w',F_w,...
                                             'sigma_F',sigma_F},...
                                            ParH1,AddPars{:});

    end
end