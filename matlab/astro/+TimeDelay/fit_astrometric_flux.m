function [Res]=fit_astrometric_flux(t,F_t,x_t,y_t,sigma_F,sigma_x,varargin)
% Fit the 2 images astrometric-flux time delay model to observations
% Package: +TimeDelay
% Description:
% Input  : - t : vector of times.
%          - F_t: vector of total flux.
%          - x_t: vector of x position.
%          - y_t: vector of y position.
%          - sigma_F: Error in flux.
%          - sigma_x: Error in position.
%          * Arbitrary number of pairs of ...,key,val,... arguments.
%            The following keywords are available:
%            'Solver' - Either @Util.fit.fminsearch_my | @Util.fit.fminunc_my
%                       Default is @Util.fit.fminunc_my
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
%            'TwoD'   - A logical indicate if to perform 2-D fit.
%                       Default is false - not operational.
%            'VecInvTau' - A vector of 1/time_delay to attempt fitting.
%                       Default is (1./100:0.5./100:1./10)
%            'EndMatching' - Apply end-matching. Default is true.
%            'Min_w'   - Minimum w. Default is 2.*pi./100.
%            'Verbose' - Default is true.
% Output : - An output structure containing the following fields:
%            .Tau 
%            .LL_H0
%            .LL_H1
%            .BestPar_H0
%            .BestPar_H1
% Example: Res=TimeDelay.fit_astrometric_flux
%          ResF=TimeDelay.timedelayed_lc;
% Res=TimeDelay.fit_astrometric_flux(ResF.T,ResF.F_t,ResF.x_t,ResF.y_t,ResF.eps_F_abs,ResF.eps_x_abs);

% [ResLC]=TimeDelay.rand_lensed;

% Res = TimeDelay.fit_astrometric_flux(ResLC.T,ResLC.F_t,ResLC.x_t,ResLC.y_t,ResLC.sigma_F_hat,ResLC.sigma_x_hat);


NPAR2D = 11 -1;
NPAR1D = 8 -1;

InPar = inputParser;
addOptional(InPar,'Solver',@Util.fit.fminunc_my);

addOptional(InPar,'FitPar',[0 NaN   NaN      0   1   -1        3]);  % [A0, A1, A2, x0, x1, x2, y0, y1, y2, gamma]
addOptional(InPar,'DefPar',[0 1     0.66     0   1   -1        3]);  % [A0, A1, A2, x0, x1, x2, y0, y1, y2, gamma]
addOptional(InPar,'Limits',[0 3; 1e-5 5;0 5;  -1 1; -2.1 2.1; -2.1 2.1;     1.5 3.5]); %  without Tau

addOptional(InPar,'TwoD',false);
addOptional(InPar,'VecInvTau',(1./100:0.5./240:1./10)); %1./(20:1:33)); %1./(10.7:1:18.7)); %(2./100:0.5./100:1./10));  % sign has meaning!
addOptional(InPar,'Min_w',2.*pi./100);
addOptional(InPar,'EndMatching',true);
addOptional(InPar,'Verbose',true);
parse(InPar,varargin{:});
InPar = InPar.Results;

InPar.FitPar = [InPar.FitPar];
InPar.DefPar = [InPar.DefPar];

% Input arguments: t,F_t,x_t,y_t,sigma_F,sigma_x

N = length(F_t);
t_step = unique(diff(t));
if numel(t_step)>1
    error('Time series must be equally spaced');
end

freqs = TimeDelay.fft_freq(N, t_step);

sigma_F_hat = sigma_F;
sigma_x_hat = sigma_x;
w = 2.*pi*freqs;

F_w = fft(F_t) ./ sqrt(N);
Gx_t = x_t.*F_t;
Gx_w = fft(Gx_t) ./ sqrt(N);
Gy_t = y_t.*F_t;
Gy_w = fft(Gy_t) ./ sqrt(N);


sigma_y = sigma_x;
sigma_y_hat = sigma_y;


%% Main Fitter 

N = length(F_t);

DFT = fft(eye(N), N, 1) ./ sqrt(N);
DFT_dagger = DFT';
LogZ = sum(log(F_t));
Gamma_1_ = ((DFT * diag(F_t.^2)) * DFT_dagger) * sigma_x_hat^2;

% verify the size of FitPar and DefPar
if InPar.TwoD
    if ~(numel(InPar.FitPar)==NPAR2D && numel(InPar.DefPar)==NPAR2D && size(InPar.Limits,1)==NPAR2D)
        error('For 2D fitting Limits, FitPar and DefPar must contain %d elements',NPAR2D);
    end
else
    if ~(numel(InPar.FitPar)==NPAR1D && numel(InPar.DefPar)==NPAR1D && size(InPar.Limits,1)==NPAR1D)
        error('For 1D fitting Limits, FitPar and DefPar must contain %d elements',NPAR1D);
    end
end


    
FlagN       = isnan(InPar.FitPar);

AddPars = {};


BestGuessH1 = InPar.DefPar(FlagN);
BestGuessH0 = InPar.DefPar(1:2);  % A0, A1
    
% fitting for each time delay

VecTau = 1./InPar.VecInvTau; 
Ntau   = numel(VecTau);



Res.Tau    = VecTau(:);
Res.LL_H1  = nan(Ntau,1);
Res.BestPar_H1 = nan(Ntau,numel(BestGuessH1));
Res.ExitFlag_H1 = nan(Ntau,1);

%Options = optimoptions ( 'fminunc','UseParallel',false,'FunctionTolerance',1e-6,'Display','off');



for Itau=1:1:Ntau
    if InPar.Verbose
        fprintf('Fitting time delay %d of %d   -  Tau=%f\n',Itau,Ntau,VecTau(Itau));
    end
    
    Limits = [VecTau(Itau), VecTau(Itau); InPar.Limits];
    
    FitParH1 = [VecTau(Itau), InPar.FitPar(1:end)];
    
    if InPar.TwoD
        FitParH0 = [VecTau(Itau), NaN, NaN, 0,  0 0 0  0 0 0 InPar.FitPar(end)];

        [Res.BestPar_H1(Itau,:),Res.LL_H1(Itau),Res.ExitFlag_H1(Itau)]=InPar.Solver({@TimeDelay.logl_x2d_given_F, ...
                                         FitParH1, Limits, w, Gx_w, Gy_w, F_t, F_w, ...
                                         sigma_F_hat, sigma_x_hat, ...
                                         DFT, DFT_dagger, LogZ, Gamma_1_},...
                                        BestGuessH1,AddPars{:});

            
        [Res.BestPar_H0(Itau,:),Res.LL_H0(Itau),Res.ExitFlag_H0(Itau)]=InPar.Solver({@TimeDelay.logl_x2d_given_F, ...
                                         FitParH0, Limits, w, Gx_w, Gy_w, F_t, F_w, ...
                                         sigma_F_hat, sigma_x_hat, ...
                                         DFT, DFT_dagger, LogZ, Gamma_1_},...
                                        BestGuessH0,AddPars{:});
    else
        % 1-D
        FitParH0 = [VecTau(Itau), NaN, NaN, 0,  0 0 0   InPar.FitPar(end)];
        
        

        [Res.BestPar_H1(Itau,:),Res.LL_H1(Itau),Res.ExitFlag_H1(Itau)]=InPar.Solver({@TimeDelay.logl_xF, ...
                                         'FitPar',FitParH1,...
                                         'Limits',Limits,...
                                         'w',w,...
                                         't',t,...
                                         'x_t',x_t,...
                                         'Gx_w',Gx_w,...
                                         'F_t',F_t,...
                                         'F_w',F_w,...
                                         'sigma_F',sigma_F_hat,...
                                         'sigma_x',sigma_x_hat,...
                                         'Min_w',InPar.Min_w,...
                                         'DFT',DFT,...
                                         'DFT_dagger',DFT_dagger,...
                                         'LogZ',LogZ,...
                                         'Gamma_1_',Gamma_1_,...
                                         'EndMatching',InPar.EndMatching,...
                                         'TwoD',false,...
                                         'Verbose',false},...
                                        BestGuessH1,AddPars{:});


        if Itau==1
            [Res.BestPar_H0(Itau,:),Res.LL_H0(Itau),Res.ExitFlag_H0(Itau)]=InPar.Solver({@TimeDelay.logl_xF, ...
                                             'FitPar',FitParH0,...
                                             'Limits',Limits,...
                                             'w',w,...
                                             't',t,...
                                             'x_t',x_t,...
                                             'Gx_w',Gx_w,...
                                             'F_t',F_t,...
                                             'F_w',F_w,...
                                             'sigma_F',sigma_F_hat,...
                                             'sigma_x',sigma_x_hat,...
                                             'Min_w',InPar.Min_w,...
                                             'DFT',DFT,...
                                             'DFT_dagger',DFT_dagger,...
                                             'LogZ',LogZ,...
                                             'Gamma_1_',Gamma_1_,...
                                             'EndMatching',InPar.EndMatching,...
                                             'TwoD',false,...
                                             'Verbose',false},...
                                            BestGuessH0,AddPars{:});
        end
       
    end
     
    
end









