function ResLC=rand_lensed_uneq(T,varargin)
% unequally spaced lensed quasar light curves and center of light position
% Package: +TimeDelay
% Input  : - Vector of times.
%          * Arbitrary pairs of arguments to pass to TimeDelay.rand_lensed.
% Output : - A structure containing the simulated light curve.
%     By : Eran O. Ofek                  Oct 2020
% Example: T=timeseries.random_time_sequence; 
%          ResLC=TimeDelay.rand_lensed_uneq(T);     

if isstruct(varargin{1})
    InPar = varargin{1};
else
    InPar = inputParser;
    addOptional(InPar,'Cyclic',false);
    addOptional(InPar,'A0',0);
    addOptional(InPar,'A',[1 0.5]); %2./3]);
    addOptional(InPar,'Tau',[25.7]);   % all positive!
    addOptional(InPar,'x0',0);  
    addOptional(InPar,'y0',0);  
    addOptional(InPar,'x',[0.5 -0.5]); %[-1 1]);  
    addOptional(InPar,'y',[0 0]);
    addOptional(InPar,'f_dc',200);
    addOptional(InPar,'Gamma',3);
    addOptional(InPar,'TotTime',240);
    addOptional(InPar,'DeltaT',1);
    addOptional(InPar,'sigma_x',0.02);  
    addOptional(InPar,'sigma_F_rel',0.05);
    addOptional(InPar,'sigmaOutTot',false);  % sigma out of total (false only multiply by f_dc)

    addOptional(InPar,'Validate',true);  % if LC is not positive than recalc
    addOptional(InPar,'StdMeanRange',[0.1 0.2]);  % if LC is not positive than recalc

    addOptional(InPar,'InterpMethod','pchip');
    parse(InPar,varargin{:});
    InPar = InPar.Results;
end

TotTime = range(T);
[Res]=TimeDelay.rand_lensed(varargin{:},'sigma_F_rel',0,'sigma_x',0,'DeltaT',0.1,'TotTime',TotTime);

Res.sigma_F_rel = InPar.sigma_F_rel;
Res.sigma_x     = InPar.sigma_x;
Res.sigma_F_hat = Res.sigma_F_rel.*Res.sigma_F_rel2abs;
Res.sigma_x_hat = Res.sigma_x;


F_t = interp1(Res.T,Res.F_t,T,'pchip');
x_t = interp1(Res.T,Res.x_t,T,'pchip');
y_t = interp1(Res.T,Res.y_t,T,'pchip');

% The power spectrum

%FunFT = @(t,M,f) sum(M(:).*exp(-2.*pi.*f(:).'.*t(:)));

N = numel(F_t);
ResLC.Base = Res;
ResLC.T    = T;
ResLC.F_t  = F_t + randn(N,1).*InPar.sigma_F_rel.*mean(F_t);
ResLC.x_t  = x_t + randn(N,1).*InPar.sigma_x;
ResLC.y_t  = y_t + randn(N,1).*InPar.sigma_x;


