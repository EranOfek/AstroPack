function Res=fit_astrometric_flux_simple(varargin)
% Fit 2-D lensed images using the simple flux/astrometry method (no red noise)
% Example: Res=TimeDelay.fit_astrometric_flux_simple(...)



InPar = inputParser;
addOptional(InPar,'Solver',@Util.fit.fminunc_my);

%[Tau, A0, A1, A2, x0, x1, x2]

addOptional(InPar,'FitPar',[0 NaN   NaN      0   1   -1    ]);  % [A0, A1, A2, x0, x1, x2, y0, y1, y2]
addOptional(InPar,'DefPar',[0 1     0.5      0   1   -1    ]);  % [A0, A1, A2, x0, x1, x2, y0, y1, y2]


addOptional(InPar,'t',[]);
addOptional(InPar,'F_t',[]);
addOptional(InPar,'x_t',[]);
addOptional(InPar,'ErrF_t',[]);
addOptional(InPar,'Errx_t',[]);
addOptional(InPar,'Limits',[-1000 1000; 0 1000; 1e-5 1000; 0 1000; -2 2; -2 2; -2 2]);
addOptional(InPar,'InterpMethod','pchip');


addOptional(InPar,'TwoD',false);
addOptional(InPar,'VecInvTau',(1./100:0.5./240:1./10)); %1./(20:1:33)); %1./(10.7:1:18.7)); %(2./100:0.5./100:1./10));  % sign has meaning!
addOptional(InPar,'Verbose',true);
parse(InPar,varargin{:});
InPar = InPar.Results;

InPar.FitPar = [InPar.FitPar];
InPar.DefPar = [InPar.DefPar];


AddPars = {};
BestGuessH1 = InPar.DefPar(isnan(InPar.FitPar));

FitParH0    = [0 InPar.DefPar];
FitParH0(2) = 0;
FitParH0(3) = NaN;
FitParH0(4) = 0;

BestGuessH0 = InPar.DefPar(isnan(FitParH0));

[Res.BestPar_H0,Res.LL_H0]=InPar.Solver({@TimeDelay.chi2_xF, ...
                                         FitParH0,...
                                         'Limits', InPar.Limits,...
                                         't', InPar.t,...
                                         'F_t',InPar.F_t,...
                                         'x_t',InPar.x_t,...
                                         'ErrF_t',InPar.ErrF_t,...
                                         'Errx_t',InPar.Errx_t,...
                                         'InterpMethod',InPar.InterpMethod},...
                                        BestGuessH0,AddPars{:});

Ntau = numel(InPar.VecInvTau);
for Itau=1:1:Ntau
    FitParH1 = [1./InPar.VecInvTau(Itau), InPar.FitPar];
    
    [Res.BestPar_H1(Itau,:),Res.LL_H1(Itau)]=InPar.Solver({@TimeDelay.chi2_xF, ...
                                         FitParH1,...
                                         'Limits', InPar.Limits,...
                                         't', InPar.t,...
                                         'F_t',InPar.F_t,...
                                         'x_t',InPar.x_t,...
                                         'ErrF_t',InPar.ErrF_t,...
                                         'Errx_t',InPar.Errx_t,...
                                         'InterpMethod',InPar.InterpMethod},...
                                        BestGuessH1,AddPars{:});
end

Res.Tau = 1./InPar.VecInvTau;

