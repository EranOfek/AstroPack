function [Chi2,Nobs,CalcF_t,f_t]=chi2_xF(Pars,FitPar,varargin)
% Return the \chi^2 for fitting astrometric time delay in the time domain
% Package: +TimeDelay
% Input  : - chi2_xF(Pars,FitPar,...)
%            where Pars is a vector of a subset of parameters.
%            The subset may include any combination of the
%            [Tau, A0, A1, A2, x0, x1, x2] parameters.
%            FitPar is a vector of default values for all the parameters
%            [Tau, A0, A1, A2, x0, x1, x2], where a parameter that is
%            passed in Pars should be NaN.
%            Example: chi2_xF([1 0.5] ,[30 0 NaN NaN 0 -1 1],...)
%            will use A_1=1 and A_2=0.5.
%          * Arbitrary number of ...,key,val,...
%            't' - Vector of times.
%            'F_t' - Vectof of combined fluxes F(t).
%            'x_t' - vector of center-of-light positions x(t).
%            'ErrF_t' - Vector/scalar of errors in F(t).
%            'Errx_t' - Vector/scalar of errors in x(t).
%            'Limits' - A two column array of lower/upper limits on the free
%                   parameters in the same order as in FitPar.
%                   If the a parameter is outside these limits,
%                   then will return Chi2=Inf.
%            'InterpMethod' - Interpolation method. Default is 'pchip'.
% Output : - \chi^2
%          - Number of observations
%          - Reconstructed F(t)
%          - Reconstructed f(t)
% 
% Reference: Springer & Ofek 2021b


InPar = inputParser;

addOptional(InPar,'t',[]);
addOptional(InPar,'F_t',[]);
addOptional(InPar,'x_t',[]);
addOptional(InPar,'ErrF_t',[]);
addOptional(InPar,'Errx_t',[]);
addOptional(InPar,'Limits',[-1000 1000; 0 1000; 1e-5 1000; 0 1000; -2 2; -2 2; -2 2]);
addOptional(InPar,'InterpMethod','pchip');
parse(InPar,varargin{:});
InPar = InPar.Results;

InPar.ErrF_t = InPar.ErrF_t(:);

% [Tau, A0, A1, A2, x0, x1, x2]
Ind = 0;
if ~isnan(FitPar(1))
   Tau = FitPar(1);
else
   Ind = Ind + 1;
   Tau = Pars(Ind);
end

if ~isnan(FitPar(2))
   A0 = FitPar(2);
else
   Ind = Ind + 1;
   A0 = Pars(Ind);
end

if ~isnan(FitPar(3))
   A1 = FitPar(3);
else
   Ind = Ind + 1;
   A1 = Pars(Ind);
end

if ~isnan(FitPar(4))
   A2 = FitPar(4);
else
   Ind = Ind + 1;
   A2 = Pars(Ind);
end

if ~isnan(FitPar(5))
   x0 = FitPar(5);
else
   Ind = Ind + 1;
   x0 = Pars(Ind);
end

if ~isnan(FitPar(6))
   x1 = FitPar(6);
else
   Ind = Ind + 1;
   x1 = Pars(Ind);
end

if ~isnan(FitPar(7))
   x2 = FitPar(7);
else
   Ind = Ind + 1;
   x2 = Pars(Ind);
end

if Tau<InPar.Limits(1,1) || Tau>InPar.Limits(1,2) || ...
    A0<InPar.Limits(2,1) || A0>InPar.Limits(2,2) || ...
    A1<InPar.Limits(3,1) || A1>InPar.Limits(3,2) || ...
    A2<InPar.Limits(4,1) || A2>InPar.Limits(4,2) || ...
    x0<InPar.Limits(5,1) || x0>InPar.Limits(5,2) || ...
    x1<InPar.Limits(6,1) || x1>InPar.Limits(6,2) || ...
    x2<InPar.Limits(7,1) || x2>InPar.Limits(7,2)

    Chi2 = Inf;
else
    % calculate the \chi^2
    
    % reconstruct the source LC
    f_t = TimeDelay.reconstruct_ft(InPar.F_t, InPar.x_t, x0, [x1, x2], A0, A1, InPar.ErrF_t, InPar.Errx_t);
    
    CalcF_t = TimeDelay.combined_Ft(InPar.t, f_t, A0, [A1 A2], Tau, InPar.InterpMethod);
    
    Flag = InPar.t> (min(InPar.t)+abs(Tau)) & InPar.t<(max(InPar.t)-abs(Tau));
    Nobs = sum(Flag);
    
    Resid = InPar.F_t(:) - CalcF_t(:);
    if numel(InPar.ErrF_t)==1
        ErrF_t = InPar.ErrF_t;
    else
        ErrF_t = InPar.ErrF_t(Flag);
    end
    ErrF_t = ErrF_t.*1;
    Chi2 = nansum((Resid(Flag)./ErrF_t).^2);
    
end
    

