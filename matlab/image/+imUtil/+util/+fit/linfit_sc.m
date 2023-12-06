function [Par,ParErr,Stat]=linfit_sc(X,Y,Err,Order,varargin)
% Linear fitting with weights and sigma clipping
% Package: +imUtil.util.fit
% Description: 
% Input  : - Vector of X.
%          - Vector of Y.
%          - Sclar or vector of errors in Y. If scalr, assume all Ys have
%            the same errors. Default is 1.
%          * Pairs of ...,key,val,... The following keywords are available:
%            'Funs' - A cell array of functions that construct the linear
%                   transformation of the form: Par1*Fun{1} +
%                   Par2*Fun{2}+...
%                   Default is {@(x) ones(size(x)), @(x) x}
%                   I.e., Y=a+b*x
%            'MaxIter' - Maximum number of outlier rejection iterations.
%                   Default is 2.
%            'ClipMethod' - Method to clip outliers.
%                   Default is 'sigclip'
%            'StdFun' - A function handle by which to calculate the
%                   residuals std. 
%                   Default is @imUtil.background.rstd
%            'MeanFun' -  A function handle by which to calculate the
%                   residuals mean. 
%                   Default is @median
%            'Nsigma' - [low high] number of sigma below, above the mean to
%                   reject outliers. Default is 3.
% Output : - Best fit parameters. Given in order of Funs.
%          - Best fit errors in parameters. Given in order of Funs.
%          - Structure with the following information:
%            .Resid - Vector of residuals from best fit
%            .Std - std of residuals
%            .Chi2 - \chi^2
%            .Npar - number of parameters
%            .Ngood - number of good points
%            .Dof   - degrees of freedom
%            .Pchi2 - Cumulative probability of chi2 given dof.
%            .FlagGood - A vector of logical indicating the points used in
%                   the last iteartion.
% Example: X=(1:1:100)'; Y=X+randn(100,1).*0.5; Y(6)=100; Y(80)=70;
%          [Par,ParErr,Stat]=imUtil.util.fit.linfit_sc(X,Y,0.5)


if nargin<3
    Err = 1;
end


InPar = inputParser;
addOptional(InPar,'Funs',{@(x) ones(size(x)), @(x) x});
addOptional(InPar,'MaxIter',2); 
addOptional(InPar,'ClipMethod','sigclip');  % 'minmax' | 'sigclip' 
addOptional(InPar,'StdFun',@imUtil.background.rstd); 
addOptional(InPar,'MeanFun',@median); 
addOptional(InPar,'Nsigma',3);
parse(InPar,varargin{:});
InPar = InPar.Results;

X = X(:);
Y = Y(:);
Err = Err(:);


% design matrix
Nfuns = numel(InPar.Funs);
Nobs  = numel(Y);
H     = zeros(Nobs,Nfuns);
for Ifuns=1:1:Nfuns
    H(:,Ifuns) = InPar.Funs{Ifuns}(X);
end

Err = Err.*ones(Nobs,1);
InvVar = 1./(Err.^2);

InPar.Nsigma = InPar.Nsigma.';
InPar.Nsigma = InPar.Nsigma.*ones(2,1);

Iter = 1;
FlagGood = true(Nobs,1);
while Iter<=InPar.MaxIter
    Iter = Iter + 1;
    
    [Par,ParErr] = lscov(H(FlagGood,:),Y(FlagGood),InvVar(FlagGood));
    
    Resid = Y - H*Par;
    
    if Iter<=InPar.MaxIter
        
        switch lower(InPar.ClipMethod)
            case 'sigclip'
                Mean = InPar.MeanFun(Resid); 
                Std  = InPar.StdFun(Resid);

                FlagGood = Resid>(Mean-Std.*InPar.Nsigma(1)) & Resid<(Mean+Std.*InPar.Nsigma(2));

            otherwise
                error('Unknown ClipMethod option');
        end
    end
    
    
end

Stat.Resid    = Resid;
Stat.Std      = std(Resid);
Stat.Chi2     = sum((Resid(FlagGood)./Err(FlagGood)).^2); 
Stat.Npar     = Nfuns;
Stat.Ngood    = sum(FlagGood);
Stat.Dof      = Stat.Ngood - Stat.Npar;
Stat.Pchi2    = chi2cdf(Stat.Chi2,Stat.Dof);
Stat.FlagGood = FlagGood;

    