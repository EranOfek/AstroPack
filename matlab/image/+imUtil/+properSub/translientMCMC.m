function [Chain] = translientMCMC(N,R,Pn,Pr, SigmaN, SigmaR, Xc,Args)
% MCMC simulations to retrieve the flux and displacement parameters.
%   The function uses a MATLAB-implemented Metropolis-Hastings
%   algorithm and returns a chain of of smaples.
% Input  : - The background subtracted new image (N). This can be in
%            the image domain or fourier domain (i.e., 'IsImFFT'=true).
%          - Like N but, the background subtracted reference image (R).
%          - The PSF of the new image N. The PSF image size must be
%            equal to the N and R image sizes, and the PSF center
%            should be located at pixel 1,1 (corner).
%            The input may be in the image domain or Fourier domain
%            (i.e., 'IsPsfFFT'=true).
%          - Like Pn, but the PSF for the reference image.
%          - (SigmaN) the standard deviation of the background new
%            image.
%          - (SigmaR) the standard deviation of the background
%            reference image.
%          - (Xc) The centeroid of the two images, obtained with
%          TRANSLIENT.
%          * ...,key,val,...
%            'IsPsfFFT' - A logical indicating if the input Pn and Pr
%                   PSFs are in Fourier domain. Default is false.
%            'ShiftPsf' - A logical indicating if to fftshift the input
%                   Pn and Pr PSFs. Default is false.
%            'Eps' - A small value to add to the demoninators in order
%                   to avoid division by zero due to roundoff errors.
%                   Default is 0. (If needed set to about 100.*eps).
%            'TrueValues' - The known or estimated true values of the
%                   parameters, to start the MC chain. 
%            'EvalInitialGuess' - Whether to use fminsearch to evaluate the
%                   starting position of the chain in case TrueValues is
%                   empty. Default is true.
%            'Nchain' - Number of samples in the chain (including dropped
%                   initial samples).
%            'Ndrop' - Number of initial samples to drop. If empty, then
%                   will drop the first 20%. Default is empty.
%            'ParameterSet' - The parameter set to use for the
%                   posterior. Values are:
%                   1 (Default): Eq. 28 parameters (DeltaX, DeltaY,AlphaR,AlphaN)
%                   2 : Eq. 29 parameters (Delta,Theta, AlphaR-AlphaN, AlphaN+AlphaR)
%                   3 : Eq. 30 parameters (Theta,AlphaR-AlphaN, Delta*(AlphaN+AlphaR))
%            'WaitBar' - Wait bar while the algorithm computes. Default
%                   is false.
%            'Verbosity' - Verbosity parameter for the mcmcrun
%                   function. Default is false.
% Output : - (Chain) Chain of samples from the posterior distribution.
%             Size is (Nchain-Ndrop)x(number of parameters).
%
% Author : Amir Sharon (August 2022)
% Example: Size=64; Pos = [30,30];
%   TrueValues = [6,0,500,500];
%   R = randn(Size,Size)+TrueValues(3)*imUtil.kernel2.gauss([5,5,0],[Size Size],Pos);
%   N = randn(Size,Size)+TrueValues(4)*imUtil.kernel2.gauss([5,5,0],[Size Size],Pos+TrueValues(1:2));
%   Xc = Pos+TrueValues(1:2)/2;
%   [ Chain] = translientMCMC(N,R,Pn,Pr, 1, 1,Xc,'TrueValues',TrueValues);
arguments
    N
    R
    Pn        % PSF in the corner
    Pr        % must have the same size as Pn, with PSF in the corner
    SigmaN
    SigmaR
    Xc

    Args.IsImFFT                = false;
    Args.IsPsfFFT               = false;

    Args.Eps                    = 0;
    Args.TrueValues             = [];
    Args.EvalInitialGuess       = false;

    Args.Nchain                 = 10000;
    Args.Ndrop                  = [];

    Args.ParameterSet           = 1; % 2,3;

    Args.WaitBar                = false;
    Args.Verbosity              = false;
end

if Args.IsImFFT
    N_hat = N;
    R_hat = R;
else
    N_hat  = fft2(N);
    R_hat  = fft2(R);
end

if Args.IsPsfFFT
    Pnhat = Pn;
    Prhat = Pr;
else
    Pnhat = fft2(Pn);
    Prhat = fft2(Pr);
end

if isempty(Args.Ndrop)
    Args.Ndrop = floor(Args.Nchain/5);
end

M     = size(Pnhat,1);
Mcenter = ceil(M/2);
FreqArr = fftshift(-Mcenter:(Mcenter-1));

[Kx,Ky] = meshgrid(FreqArr);

TotalVar = abs(Prhat).^2*SigmaN.^2+abs(Pnhat).^2*SigmaR.^2+Args.Eps;

Phase = exp(-2i*pi*(Kx*(Xc(1)-1)+Ky*(Xc(2)-1))/M);

switch Args.ParameterSet
    case 1  % Equation (28)
        Loglike = @(x,data) 1/M^2*sum(abs(Prhat.*N_hat - Pnhat.*R_hat + ...
            Phase.*Prhat.*Pnhat.*(x(3)*exp(1i*pi*(Kx*x(1)+Ky*x(2))/M)-...
            x(4)*exp(-1i*pi*(Kx*x(1)+Ky*x(2))/M))).^2./TotalVar,"all");

        ParamNames = {'DeltaX','DeltaY','AlphaR','AlphaN'};
        ParamBounds = [-M/2+1,-M/2-1,0,0;
            M/2-1,M/2-1,inf,inf];
        InitialGuess = [0,0,max([sum(R(:)),max(R(:))]),max([sum(N(:)),max(N(:))])];

    case 2  % Equation (29)
        Loglike = @(x,data) 1/M^2*sum(abs(Prhat.*N_hat - Pnhat.*R_hat + ...
            Phase.*Prhat.*Pnhat.*((x(3)+x(4))*exp(1i*pi*(Kx*x(1)*cos(x(2))+Ky*x(1)*sin(x(2)))/M)-...
            (x(4)-x(3))*exp(-1i*pi*(Kx*x(1)*cos(x(2))+Ky*x(1)*sin(x(2)))/M))/2).^2./TotalVar,"all");

        ParamNames = {'Delta','Theta','AlphaRNdiff','AlphaRNsum'};
        ParamBounds = [0,-2*pi,-inf,0;
            M/2-1,2*pi,inf,inf];
        InitialGuess = [0,0,sum(R(:))-sum(N(:)),max([sum(R(:))+sum(N(:)),max(R(:)+N(:))])];

    case 3  % Equation (30)
        Loglike = @(x,data) 1/M^2*sum(abs(Prhat.*N_hat - Pnhat.*R_hat + ...
            Phase.*Prhat.*Pnhat.*(x(2)+x(3)*1i*pi*(Kx*cos(x(1))+Ky*sin(x(1)))/M)).^2./TotalVar,"all");

        ParamNames = {'Theta','AlphaRNdiff','DeltaAlphaRNsum'};
        ParamBounds = [-2*pi,-inf,0;
            2*pi,inf,inf];
        InitialGuess = [0,sum(R(:))-sum(N(:)),max([sum(R(:))+sum(N(:)),max(R(:)+N(:))])];

    otherwise
        error('Invalid ParameterSet')
end

if isempty(Args.TrueValues)
    if Args.EvalInitialGuess
        OptionsFmin = optimset('Display','off');
        [Res,~,ExitFlag]  = fminsearch(Loglike,InitialGuess,OptionsFmin);    
        [Res,~,~,ExitFlag] = lsqnonlin(Loglike,  InitialGuess ,ParamBounds(1,:),ParamBounds(2,:),OptionsFmin );
    end
    if Args.EvalInitialGuess && ExitFlag>=0
        StartParams = Res;
    else
        StartParams = InitialGuess;
    end
else
    StartParams = Args.TrueValues;
end

for I=1:length(ParamNames)
    Params{I} = {ParamNames{I},StartParams(I),ParamBounds(1,I),ParamBounds(2,I)};
end

Options.nsimu = Args.Nchain;
Options.verbosity = Args.Verbosity;
Options.waitbar = Args.WaitBar;

Model.ssfun = Loglike;
Data.xdata = [];

[res,Chain,s2chain, sschain] = mcmcrun(Model,Data,Params,Options);

Start = Args.Ndrop+1;
Chain = Chain(Start:end,:);

% Probably unnecessary
if Args.ParameterSet==2
    Chain(:,2) = mod(Chain(:,2)+pi,2*pi)-pi;
elseif Args.ParameterSet==3
    Chain(:,1) = mod(Chain(:,1)+pi,2*pi)-pi;
    % end

end