function [LogLikeF,Sigma_F,Sigma_phi]=logl_F(Pars, varargin)
% Calculate the log-likelihood of F, Sigma_F, and Sigma_phi (flux only method)
% Package: +TimeDelay
% Input  : - Vector of parameters to use.
%            This can be any subset of [Tau, A1, A2, gamma]
%          * Arbitrary number of ...,key,val,...
%            'FitPar' - A vector of [Tau, A1, A2, gamma].
%                   If value is NaN, then will use its value from the first
%                   input argument (by order of apperance), otherwise will
%                   use the indicated value.
%                   Default is [0 1 0  2].
%            'Limits' - A two column matrix of lower/upper limits on the
%                   paramaeters.
%                   Default is [0 1000;  1e-5 1000;  0 1000; 1.5 3.5].
%                   Note that this is including Tau.
%            'Min_w' - A vector of [lower, upper] frequencies top reject.
%                   Default is [2.*pi./100, Inf].
%            'w' - A vecor of angular frequencies (observations).
%            'F_w' - The FFT of F(t).
%            't'   - A vector of times t.
%            'F_t' - A vector of combined fluxe F(t).
%            'sigma_F' - Error in F(t).
%            'G0' - Integral G_0. If empty, then calculate. Default is [].
%            'G1' - Integral G_1. If empty, then calculate. Default is [].
%            'G1t'- Integral G_1'. If empty, then calculate. Default is [].
%            'G2' - Integral G_2. If empty, then calculate. Default is [].
%            'DT' - Matrix of Delta times. If empty, then calculate.
%                   Default is [].
%            'AbsTol' - Abs. tolerance for integral soltion.
%                   Default is 1e-2.
%            'RelTol' - Rel. tolerance for integral solution.
%                   Default is 1e-6.
% Output : - An array of minus Log likelihood of F for the requested
%            Tau,Alpha1,Alpha2,gamma
%          - Sigma_F, column per parameter Tau/Alpha/gamma
%          - Sigma_phi, column per parameter Tau/Alpha/gamma
% Example: 
% [LogLikeF,Sigma_F,Sigma_phi]=TimeDelay.logl_BPL([1 2],[NaN NaN 0 0],[],[],ResLC.w, ResLC.F_w, ResLC.sigma_F_hat)




InPar = inputParser;
addOptional(InPar,'FitPar',[0 1 0  2]); % [Tau A1 A2 gamma]
addOptional(InPar,'Limits',[0 1000;  1e-5 1000;  0 1000;  1.5 3.5]); % with Tau
addOptional(InPar,'Min_w',2.*pi./100);

addOptional(InPar,'w',[]);
addOptional(InPar,'F_w',[]);
addOptional(InPar,'t',[]);
addOptional(InPar,'F_t',[]);
addOptional(InPar,'sigma_F',[]);
addOptional(InPar,'G0',0);
addOptional(InPar,'G1',[]);
addOptional(InPar,'G1t',[]);
addOptional(InPar,'G2',[]);
addOptional(InPar,'DT',[]);
addOptional(InPar,'AbsTol',1e-2);
addOptional(InPar,'RelTol',1e-6);
parse(InPar,varargin{:});
InPar = InPar.Results;

InPar.FitPar = [InPar.FitPar];
%InPar.DefPar = [InPar.DefPar];


if isempty(InPar.Min_w)
    Min_w = [0 Inf];
else
    if numel(InPar.Min_w)==1
        Min_w = [InPar.Min_w, Inf];
    else
        Min_w = InPar.Min_w;
    end
end



% [Tau, Alpha1, Alpha2, gamma]
Ind = 0;
if ~isnan(InPar.FitPar(1))
   Tau = InPar.FitPar(1);
else
   Ind = Ind + 1;
   Tau = Pars(Ind);
end

if ~isnan(InPar.FitPar(2))
   Alpha1 = InPar.FitPar(2);
else
   Ind = Ind + 1;
   Alpha1 = Pars(Ind);    
end
if ~isnan(InPar.FitPar(3))
   Alpha2 = InPar.FitPar(3);
else
   Ind = Ind + 1;
   Alpha2 = Pars(Ind);    
end
if ~isnan(InPar.FitPar(4))
   gamma = InPar.FitPar(4);
else
   Ind = Ind + 1;
   gamma = Pars(Ind);    
end



if Tau<InPar.Limits(1,1) || Tau>InPar.Limits(1,2) || ...
       Alpha1<InPar.Limits(2,1) || Alpha1>InPar.Limits(2,2) || ...
       Alpha2<InPar.Limits(3,1) || Alpha2>InPar.Limits(3,2) || ...
       gamma<InPar.Limits(4,1) || gamma>InPar.Limits(4,2)
    LogLikeF  = Inf;
    Sigma_F   = NaN;
    Sigma_phi = NaN;
else

    if ~isempty(InPar.F_w) && ~isempty(InPar.w)
        % frequency domain
        
        w_     = InPar.w;
        w_(1)  = 1.0;
        % frequency is column vector
        w_     = w_(:);
        % free parameters should be row vector
        Tau    = Tau(:).';
        Alpha1 = Alpha1(:).';
        Alpha2 = Alpha2(:).';
        gamma  = gamma(:).';

        % sigma_phi is a matrix of (frequrncy/columns, parameters/row):
        Sigma_phi = (Alpha1.^2 + Alpha2.^2 + 2.*Alpha1.*Alpha2.*cos(InPar.w.*Tau)) .* (abs(w_).^(-gamma));
        %Sigma_phi = Sigma_phi./(numel(w));
        Sigma_F   = Sigma_phi + InPar.sigma_F.^2;

        Power_F = (abs(InPar.F_w).^2);


        Flag = abs(InPar.w)>Min_w(1) & abs(InPar.w)<Min_w(2);
        LogLikeF = -0.5.*sum(Power_F(Flag) ./ Sigma_F(Flag)) - 0.5.*sum(log(2.*pi.*Sigma_F(Flag)));
        LogLikeF = -LogLikeF;
        
    elseif ~isempty(InPar.F_t) && ~isempty(InPar.t)
        % time domain
        Sigma_F = [];
        
        % populate the covariance matrix
        %Fun = @(Omega) (Alpha1.^2 + Alpha2.^2 + 2.*Alpha1.*Alpha2.*cos(Omega.*Tau)).*Omega.^(-gamma) + InPar.sigma_F.^2;
        
        
        
        Nt = numel(InPar.t);
        % matrix of time lags
        %DT = InPar.t-InPar.t.';
        if isempty(InPar.DT)
            error('DT must be provided');
        else
            DT = InPar.DT;
        end
        %CT = zeros(size(DT));
        
        
        if isempty(InPar.G1)
            FunG1  = @(Omega) (cos(Omega.*DT)  - 1).*Omega.^(-gamma);
            G1 = 2.*integral(FunG1,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        else
            G1 = InPar.G1;
        end
            
        if isempty(InPar.G1t)
            FunG1t = @(Omega) (cos(Omega.*Tau) - 1).*Omega.^(-gamma);
            G1t = 2.*integral(FunG1t,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        else
            G1t = InPar.G1t;
        end
        
        if isempty(InPar.G2)
            FunG2  = @(Omega) (cos(Omega.*DT)  - 1).*cos(Omega.*Tau).*Omega.^(-gamma);
            G2 = 2.*integral(FunG2,0,Inf,'ArrayValued',true,'AbsTol',InPar.AbsTol,'RelTol',InPar.RelTol);
        else
            G2 = InPar.G2;
        end
        
        G0 = InPar.G0;  % diverging integral
        
        
        CT = (Alpha1.^2 + Alpha2.^2).*(G0 + G1) + 2.*Alpha1.*Alpha2.*(G0 + G1t + G2);
        
        % add sigma_F^2 to the diagonal of CT
        % check if CT is a mtriax
        if size(CT,1)==size(CT,2)
            % CT is a matrix
            CT = CT + diag(InPar.sigma_F.^2);
        else
            % CT is a vector - build a matrix
            if numel(InPar.sigma_F)>1
                CT = TimeDelay.equal_diagonals_matrix(CT);
                CT = CT + diag(InPar.sigma_F.^2);
            else
                CT(1) = CT(1) + InPar.sigma_F.^2;
                CT = TimeDelay.equal_diagonals_matrix(CT);
            end
        end
            
            
        
        F_t  = InPar.F_t - mean(InPar.F_t);
        F_t  = F_t(:);
        
        LogLikeF = 0.5.*abs(TimeDelay.logdet(2.*pi.*CT)) + 0.5.* (F_t.' * (CT\F_t));
        
        % it seems that G0 contribution is constant
%         V = logspace(3,5,100);
%         for K=1:1:numel(V)
%             DD = V(K);
%             LogLikeF(K) = 0.5.*abs(TimeDelay.logdet(2.*pi.*CT)) + 0.5.* (F_t.' * ((CT+DD)\F_t));
%         end
        
        
    else
        error('Time series in either time domain or frequency domain should be supplied');
    end
end
