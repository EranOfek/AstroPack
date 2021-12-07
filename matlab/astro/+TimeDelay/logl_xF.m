function [LogL_xF,LogL_GF,LogL_F]=logl_xF(Pars,varargin)
% Return the -logL of x,F - of the astrometric-flux time-delay method
% Package: +TimeDelay
% Description: Return the -log(L) of x|F of the astrometric-flux
%              time-delay method of Springer & Ofek for the two images
%              case.
%              If no arguments are provided then run in simulation mode.
% Input  : - Vector of parameters to use.
%            This can be any subset of [Tau, Alpha0, Alpha1, Alpha2, x0, x1, x2, gamma]
%          * Arbitrary number of ...,key,val,...
%            'FitPar' - A vector of [Tau, Alpha0, Alpha1, Alpha2, x0, x1, x2, gamma]
%                   If value is NaN, then will use its value from the first
%                   input argument (by order of apperance), otherwise will
%                   use the indicated value.
%                   Default is [0 0 NaN   NaN      0   1   -1        3].
%            'Limits' - A two column matrix of lower/upper limits on the
%                   paramaeters.
%                   Default is [-300 300; 0 3; 1e-5 5;0 5;  -1 1; -2.1 2.1; -2.1 2.1;     1.5 3.5].
%                   Note that this is including Tau.
%            'w' - A vecor of angular frequencies (observations).
%            'F_w' - The FFT of F(t).
%            't'   - A vector of times t.
%            'F_t' - A vector of combined fluxe F(t).
%            'Gx_w' - FT(F(t)x(t)) - If empty, will calculate. Default is
%                   empty.
%            'sigma_F' - Error in F(t).
%            'sigma_x' - Error in x(t).
%            'Min_w' - A vector of [lower, upper] frequencies top reject.
%                   Default is [2.*pi./10000, Inf].
%            'DFT'  - DFT matrix. Calculate if empty. Default is empty.
%            'DFT_dagger' - DFT dagger matrix. Calculate if empty. Default is empty.
%            'LogZ' - Log(Z) - Calculate if empty. Default is empty.
%            'Gamma_1_' -  Gamma(1) - Calculate if empty. Default is empty.
%            'EndMatching' - Apply end-matching. Default is true.
%            'TwoD' - NOT OPERATIONAL.
%            'Verbose' - Default is true.
% Output : - -LogL_xF
%          - -LogL_GF
%          - -LogL_F
% Example: [LogL_xF,LogL_GF,LogL_F]=TimeDelay.logl_xF([1 0.5],...)



InPar = inputParser;

addOptional(InPar,'FitPar',[0 0 NaN   NaN      0   1   -1        3]);  % [Tau A0, A1, A2, x0, x1, x2, y0, y1, y2, gamma]
addOptional(InPar,'Limits',[-300 300; 0 3; 1e-5 5;0 5;  -1 1; -2.1 2.1; -2.1 2.1;     1.5 3.5]); %  with Tau

addOptional(InPar,'w',[]);  
addOptional(InPar,'t',[]);  
addOptional(InPar,'x_t',[]);  
addOptional(InPar,'Gx_w',[]);
addOptional(InPar,'F_t',[]);
addOptional(InPar,'F_w',[]);
addOptional(InPar,'sigma_F',[]);
addOptional(InPar,'sigma_x',[]);

addOptional(InPar,'Min_w',[2.*pi./10000, Inf]);
addOptional(InPar,'DFT',[]);
addOptional(InPar,'DFT_dagger',[]);
addOptional(InPar,'LogZ',[]);
addOptional(InPar,'Gamma_1_',[]);

addOptional(InPar,'EndMatching',true);
addOptional(InPar,'TwoD',false);

addOptional(InPar,'Verbose',true);
parse(InPar,varargin{:});
InPar = InPar.Results;


F_t = InPar.F_t;

t = InPar.t;

N = length(F_t);
t_step = unique(diff(t));
if numel(t_step)>1
    error('Time series must be equally spaced');
end

    
sigma_F_hat = InPar.sigma_F;
sigma_x_hat = InPar.sigma_x;
sigma_y_hat = InPar.sigma_x;

if isempty(InPar.w)
    freqs = TimeDelay.fft_freq(N, t_step);
    w = 2.*pi*freqs;
else
    w = InPar.w;
end
w = w(:).';

if isempty(InPar.F_w)
    F_w = fft(F_t) ./ sqrt(N);
else
    F_w = InPar.F_w;
end

x_t = InPar.x_t;
if isempty(InPar.Gx_w)
    Gx_t = x_t.*F_t;
    Gx_w = fft(Gx_t) ./ sqrt(N);
  
    %Gy_t = y_t.*F_t;
    %Gy_w = fft(Gy_t) ./ sqrt(N);
else
    Gx_w = InPar.Gx_w;
end
G_w = Gx_w;
    
if isempty(InPar.DFT)
    DFT = fft(eye(N), N, 1) ./ sqrt(N);
else
    DFT = InPar.DFT;
end

if isempty(InPar.DFT_dagger)
    DFT_dagger = DFT';
else
    DFT_dagger = InPar.DFT_dagger;
end

if isempty(InPar.LogZ)
    LogZ = sum(log(F_t));
else
    LogZ = InPar.LogZ;
end

if isempty(InPar.Gamma_1_)
    Gamma_1_ = ((DFT * diag(F_t.^2)) * DFT_dagger) * sigma_x_hat^2;
else
    Gamma_1_ = InPar.Gamma_1_;
end





%    Tau    = Pars(1);
%    Alpha0 = Pars(2);
%    Alpha1 = Pars(3);
%    Alpha2 = Pars(4);
%    x0     = Pars(5)
%    x1     = Pars(6);
%    x2     = Pars(7);
%    gamma  = Pars(8);
   
% FitPar is a 8 element vector
% each elemnt is either NaN - if parameter should be fitted
%             or a value - if a parameter should be fixed to this value
FitPar = InPar.FitPar;
Ind = 0;
if ~isnan(FitPar(1))
   Tau = FitPar(1);
else
   Ind = Ind + 1;
   Tau = Pars(Ind);
end
if ~isnan(FitPar(2))
   Alpha0 = FitPar(2);
else
   Ind = Ind + 1;
   Alpha0 = Pars(Ind);    
end
if ~isnan(FitPar(3))
   Alpha1 = FitPar(3);
else
   Ind = Ind + 1;
   Alpha1 = Pars(Ind);    
end
if ~isnan(FitPar(4))
   Alpha2 = FitPar(4);
else
   Ind = Ind + 1;
   Alpha2 = Pars(Ind);    
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
if ~isnan(FitPar(8))
   gamma = FitPar(8);
else
   Ind = Ind + 1;
   gamma = Pars(Ind);    
end
   
Limits = InPar.Limits;
if Tau<Limits(1,1) || Tau>Limits(1,2) || ...
       Alpha0<Limits(2,1) || Alpha0>Limits(2,2) || ...
       Alpha1<Limits(3,1) || Alpha1>Limits(3,2) || ...
       Alpha2<Limits(4,1) || Alpha2>Limits(4,2) || ...
       x0<Limits(5,1) || x0>Limits(5,2) || ...
       x1<Limits(6,1) || x1>Limits(6,2) || ...
       x2<Limits(7,1) || x1>Limits(7,2) || ...
       gamma<Limits(8,1) || gamma>Limits(8,2)
    LogL_xF = Inf;
    LogL_GF = NaN;
    LogL_F  = NaN;
else
    
    % move to x0 coordinate system
    x1 = x1 - x0;
    x2 = x2 - x0;
    x0 = 0;
       
    
    if InPar.EndMatching
        [x_t,F_t] = TimeDelay.end_matching_xt(t,x_t,F_t,x0,[x1 x2],Alpha0,[Alpha1,Alpha2],sigma_F_hat,sigma_x_hat);
        
        %[F_t,Slope,Diff]=TimeDelay.end_matching(t,F_t);
        F_w = fft(F_t) ./ sqrt(N);
        
        %x_t = x_t +0.5.*Diff.*(x1+x2) ;
        
        
        Gx_t = x_t.*F_t;
        Gx_w = fft(Gx_t) ./ sqrt(N);
  
    end
    

    %mu_G_given_F_ = mu_G_given_F(F_w, tau, alpha_1, alpha_2);


    %mu_epsilon_F_given_F_ = mu_epsilon_F_given_F(F_w, tau, alpha_1, alpha_2);

    %Sigma_phi_ = Sigma_phi(tau, alpha_1, alpha_2);

    N = numel(w);

    


    % parameters should be row vectors
    % Tau    = Tau(:).';
    % Alpha1 = Alpha1(:).';
    % Alpha2 = Alpha2(:).';
    % gamma  = gamma(:).';

    % w should be a row vector
    G_w = G_w(:).';
    F_t = F_t(:).';
    F_w = F_w(:).';

    % Apply end matching to G
%     InPar.EndMatching = false;
%     if InPar.EndMatching
%         G_t_ = ifft(G_w).*sqrt(N);
%         G_t_ = TimeDelay.end_matching((1:1:N),G_t_);
%         G_w  = fft(G_t_)./sqrt(N);
%     end
    
    w_ = w;
    w_(1) = 1.0;
    
    if numel(InPar.Min_w)==1
        Flag_w = abs(w)>InPar.Min_w;
    else
        Flag_w = abs(w)>InPar.Min_w(1) & abs(w)<InPar.Min_w(2);
    end
    
    Sigma_phi_ = (Alpha1.^2 + Alpha2.^2 + 2.*Alpha1.*Alpha2.*cos(w.*Tau)) .* (abs(w_).^(-gamma));
    Sigma_F    = Sigma_phi_ + sigma_F_hat.^2;
    
    % attempting to apply end_matching only when calcualting LogL_F
%     InPar.EndMatching = false;
%     if InPar.EndMatching
%         F_tEM = TimeDelay.end_matching(t,F_t);
%         F_wEM = fft(F_tEM)./sqrt(N);
%         Power_F    = (abs(F_wEM).^2);
%         
%         %F_t = F_tEM;
%         %F_w = F_wEM;
%     else
        Power_F    = (abs(F_w).^2);
    %end
    
    %LogL_F     = -0.5.*sum(Power_F(2:end) ./ Sigma_F(:,2:end)) - 0.5.*sum(log(2.*pi.*Sigma_F(:,2:end)));
    LogL_F     = -0.5.*sum(Power_F(Flag_w) ./ Sigma_F(Flag_w)) - 0.5.*sum(log(2.*pi.*Sigma_F(Flag_w)));

    
        
    Shrink = sigma_F_hat.^2 ./ (sigma_F_hat.^2 + Sigma_phi_);
    mu_epsilon_F_given_F_ = Shrink .* F_w;

    %X_hat_ = X_hat_of_F_w(F_w, tau, alpha_1, alpha_2);
    %x_t_ = x_of_F_w(F_w, tau, alpha_1, alpha_2));

    F_w_tilde = F_w;
    F_w_tilde(1) = F_w_tilde(1) - Alpha0.*N;

    %A_hat_ = A_hat(tau, alpha_1, alpha_2);
    num_ = Alpha1.*x1 + Alpha2.*x2.*exp(1j.*w.*Tau);
    denom_ = Alpha1 + Alpha2.*exp(1j.*w.*Tau);
    A_hat_ = num_ ./ denom_;

    F_t_ = real(ifft(F_w)) .* sqrt(N);
    x_t_ = (real(ifft(A_hat_.*F_w_tilde)) ./ F_t_) .* sqrt(N);

    X_t = diag(x_t_);
    X_hat_ = ((DFT * X_t) * DFT_dagger) ./N;  % TODO: check factor len(w)**-1

    %A_hat_ = A_hat(tau, alpha_1, alpha_2);


    %mu_ = transpose(A_hat_.*F_w) + (X_hat_ - diag(A_hat_)) * transpose(mu_epsilon_F_given_F_);
    mu_ = transpose(A_hat_.*F_w) + (X_hat_ - diag(A_hat_)) * transpose(mu_epsilon_F_given_F_);

    mu_G_given_F_ = transpose(mu_);

    G_minus_mu = G_w - mu_G_given_F_; 
    
    
    %[Gamma_G_given_F_, ~, ~] = Gamma_G_given_F(F_t, F_w, tau, alpha_1, alpha_2);
    %X_hat_ = X_hat_of_F_w(F_w, tau, alpha_1, alpha_2);

    %A_hat_ = A_hat(tau, alpha_1, alpha_2);
    X_minus_A = X_hat_ - diag(A_hat_);
    %Gamma_epsilon_F_given_F_ = Gamma_epsilon_F_given_F(tau, alpha_1, alpha_2);
    %Sigma_phi_ = Sigma_phi(tau, alpha_1, alpha_2);
    Gamma_epsilon_F_given_F_ = real(diag((sigma_F_hat.^-2 + Sigma_phi_.^-1).^-1));


    % Gamma_1_ taken from the input arguments
    %Gamma_1_ = ((DFT * diag(F_t.^2)) * DFT_dagger) * sigma_x_hat^2;
    Gamma_2_ = (X_minus_A * Gamma_epsilon_F_given_F_) * X_minus_A';
    Gamma_G_given_F_ = Gamma_1_ + Gamma_2_;


    %G_minus_mu = G_minus_mu(2:end);
    G_minus_mu = G_minus_mu(Flag_w);
    
    %Gamma_G_given_F_ = Gamma_G_givenfalse_F_(2:end, 2:end);
    Gamma_G_given_F_ = Gamma_G_given_F_(Flag_w, Flag_w);
    
    
    logdet_ = TimeDelay.logdet(pi.*Gamma_G_given_F_);
    
    Fast = true;
    if ~Fast
        inv_Gamma_G_given_F = inv(Gamma_G_given_F_);
        mahal = conj(G_minus_mu) * (inv_Gamma_G_given_F * transpose(G_minus_mu));
    else
        mahal = conj(G_minus_mu) * (Gamma_G_given_F_\transpose(G_minus_mu));
    end
    LogL_GF = -real(logdet_) - real(mahal);
    
    
    %[LogL_F]=TimeDelay.logl_F(Pars, 'FitPar',FitPar,'Limits',Limits,'t',t,'F_t',F_t,'sigma_F',sigma_F_hat,'DT',(0:1:N-1))
    
    
    %LogL_xF = LogL_F + (-1./LogZ).*LogL_GF;
    LogL_xF = LogL_F + LogZ + LogL_GF;
    
    LogL_xF = -LogL_xF;
    LogL_GF = -LogL_GF;
    LogL_F  = -LogL_F;
    
    
    
    
    %LogL_xF = LogL_GF;
end
