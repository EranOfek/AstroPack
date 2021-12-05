function [LogL_xF,LogL_GF,LogL_F]=logl_x_given_F(Pars, FitPar, Limits, w, G_w, t, F_t, F_w, sigma_F_hat, sigma_x_hat, Min_w, DFT, DFT_dagger, LogZ, Gamma_1_)
% Return the -logL of x | F - of the astrometric-flux time-delay method
% (Old version)
% Package: +TimeDelay
% Description: Return the -log(L) of x|F of the astrometric-flux
%              time-delay method of Springer & Ofek for the two images
%              case.
%              If no arguments are provided then run in simulation mode.
% Input  : - A vector of parameters to fit:
%            [Tau, Alpha0, Alpha1, Alpha2, x0, x1, x2, gamma]
%          - A vector of flags indicating which parameter to fit, or to use
%            as constant. The vector is for the following parameters:
%            [Tau, Alpha0, Alpha1, Alpha2, x0, x1, x2, gamma]
%            If NaN, then fit the parameter, otherwise, don't fit this
%            parameter and use the value as the parameter value.
%          - A two column matrix of [lower, upper] bounds on each one of
%            the parameters.
%          - Vector of angular frequency (2*pi*f) corresponding to the
%            observations.
%          - Gx_w: Vector of G(x)=x(w)*f(w) in the frequency domain.
%          - F_t: Vector of the total flux as a function of time.
%          - F_w: Vector of the total flux as a function of ang. frequency.
%          - sigma_F_hat - error in F_hat.
%          - sigma_x_hat - error in x_hat and y_hat (assuming they are
%                          identical).
%          - min(w) - Default is 2.*pi./1000
%          - DFT - The DFT matrix. If not provided, then the default is:
%                  fft(eye(N), N, 1) ./ sqrt(N).
%          - DFT_dagger - The dagger of the DFT matrix. If not provided the
%                  the default is: DFT'.
%          - LogZ - The sum(log(F_t)) scalar. If not orovided then will be
%                  calculated from the input.
%          - Gamma_1_x - The Gamma_1 paarameter. Default is
%                  Gamma_1_x = ((DFT * diag(F_t.^2)) * DFT_dagger) * sigma_x_hat^2;
% Output : - -LogL_xF
%          - -LogL_GF
%          - -LogL_F
% Example: [LogL_xF,LogL_GF,LogL_F]=TimeDelay.logl_x_given_F

if nargin==0
    params = jsondecode(fileread('/home/eran/matlab/TimeDelay/Ofer/qtd_sim_4/sims_4.json'));

    t   = load('/home/eran/matlab/TimeDelay/Ofer/qtd_sim_4/output_txt/sims_4/t.txt')';
    F_t = load('/home/eran/matlab/TimeDelay/Ofer/qtd_sim_4/output_txt/sims_4/F_t.txt')';
    x_t = load('/home/eran/matlab/TimeDelay/Ofer/qtd_sim_4/output_txt/sims_4/x_t.txt')';

    sigma_x = params.sigma_x_prop * abs(params.x_1 - params.x_2);
    sigma_F = params.sigma_F_prop * params.f_dc;

    N = length(t);
    freqs = TimeDelay.fft_freq(N, params.t_step);

    gamma = params.gamma;
    sigma_F_hat = sigma_F;
    sigma_x_hat = sigma_x;
    w = 2.*pi*freqs;
    DFT = fft(eye(N), N, 1) ./ sqrt(N);
    DFT_dagger = DFT';
    x0 = 0;
    x1 = params.x_1;
    x2 = params.x_2;
    Alpha0 = params.alpha_0;

    Tau = params.tau;
    Alpha1 = params.alpha_1;
    Alpha2 = params.alpha_2;
    F_w = fft(F_t) ./ sqrt(N);

    G_t = x_t.*F_t;
    G_w = fft(G_t) ./ sqrt(N);

    LogZ = sum(log(F_t));
    
    Limits = [5 100; 0 2; 0 2; 0 2;-1 1; -2 2;-2 2; 1.5 3.5];
else
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
   
end

w = w(:).';


%[Tau, Alpha0, Alpha1, Alpha2, x0, x1, x2, gamma]

%Tau = 18
%Alpha1 = 2
%Alpha2 = 0.7

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
       

    %mu_G_given_F_ = mu_G_given_F(F_w, tau, alpha_1, alpha_2);


    %mu_epsilon_F_given_F_ = mu_epsilon_F_given_F(F_w, tau, alpha_1, alpha_2);

    %Sigma_phi_ = Sigma_phi(tau, alpha_1, alpha_2);

    N = numel(w);

    if nargin<15
        DFT = fft(eye(N), N, 1) ./ sqrt(N);
        DFT_dagger = DFT';
        Gamma_1_ = ((DFT * diag(F_t.^2)) * DFT_dagger) * sigma_x_hat^2;
        if nargin<14
            LogZ = sum(log(F_t));
            if nargin<13
                DFT_dagger = DFT';
                if nargin<12
                    DFT = fft(eye(N), N, 1) ./ sqrt(N);
                    if nargin<11
                        Min_w = 2.*pi./1000;
                    end
                end
            end
        end
    end
    


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
    InPar.EndMatching = false;
    if InPar.EndMatching
        G_t_ = ifft(G_w).*sqrt(N);
        G_t_ = TimeDelay.end_matching((1:1:N),G_t_);
        G_w  = fft(G_t_)./sqrt(N);
    end
    
    w_ = w;
    w_(1) = 1.0;
    
    if numel(Min_w)==1
        Flag_w = abs(w)>Min_w;
    else
        Flag_w = abs(w)>Min_w(1) & abs(w)<Min_w(2);
    end
    
    Sigma_phi_ = (Alpha1.^2 + Alpha2.^2 + 2.*Alpha1.*Alpha2.*cos(w.*Tau)) .* (abs(w_).^(-gamma));
    Sigma_F    = Sigma_phi_ + sigma_F_hat.^2;
    
    % attempting to apply end_matching only when calcualting LogL_F
    InPar.EndMatching = false;
    if InPar.EndMatching
        F_tEM = TimeDelay.end_matching(t,F_t);
        F_wEM = fft(F_tEM)./sqrt(N);
        Power_F    = (abs(F_wEM).^2);
        
        %F_t = F_tEM;
        %F_w = F_wEM;
    else
        Power_F    = (abs(F_w).^2);
    end
    
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
