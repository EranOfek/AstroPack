function [Res,G]=rand_lensed(varargin)
% Generate evenly spaced combined ligt curve and position of lensed quasar
% Package: +TimeDelay
% Description: Given a two image lensed quasar, generate an evenly spaced 
%              combined flux light curve and center-of-light position.
%          * Pairs of ...,key,val,... The following keys are available:
%            'Cyclic' - Cyclic simulation. Default is false.
%            'EndMatching' - Default is true.
%            'A0' - \alpha_0. Default is 0.
%            'A' - Vector of \alpha_i. Default is [1 0.66]
%            'Tau' - Time delay. Default is 14.7.
%            'x0' - x position of lensing galaxy. Default is 0.
%            'y0' - y position of lensing galaxy. Default is 0.
%            'x' - Vector of X position of images. Default is [1 -1].
%            'y' - Vector of y position of images. Default is [0 0].
%            'f_dc'
%            'Gamma'
%            'TotTime'
%            'DeltaT'
%            'sigma_x'
%            'sigma_F_rel'
%            'Validate'
%            'StdMeanRange'
% Output : - The combined light curve.
%          - The center-of-light position of the images.
%          - sigma_F_hat
%          - sigma_x_hat
%          - w - The angular frequency of the fft.
%          - F_w - The fft of the combined light curve.
%     By : Eran O. Ofek               Oct 2020
% Example: [Res]=TimeDelay.rand_lensed;

CyclicFactor = 2;


InPar = inputParser;
addOptional(InPar,'Cyclic',false);
addOptional(InPar,'EndMatching',true);
addOptional(InPar,'AliasFactor',10);
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
addOptional(InPar,'Slope',0);

addOptional(InPar,'Validate',true);  % if LC is not positive than recalc
addOptional(InPar,'StdMeanRange',[0.1 0.2]);  % if LC is not positive than recalc

addOptional(InPar,'InterpMethod','pchip');
parse(InPar,varargin{:});
InPar = InPar.Results;


AliasFactor = InPar.AliasFactor;


if AliasFactor~=1
    InPar.DeltaT = InPar.DeltaT./AliasFactor;
end

A     = InPar.A;
A0    = InPar.A0;
Tau   = InPar.Tau;
f_dc  = InPar.f_dc;
Gamma = InPar.Gamma;


if ~InPar.Cyclic
    T = (1:InPar.DeltaT:CyclicFactor.*InPar.TotTime).';
else
    T = (1:InPar.DeltaT:InPar.TotTime).';
end

Nt = numel(T);


Freqs = TimeDelay.fft_freq(Nt, InPar.DeltaT);
w = 2.*pi.*Freqs(:);

% transform the x,y coordinates to be relative to the lensing galaxy x0,y0
InPar.x = InPar.x - InPar.x0;
InPar.y = InPar.y - InPar.y0;




%[Sigma_phi,Sigma_F] = TimeDelay.sigma_phi(w,Tau,A,Gamma,Epsilon_hat_F);


w_ = w;
w_(1) = 1;
Sigma_red_ = w_.^(-Gamma);

if InPar.sigmaOutTot
    sigma_F = InPar.sigma_F_rel .* f_dc.*sum(InPar.A);
    Res.sigma_F_rel2abs = f_dc.*sum(InPar.A);
else
    sigma_F = InPar.sigma_F_rel .* f_dc;
    Res.sigma_F_rel2abs = f_dc;
end

f_dc_w = f_dc.*sqrt(Nt);

sigma_x_hat = InPar.sigma_x;
sigma_F_hat = sigma_F;



Sigma_eps_ = ones(size(w)).*sigma_F_hat.^2;
eps_F_w = TimeDelay.rand_psd(Sigma_eps_);
f_red_w = TimeDelay.rand_psd(Sigma_red_);





eps_F_w(1) = randn(1,1).*sigma_F_hat;
f_red_w(1) = f_dc_w;

%f1_w = A(1) .* f_red_w;
%f2_w = A(2) .* f_red_w .* exp(1j.*w.*Tau(1));

% need to justify the multiplication by sqrt(AliasFactor)
f1_w = A(1).*sqrt(InPar.AliasFactor) .* f_red_w;
f2_w = A(2).*sqrt(InPar.AliasFactor) .* f_red_w .* exp(1j.*w.*Tau(1));

% add slope
if InPar.Slope~=0
    f1_S = ifft(f1_w);
    N_S  = numel(f1_S);
    DSDt = InPar.Slope./InPar.TotTime./InPar.AliasFactor;   % slope per unit time
    ttt  = (1:1:N_S) - N_S./2;
    f1_S = f1_S + ttt(:).*DSDt;
    
    f1_w = fft(f1_S);
end


phi_w = f1_w + f2_w;
phi_w(1) = phi_w(1) + A0 .* Nt;
F_w = phi_w + eps_F_w;


phi_t = real(ifft(phi_w(:))).*sqrt(Nt);
F_t   = real(ifft(F_w(:))).*sqrt(Nt);
f1_t  = real(ifft(f1_w(:))).*sqrt(Nt);
f2_t  = real(ifft(f2_w(:))).*sqrt(Nt);

eps_x_t = randn(Nt,1) .* InPar.sigma_x;
chi_x_t = (InPar.x(1) .* f1_t + InPar.x(2) .* f2_t) ./ phi_t;
% Note we are adding x0 back...
x_t = InPar.x0 + chi_x_t + eps_x_t;

Res.T           = T;
Res.F_t         = F_t;
Res.x_t         = x_t;
Res.sigma_F_hat = sigma_F_hat;
Res.sigma_x_hat = sigma_x_hat;
Res.sigma_F_rel = InPar.sigma_F_rel;
Res.sigma_x     = InPar.sigma_x;
Res.w           = w;
Res.F_w         = F_w;
Res.f1_t        = f1_t;
Res.f2_t        = f2_t;
Res.chi_x_t     = chi_x_t;


Res.StdDivMeanFlux = std(Res.F_t)./mean(Res.F_t);


if ~isempty(InPar.y)    
    eps_y_t = randn(Nt,1) .* InPar.sigma_x;
    chi_y_t = (InPar.y(1) .* f1_t + InPar.y(2) .* f2_t) ./ phi_t;
    % Note we are adding y0 back...
    y_t = InPar.y0 + chi_y_t + eps_y_t;
    
    Res.y_t = y_t;
else
    Res.y_t = [];
end

if ~InPar.Cyclic
    Res.F_t_orig = Res.F_t;
    IndEnd = floor(Nt./CyclicFactor);
    Res.T   = Res.T(1:IndEnd);
    Res.F_t = Res.F_t(1:IndEnd);
    Res.x_t = Res.x_t(1:IndEnd);
    if ~isempty(Res.y_t)
        Res.y_t = Res.y_t(1:IndEnd);
    end
    
    Freqs = TimeDelay.fft_freq(IndEnd, InPar.DeltaT);
    Res.w = 2.*pi.*Freqs(:);
    Res.F_w = fft(Res.F_t) ./ sqrt(IndEnd);  % ortho normalization
    
    Res.StdDivMeanFlux = std(Res.F_t)./mean(Res.F_t);
    
    Res.f1_t        = Res.f1_t(1:IndEnd);
    Res.f2_t        = Res.f2_t(1:IndEnd);
    Res.chi_x_t     = Res.chi_x_t(1:IndEnd);
    
end
    
if AliasFactor~=1
    % resample the light curve
    
    Res.T  = Res.T(1:AliasFactor:end);
    Res.F_t = Res.F_t(1:AliasFactor:end);
    Res.x_t = Res.x_t(1:AliasFactor:end);
    if ~isempty(Res.y_t)
        Res.y_t = Res.y_t(1:AliasFactor:end);
    end
    
    Nt = numel(Res.T);
    Freqs = TimeDelay.fft_freq(Nt, InPar.DeltaT.*AliasFactor);
    Res.w = 2.*pi.*Freqs(:);
    Res.F_w = fft(Res.F_t) ./ sqrt(Nt);  % ortho normalization
    
    Res.StdDivMeanFlux = std(Res.F_t)./mean(Res.F_t);
    
    Res.f1_t        = Res.f1_t(1:AliasFactor:end);
    Res.f2_t        = Res.f2_t(1:AliasFactor:end);
    Res.chi_x_t     = Res.chi_x_t(1:AliasFactor:end);
    
end

if InPar.Validate && (any(Res.F_t<0) || Res.StdDivMeanFlux<InPar.StdMeanRange(1) || Res.StdDivMeanFlux>InPar.StdMeanRange(2))
    S = std(Res.F_t);
    M = min(Res.F_t);
    Factor = mean(InPar.StdMeanRange)./(S./(InPar.f_dc.*sum(InPar.A)));
    [Res]=TimeDelay.rand_lensed(varargin{:},'f_dc',InPar.f_dc./Factor);
end


if nargout>1
    N = numel(Res.F_t);
    
    G.F_w = fft(Res.F_t) ./ sqrt(N);
    G.Gx_t = Res.x_t.*Res.F_t;
    G.Gx_w = fft(G.Gx_t) ./ sqrt(N);
    G.Gy_t = Res.y_t.*Res.F_t;
    G.Gy_w = fft(G.Gy_t) ./ sqrt(N);

end

Res.Mean = mean(Res.F_t);


if InPar.EndMatching
    %ParPoly = polyfit(Res.T, Res.F_t, 1);
    %ParPoly(2) = 0;
    %Res.F_t = Res.F_t - polyval(ParPoly,Res.T);
    %N = numel(Res.F_t);
    %Res.F_w = fft(Res.F_t)./sqrt(N);
    
    % end matching
    [Res.F_t] = TimeDelay.end_matching(Res.T,Res.F_t);
    
    N = numel(Res.F_t);
    Freqs = TimeDelay.fft_freq(N, InPar.DeltaT.*AliasFactor);
    Res.w = 2.*pi.*Freqs(:);
    Res.F_w = fft(Res.F_t)./sqrt(N);
end
    
    