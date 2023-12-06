function [FalsePositiveRate,TruePositiveRate] = translientROC(Alphan,Alphar,Pn,Pr,DeltaXY,Args)
    % Generate ROC curve for a given set of parameters with simulations. The SNR is determined
    %       with the Alphan and Alphar parameters while the image noise equals 1 in both reference and new images. 
    % Input  : - The point source flux level of the new image. Name chosen
    %            for consistency with Springer et al. (2022)
    %          - The point source flux level of the reference image.
    %          - The PSF of the new image. 
    %          - Like Pn, but the PSF for the reference image.
    %          - 2-element Vector of the translation of the point source.
    %          * ...,key,val,...
    %            'Nsim' - number of simulation.
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.

    % Output : - (FalsePositiveRate) False positive rate vector.
    %          - (TruePositiveRate) True positive rate vector that
    %          corresponds to FalsePositiveRate.
    % Author : Amir Sharon (August 2022)
    % Example: Size=64;  
    %          Pn = circshift(imUtil.kernel2.gauss([5,5,0],[Size Size]),-(ceil([Size Size]/2)-1));
    %          Pr=Pn;
    %          [FalsePositiveRate,TruePositiveRate] = imUtil.properSub.translientROC(100,100,Pn,Pr,[2 2])



arguments
    Alphan
    Alphar
    Pn
    Pr
    DeltaXY
    Args.Nsim       = 10000
    Args.Eps        = 0;
end

[Nrows,Ncols]     = size(Pn);

Pnhat = fft2(Pn);
Prhat = fft2(Pr);

EtaThr = zeros(2,Args.Nsim);

Noiser = randn(Nrows,Ncols,Args.Nsim);
Noisen = randn(Nrows,Ncols,Args.Nsim);
%
for i = 1:Args.Nsim
    R = Noiser(:,:,i);
    N = Noisen(:,:,i);
    Z2 = imUtil.properSub.translient(N, R, Pnhat, Prhat,1,1,'IsPsfFFT',true,'NormalizeZ2',true,'Eps',Args.Eps);
    EtaThr(1,i) = max(Z2,[],'all');
end
Sourcer = Alphar * fftshift(Pr);
Sourcen = Alphan * fftshift(Pn);
Sourcen = circshift(Sourcen,DeltaXY);

Noiser = randn(Nrows,Ncols,Args.Nsim);
Noisen = randn(Nrows,Ncols,Args.Nsim);
%
for i = 1:Args.Nsim
    R = Noiser(:,:,i)+Sourcer;
    N = Noisen(:,:,i)+Sourcen;
    Z2 = imUtil.properSub.translient(N, R,  Pnhat, Prhat,1,1,'IsPsfFFT',true,'NormalizeZ2',true,'Eps',Args.Eps);
    EtaThr(2,i) = max(Z2,[],'all');
end

Etaarr = logspace(log10(min(EtaThr(:))),log10(max(EtaThr(:))),ceil(Args.Nsim/10));

FalsePositiveRate = 1-histcounts(EtaThr(1,:),Etaarr,'Normalization','cdf');
FalsePositiveRate = [1 FalsePositiveRate];


TruePositiveRate = 1-histcounts(EtaThr(2,:),Etaarr,'Normalization','cdf');
TruePositiveRate = [1 TruePositiveRate];


end