function [Z2Prefactors,Norm] = translientAuxiliary_fast(Pn, Pr, SigmaN, SigmaR, Args)
    % Speed up of imUtil.properSub.translientAuxiliary
    % Normalization of the TRANSLIENT score image. 
    %       The function returns the normalization factor Norm so that for 
    %       pure noise, Z2/Norm is distributed as a chi-squared
    %       distribution with 2 degrees of freedom.
    % Input  : - The PSF of the new image N. The PSF image size must be
    %            equal to the N and R image sizes, and the PSF center
    %            should be located at pixel 1,1 (corner).
    %            The input may be in the image domain or Fourier domain
    %            (i.e., 'IsPsfFFT'=true).
    %          - Like Pn, but the PSF for the reference image.
    %          - (SigmaN) the standard deviation of the background new
    %            image.
    %          - (SigmaR) the standard deviation of the background
    %            reference image.  
    %          * ...,key,val,...
    %            'IsPsfFFT' - A logical indicating if the input Pn and Pr
    %                   PSFs are in Fourier domain. Default is false.
    %            'ShiftPsf' - A logical indicating if to fftshift the input
    %                   Pn and Pr PSFs. Default is false.
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    % Output : - (Norm) Normalization factor so that Z2/Norm is distributed
    %            as a chi-squared dist. with 2 degrees of freedom. 
    %          
    % Author : Ruslan Konno (April 2025)
    % Example: Size=300; 
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [Z2Prefactors,Norm] = imUtil.properSub.translientAuxiliary_fast(Pn, Pr,1,1);

    arguments
        Pn        % PSF in the corner
        Pr        % must have the same size as Pn, with PSF in the corner
        SigmaN
        SigmaR

        Args.IsPsfFFT(1,1) logical    = false;
        Args.ShiftPsf(1,1) logical    = false;

        Args.Eps                      = 0;

        Args.Kx = [];
        Args.Ky = [];
    end

    if Args.IsPsfFFT
        Pnhat = Pn;
        Prhat = Pr;
    else
        Pnhat = fft2(Pn);
        Prhat = fft2(Pr);
    end
    if Args.ShiftPsf
        Pnhat = fftshift(Pnhat);
        Prhat = fftshift(Prhat);
    end

    if isempty(Args.Kx) || isempty(Args.Ky)

        [Nrows,Ncols]     = size(Pnhat);
    
        % because the linearization of the translation phase (Delta*K), 
        % it is not 2*Pi periodic, use negative frequancies.
        FreqArrRows = fftshift(-ceil(Nrows/2):(floor(Nrows/2)-1));
        FreqArrCols = fftshift(-ceil(Ncols/2):(floor(Ncols/2)-1));
    
        [Args.Kx, Args.Ky] = meshgrid(FreqArrCols,FreqArrRows);

        Args.Kx = Args.Kx/ Ncols;
        Args.Ky = Args.Ky/ Nrows;
    end

    Zden = abs(Prhat).^2 .* SigmaN.^2 + abs(Pnhat).^2 .* SigmaR.^2 + Args.Eps;
    Znom = 4 * pi * conj(Pnhat) .* conj(Prhat);
    ZdenInv = 1 ./ Zden;
    
    ZnomScaled = Znom .* ZdenInv;
    
    Z2PrefX = ZnomScaled .* Args.Kx;
    Z2PrefY = ZnomScaled .* Args.Ky;
    Z2Prefactors = cat(3, Z2PrefX, Z2PrefY);

    SigmaR_Pnhat = SigmaR .* Pnhat;
    SigmaN_Prhat = SigmaN .* Prhat;
    
    Term1X = ifft2(Z2PrefX .* SigmaR_Pnhat);
    Term1Y = ifft2(Z2PrefY .* SigmaR_Pnhat);
    Term2X = ifft2(Z2PrefX .* SigmaN_Prhat);
    Term2Y = ifft2(Z2PrefY .* SigmaN_Prhat);
    
    Norm = ( ...
        sum(imag(Term1X(:)).^2) + sum(imag(Term1Y(:)).^2) + ...
        sum(imag(Term2X(:)).^2) + sum(imag(Term2Y(:)).^2) ) / 2;
         
end

