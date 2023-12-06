function [Z2Prefactors,Norm] = translientAuxiliary(Pn, Pr, SigmaN, SigmaR, Args)
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
    % Author : Amir Sharon (July 2022)
    % Example: Size=300; 
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [Z2Prefactors,Norm] = imUtil.properSub.translientAuxiliary(Pn, Pr,1,1);

    arguments
        Pn        % PSF in the corner
        Pr        % must have the same size as Pn, with PSF in the corner
        SigmaN
        SigmaR

        Args.IsPsfFFT(1,1) logical    = false;
        Args.ShiftPsf(1,1) logical    = false;

        Args.Eps                      = 0;
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

    [Nrows,Ncols]     = size(Pnhat);

    % because the linearization of the translation phase (Delta*K), 
    % it is not 2*Pi periodic, use negative frequancies.
    FreqArrRows = fftshift(-ceil(Nrows/2):(floor(Nrows/2)-1));
    FreqArrCols = fftshift(-ceil(Ncols/2):(floor(Ncols/2)-1));

    [Kx,Ky] = meshgrid(FreqArrCols,FreqArrRows);

    Kxy = reshape([Kx/Ncols,Ky/Nrows],Nrows,Ncols,2);

    Zden = abs(Prhat).^2 .* SigmaN.^2 + abs(Pnhat).^2 .*SigmaR.^2 + Args.Eps;
    Znom = 4*pi * conj(Pnhat).*conj(Prhat);

    % Kxy is a vector so now Z2Prefactors is two matrices, i.e., 
    % it is a higher dimension matrix with an x and an y component
    Z2Prefactors = Znom./Zden.*Kxy;

    Term1 = ifft2(Z2Prefactors.*SigmaR.*Pnhat);
    Term2 = ifft2(Z2Prefactors.*SigmaN.*Prhat);
    
    %Norm = sum(imag(Term1).^2,3)+sum(imag(Term2).^2,3);
    Norm = sum(imag(Term1(:)).^2+imag(Term2(:)).^2)/2;
         
end

