function [GaborBank, Kx, Ky]=gaborFromPSF(PSF, Args)
    %Mosleh, A., Langlois, J.M.P., Green, P. (2014). 
    %{
    Get a bank of Gabor filters derived from local frequency minima of a PSF.
    Input : - PSF (Matrix holding the PSF).
            * ...,key,val,...
              'Phase' - Phase offsets of sinusoid, given as an array in rad. 
                     Default is [0].
              'SigX' - Standard deviation of the Gaussian envelope in x.
                     Default is 1.
              'SigY' - Standard deviation of the Gaussian envelope in y.
                     Default is 1.
              'Theta' - Rotation angle of Gaussian envelope. Default is 0 rad.
    Output :- GaborBank (Cube containing Gabor filters).
            - Kx (Wavelengths of local minima in x-direction).
            - Ky (Wavelengths of local minima in y-direction).
    Author : Ruslan Konno (Apr 2024)
    Example: PSF = fspecial('gaussian', [25 25], 1);
             GaborBank = imUtil.filter.gaborFromPSF(PSF);
    %}

    arguments
        PSF

        Args.Phase = [0];
        Args.SigX = 1;
        Args.SigY = 1;
        Args.Theta = 0;
    end

    % Get frequency magnitude from PSF
    PSF_hat = ifftshift(PSF);
    PSF_hat = fft2(PSF_hat);
    K_hat = real(PSF_hat);

    % Find local frequency minima 
    % and corresponding frequencies (here as wavelengths)
    TF = islocalmin(K_hat, 1).*islocalmin(K_hat, 2);
    [Kx, Ky] = find(TF);
    
    % Get sizes and pre-allocate memory
    sizeTF = size(TF);
    N = sizeTF(1);
    M = sizeTF(2);

    Nfreq = numel(Kx);
    Nphase = numel(Args.Phase);
    Nfilters = Nfreq*Nphase;
    GaborBank = zeros(sizeTF(1), sizeTF(2), Nfilters);

    % Get Gabor filter for each combination 
    % of wavelength pairs and phase offsets

    j = 0;
    for Iphase=1:Nphase
        for Ifreg=1:Nfreq
            j = j + 1;
            Gabor = imUtil.filter.gabor2d(N,M,Kx(Ifreg),Ky(Ifreg),...
                'SigX',Args.SigX,'SigY',Args.SigY,...
                'Phase',Args.Phase(Iphase), 'Theta', Args.Theta);
            GaborBank(:,:,j) = Gabor;
        end
    end
 
end