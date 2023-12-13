function [Result] = fitPSF1d(Image, SpatPos, Args)
    % Given a linear spectrum in a 2D image, fit the flux of a line-PSF to each wavelength.
    %   This function assumes that the spectrum in a 2D image is in a
    %   vertical or horizontal form, and it fits only the flux level.
    %   Assuming the spectrum is background subtracted.
    % Input  : - A 2D matrix containing a spectrum, where the spectrum is
    %            either horizontal or vertical.
    %          - Spatial position of the spectrum. If empty, then will
    %            assume it is in (Nspat-1).*0.5, where Nspat is the number
    %            of spatial pixels.
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (Dec 2023) 
    % Example: imUtil.spec.extract.fitPSF1d

    arguments
        Image
        SpatPos                = [];
        Args.DimWave           = 2;
        Args.PSF               = @(Sigma, Mu, X) normpdf(X, Mu, Sigma);         % line PSF
        Args.InterpMethod      = 'cubic';
        
        Args.ParPSF            = [2, 3, 4];
        Args.FunPSF            = @(Sigma, Mu, X) normpdf(X, Mu, Sigma);
        Args.InitPos           = [];  % initial position of trace
        Args.FixedPos          = [];  % assume trace position is exactly known
    end

    % Convert to wave dir is in 2nd dim.
    if Args.DimWave==1
        Image = Image.';
    end
        
    % number of pixels in each axis
    [Nspat, Nwave] = size(Image);
    
    if isempty(SpatPos)
        SpatPos = (Nspat - 1).*0.5;
    end
    
    % prepare the PSF for the fit
    if isa(Args.PSF, 'function_handle')
        % evaluate PSF
        PSF = Args.PSF
        
    end
    
    Yxx=tools.interp.interp1_sinc(Args.PSF
    
    
end
