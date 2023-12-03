function [Result] = fitPSF1d(Image, Args)
    % Given a 2D spectrum, fit a line-PSF to each wavelength
    % 
    % Input  : - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (Dec 2023) 
    % Example: imUtil.spec.extract.fitPSF1d

    arguments
        Image
        Args.DimWave           = 2;
        Args.PSF               = @(Sigma, Mu, X) normpdf(X, Mu, Sigma);         % line PSF
        Args.InterpMethod      = 'cubic';
        
        Args.ParPSF            = [2, 3, 4];
        Args.FunPSF            = @(Sigma, Mu, X) normpdf(X, Mu, Sigma);
        Args.InitPos           = [];  % initial position of trace
        Args.FixedPos          = [];  % assume trace position is exactly known
    end

    % Convert to wave dir is in 1st dim.
    if Args.DimWave==2
        Image = Image.';
    end
    
    % number of pixels in each axis
    [Ndisp, Nspat] = size(Image);
    
    
    % prepare the PSF for the fit
    if isa(Args.PSF, 'function_handle')
        % evaluate PSF
        Args.PSF
        
    end
    
    Yxx=tools.interp.interp1_sinc(Args.PSF
    
    
end
