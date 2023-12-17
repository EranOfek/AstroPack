function [Result] = aperPhot(Image, SpatPos, Args)
    % Given a linear spectrum in a 2D image, extract flux in aperture from each wavelength.
    %   This function assumes that the spectrum in a 2D image is in a
    %   vertical or horizontal form, and it fits only the flux level.
    %   Assuming the spectrum is background subtracted.
    % Input  : - A 2D matrix containing a spectrum, where the spectrum is
    %            either horizontal or vertical.
    %          - Spatial position of the spectrum. If empty, then will
    %            assume it is in (Nspat+1).*0.5, where Nspat is the number
    %            of spatial pixels.
    %          * ...,key,val,... 
    %            'DimWave' - Dimension of the wavelength axis.
    %                   Default is 2.
    %            'AperRadius' - Aperture radius, from trace pixel, in which
    %                   to calculate some statistics of the light (e.g.,
    %                   mean, std).
    %                   Default is 3.
    %            'BackAnnulus' - Region in which to calculate the
    %                   background. The annulus position for calcultaing
    %                   the background std are in 'BackStdArgs'.
    %                   Default is [15 20].
    %            'FlagImage' - An optional logical image, with the same
    %                   size as the input image. Pixels with false, will be
    %                   not used in the fitting process.
    %                   If empty, use a matrix of true for all pixels.
    %                   Default is [].
    %            'BackStd' - A vector of background std. If given, this will be used
    %                   in the calculation of the weights in the \chi^2
    %                   fitting. The length of this vector should be as the
    %                   wavelength dimension of the input matrix.
    %                   If 'fit' - then will call
    %                   imUtil.spec.extract.backStd.
    %                   Default is 'fit'.
    %            'BackStdArgs' - A cell array of additional arguments to
    %                   pass to: imUtil.spec.extract.backStd.
    %            'BackMean' - An optional vector of background level per
    %                   wavelength position. If empty, then will be
    %                   calculated using: imUtil.spec.extract.backStd.
    %                   Default is [].
    %            'Fun' - Function handle for calculating the statistics
    %                   over all pixels in the aperture, in each wavelength
    %                   position. Default is @mean.
    %            'FunArgs' - A cell array of additional arguments to pass
    %                   to 'Fun'. Default is {1, 'omitnan'}.
    %            'SubBack' - A logical indicating if to subtracted the
    %                   background measured in the annulus.
    %                   Default is true.
    % Output : - A structure with the following fields:
    %            .Wave - A vector of of wavelength pixel positions.
    %            .Spec - A vector of flux measured in aperture as a function of
    %                   wavelength pixel position. This quantity is the Fun
    %                   operated over the spatial dimension in each aperture.
    %            .Nused - A vector the number of pixel used in each wavelength
    %                   position (i.e., pixels flagged as good in 'FlagImag').
    %            .BackMean - Background mean as a function of wavelength
    %                   position.
    %            .BackStd - Background std as a function of wavelength
    %                   position.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [Result] = imUtil.spec.extract.aperPhot(randn(100,30), 'DimWave',1)

    arguments
        Image
        SpatPos                = [];
        Args.DimWave           = 2;
        Args.AperRadius        = 3;
        Args.BackAnnulus       = [15 20];   % note BackStd has its own annulus
        Args.FlagImage         = [];
        Args.BackStd           = 'fit';  % will fit also BackMean
        Args.BackStdArgs cell  = {};
        Args.BackMean          = [];
        
        Args.Fun               = @mean;
        Args.FunArgs           = {1, 'omitnan'};  % for @std use: {[],1,'omitnan'}
        Args.SubBack logical   = true;
    end
    
    % Convert to wave dir is in 2nd dim.
    if Args.DimWave==1
        Image          = Image.';
        Args.FlagImage = Args.FlagImage.';
        Args.DimWave   = 2;
    end
          
    if isempty(Args.FlagImage)
        % create a true FlagImage
        Args.FlagImage = true(size(Image));
    end
    
    % number of pixels in each axis
    [Nspat, Nwave] = size(Image);
    
    if isempty(SpatPos)
        SpatPos = (Nspat + 1).*0.5;
    end
    
    if ischar(Args.BackStd)
        [BackStd, BackMean] = imUtil.spec.extract.backStd(Image, SpatPos, Args.BackStdArgs{:}, 'DimWave',Args.DimWave);
    else
        BackStd  = Args.BackStd;
        BackMean = Args.BackMean;
    end
    
    SpatCoo  = SpatPos - (1:1:Nspat).';
    FlagSpat = abs(SpatCoo)<=Args.AperRadius;
    Image    = Image(FlagSpat,:);
    
    % replace bad pixels with NaN
    if ~isempty(Args.FlagImage)
        Image(~Args.FlagImage) = NaN;
    end
    
    Result.Wave     = (1:1:Nwave).';
    Result.Spec     = Args.Fun(Image, Args.FunArgs{:}).';
    Result.Nused    = sum(~isnan(Image), 1).';
    Result.BackMean = BackMean(:);
    Result.BackStd  = BackStd(:);
    
    if Args.SubBack
        Result.Spec = Result.Spec(:) - Result.BackMean(:);
    end

end
