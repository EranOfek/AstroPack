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
    %            'AperRadius' - Vector of Aperture radius, from trace pixel, in which
    %                   to calculate some statistics of the light (e.g.,
    %                   mean, std).
    %                   Default is [2 3 4].
    %            'FlagImage' - An optional logical image, with the same
    %                   size as the input image. Pixels with false, will be
    %                   not used in the fitting process.
    %                   If empty, use a matrix of true for all pixels.
    %                   Default is [].
    %            'Back' - A matrix, or vector, of background level.
    %                   If is empty, then Vack and Std will be estimated
    %                   using: imUtil.spec.extract.backStd
    %                   Default is [].
    %            'Std' - A vector of the std of the background level.
    %                   Default is [].
    %            'BackAnnulus' - Region in which to calculate the
    %                   background. The annulus position for calcultaing
    %                   the background std are in 'BackStdArgs'.
    %                   Default is [15 20].
    %            'FunMethod' - Method for calculating the aperture
    %                   photometry sum:
    %                   'sum' - Sum of flux in aperture.
    %                   'mediansum' - medain of flux in aperture multiplied
    %                           by numbre of used pixels.
    %                   'mean' - Mean of flux in aperture.
    %                   'median' - Median of flux in aperture.
    %                   'std' - std of flux in aperture.
    %            'SubBack' - A logical indicating if to subtracted the
    %                   background measured in the annulus.
    %                   Default is true.
    % Output : - A structure with the following fields:
    %            .Wave - A vector of of wavelength pixel positions.
    %            .AperPhot - A amtrix with column per aperture, with the
    %                   aperture photometry spectrum.
    %            .Nused - A amtrix with column per aperture, with the
    %                   number of data points used (not NaN) per wavelength.
    %            .SNmeas - A amtrix with column per aperture, with the S/N
    %                   for a measurment process, as a function of
    %                   wavelength.
    %            .SNdet - A amtrix with column per aperture, with the S/N
    %                   for a detection process, as a function of
    %                   wavelength.
    %            .Back - A column vector of background level.
    %            .BackStd - A column vector of std of background.
    %            .AperRadius - A vector of aperture radius used.
    %
    % Author : Eran Ofek (2023 Dec) 
    % Example: [Result] = imUtil.spec.extract.aperPhot(randn(100,30), 'DimWave',1)

    arguments
        Image
        SpatPos                = [];
        Args.DimWave           = 2;
        Args.AperRadius        = [2 3 4];
        Args.BackAnnulus       = [15 20];   % note BackStd has its own annulus
        Args.FlagImage         = [];   % true for Pixels to use
        
        Args.Back              = [];
        Args.Std               = [];
        
        %Args.BackStd           = 'fit';  % will fit also BackMean
        %Args.BackStdArgs cell  = {};
        %Args.BackMean          = [];
        
        Args.FunMethod         = 'sum';  % 'sum'|'mediansum'|'mean'|'median'
        %Args.Fun               = @mean;
        %Args.FunArgs           = {1, 'omitnan'};  % for @std use: {[],1,'omitnan'}
        
        Args.SubBack logical   = true;
    end
    
        
    % Convert to wave dir is in 2nd dim.
    if Args.DimWave==1
        Image          = Image.';
        Args.FlagImage = Args.FlagImage.';
        Args.Back      = Args.Back.';
        Args.Std       = Args.Std.';
        Args.DimWave   = 2;
    end
          
    if isempty(Args.FlagImage)
        % create a true FlagImage
        Args.FlagImage = true(size(Image));
    end
    
    % set bad pixels to NaN
    Image(~Args.FlagImage) = NaN;
    
    % number of pixels in each axis
    [Nspat, Nwave] = size(Image);
    
    if isempty(SpatPos)
        SpatPos = (Nspat + 1).*0.5;
    end
    
    if isempty(Args.Back)
        % Back is not provided - estimate Back and Std
        [Batd,Back] = imUtil.spec.extract.backStd(Image,[], 'Annulus',Args.BackAnnulus);
    else
        Back = Args.Back;
        Std  = Args.Std;
    end
    
    BackVector = mean(Args.Back, 1, 'omitnan');
    StdVector  = mean(Args.Std, 1, 'omitnan');
    
    
    % subtract background
    if Args.SubBack
        Image = Image - Back;
    end
    Result.Wave     = (1:1:Nwave).';    
    
    SpatCoo  = SpatPos - (1:1:Nspat).';
    
    %Image    = Image(FlagSpat,:);   
    
    Naper = numel(Args.AperRadius);
    Result.AperPhot = zeros(Nwave, Naper);
    Result.Nused    = zeros(Nwave, Naper);
    Result.SNmeas   = zeros(Nwave, Naper);
    Result.SNdet    = zeros(Nwave, Naper);
    Result.Back     = BackVector(:);
    Result.BackStd  = StdVector(:);
    Result.AperRadius = Args.AperRadius;
    for Iaper=1:1:Naper
        FlagSpat = abs(SpatCoo)<=Args.AperRadius(Iaper);
        switch lower(Args.FunMethod)
            case 'sum'
                AperPhot = sum(Image(FlagSpat,:), 1, 'omitnan');
                Nused    = (Args.AperRadius(Iaper).*2 + 1).*ones(1, Nwave);
                
            case 'mediansum'
                AperPhot = median(Image(FlagSpat,:), 1, 'omitnan');
                Nused    = sum(~isnan(Image(FlagSpat,:)), 1);
                
            case 'mean'
                AperPhot = mean(Image(FlagSpat,:), 1, 'omitnan');
                Nused    = sum(~isnan(Image(FlagSpat,:)), 1);
                
            case 'median'
                AperPhot = median(Image(FlagSpat,:), 1, 'omitnan');
                Nused    = sum(~isnan(Image(FlagSpat,:)), 1);
                
            case 'std'
                AperPhot = std(Image(FlagSpat,:), [], 1, 'omitnan');
                Nused    = sum(~isnan(Image(FlagSpat,:)), 1);
                
            otherwise
                error('Unknown FunMethod option');
        end
        Result.AperPhot(:,Iaper) = AperPhot(:);
        Result.Nused(:,Iaper)    = Nused(:);
        
        Result.SNmeas(:,Iaper) = sqrt(sum(Image(FlagSpat,:).^2./(Image(FlagSpat,:) + StdVector.^2), 1, 'omitnan')).';
        Result.SNdet(:,Iaper)  = sqrt(sum(Image(FlagSpat,:).^2./(StdVector.^2), 1, 'omitnan')).';
        
    end
        
end
