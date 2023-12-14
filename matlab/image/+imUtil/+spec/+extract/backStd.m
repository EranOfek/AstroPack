function [BackStd] = backStd(Image, SpatPos, Args)
    % Calculate the std of the background in a 2D spectral image.
    %       Assuming a 2D image (usually containing a linearized spectrum)
    %       calculate the background std as a function of the wavelength
    %       axis (i.e., the std over some spatial positions).
    %       The std is calculated in spatial positions specified by the
    %       user (eithre relative to some trace position, or in absolute
    %       coordinates).
    % Input  : - A 2D matrix containing an image.
    %          - The spatial position of the trace (scalar).
    %            If NaN, then the background std will be calculate in the
    %            spatial positions (in pixel units) specified in
    %            Args.BackAnnulus.
    %            If [], then will take the center of the spatial axis:
    %            (Nspat - 1).*0.5, where Nspat is the number of pixels in
    %            the spatial dimension.
    %            If SpatPos is given, or empty, then the BackAnnulus is
    %            taken in both sides of the trace.
    %            Default is [].
    %          * ...,key,val,...
    %            'DimWave' - Dimension of the wavelength axis.
    %                   Default is 2.
    %            'BackAnnulus' - Region in which to calculate the StD
    %                   Default is [10 15].
    %            'RobustStd' - Use robust std. Default is false.
    % Output : - A vector of background std per pixel in the wavelength
    %            axis.
    % Author : Eran Ofek (2023 Dec) 
    % Example: BackStd = imUtil.spec.extract.backStd(randn(100,30))
    %          BackStd = imUtil.spec.extract.backStd(randn(100,30),NaN)
    %          BackStd = imUtil.spec.extract.backStd(randn(100,30),[],'RobustStd',true)

    arguments
        Image
        SpatPos                = [];  % if NaN - BackAnnulus is a a range in X coo
        
        Args.DimWave           = 2;
        Args.BackAnnulus       = [10 15];
        Args.RobustStd logical = false;
        
    end

    % convert wavelength to Y axis.
    if Args.DimWave==2
        Image = Image';
    end
    
    [~, Nspat] = size(Image);
    
    if isempty(SpatPos)
        SpatPos = (Nspat - 1).*0.5;
    end
    
    if isnan(SpatPos)
        Xspat = (1:1:Nspat);
        FlagBack = Xspat>min(Args.BackAnnulus) & Xspat<max(Args.BackAnnulus);
    else
        % estimated background std from image back region std
        Xspat = (1:1:Nspat) - SpatPos;
        FlagBack = abs(Xspat)>min(Args.BackAnnulus) & abs(Xspat)<max(Args.BackAnnulus);
    end
    
    BackRegion   = Image(:,FlagBack);
    if Args.RobustStd
        BackStd = tools.math.stat.rstd(BackRegion, 2);
    else
        BackStd = std(BackRegion, [], 2);
    end
    
end
