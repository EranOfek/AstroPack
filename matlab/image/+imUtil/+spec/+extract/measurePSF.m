function [Result] = measurePSF(Image, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    %            'DimWave' - Dimension of the wavelength axis.
    %                   Default is 2.
    %            'WaveEdges' - The edges of wavelength dimension (pix) in
    %                   which to estimate PSFs. The PSF will be measured
    %                   between each consecutive pair of edges.
    %                   Use Inf, to extract the PSF using all the
    %                   wavelengths.
    %                   If empty, then will estimate the edges such that
    %                   the S/N in each section is larger than what is
    %                   specified in the 'MinSN' argument.
    %                   Default is [].
    %            'MinSN' - Minimum S/N required in each section (used in
    %                   case that WaveEdges is []).
    %                   Default is 50.
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: imUtil.spec.extract.measurePSF

    arguments
        Image
        SpatPos                = [];
        
        Args.DimWave           = 2;
        
        Args.BackStd           = [];
        Args.BackAnnulus       = [10 15];
        
        Args.WaveEdges         = [];
        Args.MinSN             = 50;
    end

    % convert wavelength to Y axis.
    if Args.DimWave==2
        Image = Image';
    end
    
    [Nwave, Nspat] = size(Image);
    
    if isempty(SpatPos)
        SpatPos = (Nspat - 1).*0.5;
    end
    
    %====== move this to: imUtil.spec.extract.backStd function
    Xspat = (1:1:Nspat) - SpatPos;
    if isempty(Args.BackStd)
        % estimated background std from image back region std
        FlagBack = abs(Xspat)>min(Args.BackAnnulus) & abs(Xspat)<max(Args.BackAnnulus);
        
        BackRegion   = Image(:,FlagBack);
        Args.BackStd = std(BackRegion, [], 2);
        
                
    end
    
    
    % peak flux as a function of wavelength
    PeakFlux = Image(:,SpatPos);
    
    % Unweighted collapsed PSF
    UnwPSF = sum(Image, 1);
    
    % weighted PSF
    WeiPSF = PeakFlux.*
    
    if isempty(Args.WaveEdges)
        
        
    end
    
    
    
end
