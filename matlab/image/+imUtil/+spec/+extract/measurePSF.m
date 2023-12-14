function [Result, BackStd] = measurePSF(Image, SpatPos, Args)
    % Measure line-PSF in spectral image
    %       The PSF can measured over the entire image, or in sections
    %       along the wavelength dimension.
    %       The PSF is calculated by the weighted sum of all PSF over all
    %       relevant wavelengths. The weighting is by the flux and inverse
    %       background variance.
    % Input  : - A 2D matrix containing spectral image in which the trace
    %            is linearized (vertical or horizontal).
    %          - The spatial position of the trace (scalar).
    %            If [], then will take the center of the spatial axis:
    %            (Nspat - 1).*0.5, where Nspat is the number of pixels in
    %            the spatial dimension.
    %            Default is [].
    %          * ...,key,val,... 
    %            'DimWave' - Dimension of the wavelength axis.
    %                   Default is 2.
    %            'BackStd' - An optional vector of background std for along
    %                   the wavelength dimension. If empty, will be
    %                   calculated using: imUtil.spec.extract.backStd
    %                   Default is [].
    %            'Annulus' - Region in which to calculate the StD.
    %                   This is the [Rmin Rmax] around the trace position
    %                   (two sided) in which to estimate the background.
    %                   Default is [15 20].
    %            'RobustStd' - Use robust std. Default is false.
    %
    %            'WaveEdges' - The edges of wavelength dimension (pix) in
    %                   which to estimate PSFs. The PSF will be measured
    %                   between each consecutive pair of edges.
    %                   If empty, then will estimate one PSF for the entire
    %                   image (all wavelengths).
    %                   Default is [].
    %            'WinSN' - Half size window around the spatial position inw
    %                   which to calculate the S/N of the PSF. This S/N
    %                   will be reported in the SN_PSF field of the output.
    % Output : - A structure array of PSF, one element per edge range.
    %            Available fields are:
    %            .WaveEdges - [Min Max] pix along the wavelength direction
    %                   in which the PSF was estimated.
    %            .PSF - The line PSF.
    %            .SN_PSF - S/N of the PSF.
    %            .BackStdMean - maen of the background std in this range.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [R,Bstd] = imUtil.spec.extract.measurePSF(BackSubIm,'DimWave',1);

    arguments
        Image
        SpatPos                = [];
        
        Args.DimWave           = 2;
        
        Args.BackStd           = [];
        Args.Annulus           = [15 20];
        Args.RobustStd logical = false;
        
        Args.WaveEdges         = (1:100:2800); %[1 700 1400 2100 2800];
        Args.WinSN             = 3;
        
    end

    % convert wavelength to Y axis.
    if Args.DimWave==2
        Image = Image.';
        Args.DimWave = 1;
    end
    
    [Nwave, Nspat] = size(Image);
    
    if isempty(SpatPos)
        SpatPos = (Nspat - 1).*0.5;
    end
        
    if isempty(Args.BackStd)
        BackStd = imUtil.spec.extract.backStd(Image, SpatPos, 'DimWave',Args.DimWave, 'Annulus',Args.Annulus, 'RobustStd',Args.RobustStd);
    else
        BackStd = Args.BackStd;
    end
    
    % peak flux as a function of wavelength
    PeakFlux = Image(:,SpatPos);
    
    % Unweighted collapsed PSF
    UnwPSF = sum(Image, 1);
    
    % weighted PSF
    WeiPSF = PeakFlux.*Image./(BackStd.^2);
    % (S/N)^2 including source noise
    SN2map = sign(Image).*(Image./sqrt(Image + BackStd.^2)).^2;  
    %SN2map(SN2map<25) = 0;
    
    SpatCoo  = (SpatPos-Args.WinSN:SpatPos+Args.WinSN);
    
    if isempty(Args.WaveEdges)
        PSF = sum(WeiPSF, 1);
        Result.WaveEdges = [1 Nwave];
        Result.PSF = PSF./sum(PSF);
        Result.SN_PSF   = sqrt(sum(SN2map(:, SpatCoo), 'all'));
        Result.BackStdMean = mean(BackStd);
    else
        Nedges = numel(Args.WaveEdges);
        for Iedges=1:1:(Nedges-1)
            Pos = (Args.WaveEdges(Iedges):Args.WaveEdges(Iedges+1)).';
            PSF = sum(WeiPSF(Pos, :), 1);
            Result(Iedges).WaveEdges = [Args.WaveEdges(Iedges), Args.WaveEdges(Iedges+1)];
            Result(Iedges).PSF       = PSF./sum(PSF);
            Result(Iedges).SN_PSF    = sqrt(sum(SN2map(Pos, SpatCoo), 'all'));
            Result(Iedges).BackStdMean = mean(BackStd(Pos));
            
            %plot(Result(Iedges).PSF)
            %hold on
        end
    end
     
end
