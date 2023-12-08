function [Result] = unitTest()
    % UnitTest for imUtil.spec
    % Example: imUtil.spec.unitTest
    
    
    % Step for spectroscopic reduction:
    % * Bias subtraction
    % * overscan subtraction
    % * Gain correction
    % Flat in the dispersion direction is not needed (standar star will do it)
    % * background estimation and subtraction
    % * Find trace starting position
    %       (Done) imUtil.spec.trace.collapse
    %       (Done) imUtil.spec.trace.peakDetectionFilter1
    %       
    % * Find trace
    %       (Done) imUtil.spec.trace.moment1d
    %
    % * Extract trace into a linearized trace
    %       (Done) imUtil.trace.image2cutouts1d
    %       (started) imUtil.spec.trace.trace - Return trace from 2D image
    %
    % * Measure flux as a fun. of dispersion direction
    %       (Done) imUtil.spec.extract.fitBackground
    %       (started) imUtil.spec.extract.fitPSF1d
    %
    % * Wavelength calibration
    %       (Done) AstroSpec/getSkyArcsSpecLines
    %       (OLD) ImUtil.Spec.spec_wavecalib_xcorr
    %       (OLD) ImUtil.Spec.spec_wavecalib_lines
    %       (Done) imUtil.filter.xcorr1_fft_multi
    %       (Done) imUtil.spec.lines.xcorrLineWidth
    %       (started/debug) imUtil.filter.xcorr1_scale_shift
    %       (Done) timeSeries.peaks.localMax
    %
    %
    % * calibration/extinction
    %       (Done) AstroSpec/getSpecPhotStandard
    % * Telluric correction
    % * stitching
    % * line width measurments (on lamp and sky)
    %       (started) imUtil.spec.lines.xcorrLineWidth
    %
    
    %% imUtil.spec.trace.collapse
    % collapse 2-D image to 1-D
    Xmax = 30;
    R=randn(100,100);
    R(Xmax,:)=R(Xmax,:)+1;
    Res=imUtil.spec.trace.collapse(R, 'Fun','mean', 'DimWave',2);
    % apply filter prior to collapse:
    Res=imUtil.spec.trace.collapse(R, 'Fun','mean', 'DimWave',2, 'PreConv',[2]);
    % apply filter also after collapse:
    Res=imUtil.spec.trace.collapse(R, 'Fun','mean', 'DimWave',2, 'PreConv',[2], 'PostFilter',2);
    % verify that max is recovered
    [~,Imax]=max(Res);
    if abs(Imax-Xmax)>1.1
        error('imUtil.spec.trace.collapse - max position was not recovered');
    end
    
    %% imUtil.spec.trace.peakDetectionFilter1 - Identify peak
    [SN, Peak]=imUtil.spec.trace.peakDetectionFilter1(Res, 'Threshold',5);
    Imax = find(Peak.Flag);
    if abs(Imax-Xmax)>1.1
        error('imUtil.spec.trace.peakDetectionFilter1 - max position was not recovered');
    end
    
    
    %% imUtil.trace.image2cutouts1d
    % Cut line-stamps from an array, around some central position.
    Size = [100 200];
    Y=(1:1:Size(1))';  
    Ypos = 22;
    Image=repmat(normpdf(Y,Ypos,2),1,Size(2)).*20 + randn(Size);
    %surface(Image); shading interp
    Res=imUtil.spec.trace.collapse(Image, 'Fun','mean', 'DimWave',2, 'PreConv',[2], 'PostFilter',2);
    [~,Ymax]=max(Res);
    % Here Ymax can be a vector of the trace...
    CutoutImage = imUtil.spec.trace.image2cutouts1d(Image, Ymax, 'Dim',1, 'WinHalfSize',7);
    
    %% imUtil.spec.trace.moment1d
    % in practice this should be done after some filtering...
    Mom = imUtil.spec.trace.moment1d(Image, Ypos);
    if abs(mean(Mom.X1) - Ypos)>0.3
        error('imUtil.spec.trace.moment1d - 1st moment estimation not consistent with initial position');
    end
    
    
    
    %% imUtil.spec.extract.fitBackground
    Image = 100+randn(50,1000);
    [Back] = imUtil.spec.extract.fitBackground(Image);
    [Back] = imUtil.spec.extract.fitBackground(Image, 'Method','global');
    [Back] = imUtil.spec.extract.fitBackground(Image, 'Method','poly');
    %Test with NaN
    Image(11,10) = NaN;
    Image(12,12) = NaN;
    [Back] = imUtil.spec.extract.fitBackground(Image, 'Method','poly');
    if any(isnan(Back(:,10)))
        error('imUtil.spec.extract.fitBackground - Not treating NaNs correctly');
    end
    if abs(mean(Back,'all')-100)>0.05
        error('imUtil.spec.extract.fitBackground - Not estimated back correctly');
    end

    

    Result = true;
end
