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
    % * Find trace
    % * Extract trace into a linearized trace
    % * Measure flux as a fun. of dispersion direction
    % * Wavelength calibration
    % * calibration/extinction
    % * stitching
    % * line width measurments (on lamp and sky)
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
    if abs(mean(Mom.X1) - Ypos)>0.1
        error('imUtil.spec.trace.moment1d - 1st moment estimation not consistent with initial position');
    end
    
    
    
    %%
    

    Result = true;
end
