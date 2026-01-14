function FFIc = FFI2calibCutout(FFI, RA, Dec, Args)
    %{
    Creates a calibrated cutout AstroImage around a sky position (RA, Dec) from
    a TESS FFI AstroImage. The function converts the input sky coordinates to
    pixel coordinates using the FFI WCS, crops the image to the requested frame,
    and then performs standard calibration steps on the cutout:
      (1) background and variance estimation,
      (2) source detection and measurement,
      (3) PSF construction/population,
      (4) PSF-fit photometry.
    
    Input   : - FFI. AstroImage object containing a TESS FFI image and valid WCS
                solution (FFI.WCS must support sky2xy).
              - RA. Right ascension of target position. Units: degrees.
              - Dec. Declination of target position. Units: degrees.
              * ...,key,val,...
                'CutoutFrame' - Cutout half-sizes in pixels in the order:
                       [X_left, X_right, Y_down, Y_up]. The cutout boundaries are
                       computed as:
                         Xmin = X - X_left,  Xmax = X + X_right
                         Ymin = Y - Y_down,  Ymax = Y + Y_up
                       Default is [100 100 100 100].
                'background_Par' - Cell array of parameter name/value pairs
                       forwarded to imProc.background.background. Default is:
                       {'BackFun', @median, 'VarFun', @imUtil.background.rvar, ...
                        'BackFunPar', {'all'}}.
                'populatePSF_Par' - Cell array of parameter name/value pairs
                       forwarded to imProc.psf.populatePSF. Default is:
                       {'CropByQuantile', true, 'RadiusPSF', 9}.
    
    Output  : - FFIc. AstroImage object representing the cropped cutout with
                background model and variance estimates, detected sources, PSF
                model, and PSF-fit photometry results populated by:
                  imProc.background.background
                  imProc.sources.findMeasureSources
                  imProc.psf.populatePSF
                  imProc.sources.psfFitPhot
    
    Notes   : - The function assumes the requested cutout region lies within the
                image bounds. If the crop extends beyond the image edges, the
                behavior depends on AstroImage.crop implementation.
              - For best results, ensure the FFI has up-to-date WCS and that the
                background/PSF parameters are appropriate for the TESS cadence
                and image characteristics.
    
    Author  : Ruslan Konno (Jan 2026)
    Example : % Create a 200x200 pixel cutout (100 pixels each side) and run
              % background, PSF, and PSF photometry:
              FFIc = pipeline.tess.reduction.FFI2calibCutout(FFI, RA, Dec);
    
              % Asymmetric cutout and custom PSF radius:
              FFIc = pipeline.tess.reduction.FFI2calibCutout(FFI, RA, Dec, ...
                        'CutoutFrame', [150 50 120 80], ...
                        'populatePSF_Par', {'CropByQuantile', true, 'RadiusPSF', 11});
    %}

    arguments
        FFI
        RA
        Dec

        Args.CutoutFrame = [100 100 100 100];

        Args.background_Par = {'BackFun', @median, ...
            'VarFun', @imUtil.background.rvar,'BackFunPar', {'all'}};
        Args.populatePSF_Par = {'CropByQuantile', true, 'RadiusPSF',9};

    end

    % Get X,Y coordinates of RA, Dec via WCS
    [X,Y] = FFI.WCS.sky2xy(RA,Dec);

    % Get crop X,Y coordinates
    Crop_X_min = X-Args.CutoutFrame(1);
    Crop_X_max = X+Args.CutoutFrame(2);
    Crop_Y_min = Y-Args.CutoutFrame(3);
    Crop_Y_max = Y+Args.CutoutFrame(4);

    FFIc = FFI.copy();

    FFIc = FFIc.crop([Crop_X_min Crop_X_max Crop_Y_min Crop_Y_max]);
       
    FFIc = imProc.background.background(FFIc, Args.background_Par{:});
    FFIc = imProc.sources.findMeasureSources(FFIc);
    FFIc = imProc.psf.populatePSF(FFIc, Args.populatePSF_Par{:});
    FFIc = imProc.sources.psfFitPhot(FFIc);
end