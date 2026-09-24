function FFIs = FFI2calibSubimages(FFI, Args)
    %{
    Splits a calibrated TESS FFI AstroImage into overlapping sub-images and performs
    standard calibration and characterization steps on each sub-image. The
    function divides the input FFI into blocks of a given size with configurable
    overlap, then applies background and variance estimation, source detection
    and measurement, and PSF construction on each resulting sub-image. This is
    typically used to prepare large TESS FFIs for downstream processing steps
    (e.g. image subtraction or transient detection) on smaller, more uniform
    image tiles.
    
    Input   : - FFI. AstroImage object representing a calibrated Full Frame
                Image (FFI).
    
              * ...,key,val,...
                'Block' - Two-element vector [Nx Ny] specifying the size (in
                       pixels) of each sub-image in the X and Y directions.
                       Default is [674 674].
                'OverlapXY' - Two-element vector [Ox Oy] specifying the overlap
                       (in pixels) between adjacent sub-images in X and Y.
                       Default is [30 30].
                'background_Par' - Cell array of parameter name/value pairs
                       forwarded to imProc.background.background for background
                       and variance estimation. Default is:
                       {'BackFun', @median,
                        'VarFun', @imUtil.background.rvar,
                        'BackFunPar', {'all'}}.
                'populatePSF_Par' - Cell array of parameter name/value pairs
                       forwarded to imProc.psf.populatePSF for PSF construction
                       in each sub-image. Default is:
                       {'CropByQuantile', true}.
    
    Output  : - Array of AstroImage objects corresponding to the processed
                sub-images. Each sub-image includes populated background and
                variance information, detected and measured sources, and a PSF
                model.
    
    Notes   : - The sub-image tiling is performed using
                imProc.image.image2subimages, which determines the exact tiling
                geometry given Block and OverlapXY.
              - PSF-fit photometry on the detected sources is not performed by
                default (the corresponding call is commented out), but can be
                enabled if needed for downstream analysis.
              - The input FFI is not modified; all processing is performed on
                the returned sub-image objects.
    
    Author  : Ruslan Konno (Jan 2026)
    Example : % Split an FFI into overlapping 674x674 pixel sub-images and
              % prepare them for further processing:
              FFIs = pipeline.tess.reduction.FFI2calibSubimages(FFI);
    
              % Use larger sub-images with smaller overlap:
              FFIs = pipeline.tess.reduction.FFI2calibSubimages(FFI, ...
                        'Block', [800 800], ...
                        'OverlapXY', [20 20]);
    %}

    arguments
        FFI

        Args.Block = [674 674];
        Args.OverlapXY = [30 30]

        Args.background_Par = {'BackFun',@median, ...
                    'VarFun',@imUtil.background.rvar,'BackFunPar',{'all'}}
        Args.populatePSF_Par = {'CropByQuantile', true}
    end

    [FFIs, ~] = imProc.image.image2subimages(FFI, Args.Block, 'OverlapXY', Args.OverlapXY);
    
    FFIs = imProc.background.background(FFIs, Args.background_Par{:});
    FFIs = imProc.sources.findMeasureSources(FFIs);
    FFIs = imProc.psf.populatePSF(FFIs, Args.populatePSF_Par{:});
    %FFIs = imProc.sources.psfFitPhot(FFIs);

end