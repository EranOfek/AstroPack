function Mosaic = mosaic(Args)
    % Make a mosaic sky image from a set of input AstroImages
    % Package: imProc.stack
    % Description: Make a mosaic sky image from a set of input AstroImages
    %          - Args.PSFScaling: Image pixel size / PSF pixel size ratio
    %          - Args.RotatePSF : PSF rotation angle, either a single value
    %                             for all the sources or a vector of angles
    %          - Args.Jitter    : apply PSF blurring due to the S/C jitter  
    %          - Args.Method    : source injection method, either 'direct' 
    %                             or 'PSFshift'
    %          - Args.MeasurePSF: whether to measure PSF flux containment and pseudo-FWHM (diagnostics)
    %          
    % Output : - Mosaic: a 2D array containing the resulting image in sky (?) coordinates
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    May 2023
    % Example: Mosaic = mosaic();

    arguments
        
        Args.DataDir        =    '.';                            % data directory
        Args.InputImages    =    'LAST*coadd_Image*.fits';       % PSF stamp rotation angle
        Args.Jitter         =    0;       % PSF blurring due to the S/C jitter
        Args.Method         =   'direct'; % injection method
                                          % 'direct' or 'FFTShift'
        Args.MeasurePSF     =    0;       % measure PSF flux containment and pseudo-FWHM
        
    end

    % set the path

    Args.DataDir = '/home/sasha/Obs1/';
    cd(Args.DataDir);
    FN=FileNames.generateFromFileName( Args.InputImages );
    AI=AstroImage.readFileNames(FN);   


end