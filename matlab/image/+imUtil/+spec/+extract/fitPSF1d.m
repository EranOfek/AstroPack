function [Result] = fitPSF1d(Image, SpatPos, Args)
    % Given a linear spectrum in a 2D image, fit the flux of a line-PSF to each wavelength.
    %   This function assumes that the spectrum in a 2D image is in a
    %   vertical or horizontal form, and it fits only the flux level.
    %   Assuming the spectrum is background subtracted.
    % Input  : - A 2D matrix containing a spectrum, where the spectrum is
    %            either horizontal or vertical.
    %          - Spatial position of the spectrum. If empty, then will
    %            assume it is in (Nspat-1).*0.5, where Nspat is the number
    %            of spatial pixels.
    %          * ...,key,val,... 
    %            'DimWave' - Dimension of the wavelength axis.
    %                   Default is 2.
    %            'PSF' - Either a column vector of a line PSF for all
    %                   wavelengths, or a matrix in which each column is a
    %                   line PSF for a corresponding wavelength positin
    %                   specified in the 'WaveAxisPSF' argument.
    %                   Default is []. This argument must be specified.
    %            'WaveAxisPSF' - A vector of wavelength position of the
    %                   line PSFs in the 'PSF' argument.
    %                   Default is []. If 'PSF' is a matrix, then this
    %                   argument must be supplied.
    %            'FitRad' - Fit radius, in the spatial direction, around
    %                   the trace center of pixels that will be used in the
    %                   fit. Default is 3.
    %            'FitMethod' - One of the following fit methods:
    %                   'lscov' - use weighted \chi^2 fitting on each
    %                           wavelength, with muliople iterations and
    %                           sigma clipping.
    %                           The weighting includes the source noise,
    %                           and the background noise (if supplied).
    %                   '\' - Single iteration, un-weighted lsq fiiting
    %                           using the \ operator.
    %                   'mean' - The mean of the PSF over the spectrum for
    %                           each wavelength.
    %            'FlagImage' - An optional logical image, with the same
    %                   size as the input image. Pixels with false, will be
    %                   not used in the fitting process.
    %                   If empty, use a matrix of true for all pixels.
    %                   Default is [].
    %            'Niter' - For FitMethod='lscov', this is the number of
    %                   sigma clipping iterations.
    %                   For no sigma clipping use 1.
    %                   Default is 2.
    %            'SigmaClip' - [Lower, Upper] sigma clipping in std units.
    %                   Default is [3 3].
    %            'StdFun' - Function handle for estimating the std for the
    %                   sigma clipping.
    %                   E.g., @mad, @std, @tools.math.stat.rstd
    %                   Default is @mad.
    %            'MinNptFit' - Minimum number of spatial points (in each
    %                   wavelength) that are needed in order to fit the data.
    %                   If this number of points is not available, then the
    %                   function will not continue to the second sigma
    %                   clipping iteration (this will reported in the
    %                   TerminatedIter field in the output.
    %                   Default is 3.
    %            'BackStd' - A vector of background std. If given, this will be used
    %                   in the calculation of the weights in the ]chi^2
    %                   fitting. The length of this vector should be as the
    %                   wavelength dimension of the input matrix.
    %                   Default is [].
    %            'InterpPSF' - A method by which to interpolate the PSF
    %                   lines for all wavelengths.
    %                   Default is 'linear'.
    %            
    % Output : - A structure with the following fields:
    %            .Wave - Vector of Wavelength pixel position.
    %            .Flux - Vector of fitted flux.
    %            
    %            The following are populated only for FitMethod='lscov':
    %            .FluxErr - Vector of flux errors.
    %            .Nused - Vector of number of used spatial pixels in each
    %                   wavelelngth.
    %            .Chi2 - A vector of \chi^2 per wavelength.
    %            .TerminatedIter - A vector of the number of sigma clipping
    %                   iterations performed in each wavelength.
    % Author : Eran Ofek (Dec 2023) 
    % Example: [F,Fe]=imUtil.spec.extract.fitPSF1d(BackSubIm, [], 'PSF',PSF, 'WaveAxisPSF',WavePSF, 'DimWave',1)

    arguments
        Image
        SpatPos                = [];
        Args.DimWave           = 2;
        Args.PSF               = []; %[Spat, Wave] %@(Sigma, Mu, X) normpdf(X, Mu, Sigma);         % line PSF        
        Args.WaveAxisPSF       = [];
        
        Args.FitRad            = 3;
        Args.FitMethod         = 'lscov';  % 'mean'|'\'|'lscov'
        Args.FlagImage         = [];
        Args.Niter             = 2;
        Args.SigmaClip         = [3 3];
        Args.StdFun            = @mad;  % @std, @tools.math.stat.rstd
        Args.MinNptFit         = 3;
        Args.BackStd           = [];
        
        Args.InterpPSF         = 'linear';
        
    end

    % Convert to wave dir is in 2nd dim.
    if Args.DimWave==1
        Image          = Image.';
        Args.FlagImage = Args.FlagImage.';
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
    
    if isempty(Args.WaveAxisPSF)
        % assume a single PSF for all wavelength
        PSF = Args.PSF;
    else
        % multiple PSF - interpolate to all wavelengths
        NspatPSF = size(Args.PSF,1);
        VecSpat  = (1:1:NspatPSF);
        VecWave  = (1:1:Nwave).';
        PSF      = interp2(Args.WaveAxisPSF, VecSpat, Args.PSF, VecWave, VecSpat, Args.InterpPSF);
    end
    [~,NwavePSF] = size(PSF);
    
    SpatCoo = SpatPos - (1:1:Nspat).';
    Flag    = abs(SpatCoo)<=Args.FitRad & Args.FlagImage;
    
    switch lower(Args.FitMethod)
        case 'mean'
            SpecFlux = zeros(Nwave,1);
            for Iwave=1:1:Nwave
                IwavePSF = min(Iwave, NwavePSF);
                Flag1    = Flag(:,Iwave);
                SpecFlux    = mean(Image(Flag1,:)./PSF(Flag1,:));
            end
            SpecFluxErr    = nan(size(SpecFlux));
            Nused          = [];
            Chi2           = [];
            TerminatedIter = [];
        case '\'
            % fit for each wavelength
            SpecFlux = zeros(Nwave,1);
            for Iwave=1:1:Nwave
                IwavePSF = min(Iwave, NwavePSF);
                Flag1    = Flag(:,Iwave);
                SpecFlux(Iwave) = PSF(Flag1, IwavePSF) \ Image(Flag1, Iwave);
            end
            SpecFluxErr = nan(size(SpecFlux));
            Nused       = [];
            Chi2        = [];
            TerminatedIter = [];
        case 'lscov'
            % fit for each wavelength   
            
            
            SpecFlux       = zeros(Nwave,1);
            SpecFluxErr    = zeros(Nwave,1);
            Nused          = zeros(Nwave,1);
            Chi2           = zeros(Nwave,1);
            TerminatedIter = zeros(Nwave,1);
            for Iwave=1:1:Nwave
                IwavePSF = min(Iwave, NwavePSF);
                Flag1    = find(Flag(:,Iwave));
                SpecFlux(Iwave)   = 1; %tmean(Image(Flag1,Iwave)./PSF(Flag1,Iwave)); % first guess for weight
                
                for Iiter=1:1:Args.Niter
                    if sum(Flag1)>=Args.MinNptFit
                        if SpecFlux(Iwave)<0
                            SpecFlux(Iwave) = 1000.*eps;
                        end
                        Weight = PSF(Flag1, IwavePSF); %.*SpecFlux(Iwave);
                        if ~isempty(Args.BackStd)
                            Weight = Weight + Args.BackStd(Iwave).^2;
                        end    

                        [SpecFlux(Iwave), SpecFluxErr(Iwave)] = lscov(PSF(Flag1, IwavePSF), Image(Flag1, Iwave), Weight);

                        if Iiter<Args.Niter
                            % residuals
                            Resid = Image(Flag1, Iwave) - PSF(Flag1, IwavePSF)*SpecFlux(Iwave);

                            Std = Args.StdFun(Resid(:));
                            Z   = (Resid./Std);

                            FlagGoodClip = Z>-abs(Args.SigmaClip(1)) & Z<abs(Args.SigmaClip(2));
                            Flag1        = Flag1(FlagGoodClip);

                        end
                        TermI = Iiter;
                    else
                        TermI = Iiter - 1;
                    end
                end
                Nused(Iwave) = sum(FlagGoodClip);
                Chi2(Iwave)  = sum(Z(FlagGoodClip).^2);
                TerminatedIter(Iwave) = TermI;
                
            end
            
        otherwise
            error('Unknown FitMethod option');
    end
        
    Result.Wave    = (1:1:Nwave).';
    Result.Flux    = SpecFlux;
    Result.FluxErr = SpecFluxErr;
    Result.Nused   = Nused;
    Result.Chi2    = Chi2;
    Result.TerminatedIter = TerminatedIter;
    
end
