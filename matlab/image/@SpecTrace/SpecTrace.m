% SpecTrace - A container class for 2d spectral traces
%
% Remakrs on operations:
% This class is an array. Typically an object is associated with an image,
% where each element in the object refers to a single trace in the image.
% The User is responsible to populate DimWave of the first element of the
% object. However, the function 'estimateDimWave' can be used.



classdef SpecTrace < Component
    
    properties (SetAccess = public)
        DimWave
        ExpectedSpatPos            % user specified expected spatial position of the trace
        %ExpectedWavePos      = [];
        SNdet                      % detection S/N
        SNint                      % integrated S/N
        %MeasuredSpatPos            % measured spatial mean position of the trace
        %Traces               = struct('MeanX',[], 'WavePix',[], 'Wave',[], 'X',[], 'Intensity',[], 'X2',[], 'FWHM',[], 'Image',[], 'Name',[], 'SN',[], 'ExpectedSpatPos',[], 'ExpectedWavePos',[],);
        
        % Wave, WavePix - refer to Wave direction
        % Pos - refer to spatial position
        
        Wave
        WavePix
        PosBest
        PosMean
        PosMethod
        FluxPeak
        Back            = [];
        Var             = [];
        Mask            = [];
        FluxPSF
        FluxAper
        
        Mom2
        FWHM
                
        
        
        %MeanSpatPos
        
        %DispPos
        %SpatPos
        ExtractShift       = 0;
        %Intensity
        
        %SpatMom2
        %SpatFWHM
        
        LinTraceImage
        LinTracePos
        
        FitInfo
        MatchedInfo
        Pix2Wave            
        
        %LinTraceImage
        %LinTracePos
    end
    
    properties (Hidden, Constant)
        %TraceCol  = ["MeanX", "WavePix", "Wave", "X", "Intensity", "X2", "FWHM", "Image", "Name", "SN", "ExpectedWavePos", "ExpectedSpatPos"];
    end
    
    methods % Constructor
        function Obj=SpecTrace(SizeArray)
            % Constructor for SpecTrace
           
            arguments
                SizeArray = [];
            end
            
            if isempty(SizeArray)
                % defined
            else
            
                Nel = prod(SizeArray);
                for Iel=1:1:Nel
                    [I,J]=ind2sub(SizeArray, Iel);
                    Obj(I,J) = SpecTrace([]);
                end
            end
        end
    end
    
    methods % utilities
        function [Obj,DimWave]=estimateDimWave(Obj, ImageMat)
            %
            
            arguments
                Obj
                ImageMat
            end
            
            if isa(ImageMat, 'AstroImage')
                ImageMat = ImageMat.Image;
            end
            
            
                
            
        end
    
        % DONE
        function Result = isEmptyTrace(Obj)
            % Check if elements of object are populated with: DispPixPos and SpatPixPos
            % Input  : - self
            % Output : - An array of logicals indicating (per element of
            %            the input object) if DispPixPos | SpatPixPos are
            %            empty (that is the minimal trace information
            %            doesnot exist).
            % Author : Eran Ofek (2025 Jan)
            % Example: TS.isEmptyTrace
                        
            Result = Obj.AI.isemptyProperty('DispPixPos') | Obj.AI.isemptyProperty('SpatPixPos');
            
        end
        
        % DONE
        function Result = isEmptyWaveSolution(Obj)
            % Check if elements of object are populated with: DispPixPos, SpatPixPos, Wave
            % Input  : - self
            % Output : - An array of logicals indicating (per element of
            %            the input object) if DispPixPos | SpatPixPos | Wave are
            %            empty (that is the minimal wavelength solution information
            %            doesnot exist).
            % Author : Eran Ofek (2025 Jan)
            % Example: TS.isEmptyTrace
           
            Result = Obj.AI.isemptyProperty('DispPixPos') | Obj.AI.isemptyProperty('SpatPixPos') | Obj.AI.isemptyProperty('Wave');
        end
        
    end
    
    methods % trace functionality        
        % see imProc.spec.trace.traceByCollapse
        
        % DONE
        function Obj=setLinImage(Obj, ImageMat, Args)
            % Set LinTraceImage using the trace and an input image
            %   Populate the LinTraceImage property by using the Trace to
            %   interpolated a linearized trace.
            %   A linerized tarce is a trace in which the spectrum fall in
            %   the central pixel.
            %   Using : imUtil.spec.trace.linearizeTrace(
            % Input  : - self.
            %          - A 2D matrix image or a single element AstroImage.
            %          * ...,key,val,...
            %            'ExtractShift' - The distance (pix) in which to
            %                   extract the spectrum relative to the trace
            %                   position. Default is 0.
            %            'LinTraceHalfWidth' - LinTraceImage half spatial
            %                   size. Default is 50.
            %            'linearizeTraceArgs' - A cell array of additional
            %                   arguments to pass to: 
            %                   imUtil.spec.trace.linearizeTrace
            %                   Defaut is {}.
            % Output : - self populated with LinTraceImage and LinTracePos
            % Author : Eran Ofek (2025 Jan)
            % Example: TS.setLinImage(AI)
            
            arguments
                Obj
                ImageMat
                Args.ExtractShift       = 0;
                Args.LinTraceHalfWidth  = 50;
                Args.linearizeTraceArgs = {};
            end
            
            if isa(ImageMat, 'AstroImage')
                Array = ImageMat.Image;
            else
                Array = ImageMat;
            end
            
            N = numel(Obj);
            for I=1:1:N
                if isempty(Obj(I).SpatPixPos)
                    error('SpatPixPos for element %d is empty',I);
                end
                [Obj(I).LinTraceImage, Obj(I).LinTracePos] = imUtil.spec.trace.linearizeTrace(Array,...
                                     Obj(I).SpatPixPos + Args.ExtractShift,...
                                    'DimWave',Obj(I).DimWave,...
                                    'HalfWidth',Args.LinTraceHalfWidth,...
                                    Args.linearizeTraceArgs{:});
            end
                
            
        end
        
        % DONE
        function Obj=background(Obj, Args)
            % Populate the background and variance in Lineraized trace image
            % Input  : - self.
            %          * ...,key,val,...
            %            'Annulus' - Annulus (like in 2D photometry) is the
            %                   [inner, outer] pixel position above and below the
            %                   'TracePos' position from which to extract the
            %                   pixels that will be used for the background
            %                   estimation.
            %                   Default is [10 15].
            %            'Method' - One of the following background fitting methods:
            %                   'global' - Global background in the annulus region. 
            %                   'wave' - Background is estimated per wavelength.
            %                   'poly' - Fit a polynomial per each wavelength
            %                           (with sigma clipping).
            %                           Note that if 'FitOrders' is set to [0],
            %                           then this is like 'wave', but with the
            %                           sigma clipping option.
            %                   Default is 'wave'.
            %            'Fun' - If Method=global|wave, then this is a the function
            %                   handle that used to calculate the background.
            %                   The function have the form Back=Fun(Data, Dim, FunArgs{:})
            %                   Default is @median.
            %            'FunArgs' - A cell array of additional arguments to pass
            %                   to Fun. Default is {'omitnan'}.
            %            'FitOrders' - In case Method=poly, then this is a vector
            %                   of polynomial orders to fit.
            %                   Default is [0 1].
            %            'SigmaClip' - In case Method=poly, then this is the
            %                   [lower, upper] sigma clipping in units of the rstd.
            %                   Default is [3 3].
            %            'Niter' - Number of sigma clipping iterations.
            %                   1 for no sigma clipping.
            %                   Default is 2.
            %            'FlagsToRemove' - A cell array of flags in the
            %                   Mask image that will no be used in the background
            %                   estimation. If empty, do not remove any pixels.
            %                   Default is [].
            % Output : - An updated SpecTrace object with the Back and Var
            %            properties populated.
            %            The Back property is an image, while the Var is estimated
            %            per wavelength.
            % Author : Eran Ofek (Mar 2025)
            % Example: AI.Trace.background
            
            arguments
                Obj
                Args.Method        = 'poly';   % 'poly'|'wave'|'global';  
                Args.Annulus       = [15 20];
                Args.Fun           = @median;
                Args.FunArgs                 = {'omitnan'};  % F(X, Dim, other pars)
                Args.FitOrders               = [0 1];  % for first order poly
                Args.SigmaClip               = [3 3];
                Args.Niter                   = 2;
                Args.FlagsToRemove = {'NaN','CR_DeltaHT'};
            end
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if ~isempty(Obj(Iobj).Mask) && ~isempty(Args.FlagsToRemove)
                    % select pixels to ignore (true)
                    % use FlagsToRemove
                    error('FlagsToRemove is not yet operational');
                    Flag = [];
                else
                    Flag = [];
                end
                    
                % Note these need to be transposed!
                [BackStd,BackMean] = imUtil.spec.extract.backStd(Obj(Iobj).LinTraceImage, 'RobustStd',true, 'DimWave',Obj(Iobj).DimWave);
                    
                [Back] = imUtil.spec.extract.fitBackground(Obj(Iobj).LinTraceImage, 'DimWave',Obj(Iobj).DimWave,...
                                                                  'Method','poly',...
                                                                  'Flag',Flag,...
                                                                  'TracePos',Obj(Iobj).LinTracePos,...
                                                                  'Annulus',Args.Annulus,...
                                                                  'Fun',Args.Fun,...
                                                                  'FunArgs',Args.FunArgs,...
                                                                  'FitOrders',Args.FitOrders,...
                                                                  'SigmaClip',Args.SigmaClip,...
                                                                  'Niter',Args.Niter);
                                                                  
                Obj(Iobj).Back = Back;
                Obj(Iobj).Var  = BackStd.'.^2;
           
            end
        end
        
        function Result=aperphot(Obj, Args)
            % Extract aperture photometry as a function of wavelength from linearized trace image 
            % Input  : - self. 
            %          * ...key,val,...
            %            'AperRadius' - Vector of Aperture radius, from trace pixel, in which
            %                   to calculate some statistics of the light (e.g.,
            %                   mean, std).
            %                   Default is [2 3 4].
            %            'FlagImage' - An optional logical image, with the same
            %                   size as the input image. Pixels with false, will be
            %                   not used in the fitting process.
            %                   If empty, use a matrix of true for all pixels.
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
            %            'SubBack' - Subtract background image.
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
            % Author : Eran Ofek (Mar 2025)
            % Example: RR=AI.Trace.extract
            
            arguments
                Obj
                Args.AperRadius        = [2 3 4];
                Args.BackAnnulus       = [15 20];   % note BackStd has its own annulus
                Args.FlagImage         = [];
                Args.FunMethod         = 'sum';
                Args.SubBack logical   = true;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % extract spectrum
                
                % aperture photometry:
                [Result(Iobj)] = imUtil.spec.extract.aperPhot(Obj(Iobj).LinTraceImage,...
                                                        'DimWave',Obj(Iobj).DimWave,...
                                                        'AperRadius',Args.AperRadius,...
                                                        'BackAnnulus',Args.BackAnnulus,...
                                                        'FlagImage',Args.FlagImage,...
                                                        'Std',sqrt(Obj(Iobj).Var),...
                                                        'Back',Obj(Iobj).Back,...
                                                        'Fun',Args.FunMethod,...
                                                        'SubBack',Args.SubBack);
                                                        
               
                
            end
            
        end
        
        function Result=psfphot(Obj, Args)
            %
        end
    end
    
    
    methods % wavelength calibration
        function Obj=waveCalib_LinesMatch(Obj, TraceInd, Args)
            %
           
            arguments
                Obj
                TraceInd
                Args
            end
            
            
        end
    end
    
    
        
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    
end

           
