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

           
