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
        ExpectedWavePos      = [];
        SN
        %MeasuredSpatPos            % measured spatial mean position of the trace
        %Traces               = struct('MeanX',[], 'WavePix',[], 'Wave',[], 'X',[], 'Intensity',[], 'X2',[], 'FWHM',[], 'Image',[], 'Name',[], 'SN',[], 'ExpectedSpatPos',[], 'ExpectedWavePos',[],);
        MeanSpatPos
        
        Wave
        DispPixPos
        SpatPixPos
        Intensity
        SpatMom2
        SpatFWHM
        LinTraceImage
        LinTracePos
        Name
        WaveSolutionInfo
        
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
        function Obj=tarceByCollapse(Obj, Image)
            %
            
            
        end
        
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
    
    methods % spetial setters
        % REMOVE
        function TraceInd=setTrace(Obj, TraceInd, varargin)
            % Set a trace into the SpecTrace object
            % Input  : - A single element SpecTrace object.
            %          - Trace index. If empty, add trace.
            %            otherwise, edit existing trace with a given index.
            %          * ...,key,val,...
            %            Any of 'WavePos','Wave','X','X2','FWHM',...
            %            with values.
            %            WavePos will be attempted to populate
            %            automatically.
            % Output : - TraceInd used.
            % Author : Eran Ofek (2025 Jan)
            % Example: Tr.setTrace([], 'WavePix',(1:100), 'X',rand(100,1));
            
            arguments
                Obj(1,1)
                TraceInd     = [];
                
                Args.WavePix = [];
                Args.Wave    = [];
                Args.X       = [];
                Args.X2      = [];
                Args.FWHM    = [];
                Args.Intensity = [];
            end
            
            
            
            Args.MeanX = [];
            
            Ntraces = numel([Obj.Traces.MeanX]);
            %Ntraces = numel(Obj.Traces);
            
            if isempty(TraceInd)
                TraceInd = Ntraces + 1;
            else
                if TraceInd>Ntraces
                    error('Trace index doesnot exist');
                end
            end
            
            
            Ncol = numel(Obj.TraceCol);
            for Icol=1:1:Ncol
                ColName = Obj.TraceCol{Icol};
                if ~isempty(Args.(ColName))
                    Obj.Traces(TraceInd).(ColName) = Args.(ColName)(:);
                    Nwave = numel(Obj.Traces(TraceInd).(ColName));
                end
            end
            
            if isempty(Obj.Traces(TraceInd).WavePix)
                Obj.Traces(TraceInd).WavePix = (1:1:Nwave).';
            end
            
            if isempty(Obj.Traces(TraceInd).X)
                Obj.Traces(TraceInd).MeanX = NaN;
            else
                Obj.Traces(TraceInd).MeanX = median(Obj.Traces(TraceInd).X, 'all','omitnan');
            end
            
        end
        
        % REMOVE
        function Obj=setLinTrace(Obj, Image, TraceInd, Args)
            %
           
            arguments
                Obj
                Image            % if AastroImage: interpolate, if matrix use as is
                TraceInd   = [];
                
                Args.DimWave
                Args.TraceXY
            end
            
            % get index of LinTrace
            if isempty(TraceInd)
                if isempty(Obj.LinTrace.Image)
                    TraceInd = 1;
                else
                    TraceInd = numel(Obj.LinTrace) + 1;
                end
            end
            
            if isnumeric(Image)
                % assume 2D image is linearized trace spectrum
                Obj.LinTrace(TraceInd).Image = Image;
            elseif isa(Image, 'AstroImage')
                % input is an AstroImage
                
                 
            else
                error('Unknown Image type');
            end
                
            
            
        end
    end
        
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    
end

           
