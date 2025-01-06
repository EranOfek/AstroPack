% SpecTrace - A container class for 2d spectral traces
%


classdef SpecTrace < Component
    
    properties (SetAccess = public)
        WaveDim
        ExpectedSpatPos            % user specified expected spatial position of the trace
        ExpectedWavePos      = [];
        SN
        %MeasuredSpatPos            % measured spatial mean position of the trace
        Traces               = struct('MeanX',[], 'WavePix',[], 'Wave',[], 'X',[], 'Intensity',[], 'X2',[], 'FWHM',[]);
        TraceNames
        LinTraceImage
        LinTracePos
    end
    
    properties (Hidden, Constant)
        TraceCol  = ["MeanX", "WavePix", "Wave", "X", "Intensity", "X2", "FWHM"];
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
    
    methods % spetial setters
        function Obj=setTrace(Obj, TraceInd, Args)
            % Set a trace into the SpecTrace object
            % Input  : - A single element SpecTrace object.
            %          - Trace index. If empty, add trace.
            %            otherwise, edit existing trace with a given index.
            %          * ...,key,val,...
            %            Any of 'WavePos','Wave','X','X2','FWHM'
            %            with a vector of values.
            %            WavePos will be attempted to populate
            %            automatically.
            % Output : - self.
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
        
        
    end
        
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    
end

           
