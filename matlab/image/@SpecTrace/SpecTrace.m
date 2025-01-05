% SpecTrace - A container class for 2d spectral traces
%


classdef SpecTrace < Component
    
    properties (SetAccess = public)
        WaveDim
        ExpectedSpatPos            % user specified expected spatial position of the trace
        MeasuredSpatPos            % measured spatial mean position of the trace
        Traces      = struct('X',[], 'FiltX1',[], 'FiltX1W',[], 'UnfiltX1',[], 'UnfiltX1W',[], 'FiltFit',[], 'UnfiltFit',[]);
        TracesWidth = struct('X',[], 'X2',[], 'FWHM',[]);
        TraceMethod = 'FiltFit';
                
    end
    
    methods % Constructor
        
    end
    
    methods %
       
        
        
    end
    
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    
end

           
