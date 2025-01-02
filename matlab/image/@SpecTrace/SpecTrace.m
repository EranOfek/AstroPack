% SpecTrace - A container class for 2d spectral traces
%


classdef SpecTrace < Component
    
    properties (SetAccess = public)
        ExpectedPos(1,1)       % user specified expected spatial position of the trace
        MeasuredPos(1,1)       % measured spatial mean position of the trace
        Traces      = struct('X',[], 'FiltX1',[], 'FiltX1W',[], 'UnfiltX1',[], 'UnfiltX1W',[], 'FiltFit',[], 'UnfiltFit',[]);
        TracesWidth = struct('X',[], 'X2',[], 'FWHM',[]);
        TraceMethod = 'FiltFit';
                
    end
    
    methods % Constructor
        
    end
    
    methods %
        function Obj=searchTrace(AI, Args)
            % Start with image - find all traces
            
            
            %RR=imUtil.spec.trace.trace(Array);
            
            
        end
        
        function Obj=traceKalman(AI, Args)
            %
            
        end
        
        function Obj=traceByFilt(AI, Args)
            %
            
        end
        
        function LinAI=linTrace(Obj, AI, Args)
            %
            
        end
        
    end
    
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    
end

           
