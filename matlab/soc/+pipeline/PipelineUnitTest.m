
%--------------------------------------------------------------------------

classdef PipelineUnitTest < Component
    % U
    
    
    properties (SetAccess = public)

    end
    
   
    methods
        % Constructor    
        function Obj = PipelineUnitTest()
            Obj.setName('PipelineUnitTest');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);            
        end
    end
   


    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PipelineUnitTest test started\n');
            
            pipe = PipelineUltrasat;
            ImageFileName = '';
            HeaderFileName = '';
            
            pipe.processInputImage(Obj, ImageFileName, HeaderFileName);
                        
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'PipelineUnitTest test passed')
            Result = true;            
        end
    end    
    
end
