% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Component < Base
    % Properties
    properties (SetAccess = public)
        Config Configuration
        %Log LogFile
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Component()
            Obj.Config = Configuration.getSingle();
        end
        
        function msgLog(Obj, Level, varargin)            
            MsgLogger.msgLog(Level, varargin{:});
        end
    end
    
      
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'Component test started');
            
            a = Component;
            a.msgLog(LogLevel.Test, 'a created');
            
            b = Component;
            b.msgLog(LogLevel.Test, 'b created');            
            
            c = Component;
            c.msgLog(LogLevel.Test, 'c created');            
           
            io.msgLog(LogLevel.Test, 'Component test passed');            
            Result = true;
        end
    end    
end

