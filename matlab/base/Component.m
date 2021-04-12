% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Component < Base
    % Parent class for all components
    
    % Properties
    properties (SetAccess = public)
        Name                    % Name string
        Owner                   % Indicates the component that is responsible for streaming and freeing this component
        Tag                     % Optional tag (i.e. for events handling)
        Config Configuration    % Configuration, deafult is system configuration
        Log LogFile             % Logger, default is system logger
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Component()
            % By default use system log and configuration
            Obj.LogFile = MsgLogger.getSingle();
            Obj.Config = Configuration.getSingle();
        end
        
        function msgLog(Obj, Level, varargin)  
            % Write message to log
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

