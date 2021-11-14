% LogLevel enumeration, used as parameter to the various
% log functions to specify the level of the message.
% Only messages with log level above current log level are
% displayed and written to log file.

% #functions (autogen)
% unitTest -
% #/functions (autogen)
%

classdef LogLevel < uint32
    % Log levels, used with MsgLogger class, msgLog(), etc.
    % Values should continuous and incremental
	enumeration
        None(0)         % Not set
        Fatal(1)        % Fatal error, must terminate
        Error(2)        % Error
        Assert(3)       % Assert
        Warning(4)      % Warning
        Info(5)         % General info
        Verbose(6)      % Verbose info
        Debug(7)        % Detailed debug
        DebugEx(8)      % Very detailed debug, above Test level
        Perf(9)         % Performance timing
        Test(10)        % Unit-Test        
        All(11)         % All
    end
    
    
    methods(Static)
        function Result = unitTest()
            Result = true;
        end
    end
    
end
