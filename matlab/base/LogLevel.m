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
        Verbose(6)      % Verbose
		Debug(7)        % Detailed debug
        Test(8)         % Unit-Test
        DebugEx(9)      % Very detailed debug, above Test level
        All(10)          % All
    end
    
    
    methods(Static)
        function Result = unitTest()
            Result = true;
        end
    end
    
end
