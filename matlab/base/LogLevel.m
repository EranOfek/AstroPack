% LogLevel enumeration, used as parameter to the various 
% log functions to specify the level of the message.
% Only messages with log level above current log level are 
% displayed and written to log file.

classdef LogLevel < uint32
    % Log levels, used with MsgLogger class, msgLog(), etc.
    % Values should continuous and incremental
	enumeration
        None(0)         % Not set
        Fatal(1)        % Fatal error, must terminate
		Error(2)        % Error
		Warning(3)      % Warning
		Info(4)         % General info
        Verbose(5)      % Verbose 
		Debug(6)        % Detailed debug
        Test(7)         % Unit-Test
        DebugEx(8)      % Very detailed debug, above Test level
        All(9)          % All 
    end
end
