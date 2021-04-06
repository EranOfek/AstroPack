
classdef LogLevel < uint32
    % Log levels
	enumeration
        None(0)         % Not set
		Error(1)        % Error
		Warning(2)      % Warning
		Info(3)         % General info
		Debug(4)        % Detailed debug        
        Test(5)         % Unit-Test
    end
end

