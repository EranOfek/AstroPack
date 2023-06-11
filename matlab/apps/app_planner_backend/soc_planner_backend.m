%==========================================================================
% Author: Chen Tishler, May 2023
%
%
%==========================================================================

function soc_planner_backend()
    % App

    % Set logfile name
	fprintf('soc_planner_backend started\n');
    LogFile.getSingleton('FileName', 'soc_planner_backend');

    while true
        try
            fprintf('Calling PlannerBackend.unitTest\n');
            Backend = PlannerBackend();
            Backend.run();
            fprintf('Returned from FileProcessor.unitTest\n');
        catch
            fprintf('soc_planner_backend exception\n');
        end
    end
    
    fprintf('soc_planner_backend done\n');
end


