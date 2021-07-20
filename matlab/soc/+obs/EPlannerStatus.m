
classdef EPlannerStatus < uint32
    % Planner status, used in table ...
	enumeration
        None(0)         % Not set
        Pending(1)      % Error
        Confirmed(2)    % Error
        Ready(3)        % Error
        Active(4)       % Error
		Aborted(5)      % Error

    end
end
