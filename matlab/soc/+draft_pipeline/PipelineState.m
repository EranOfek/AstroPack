
classdef PipelineState < uint32
    % Pipeline states
	enumeration
        None(0)         % Not set
		Ready(1)        % Ready to process new image
		ImageStart(2)   %
        
        % Image processing states
        ImageProcess(4)      %
		
        % Post process
        ImageDone(6)    % Image process done
        ImageDelay(7)   % Optional post-image delay (to reduce CPU load?)
        FatalError(9)   %
    end
end
