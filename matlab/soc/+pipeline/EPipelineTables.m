
classdef EPipelineTables < uint32
    % Pipeline tables
	enumeration
        
        % Images
        RawImages
        ProcessedCroppedImages
        StackedCroppedImages
        CalibrationImages
        ReferenceImages
        CoaddedImages
        
        % Sources
        Sources
        StackedCroppedSources
        ReferenceSources
        TransientSources
        
    end
end
