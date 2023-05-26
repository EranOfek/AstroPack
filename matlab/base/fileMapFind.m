function Result = FileMapFind(FileName, Args)
    % Use singleton FileMap object to locate file, when it does not include
    % a path. This is required to solve the 'addpath issue' for compiled
    % applications.
        % Description: load a mat file containing a single variable to a variable
    %              name (rather than a structure, like load.m).
    %              If multiple variables are returned then will behave like
    %              load.m
    % Input  : - Mat file name.
    %           * Pairs of ...,key,val,...
    %             The following keys are available:            			            
    %             'Single' - True to check that 
    %             'Assert' - True to assert on error (will generate exception)
    %
    % Output : - Variable name.
    % Author : Chen Tishler, Dec. 2022
    arguments
        FileName
        Args.Single = true      %
        Args.Assert = true      %
    end
    
    io.msgLog(LogLevel.Debug, 'FileMapFind: %s', FileName);
    
    if Args.Single
        
        if contains(FileName, '/') || ~contains(FileName, '\')
            [Path, Name, Ext] = fileparts(FileName);
            FileName = strcat(Name, Ext);
            io.msgLog(LogLevel.Debug, 'FileMapFind: Path removed: %s', FileName);            
        end
        
        Result = FileMap.getSingleton().findFile(FileName, 'Single', true);
        
%         if numel(Result) == 0
%             io.msgLog(LogLevel.Debug, 'FileMapFind: NOT found: %s', FileName); 
%         end
%         
%         if numel(Result) > 1
%             io.msgLog(LogLevel.Debug, 'FileMapFind: Expected single result, got: %d', numel(Result)); 
%         end        
        
        if Args.Assert
            assert(numel(Result) > 0);
        end
        
        if isfile(Result)        
            io.msgLog(LogLevel.Debug, 'FileMapFind: Found: %s', Result); 
        else
            % File not found - WE HAVE A PROBLEM        
            dbstack();
        end
    else
        Result = FileMap.getSingleton().findFile(FileName, 'Single', false);
        if Args.Assert
            assert(numel(Result) > 0);
        end
    end
end
