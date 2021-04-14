% Basic log file
                                   
%--------------------------------------------------------------------------

classdef LogFile < handle
    % Properties
    properties (SetAccess = public)
        FileName
        UserData
        LogPath = ''; %"C:\\_Ultrasat\\log";
    end
    
    %-------------------------------------------------------- 
    methods
        
        function Obj = LogFile(FileName)
            % Constructor for LogFile
            
            arguments
                FileName = 'Default';
            end
            fn = sprintf('%s-%s.log', Obj.getFileNameTimestamp(), FileName);
            Obj.FileName = fullfile(Obj.LogPath, fn);
            Obj.write('=========================================== Started');
        end
    end
    

    methods
       
        function Result = write(Obj, varargin)
            % Log text line to file
            Result = write2(Obj, '', varargin{:});
        end
        
        
        function Result = write2(Obj, Title, varargin)
            % Log text line to file
            if isempty(Title)
                Prompt = sprintf('%s > ', Obj.getTimestamp());
            else
                Prompt = sprintf('%s > %s ', Obj.getTimestamp(), Title);
            end
            
            Fid = fopen(Obj.FileName, 'at');
            fprintf(Fid, Prompt);
            fprintf(Fid, varargin{:});
            fprintf(Fid, '\n');
            fclose(Fid);
            Result = true;
        end
        
    end

    
    methods(Static)        
                      
        function Result = getSingle()
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = LogFile('');
            end
            Result = PersObj;
        end
        
        
        function Result = getTimestamp()
            % Return current time as string
            Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
        end
        
        function Result = getFileNameTimestamp()
            Result = datestr(now, 'yyyy-mm-dd');
        end
    end
    
       
    methods(Static) % Unit test
        function Result = unitTest()
            fprintf('LogFile test started\n');
            Lf = LogFile.getSingle();
            Lf.write('LogFile test started');
            
            for i=1:1:3
                Lf.write('Line: %d', i);
            end
           
            Lf.write('LogFile test passed');
            fprintf('LogFile test passed\n');
            Result = true;
        end
    end
        
end

