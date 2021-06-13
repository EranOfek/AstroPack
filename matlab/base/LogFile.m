% Basic log file
                                   
%--------------------------------------------------------------------------

classdef LogFile < handle
    % Properties
    properties (SetAccess = public)
        FileName
        Fid = []            % File handle
        UserData
        LogPath = '';       %"C:\\_Ultrasat\\log";
        UseFlush = true;    % DO NOT CHANGE - It does not work without close (13/06/2021)
    end
    
    %-------------------------------------------------------- 
    methods
        
        function Obj = LogFile(FileName)
            % Constructor for LogFile
            
            arguments
                FileName = 'default';
            end
            
            if isempty(Obj.LogPath)
                %if tools.os.iswindows()
                if ~isunix
                    Obj.LogPath = 'C:\\Temp';
                else
                    Obj.LogPath = '/tmp/';
                end
            end
            
            fn = sprintf('%s-%s.log', Obj.getFileNameTimestamp(), FileName);
            Obj.FileName = fullfile(Obj.LogPath, fn);
            Obj.write('=========================================== Started');
        end
        
        
        function delete(Obj)
            % Destructor - close file
            if ~isempty(Obj.Fid)
                fclose(Obj.Fid);
                Obj.Fid = [];
            end            
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
            
            % Open file
            if isempty(Obj.Fid)
                Obj.Fid = fopen(Obj.FileName, 'at');
            end
            
            % Write
            % The default behavior of MATLAB File I/O commands FPRINTF 
            % and FWRITE is to flush the buffer after every call when 
            % writing to a file other than the special output streams 
            % STDOUT and STDERR. There is, therefore, no need to 
            % explicitly flush the output buffer in this case.
            fprintf(Obj.Fid, Prompt);
            fprintf(Obj.Fid, varargin{:});
            fprintf(Obj.Fid, '\n');
            
            % Close file, as fflush does not work well (why?)
            if Obj.UseFlush
                fclose(Obj.Fid);
                Obj.Fid = [];
            end
            
            Result = true;
        end
        
    end

    
    methods(Static)        
                      
        function Result = getSingleton()
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = LogFile();
            end
            Result = PersObj;
        end
        
        
        function Result = getTimestamp()
            % Return current time as string
            Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
        end
        
        function Result = getFileNameTimestamp()
            Result = datestr(now, 'yyyy-mm-dd__HH-MM-SS');
        end
    end
    
       
    methods(Static) % Unit test
        function Result = unitTest()
            fprintf('LogFile test started\n');
            Lf = LogFile.getSingleton();
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

