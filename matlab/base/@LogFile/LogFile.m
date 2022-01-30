% #autogen:_off
%
% Simple textual log file with timestamp and other features.
% Note that this class is derived from 'handle' and not from 'Component'
% 
% Author: Chen Tishler (Apr 2021)
%

% #functions (autogen)
% LogFile - Constructor for LogFile
% delete - Destructor - close file
% getFileNameTimestamp - Return current date/time as sortable string
% getSingleton - Return singleton object, this is the default log file to be used by current process (or workspace)
% getTimestamp - Return current date/time as sortable string with milliseconds
% write - Write text line to file
% write2 - Log title and text line to file
% #/functions (autogen)
%

classdef LogFile < handle
    % Properties
    properties (SetAccess = public)
        FileName                % File name
        Fid = []                % File handle
        UserData                % Optional user data
        LogPath = ''            % 'C:\\_Ultrasat\\log'
        Timestamp = ''          % Timestamp when log file created
        MaxFileSize = 10000000  % Limit file size automatic switching, will be renamed to '.old', default is 10MB
        UseFlush = true         % Due to issue with fflush(), fclose() is called on each write (Chen, 13/06/2021), @Todo
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = LogFile(FileName, Args)
            % Constructor for LogFile
            % Input:  FileName - Log file name. If empty, use 'DefaultLogFile'.
            %               If FileName does not contain path separator, call 
            %               LogFile.defaultPath() to get default path for log files.
            %         'UseTimestamp' - When true, add current timestamp to file name
            arguments
                FileName = ''               % File name
                Args.UseTimestamp = false   % True to add timestamp to file name, 
            end

            % Make timestamp
            Obj.Timestamp = Obj.getFileNameTimestamp();            
            
            % Empty file name, use default
            if isempty(FileName)
                FileName = 'DefaultLogFile';
            end
            
            % Extension is not specified, use default
            [Path, Fn, Ext] = fileparts(FileName);
            if isempty(Ext)
                FileName = [FileName, '.log'];
            end
            
            % Filename does not include folder name, use deafult folder
            if ~contains(FileName, '/') && ~contains(FileName, '\')
                FileName = fullfile(LogFile.defaultPath(), FileName);                
            end
                     
            % Add timestamp before file name
            if Args.UseTimestamp                
                [Path, FileName, Ext] = fileparts(FileName);
                FileName = [FileName, Ext];
                FileName = fullfile(Path, sprintf('%s-%s', Obj.Timestamp, FileName));
            end
            
            % Store
            Obj.FileName = FileName;

            % Create folder
            [Path, FileName, Ext] = fileparts(FileName);
            if ~isfolder(Path)
                fprintf('LogFile: Creating folder: %s\n', Path);
                mkdir(Path);
            end
                
            % Write separation line to clearly mark that we started now
            Obj.write('=========================================== Started');
        end


        function delete(Obj)
            % Destructor - close file if open
            if ~isempty(Obj.Fid)
                fclose(Obj.Fid);
                Obj.Fid = [];
            end
        end

    end


    methods

        function Result = write(Obj, varargin)
            % Write text line to file
            % Input:   varargin - Any fprintf() arguments
            % Output:  true on success
            % Example: MyLogFile.write('Elapsed time: %f', toc)
            Result = write2(Obj, '', varargin{:});
        end


        function Result = write2(Obj, Title, varargin)
            % Write title and text line to file, usually used internally by
            % other components.
            % Input:   varargin - Any fprintf() arguments
            % Output:  true on success            
            % Example: MyLogFile.write2('Perf', 'Elapsed time: %f', toc)
            
            % Prepare prompt from timestamp and title
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

            % Check file size and switch
            if Obj.MaxFileSize > 0
                FileSize = ftell(Obj.Fid);
                if FileSize >= Obj.MaxFileSize
                    fprintf('LogFile: Switching file, current size: %d, MaxFileSize: %d - %s\n', FileSize, Obj.MaxFileSize, Obj.FileName);
                    
                    % Close file
                    fprintf(Obj.Fid, '%s > Switching file, current size: %d, MaxFileSize: %d\n', Obj.getTimestamp(), FileSize, Obj.MaxFileSize);
                    fclose(Obj.Fid);
                    Obj.Fid = [];
                    
                    % Rename file, delete existing .old if exists
                    [TmpPath, TmpFileName, TmpExt] = fileparts(Obj.FileName);
                    OldName = fullfile(TmpPath, strcat(TmpFileName, '.old'));
                    if isfile(OldName)
                        delete(OldName);
                    end
                    movefile(Obj.FileName, OldName);                    
                end
            end
            
            % Close file, NOTE: fflush() does not work well (why?)
            % @Todo - Fix to work with fflush() and not with fclose()
            %if Obj.UseFlush
            if ~isempty(Obj.Fid)
                fclose(Obj.Fid);
                Obj.Fid = [];
            end

            Result = true;
        end

    end


    methods(Static)

        function Result = defaultPath(varargin)
            % Set/get default log path
            % Input: When input argument is specified, store it as the
            % current log path, replacing the old value.
            % Example: MyPath = LogFile.defaultPath()
            % Example: LogFile.defaultPath('/tmp/log_folder')
            persistent Path
            if numel(varargin) > 0
                Path = varargin{1};
            end
            
            % Use system folder if not set
            if isempty(Path)
                if ~isunix
                    Path = 'C:/AstroPack/log';
                else
                    Path = '~/AstroPack/log';
                end                
            end
            Result = Path;
        end
        
        
        function Result = getSingleton(Args)
            % Return singleton object, this is the default log file
            % to be used by current process (or workspace)
            % Input:   -
            % Output:             
            % Example: SysLogFile = LogFile.getSingleton();
            arguments
                Args.FileName = ''          % File name
                Args.UseTimestamp = false   % true to add current timestamp to file name
            end
            persistent PersObj
            if isempty(PersObj)
                PersObj = LogFile(Args.FileName, 'UseTimestamp', Args.UseTimestamp);
            end
            Result = PersObj;
        end


        function Result = getTimestamp()
            % Return current date/time as sortable string with
            % milliseconds, such as '2021-12-20 11:05:59.357'
            % Input:   -
            % Output:  Current time formated as 'yyyy-mm-dd HH:MM:SS.FFF'
            % Example: T = LogFile.getTimestamp()
            Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
        end


        function Result = getFileNameTimestamp()
            % Return current date/time as sortable string, suitable for
            % file name on Linux and Windows, such as '2021-12-20__11-07-03'
            % Input:   -
            % Output:  Current time formated as 'yyyy-mm-dd__HH-MM-SS'
            % Example: T = LogFile.getFileNameTimestamp()
            Result = datestr(now, 'yyyy-mm-dd__HH-MM-SS');
        end

    end


    methods(Static) 
        Result = unitTest()
            % Unit test
    end

end
