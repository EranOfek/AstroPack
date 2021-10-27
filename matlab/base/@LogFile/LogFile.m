% Simple textual log file
% Note that this class is derived from handle and not from Component

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
        FileName            % File name
        Fid = []            % File handle
        UserData            % Optional user data
        LogPath = ''        % 'C:\\_Ultrasat\\log'
        Timestamp = ''      % Timestamp when log file created
        UseFlush = true     % DO NOT CHANGE - It does not work without close (Chen, 13/06/2021)
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = LogFile(FileName, Args)
            % Constructor for LogFile
            arguments
                FileName = ''               %
                Args.UseTimestamp = false   %
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

            % Write separation line to clearly mark that we started now
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
            % Write text line to file
            Result = write2(Obj, '', varargin{:});
        end


        function Result = write2(Obj, Title, varargin)
            % Log title and text line to file

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

            % Close file, NOTE: fflush() does not work well (why?)
            if Obj.UseFlush
                fclose(Obj.Fid);
                Obj.Fid = [];
            end

            Result = true;
        end

    end


    methods(Static)

        function Result = defaultPath(varargin)
            % Set/get default log path
            
            % Argument is specified, store it as default
            persistent Path
            if numel(varargin) > 0
                Path = varargin{1};
            end
            
            % Use system folder if not set
            if isempty(Path)
                if ~isunix
                    Path = 'C:\Temp';
                else
                    Path = '/tmp/';
                end                
            end
            Result = Path;
        end
        
        
        function Result = getSingleton(Args)
            % Return singleton object, this is the default log file
            % to be used by current process (or workspace)
            arguments
                Args.FileName = ''
                Args.UseTimestamp = false
            end
            persistent PersObj
            if isempty(PersObj)
                PersObj = LogFile(Args.FileName, 'UseTimestamp', Args.UseTimestamp);
            end
            Result = PersObj;
        end


        function Result = getTimestamp()
            % Return current date/time as sortable string with milliseconds
            Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
        end


        function Result = getFileNameTimestamp()
            % Return current date/time as sortable string
            Result = datestr(now, 'yyyy-mm-dd__HH-MM-SS');
        end


        %function Result = getFileName(SubName)
            % Currently unused
            %Obj = LogFile.getSingleton();
            %fn = sprintf('%s-%s.log', Obj.Timestamp, SubName);
            %Result = fullfile(Obj.LogPath, fn);
        %end
    end


    methods(Static) % Unit test
        Result = unitTest()
    end

end
