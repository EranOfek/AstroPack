% Simple textual log file
% Note that this class is derived from handle and not from Component

% #functions
% LogFile - Constructor for LogFile
% delete - Destructor - close file
% getFileName (Static) -
% getFileNameTimestamp (Static) - Return current date/time as sortable string
% getSingleton (Static) - Return singleton object, this is the default log file to be used by current process (or workspace)
% getTimestamp (Static) - Return current date/time as sortable string with milliseconds
% unitTest - LogFile.unitTest
% write - Write text line to file
% write2 - Log title and text line to file
% #/functions
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

        function Obj = LogFile(FileName)
            % Constructor for LogFile
            arguments
                FileName = 'default';
            end

            % Filename includes folder name
            if contains(FileName, '/') || contains(FileName, '\\')
                [Obj.LogPath, FileName, Ext] = fileparts(FileName);
                FileName = [FileName, Ext];
            else

                % Filename does not include folder name, make sure that
                % we have default folder name
                if isempty(Obj.LogPath)
                    if ~isunix
                        Obj.LogPath = 'C:\\Temp';
                    else
                        Obj.LogPath = '/tmp/';
                    end
                end
            end

            % Prepare file name from path, timestamp, and specified name
            Obj.Timestamp = Obj.getFileNameTimestamp();
            fn = sprintf('%s-%s.log', Obj.Timestamp, FileName);
            Obj.FileName = fullfile(Obj.LogPath, fn);

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

        function Result = getSingleton()
            % Return singleton object, this is the default log file
            % to be used by current process (or workspace)
            persistent PersObj
            if isempty(PersObj)
                PersObj = LogFile();
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


        function Result = getFileName(SubName)
            %
            Obj = LogFile.getSingleton();
            fn = sprintf('%s-%s.log', Obj.Timestamp, SubName);
            Result = fullfile(Obj.LogPath, fn);
        end
    end


    methods(Static) % Unit test
        Result = unitTest()
    end

end
