% A FileManager class
%   For additional help - see static function FileManager.help
%
%   Specifically designed for managing multiple log files, but can be used
%   for other applications.
%
%   The class can manage the file names including adding path, extension,
%   and time stamps to the full file name.
%   The writeLine method can be used to write a message line to the cosole
%   and file.
%   
%   Properties:
%       
%   Functionality:
%       FileManager(N) - Constructor for FileManager
%       delete(Obj) - Destructor - close file if open
%       setters/getters - for file name and file name modifications
%       searchID(Obj, Name) - Search NameID in FileManager NameID strings and return the indices of the found files in the FileManager array.
%       isOpen(Obj, Name) - Check if files in FileManager are open correctly
%       open(Obj, Name) - Open a file in FileManager according to its NameID.
%       close(Obj, Name) - Close a file in FileManager according to its NameID.
%       writeHeader(Obj, Args) - Write an header to a file (Creation date and name)
%       writeLine(Obj, Line, Args) - Write a line to the file and/or console
%       deleteFiles(Obj, Name) - Delete files from disks.
%
%   Static functions:
%       getTimestamp - Return current date/time as sortable string with milliseconds
%       unitTest - unitTest for FileManager
%       help - An mlx help for FileManager.
%
% Examples:
%          FM = FileManager; FM.FileName = 'try1'; FM.isOpen
%          FM.open; FM.isOpen
%          FM.writeLine('bla bla bla');
%          FM.writeLine('bla bla bla','LevelStr','[DBG]');
%          FM.writeLine('bla bla bla','LineFormat','%20s');
%          FM.close          
%
            
classdef FileManager < handle
    % Properties
    properties 
        NameID char
        FullName                    = '';
        FileName                    = '';
        Path                        = '';FM = FileManager;
        ExtName                     = 'log';
        FileTimeStamp
        AddDateToFileName logical   = true;
        FID                         = [];
        WriteFile logical           = true;
        WriteConsole logical        = true;
        WriteHeader logical         = true;
    end
    
    
    properties (Hidden)
        CreatePath logical           = true; % will create path (mkdir) 
        OpenPermission               = 'a+';
        MachineFormat                = 'native';
        Encoding                     = '';
        TimeStampFormat              = '.yyyymmdd';
        DateChangeFracDay            = 0;    % fraction of day in which to change date
    end
    
    properties (Hidden, SetAccess=private)
        FileTime     % time corresponding to time stamp
        DateChanged logical          = false;
        FullNamePopulated logical    = false;
    end
    
        
%     
%     
%         FileName                % File name
%         Fid = []                % File handle
%         UserData                % Optional user data
%         LogPath = ''            % 'C:\\_Ultrasat\\log'
%         Timestamp = ''          % Timestamp when log file created
%         MaxFileSize = 10000000  % Limit file size automatic switching, will be renamed to '.old', default is 10MB
%         UseFlush = true         % Due to issue with fflush(), fclose() is called on each write (Chen, 13/06/2021), @Todo
%     end

    methods % Constructor
        function Obj = FileManager(N, Args)
            % Constructor for FileManager
            % Input  : - Number of elements in FileManager array.
            %            Default is 1.
            %          * ...,key,val,...
            %            'FileName' - A cell array of file bames.
            % Output : - A FileManager object.
            % Author : Eran Ofek (Jan 2022)
            % Example: FM = FileManager
            %          FM = FileManager(2);  % array of 2 objects
            %          FM = FileManager(2, 'FileName',{'a','b'});
            arguments
                N = 1;
                Args.FileName = '';
            end
            
            for I=1:1:N
                if ~isempty(Args.FileName)
                    Obj(I).FileName = Args.FileName{I};
                end
                %Obj(I)    = Obj(I).defaultPath;
                
                if ~isunix
                    Obj(I).Path = 'C:/log';
                else
                    Obj(I).Path = [tools.os.get_userhome, filesep, 'log'];
                end
            end
        end
             
        function delete(Obj)
            % Destructor - close file if open
            
            if ~isempty(Obj.FID) && Obj.FID>0
                Obj.close;
            end
        end

    end
    
    methods % setters and getters
        function Result = get.FileTime(Obj)
            % getter for FileTime - if empty set it to now
            % Example: FM = FileManager;
            %          FM.FileTime
            
            if isempty(Obj.FileTime)
                Obj.FileTime = now;
            end
            Result = Obj.FileTime;
        end
                
        function Result = get.FileTimeStamp(Obj)
            % getter for FileTimeStamp
            % Return the TimeStamp string to be added (if requested) to the
            % file name
            % Example: FM=FileManager;
            %          FM.FileTimeStamp
           
            Now = now;
            if Now>ceil(Obj.FileTime + Obj.DateChangeFracDay)
                % a new day
                Obj.FileTime = Now;
                % also delete FullName, so it will be constructed on next
                % request
                Obj.FullName = '';
                Obj.DateChanged = true;
            else
                Obj.DateChanged = false;
            end
            Obj.FileTimeStamp = datestr(Obj.FileTime, Obj.TimeStampFormat);
            Result = Obj.FileTimeStamp;
            
        end
        
        function Result = get.DateChanged(Obj)
            % Update the hidden property DateChanged by calling FileTimeStamp
            % Example: FM=FileManager;
            %          FM.DateChanged 
            
            Obj.FileTimeStamp;
            Result = Obj.DateChanged;
        end
    
        function Obj = set.FullName(Obj, Val)
            % setter for FullName, including updating FullNamePopulated
            %   If FullName is non empty, then FullNamePopulated is set to
            %   true.
            
            Obj.FullName = Val;
            if isempty(Val)
                % populated by empty
                Obj.FullNamePopulated = false;
            else
                % populated
                Obj.FullNamePopulated = true;
            end
        end
        
        function Obj = set.FileName(Obj, Val)
            % setter for FileName, including updating FullNamePopulated
            %   FullNamePopulated is set to false on any change.
                      
            Obj.FileName = Val;
            Obj.FullNamePopulated = false;
        end
        
        function Obj = set.Path(Obj, Val)
            % setter for Path, including updating FullNamePopulated
            %   FullNamePopulated is set to false on any change.
            
            Obj.Path = Val;
            Obj.FullNamePopulated = false;
        end
        
        function Obj = set.ExtName(Obj, Val)
            % setter for ExtName, including updating FullNamePopulated
            %   FullNamePopulated is set to false on any change.
            
            Obj.ExtName = Val;
            Obj.FullNamePopulated = false;
        end
        
        function Obj = set.AddDateToFileName(Obj, Val)
            % setter for AddDateToFileName, including updating FullNamePopulated
            %   FullNamePopulated is set to false on any change.
            
            Obj.AddDateToFileName = Val;
            Obj.FullNamePopulated = false;
        end
        
        function Result = get.FullName(Obj)
            % getter for file FullName
            %   The file full name is constrcured from the Path, FileName,
            %   FileTimeStamp, and ExtName.
            %   Note that the FileTimeStamp getter may change the date if
            %   needed.
            % Author : Eran Ofek (Jan 2022)
            % Example: FM=FileManager;
            %          FM.FullName
            %           
                 
            %if isempty(Obj.FullName) || 
            if ~isempty(Obj.FileName) && (Obj.DateChanged || ~Obj.FullNamePopulated)
                % construct FullName
                if isempty(Obj.ExtName)
                    ExtName = '';
                else
                    ExtName = sprintf('.%s',Obj.ExtName);
                end
                
                if Obj.AddDateToFileName
                    Result = sprintf('%s%s%s%s%s%s',Obj.Path, filesep, Obj.FileName, Obj.FileTimeStamp, ExtName);
                else
                    Result = sprintf('%s%s%s%s%s',Obj.Path, filesep, Obj.FileName, ExtName);
                end  
                Obj.FullName = Result;
                Obj.FullNamePopulated = true;
            else
                Result = Obj.FullName;
            end
        end
    end

    methods  % search by ID file names and path
        function Ind = searchID(Obj, Name)
            % Search NameID in FileManager NameID strings and return the indices of the found files in the FileManager array.
            % Input  : - A FileManager object (multi elements are supported).
            %          - A char or a cell array of chars containing NameID
            %            to search in the FileManager object.
            %            If numeric indices then return these indices
            %            without change.
            %            If empty, then will return all indices.
            %            Default is empty.
            % Output : - The indices of the identified NameID in the
            %            FileManager object.
            % Author : Eran Ofek (Jan 2022)
            % Example: FM = FileManager; FM.NameID = 'myname';
            %          Ind = searchID(FM, 'myname'); % return 1.
            
            arguments
                Obj
                Name   = [];
            end
            
            if isempty(Name)
                Ind = (1:1:numel(Obj));
            else
                if isnumeric(Name)
                    Ind = Name;
                else
                    if ischar(Name)
                        Name = {Name};
                    end
                    [~,Ind] = ismember(Name, {Obj.NameID});
                end
            end
        end   
        
    end

    methods % open/close
        function Result = isOpen(Obj, Name)
            % Check if files in FileManager are open correctly
            %   FID not empty and >0
            % Input  : - A FileManager object.
            %          - A char or a cell array of chars containing NameID
            %            to search in the FileManager object.
            %            If empty, then will return all indices.
            %            Default is empty.
            % Output : - A vector of of logicals, indicating if the files
            %            are open. Each element in this vector corresponds
            %            to a NameID, or if NamID is empty, it corresponds
            %            to elements in the FileManager input object.
            % Author : Eran Ofek (Jan 2022)
            % Example: FM = FileManager;
            %          FM.FileName = 'try1';
            %          FM.isOpen
            
            arguments
                Obj
                Name     = [];
            end
            
            Ind    = searchID(Obj, Name);
            Nind   = numel(Ind);
            Result = false(Nind,1);
            for I=1:1:Nind
                Result(I) = ~isempty(Obj(Ind(I)).FID) && Obj(Ind(I)).FID>0;
            end
            
        end
        
        function Obj = open(Obj, Name)
            % Open a file in FileManager according to its NameID.
            % Input  : - A FileManager object.
            %          - A char or a cell array of chars containing NameID
            %            to search in the FileManager object.
            %            If empty, then will return all indices.
            %            Default is empty.
            % AUthor : Eran Ofek (Jan 2022)
            % Example: FM = FileManager;
            %          FM.FileName = 'try1';
            %          FM.isOpen
            %          FM.open; FM.isOpen
            
            
            arguments
                Obj
                Name      = [];
            end
            
            Ind = searchID(Obj, Name);
            Nind = numel(Ind);
            for I=1:1:Nind
                if isempty(Obj(Ind(I)).FID) || Obj(Ind(I)).FID<0 || ~isempty(Obj(Ind(I)).FileName)
                    % open file
                    % create path if needed
                    if Obj(Ind(I)).CreatePath
                        [Obj.Path] = fileparts(Obj(Ind(I)).FullName);
                        mkdir(Obj.Path);
                    end
                    Obj(Ind(I)).FID = fopen(Obj(Ind(I)).FullName, Obj.OpenPermission, Obj.MachineFormat, Obj.Encoding);
                    if Obj(Ind(I)).WriteHeader
                        Obj(Ind(I)).writeHeader;
                    end
                end
            end
        end
        
        function Obj = close(Obj, Name)
            % Close a file in FileManager according to its NameID.
            % Input  : - A FileManager object.
            %          - A char or a cell array of chars containing NameID
            %            to search in the FileManager object.
            %            If empty, then will return all indices.
            %            Default is empty.
            % AUthor : Eran Ofek (Jan 2022)
            % Example: FM = FileManager;
            %          FM.FileName = 'try1';
            %          FM.isOpen
            %          FM.open; FM.isOpen
            %          FM.close
            %          FM.deleteFiles
           
            arguments
                Obj
                Name      = [];
            end
            
            Ind = searchID(Obj, Name);
            Nind = numel(Ind);
            for I=1:1:Nind
                if ~isempty(Obj(Ind(I)).FID)
                    % close file
                    fclose(Obj(Ind(I)).FID);
                    Obj(Ind(I)).FID = [];
                end
            end
        end
        
        function Obj = deleteFiles(Obj, Name)
            % Delete files from disks.
            % Input  : - A FileManager object.
            %          - A char or a cell array of chars containing NameID
            %            to search in the FileManager object.
            %            If empty, then will return all indices.
            %            Default is empty.
            % AUthor : Eran Ofek (Jan 2022)
            % Example: FM = FileManager;
            %          FM.FileName = 'try1';
            %          FM.isOpen
            %          FM.open; FM.isOpen
            %          FM.close
            %          FM.deleteFiles
            
            arguments
                Obj
                Name      = [];
            end
            
            Ind = searchID(Obj, Name);
            Nind = numel(Ind);
            for I=1:1:Nind
                if ~isempty(Obj(Ind(I)).FID)
                    % close file
                    fclose(Obj(Ind(I)).FID);
                    Obj(Ind(I)).FID = [];
                end
                % delete file
                delete(Obj(Ind(I)).FullName);
            end
            
        end
    end
      
    methods % write
        function writeHeader(Obj, Args)
            % Write an header to a file (Creation date and name)
            % Input  : - A single element FileManager object.
            %          * ...,key,val,...
            %            'TimeStampFormat' - Time stamp format.
            %                   Default is 'yyyy-mm-dd HH:MM:SS.FFF'.
            % Eran Ofek (Jan 2022)
            % Example: FM = FileManager; FM.FileName = 'try1'; FM.isOpen
            %          FM.open; FM.isOpen; FM.close;
            
            arguments
                Obj(1,1)
                Args.TimeStampFormat       = 'yyyy-mm-dd HH:MM:SS.FFF';
            end
            
            if Obj.WriteFile && ~isempty(Obj.FID) && Obj.FID>0
                fprintf(Obj.FID,'   FileName: %s\n',Obj.FullName);
                fprintf(Obj.FID,'   Created : %s\n',datestr(Obj.FileTime, Args.TimeStampFormat));
                fprintf(Obj.FID,'---------------------------------------\n');
            end
        end
        
        function writeLine(Obj, Line, Args)
            % Write a line to the file and/or console
            %   This can be used to write a message line into the file and
            %   or console.
            %   Will write the line to the file only if WriteFile=true, and
            %   will write it to the console only if WriteConsole=true.
            %   Can add time stamp as well as additional strings to the
            %   message.
            %   The LevelStr appears after the time stamp.
            % Input  : - A single element FileManager object.
            %          - A char array or string to write.
            %          * ...,key,val,...
            %            'LineFormat' - Line format. Default is '%s'.
            %            'AddTimeStamp' - Add time stamp. Default is true.
            %            'TimeStampFormat' - Time stamp format.
            %                   Default is 'yyyy-mm-dd HH:MM:SS.FFF'.
            %            'LevelStr' - Additional string that will appear
            %                   after the time stamp and before the message.
            %                   Default is ''.
            %            'Blanks' - A two elements vector containing the
            %                   number of blanks before and after the LevelStr
            %                   string. Default is [1 1].
            %            'AddCR' - Add CR ("\n") at the end of line.
            %                   Default is true.
            % Author : Eran Ofek (Jan 2022)
            % Example: FM = FileManager; FM.FileName = 'try1'; FM.isOpen
            %          FM.open; FM.isOpen
            %          FM.writeLine('bla bla bla');
            %          FM.writeLine('bla bla bla','LevelStr','[DBG]');
            %          FM.writeLine('bla bla bla','LineFormat','%20s');
            %          FM.close
            
            arguments
                Obj(1,1)
                Line
                Args.LineFormat            = '%s';
                Args.AddTimeStamp logical  = true;
                Args.TimeStampFormat       = 'yyyy-mm-dd HH:MM:SS.FFF';
                Args.LevelStr              = '';
                Args.Blanks                = [1 1];
                Args.AddCR logical         = true;   % add \n
            end
            
            if Obj.WriteFile || Obj.WriteConsole
                if Args.AddTimeStamp
                    TimeStamp = datestr(now, Args.TimeStampFormat);
                else
                    TimeStamp = '';
                end
               
                if Args.AddCR
                    FullLine = sprintf('%s%s%s%s%s\n', TimeStamp, blanks(Args.Blanks(1)), Args.LevelStr, blanks(Args.Blanks(1)), Line);
                else
                    FullLine = sprintf('%s%s%s%s%s', TimeStamp, blanks(Args.Blanks(1)), Args.LevelStr, blanks(Args.Blanks(1)), Line);
                end
                
                if Obj.WriteFile && ~isempty(Obj.FID) && Obj.FID>0
                    fprintf(Obj.FID, Args.LineFormat, FullLine);
                end
                if Obj.WriteConsole
                    fprintf(Args.LineFormat, FullLine);
                end
            end                    
        end
    end
    
    methods (Static)  % aux static functions

        function Result = getTimestamp()
            % Return current date/time as sortable string with
            % milliseconds, such as '2021-12-20 11:05:59.357'
            % Output:  Current time formated as 'yyyy-mm-dd HH:MM:SS.FFF'
            % Example: T = LogFile.getTimestamp()
            Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
        end
    end
    
    methods (Static) % singelton

%         function Result = getSingleton(Args)
%             % Return singleton FileManager object.
%             % Input:   -
%             % Output:             
%             % Example: SysLogFile = LogFile.getSingleton();
%             
%             arguments
%                 Args.FileName = ''          % File name
%                 Args.UseTimestamp = false   % true to add current timestamp to file name
%             end
%             
%             persistent Result
%             if isempty(Result)
%                 Result = FileManager;
%             end
%         end

    end
    
    methods (Static) % help
        function help
            % mlx help for FileManager
            edit manuals.classes.FileManager
        end
    end
    
    methods (Static) 
        Result = unitTest()
            % Unit test
    end

end
