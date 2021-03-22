% @Chen - Basic log file

% Logger Class
% Package: 
% Description:

% See: DbStack
% Add: Log Level
%--------------------------------------------------------------------------


classdef LogFile < handle
    % Properties
    properties (SetAccess = public)
        FileName
        UserData
        LogPath = "C:\\iai\\log";
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function obj = LogFile(FileName)
            obj.FileName = fullfile(obj.LogPath, FileName);
        end
    end
    
    % setters/getters
    methods
       
        function Result = log(Obj, Msg)
            % Log text line to file
            Line = Obj.getTimestamp() + " " + Msg;
            Fid = fopen(Obj.Filename, "at");
            fprintf(Fid, "%s\n", Line);
            fclose(Fid);
            Result = true;
        end
        

        function Result = getTimestamp(Obj)
            % Return current time as string
            Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
        end
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            Lf = TLogFile("m1.log");
            
            for i=1:1:10
                Lf.log("Line: " + string(i));
            end
           
            Result = true;
        end
    end
        
end

