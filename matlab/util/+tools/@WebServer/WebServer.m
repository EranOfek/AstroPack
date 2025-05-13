% tools.WebServer - A class for launching a webserver listenr
%
% Example:
% WS = tools.WebServer;
% WS.runWebServer;  % run web server in background
% WS.killWebServer  % kill the WebServer
% Use: curl "http://localhost:8080/echo?Args1=123&Args2=hello%20world&Args3=-112.6661"

classdef WebServer < Component
    %
    
    properties    
        PID         = [];
        OutputFile  = "/tmp/http_requests.log";

       
    end
    
    properties (Constant, Hidden)
        Port        = 8080;
        JavaFunName = 'SimpleHttpLogger';
    end
    
    
    methods % Constructor
       
        %function Obj = Db(Args)
        %    % Constructor for db.Db
        %end
             
        function delete(Obj)
            % Destractor for db.Db


            Obj.killWebServer;
        end

    end
    
    methods % setter/getters
        function Obj=set.PID(Obj, Val)
            % setter for PID

            if ischar(Val) || isstring(Val)
                Obj.PID = str2double(Val);
            else
                Obj.PID = Val;
            end
        end
    end
      
    methods (Static) % construction
        function Path = getWebServerPath()
            % Get Java WebServer path
            % Input  : null
            % Output : - Path
            % Author : Eran Ofek (May 2025)
            % Example: tools.WebServer.getWebServerPath

            FullPath = mfilename('fullpath');
            Path = fileparts(FullPath);
        end

        function Status=compileWebServer()
            % Compile the Java Http logger
            % Input  : null
            % Output : - Status, 0 for sucess.
            % Author : Eran Ofek (May 2025)
            % Example: tools.WebServer.compileWebServer

            Status = 0;
            PWD = pwd;
            Path = tools.WebServer.getWebServerPath;
            cd(Path);
            if ~isfile(sprintf('%s.class', tools.WebServer.JavaFunName))
                [Status, CmdOut] = system(sprintf('javac %s.java',tools.WebServer.JavaFunName));
                fprintf('Copilation Status: %f\n', Status);
                fprintf('Compilation output : %s\n',CmdOut);
            end
            cd(PWD);
           
        end   

    end

    methods % executing
        function runWebServer(Obj, OutputFile)
            % Run WebServer and store its PID
            % Input  : - self
            %          - OutptFile name.
            %            If empty, use the OutputFile property.
            %            Default is [].
            % Output : null
            % Author : Eran Ofek (May 2025)
            % Example: WS=tools.WebServer; WS.runWebServer();


            arguments
                Obj
                OutputFile  = [];
            end

            if isempty(OutputFile)
                OutputFile = Obj.OutputFile;
            end
            if isempty(OutputFile)
                error('OutputFile is not provided');
            end

            tools.WebServer.compileWebServer;

            Obj.killWebServer;
            FunAndArgs = sprintf('%s %s %d %s', tools.WebServer.getWebServerPath, Obj.JavaFunName, Obj.Port, OutputFile);
            [Status, OutCmd] = system(sprintf('java -cp %s&', FunAndArgs));
            [Status, CmdOut] = system(sprintf('pgrep -f %s', Obj.JavaFunName));
            Obj.PID          = strip(CmdOut);

        end

        function killWebServer(Obj)
            % kill WebServer based on its stored PID
            % Input  : - self
            % Output : null
            % Author : Eran Ofek (May 2025)
            % Example: WS=tools.WebServer; WS.killWebServer;

            if ischar(Obj.PID) || isstring(Obj.PID)
                Obj.PID = str2double(Obj.PID);
            end
            if ~isempty(Obj.PID)
                if isunix
                    system(sprintf('kill -9 %d',Obj.PID));
                elseif ismax
                    error('KillWebServer is not defined for MAC');
                else
                    error('KillWebServer is not defined for Windows');
                end
            end

        end

    end
        
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
