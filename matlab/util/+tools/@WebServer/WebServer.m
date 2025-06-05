% tools.WebServer - A class for launching a webserver listenr
%
% Example:
% WS = tools.WebServer;
% DB = db.Db;
% DB.User = 'euclid/root'
% DB.useDB('last');
% WS.DB = DB;
% WS.runWebServer;  % run web server in background
% WS.killWebServer  % kill the WebServer
% Use: curl "http://localhost:8080/echo?Args1=123&Args2=hello%20world&Args3=-112.6661"


% python3 WebServer.py --host socsrv --port 8123 --user default --password PassRoot --ingestTime ingestiontimejd --userNameColumn user --userPassword MyPassword
% curl -u eran:MyPassword "http://localhost:8080/last.test1?ra=111.1&dec=23.3"
% curl -X POST http://localhost:8080/last.test1 -H "Content-Type: application/json" -H "Authorization: Basic $(echo -n alice:PassRoot | base64)" -d '[{"ra": 21.5, "dec": 55}, {"ra": 22.3, "dec": -50}]'

classdef WebServer < Component
    %
    
    properties    
        PID            = [];
        WebServerType  = "Insert2DB"; % "RunFun2html"|"Java"

        DB             = [];



        OutputFile  = "/tmp/http_requests.log";

       
    end
    
    properties (Constant, Hidden)
        Port        = 8080;
        WebServer_Insert2DB = 'WebServer.py';
        WebServer_Java      = 'SimpleHttpLogger';
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

        function Status=compileJavaWebServer()
            % Compile the Java Http logger
            % Input  : null
            % Output : - Status, 0 for sucess.
            % Author : Eran Ofek (May 2025)
            % Example: tools.WebServer.compileWebServer

            Status = 0;
            PWD = pwd;
            Path = tools.WebServer.getWebServerPath;
            cd(Path);
            if ~isfile(sprintf('%s.class', tools.WebServer.WebServer_Java))
                [Status, CmdOut] = system(sprintf('javac %s.java',tools.WebServer.WebServer_Java));
                fprintf('Copilation Status: %f\n', Status);
                fprintf('Compilation output : %s\n',CmdOut);
            end
            cd(PWD);
           
        end   

    end

    methods % executing
        function runWebServer(Obj, Args)
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
                Args.OutputFile  = [];

                Args.IngestTimeCol = 'ingestiontimejd';
                Args.UserCol       = 'user'
                Args.UserPassword  = Obj.DB.Password;
            end


            switch Obj.WebServerType
                case 'Java'
                    if isempty(Args.OutputFile)
                        OutputFile = Obj.OutputFile;
                    else
                        OutputFile = Args.OutputFile;
                    end
                    if isempty(OutputFile)
                        error('OutputFile is not provided');
                    end
        
                    tools.WebServer.compileJavaWebServer;
        
                    Obj.killWebServer;
                    FunAndArgs = sprintf('%s %s %d %s', tools.WebServer.getWebServerPath, Obj.WebServer_Java, Obj.Port, OutputFile);
                    [Status, OutCmd] = system(sprintf('java -cp %s&', FunAndArgs));
                    [Status, CmdOut] = system(sprintf('pgrep -f %s', Obj.WebServer_Java));
                    Obj.PID          = strip(CmdOut);
                case 'Insert2DB'
                    Obj.killWebServer;

                    WebServerFun = sprintf('%s%s%s', tools.WebServer.getWebServerPath, filesep, Obj.WebServer_Insert2DB);
                    if isempty(Obj.DB)
                        error('DB property must be provided - a db.Db object');
                    end
                    Cmd = sprintf('python3 %s --host %s --port %s --user %s --password %s --ingestTime %s --userNameColumn %s --userPassword %s', WebServerFun, Obj.DB.Host, Obj.DB.Port, Obj.DB.User, Obj.DB.Password, Args.IngestTimeCol, Args.UserCol, Args.UserPassword);
                    [Status, OutCmd] = system(sprintf('%s &',Cmd));
                    [Status, CmdOut] = system(sprintf('pgrep -f %s', Obj.WebServer_Insert2DB));
                    Obj.PID          = strip(CmdOut);

                case 'RunFun2html'


                otherwise

            end

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
                    Obj.PID = [];
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
