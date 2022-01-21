% Gcs -

%--------------------------------------------------------------------------

% #functions (autogen)
% #/functions (autogen)
%

classdef Gcs < Component

    % Properties
    properties (SetAccess = public)

        % Connection details
        Query           = []        % DbQuery, required to execute statements
    end

    %----------------------------------------------------------------------
    methods % Constructor

        % Constructor
        function Obj = Gcs(Args)
            % 
            % Input:
            %    'DbCon'     -
            %
            % Examples:
            %
            %
            %
            arguments
                Args.DbCon         = []        % DbConnection object
            end

            % Setup component
            Obj.setName('DbAdmin');
            Obj.needUuid();
            Obj.DebugMode = true;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
        end


        % Destructor
        function delete(Obj)
            Obj.clear();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
        
    end

        
    methods
        
        function Result = runGui(Obj, Args)
            %
            % Input:
            % Output:
            % Example:
            arguments
                Obj
                Args.A
            end

            Result = false;
            try
                                
                % Prepare command line
                Cmd = sprintf('psql -h %s -p %d -U %s -w', Args.Host, Args.Port, Args.UserName);
                
                % -d
                if ~isempty(Args.DatabaseName)
                    Cmd = sprintf('%s -d %s', Cmd, Args.DatabaseName);
                end
                
                % -f
                if ~isempty(Args.SqlFileName)
                    Cmd = sprintf('%s -f %s', Cmd, Args.SqlFileName);
                end
                
                % Additional params
                if ~isempty(Args.Params)
                    Cmd = sprintf('%s %s', Cmd, Args.Params);
                end
                
                % Password
                if ~isempty(Args.Password)
                    
                    % Windows - note that we MUST NOT have a spaces next to '&&'
                    if tools.os.iswindows()
                        Cmd = sprintf('set PGPASSWORD=%s&&%s', Args.Password, Cmd);
                        
                    % Linux - use 'export'
                    else
                        Cmd = sprintf('export PGPASSWORD=''%s'' ; %s', Args.Password, Cmd);
                    end
                end
                
                io.msgLog(LogLevel.Info, 'psql: %s', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, 'psql: %d', Status);
                io.msgLog(LogLevel.Info, 'psql: %s', Output);
                Result = true;
            catch Ex
                io.msgLogEx(LogLevel.Info, Ex, 'psql');
            end
        end
                
    
    end
    %----------------------------------------------------------------------
    
    methods(Hidden)
        
    end
    
    
    methods(Static) % Unit-Tests

        Result = unitTest()
            % Unit-Test

    end
end
