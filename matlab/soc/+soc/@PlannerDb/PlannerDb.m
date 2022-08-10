
classdef PlannerDb < Component

    % Properties
    properties (SetAccess = public)
    end

    %----------------------------------------------------------------------
    methods % Constructor

        % Constructor
        function Obj = PlannerDb(DbTableOrConn, Args)
            % Create new PlannerDb obeject
            % Input : - DbConnection object, or database alias from Database.yml.
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            
            %             'InsertRecFunc' - Default function used with insert(). See help of insert()
            %
            % Output   : - New instance of PlanerDb object
            % Author   : Chen Tishler (2022)
            % Examples :
            %   % Create query object for
            %   Q = PlannerDb('UnitTest')
            %
            arguments
                DbTableOrConn   = []  % DbAlias / DbAlias:TableName / DbConnection object
                Args.TableName        % Set TableName when not included in DbTable parameter
                Args.InsertRecFunc    % Default function used with insert(). See help of insert()
                
                % These arguments are used when both DbQuery and DbCon are NOT set:
                Args.Host          = ''        % Host name or IP address
                Args.Port          = 5432      % Port number
                Args.DatabaseName  = ''        % Use 'postgres' to when creating databases or for general
                Args.UserName      = ''        % User name
                Args.Password      = ''        % Password
                
            end

            % Setup component
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.DebugMode = true;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);

            %...
            
            
            % Override TableName and set other properties
            Obj.setProps(Args);
        end

        
        function delete(Obj)
            % Destructor            
            % Internally called by Matlab when the object is destroyed.            
            Obj.clear();
            %Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end        
    end
    
    
    %----------------------------------------------------------------------
    methods % High-level

        function Result = insertTarget(Obj)
            % Insert new target
            Result = true;
        end
        
    end
    
    %----------------------------------------------------------------------
    methods(Static) % Static

        Result = unitTest()
            % Unit-Test

    end
    
end

