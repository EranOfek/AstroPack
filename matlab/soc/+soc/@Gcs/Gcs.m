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
    
    %======================================================================    
    
    %======================================================================    
    methods
        
        function header = newMsgHeader(Obj)
            % Create new message header
            % Input  : - 
            %          -
            %          -
            % Output : Header struct
            % Example: header = newMsgHeader()
            
            header = struct;
            header.msg_id = 'id';               % Unique ID for every message created by the GCS/SOC
            header.msg_time = 0;                % Date and Time of message creation
            header.msg_type = 'ImagingTask';    % Message type: Imaging task, OBRD task, non-imaging activity request, etc.
            header.source = 'SOC';              % Source of message: 'SOC' or 'GCS'
            header.task_id = 'id';              % SOC task ID: a unique ID for every task created by the SOC (if applicable)
            header.org_msg_id = 'org';          % To which GCS message ID this message is relevant (if applicable)
        end
        
        
        function msg = newImagingTaskMsg(Obj)
            % 
            % Input  : - 
            %          -
            %          -
            % Output :
            % Example:
            msg = struct;
            msg.header = Obj.newMsgHeader();
            msg.tasks = struct;
            msg.tasks.task(1) = Obj.newImagingTask();
            msg.tasks.task(2) = Obj.newImagingTask();
        end
        
    
        function task = newImagingTask(Obj)
            % Create new imaging task
            % Input  : - 
            %          -
            %          -
            % Output :
            % Example:            
            task = struct;
            task.start_time = 0;
            task.target_count = int32(3);
            task.targets = struct;
            task.targets.target(1) = Obj.newImagingTaskTarget();
            task.targets.target(2) = Obj.newImagingTaskTarget();
            task.targets.target(3) = Obj.newImagingTaskTarget();
        end
        
        
        function target = newImagingTaskTarget(Obj)
            % Create new imaging task target
            % Input  : - 
            %          -
            %          -
            % Output :
            % Example:            
            target = struct;
            target.start_time = 0;
            target.coord_ra = 0;
            target.coord_dec = 0;
            target.coord_roll = 0;
            target.exposure_duration = 0;
            target.image_count = int32(0);
            target.tiles = int32(0);
            target.image_id_format = 'fmt';
            target.first_image_num = int32(1);
        end
              
    end

    %======================================================================
    
    %======================================================================

    methods
        function Result = insertImagingTaskMsg(Obj, Msg)
            
            % 
            TempPath = '';
            TempFileName = tempname(TempPath);
            
            %
            msg = struct;
            msg.msg = Msg;
            yaml.WriteYaml(TempFileName, msg);
            Text = fileread(TempFileName);
            delete(TempFileName);
            
            Row = struct;
            Row.yml_text = Text;
            
            Rec = db.DbRecord(Row);

            
            Query = db.DbQuery();
            Query.insert('gcs_tasks', Rec);
        end
        
    end
    
    
    %======================================================================
    
    %======================================================================
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

            
        function Result = testImagingTaskMsg()
            % soc.Gcs.testImagingTaskMsg()
            % https://jsonformatter.org/yaml-to-xml
            
            G = soc.Gcs();
            msg = struct;
            msg.msg = G.newImagingTaskMsg();
            FileName = 'c:/temp/___yml1.yml';
            yaml.WriteYaml(FileName, msg);
            
            %G.insertImagingTaskMsg(msg.msg);
            
            Result = true;
        end
            
    end
end
