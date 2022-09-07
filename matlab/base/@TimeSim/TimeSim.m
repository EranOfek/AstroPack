% TimeSim class 
%
% Author: Chen Tishler (09/2022)
%
%--------------------------------------------------------------------------
%
% #functions (autogen)

% #/functions (autogen)
%

classdef TimeSim < handle
    % Parent class for all components

    % Properties
    properties (SetAccess = public)           
        Enabled         = false
        Running         = false
        Factor          = 1
        Base            = []
        Start           = []
        Pause           = []
        UseDb           = true
        TableName       = 'sys.sys_objects'
        Id              = 'sim1'        
        LastRefreshTime = 0
        Query db.DbQuery        
    end
    
    %--------------------------------------------------------
    methods % Constructor

        function Obj = TimeSim()
            % Constructor
                     
            Obj.Query = db.DbQuery('socdb');            
        end
        
        
        function Result = getTime(Obj)       
            % Get current time as double.
            % When simulation is enabled, we should replace get the time base from the database
            % and adjust it, or used shared-memory object to synchronize time between all processes        
            Obj.refresh();
            if Obj.Enabled
                if Obj.Running
                    real_t = Obj.getRealTime();
                    t = Obj.Base + ((real_t - Obj.Start) * Obj.Factor);
                else
                    t = Obj.Pause;
                end
            else
                t = Obj.getRealTime();
            end            
            Result = t;            
        end
            

        function Result = getRealTime(Obj)
            % Get real computer time, without simulation
            Result = posixtime(datetime('now', 'TimeZone', 'UTC'));
        end
        

        function Result = getDateTime(Obj)       
            % Get current timestamp as datetime class.
            % When simulation is enabled, we should replace get the time base from the database
            % and adjust it, or used shared-memory object to synchronize time between all processes
            t = Obj.getTime();            
            Result = Obj.pyUtcTimeToDateTime(t);
        end


        function Result = getRealDateTime(Obj)       
            % Get real computer time, without simulation.
            t = Obj.getRealTime();
            Result = Obj.pyUtcTimeToDateTime(t);
        end
        

        function Result = refresh(Obj)
            %
            if Obj.UseDb && (toc() - Obj.LastRefreshTime >= 1)
                Obj.loadFromDb();
            end
            Obj.LastRefreshTime = toc();
            Result = true;
        end


        function Result = loadFromDb(Obj)
            % Load current values from db table        
            Obj.Enabled = Obj.selectDbKey('enabled', 'value_bool', false);
            Obj.Running = Obj.selectDbKey('running', 'value_bool', false);
            Obj.Factor = Obj.selectDbKey('factor', 'value_float', 1);
            Obj.Base = Obj.selectDbKey('base', 'value_float', 0);
            Obj.Start = Obj.selectDbKey('start', 'value_float', 0);
            Obj.Pause = Obj.selectDbKey('pause', 'value_float', 0);
            Result = true;
        end


        function Result = updateDb(Obj)
            %
            if Obj.UseDb
                % Set enable=false before changing other values @Todo - use mutex ???
                Obj.updateDbKey('enabled', 'value_bool', false);
                Obj.updateDbKey('factor',  'value_float', Obj.Factor);
                Obj.updateDbKey('base',    'value_float', Obj.Base);
                Obj.updateDbKey('start',   'value_float', Obj.Start);
                Obj.updateDbKey('pause',   'value_float', Obj.Pause);
                Obj.updateDbKey('running', 'value_bool', Obj.Running);
                Obj.updateDbKey('enabled', 'value_bool', Obj.Enabled);
            end
            Result = true;
        end
        

        function Result = updateDbKey(Obj, Param, ColumnName, Value)
            %
            disp('TimeSim.updateDbKey - NOT SUPPORTED YET!');
            ObjName = Obj.getDbKey(Param);
            Where = strcat('obj_name = ', ObjName);
            %Obj.Query.update(self.tn, 'Where', Where, data={column_name: Value})
            Result = true;
        end


        function Result = selectDbKey(Obj, Param, ColumnName, DefaultValue)
            %
            Where = strcat('obj_name = ''', Obj.getDbKey(Param), '''');
            Res = Obj.Query.select('*', 'TableName', Obj.TableName, 'Where', Where, 'Order', 'pk');
            if numel(Res) > 0
                Result = Res.Data(1).(ColumnName);
            else
            	Result = DefaultValue;
            end
        end
        

        function Result =  getDbKey(Obj, Param)
            %
            Result = strcat('sys.time.sim.', Obj.Id, '.', Param);
        end
        
        
        function Result = getPyUtcTime(Obj)
            %
            Result = posixtime(datetime('now', 'TimeZone', 'UTC'));
        end        
        
        
        function Result = pyUtcTimeToDateTime(Obj, PyTime)
            %
            Result = datetime(PyTime, 'ConvertFrom', 'posixtime', 'TimeZone', 'UTC');
        end                
    end

    
    methods(Static)
        
        function Result = getSingleton()
            % **Internal function**
            % Return singleton TimeSim object
            % Example: sim = TimeSim.getSingleton();
            persistent Sim
            if isempty(Sim)
                io.msgLog(LogLevel.Debug, 'TimeSim.init: Creating Sonf');
                Sim = TimeSim();
            end
            Result = Sim;
        end
        
        
        Result = unitTest()
            % unitTest for Component class
    end
end
