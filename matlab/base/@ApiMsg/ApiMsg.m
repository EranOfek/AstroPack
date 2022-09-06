% Component class - this class hinerits from Base class, and most
% astro-related class are hinerits from this class.
%
% Authors: Chen Tishler
%
%--------------------------------------------------------------------------
%
% #functions (autogen)

% #/functions (autogen)
%

classdef ApiMsg < handle
    % Parent class for all components

    % Properties
    properties (SetAccess = public)   
        pk = 0
        api_src = ''
        api_dst = ''

        %
        api_msg_time = 0
        api_msg_type = ''
        api_msg_subtype = ''
        api_msg = ''
        api_msg_params = ''
        api_msg_fname = ''
        api_msg_processed = false

        % Reply
        api_rep_time = 0
        api_rep_result = ''
        api_rep_status = ''
        api_rep = ''
        api_rep_params = ''
        api_rep_fname = ''
        api_rep_processed = false

        %
        TableName = 'sys.sys_api'
        Query db.DbQuery                %
        Rows = []
    end
    
  
    %--------------------------------------------------------
    methods % Constructor

        function Obj = ApiMsg()
            % Constructor
            % By default use singleton MsgLogger and Configuration
            % Input  - Optional arguement of type Component, stored in Obj.Owner
            %   NewComp = Component()
            %   NewComp = Component(Owner)

            
            % Validate configuration of derived classes
            Obj.Query = db.DbQuery('socdb');
            
        end
        
           
        function Result = post(Obj)

            Row = struct;
            Row.api_src = Obj.api_src;
            Row.api_dst = Obj.api_dst;
            Row.api_msg_type = Obj.api_msg_type;
            Row.api_msg_subtype = Obj.api_msg_subtype;
            Row.api_msg = Obj.api_msg;
            Row.api_msg_params = Obj.api_msg_params;
            Row.api_msg_fname = Obj.api_msg_fname;
            Obj.Query.insert(Row, 'TableName', Obj.TableName);

            % Get PK - replace with RETURNING !!! @Todo
            Obj.Rows = Obj.Query.select('*', 'TableName', Obj.TableName, 'Order', 'pk DESC', 'Limit', 1);
            Row = Obj.Rows.Data(1);
            Obj.pk = Row.pk;
        end
        
       
        function Result = pollResult(Obj)
            Result = '';

            Obj.Rows = Obj.Query.select('*', 'TableName', Obj.TableName, 'Where', sprintf('pk=%d', Obj.pk));
            if numel(Obj.Rows.Data) > 0
                Row = Obj.Rows.Data(1);
                Obj.api_rep_time      = Row.api_rep_time;
                Obj.api_rep_result    = Row.api_rep_result;
                Obj.api_rep_status    = Row.api_rep_status;
                Obj.api_rep           = Row.api_rep;
                Obj.api_rep_params    = Row.api_rep_params;
                Obj.api_rep_fname     = Row.api_rep_fname;
                Obj.api_rep_processed = Row.api_rep_processed;

                Result = Obj.api_rep_result;
            end
        end

        
        function Result = selectNew(Obj, Dst)
            Sql = spritnf('SELECT * FROM %s WHERE api_dst=' + Dst + ' AND api_msg_processed=false ORDER BY api_msg_time ASC LIMIT 100', Obj.TableName);
            Obj.Rows = Obj.Query.select(Sql);    
            Result = numel(Obj.Rows) > 0;
        end
        
        % Convert JSON string to struct
        function Result = getJson(Obj, Text)
            Result = jsonencode(Text);
        end
        
    end

    
    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Component class
    end
end
