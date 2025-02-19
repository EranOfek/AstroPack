%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.MissionClient.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 16/02/2025
%
%==========================================================================


classdef MissionClient < ultrasat.api.MissionClientBase
    % 
    
    properties
        Client          % api.ClientBase
    end


    methods
        function obj = MissionClient(Args)
            arguments          
                Args.SubUrl = '/mission';  % planner_backend  
            end
            ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.MissionClientBase(ArgsCell{:});
            obj.Client = api.ClientBase('SubUrl', Args.SubUrl);
        end        
        

        % -------------------------------------------------------------------

        function response = login(obj, UserName, Password)
            % 
            obj.msglog('login: user=%s, password=%s - @TODO', UserName, Password);
            params = ultrasat.api.MissionModels.LoginParams(UserName, Password);
            response = obj.Client.postRequest('/login/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = logout(obj, UserName)
            % 
            params = ultrasat.api.MissionModels.LogoutParams(UserName);
            response = obj.Client.postRequest('/logout/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end        

        % -------------------------------------------------------------------

        function response = getKeyValue(obj, Store, Key, Default)
            % 
            obj.msglog('getKeyValue: store=%s, key=%s - @TODO', Store, Key);
            params = ultrasat.api.MissionApiModels.GetKeyValueParams(Store, Key, Default);
            response = obj.Client.postRequest('/get_key_value/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = setKeyValue(obj, Store, Key, Value)
            % 
            obj.msglog('setKeyValue: store=%s, key=%s - @TODO', Store, Key);
            params = ultrasat.api.MissionApiModels.SetKeyValueParams(Store, Key, Value);
            response = obj.Client.postRequest('/set_key_value/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end        

        % -------------------------------------------------------------------

        function response = getApprovedTargets(obj, start_time, end_time)
            % 
            params = ultrasat.api.MissionApiModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/get_approved_targets/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = validate(obj, Plan)  %Targets)
            % 
            params = ultrasat.api.MissionModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/validate/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = submit(obj, Plan)  % Targets)
            % 
            params = ultrasat.api.MissionApiModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/submit/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end
        
        
        function response = getExposure(obj, table_name, healpix_indices, start_timestamp, end_timestamp, select_all)
            % Select rows from the Sky Exposure Tracker table.
            % Params:
            %   table_name: Name of the table to select from
            %   healpix_indices: Optional list of HEALPix indices
            %   start_timestamp: Optional start timestamp for filtering
            %   end_timestamp: Optional end timestamp for filtering
            %   select_all: Boolean indicating whether to use select_all
            if nargin < 6
                select_all = false;
            end
            params = ultrasat.api.SkyExposureTrackerModels.SelectParams(table_name, healpix_indices, start_timestamp, end_timestamp, select_all);
            response = obj.Client.postRequest('/get_exposure', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end
        
        % -------------------------------------------------------------------

        function response = getPlansList(obj, Args)
            % 
            params = ultrasat.api.MissionApiModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/get_plans_list/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = loadPlan(obj, PlanPk)
            % 
            params = ultrasat.api.MissionApiModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/load_plan/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = savePlan(obj, Args)
            % 
            params = ultrasat.api.MissionApiModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/save_plan/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = deletePlan(obj, Args)
            % 
            params = ultrasat.api.MissionApiModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.Client.postRequest('/delete_plan/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = getPlanStatus(obj, Args)
            % 
            params = ultrasat.api.MissionApiModels.GetPlanStatusParams(start_time, end_time);
            response = obj.Client.postRequest('/get_plan_status/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end             

        % -------------------------------------------------------------------

    end
end

