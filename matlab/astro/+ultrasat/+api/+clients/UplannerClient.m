%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UplannerClient.m
% Author      : Chen Tishler
% Created     : 19/02/2026
% Updated     : 19/02/2026
% Description : Adapter class for the uplanner class.
%==========================================================================

classdef UplannerClient < handle
    % Adapter class for the uplanner class.
    % This class is used by 'uplanner' claass to call the plans, schedule,
    % and validator clients, in a way that the actual implementation of the 
    % clients is hidden from the uplanner class.
    % Created and set in MainModule.setPlanner() function.

    properties
        PlansClient            % Instance of the PlansManagerClient class
        ScheduleClient         % Instance of the ScheduleManagerClient class
        ValidatorClient        % Instance of the ValidatorManagerClient class
    end


    methods
        function obj = UplannerClient(plansClient, scheduleClient, validatorClient)
            % Constructor
            % :param plansClient: Instance of the PlansManagerClient class
            % :param scheduleClient: Instance of the ScheduleManagerClient class
            % :param validatorClient: Instance of the ValidatorManagerClient class
            obj.PlansClient = plansClient;
            obj.ScheduleClient = scheduleClient;
            obj.ValidatorClient = validatorClient;
        end


        function approved = getApprovedTargets(obj, startTime, endTime)
            % Returns the list of approved targets for the given time range.
            approved = obj.ScheduleClient.getTargets(startTime, endTime);
        end

        function resp = validatePlan(obj, planStruct)
            % Validates the given plan.
            resp = obj.ValidatorClient.validatePlan(planStruct);
        end

        function resp = submitPlan(obj, planStruct)
            % Submits the given plan.
            resp = obj.PlansClient.submitPlan(planStruct);
        end

    end
end
