%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UplannerClient.m
% Author      : Chen Tishler
% Created     : 19/02/2026
% Updated     : 19/02/2026
% Description : Adapter class for the uplanner class.
%==========================================================================

classdef UplannerClient < handle
    properties
        PlansClient
        ScheduleClient
        ValidatorClient
    end

    methods
        function obj = UplannerClient(plansClient, scheduleClient, validatorClient)
            obj.PlansClient = plansClient;
            obj.ScheduleClient = scheduleClient;
            obj.ValidatorClient = validatorClient;
        end

        function approved = getApprovedTargets(obj, startTime, endTime)
            approved = obj.PlansClient.getApprovedTargets(startTime, endTime);
        end

        function resp = validatePlan(obj, planStruct)
            resp = obj.ValidatorClient.validatePlan(planStruct);
        end

        function resp = submitPlan(obj, planStruct)
            resp = obj.PlansClient.submitPlan(planStruct);
        end

    end
end
