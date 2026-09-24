%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainAppHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 21/10/2025
% Description : App Helper for Main Planner
%==========================================================================

classdef PlannerMainAppHelper < ultrasat.api.core.Loggable
    % Helper class for PlannerMain.mlapp
    %
    % Shell/stub placeholder for application-level PlannerMain behaviors that do not
    % belong in a domain-specific helper (startup, global app state, cross-cutting UI).
    % No methods implemented yet.
    %
    % All methods require the PlannerMain instance as the first argument, named 'app'.
    % This is NOT implicit: even when calling from PlannerMain.mlapp, pass 'app'
    % explicitly to the helper method.
    %
    % Internal call example (from PlannerMain.mlapp):
    %   app.UniqueTargetsHelper.setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp);
    %
    % External call example (from another window/module):
    %   app.MainModule.MainApp.PlanParamsHelper.applyCheckTimes(app.MainModule.MainApp, ParamsApp);
    %
    % Notes:
    %   - 'app' always refers to the PlannerMain instance.
    %   - Additional parameters (e.g., ParamsApp) are the calling window/modules as needed.
    %

    methods (Access = public)

        function obj = PlannerMainAppHelper()
            % Constructor
            obj.LogPrefix = 'AppHelper';
        end

    end

    % =====================================================================
    %                           Helper Methods
    % =====================================================================

    methods (Access = private)
    end

end
