%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainSessionHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 21/10/2025
% Description : Session Helper for Main Planner (Login, Logout, etc.)
%==========================================================================

classdef PlannerMainSessionHelper < ultrasat.api.core.Loggable
    % Helper class for PlannerMain.mlapp
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

        function obj = PlannerMainSessionHelper()
            % Constructor
            obj.LogPrefix = 'SessionHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function login(obj, app)
            % User login
            app.msglog('login');

            % Do nothing if already connected
            if ~isempty(app.MainModule.UserName)
                return;
            end

            % Check active planner user name
            if app.hasPlanner()
                if ~strcmp(app.AppUtils.askYesNo('You currently have an active plan. After logging in, the planner will automatically set your name as the AstPlanner. Continue?', 'Confirmation'), 'Yes')
                    return;
                end
            end

            % Create app
            if isempty(app.LoginApp) || ~isvalid(app.LoginApp)
                app.LoginApp = ultrasat.planner.gui.Login(app.MainModule);
            end

            % Show the login dialog, it will call MainModule.login()
            app.showModal(app.LoginApp);

            % Dialog was canceled or failed
            if isempty(app.MainModule.UserName)
                app.msglog('Login canceled or failed.');
            end

            obj.setLoginButtonStatus(app);
            obj.setButtons(app);

            % Set AstPlanner to connected user
            if app.hasPlanner()
                if ~strcmp(app.MainModule.Planner.AstPlanner, app.MainModule.UserName)
                    app.msglog(sprintf('Login: Setting AstPlanner field of open plan: %s, %s', app.MainModule.Planner.AstPlanner, app.MainModule.UserName));
                    app.MainModule.Planner.AstPlanner = app.MainModule.UserName;
                end
            end
        end


        function logout(obj, app)
            % User logout
            app.msglog('logout');

            % Do nothing if not connected
            if isempty(app.MainModule.UserName)
                return;
            end

            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to logout?', 'Confirm'), 'Yes')
                return;
            end

            app.MainModule.logout();
            obj.setLoginButtonStatus(app);
            obj.setButtons(app);
        end


        function Result = isLogin(obj, app, varargin)
            % Return true is user is loggned in, show popup message if Args.Message is true
            Message = false;
            if ~isempty(varargin)
                Message = varargin{1};
            end

            % Return true if used is logged-in
            Result = ~isempty(app.MainModule.UserName);
            if ~Result
                app.msglog('isLogin: not loggedin');
                if Message
                    uialert(app.UIFigure, 'Login to the server is required to proceed with this operation', 'Message', 'Icon', 'success');
                end
            end
        end


        function Result = isAllowed(obj, app, Action)
            % Return true if specified action is allowed for current logged-in user
            % @TODO - To be replaced with checking permissions like Delphi code
            Result = ~isempty(app.MainModule.UserName);
            if ~Result
                app.msglog(sprintf('isAllowed: not allowed: %s', Action));
            end
        end


        function exitPlanner(obj, app)
            % Exit the planner GUI
            %answer = questdlg('Are you sure you want to exit the Observaion Planner?', 'Confirm exit', 'Yes', 'No', 'No');
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to exit the planner application?', 'Confirmation'), 'Yes')
                return;
            end

            % Shut down the entire app
            app.delete();
        end

        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

   
        function setButtons(obj, app)
            % Enable/disable buttons and menu options based on login status and plan state.
            %
            % Login-dependent controls require a valid connection.
            % Plan-dependent controls require an active loaded plan.
        
            isLoggedIn = obj.isLogin(app);
            hasPlan    = app.hasPlanner();
        
            % === File Menu ===
            app.NewMenu.Enable               = obj.bool2str(isLoggedIn);
            app.OpenMenu.Enable              = obj.bool2str(isLoggedIn);
            app.SaveMenu.Enable              = obj.bool2str(hasPlan);
            app.DuplicateMenu.Enable         = obj.bool2str(hasPlan);
            app.CloseMenu_2.Enable           = obj.bool2str(hasPlan);
            app.DeleteMenu.Enable            = obj.bool2str(hasPlan);
            app.SaveToLocalFileMenu.Enable   = obj.bool2str(hasPlan);
            app.OpenFromLocalFileMenu.Enable = 'on'; % always allowed (local file)
            app.ConnectLoginMenu.Enable      = obj.bool2str(~isLoggedIn);
            app.DisconnectLogoutMenu.Enable  = obj.bool2str(isLoggedIn);
            app.ExitPlannerMenu.Enable       = 'on';
        
            % === Targets Menu ===
            app.AddUniqueTargetMenu.Enable        = obj.bool2str(hasPlan);
            app.EditUniqueTargetMenu.Enable       = obj.bool2str(hasPlan);
            app.DeleteUniqueTargetMenu.Enable     = obj.bool2str(hasPlan);
            app.ClearAllUniqueTargetsMenu.Enable  = obj.bool2str(hasPlan);
            app.ViewUniqueTargetsTableMenu.Enable = obj.bool2str(hasPlan);
            app.SaveUniqueTargetsToFileMenu.Enable= obj.bool2str(hasPlan);
            app.LoadUniqueTargetsFromFileMenu.Enable = obj.bool2str(hasPlan);
        
            % === Plan Menu ===
            app.EditPlanTargetMenu.Enable        = obj.bool2str(hasPlan);
            app.DeletePlanTargetMenu.Enable      = obj.bool2str(hasPlan);
            app.ClearAllPlanTargetsMenu.Enable   = obj.bool2str(hasPlan);
            app.ViewPlanTableMenu.Enable         = obj.bool2str(hasPlan);
            app.ParamsMenu.Enable                = obj.bool2str(hasPlan);
            app.PlanHistoryMenu.Enable           = obj.bool2str(hasPlan);
            app.BuildMenu.Enable                 = obj.bool2str(hasPlan);
            app.ValidateMenu.Enable              = obj.bool2str(isLoggedIn && hasPlan);
            app.SubmitMenu.Enable                = obj.bool2str(isLoggedIn && hasPlan);

            app.RefreshMenu.Enable               = obj.bool2str(hasPlan);        
        
            % === Plots Menu ===
            app.ViewSkyMapPlotWindowMenu.Enable  = obj.bool2str(hasPlan);
            app.ViewGraphsPlotWindowMenu.Enable  = obj.bool2str(hasPlan);
            app.ClearPlotsMenu.Enable            = obj.bool2str(hasPlan);
        
            % === Toolbar Buttons ===
            app.NewButton.Enable         = obj.bool2str(isLoggedIn);
            app.OpenButton.Enable        = obj.bool2str(isLoggedIn);
            app.SaveButton.Enable        = obj.bool2str(hasPlan);
            app.DuplicateButton.Enable   = obj.bool2str(hasPlan);
            app.ParamsButton.Enable      = obj.bool2str(hasPlan);
            app.ValidateButton.Enable    = obj.bool2str(isLoggedIn && hasPlan);
            app.SubmitButton.Enable      = obj.bool2str(isLoggedIn && hasPlan);
            app.RetractButton.Enable     = obj.bool2str(isLoggedIn && hasPlan);
            app.LoginButton.Enable       = obj.bool2str(~isLoggedIn);
            app.SNRCalcButton.Enable     = 'on';
            app.HelpButton.Enable        = 'on';
            app.QAButton.Enable          = 'on';
        
            % === Target Panels ===
            app.AddUniqueTargetButton.Enable     = obj.bool2str(hasPlan);
            app.EditUniqueTargetButton.Enable    = obj.bool2str(hasPlan);
            app.LoadUniqueTargetsButton.Enable   = obj.bool2str(hasPlan); % Load Unique Targets
            app.EditPlanTargetButton_4.Enable    = obj.bool2str(hasPlan); % Delete
            app.EditPlanTargetButton_5.Enable    = obj.bool2str(hasPlan); % Clear All
        
            % === Plan Panel ===
            app.BuildButton.Enable               = obj.bool2str(hasPlan);
            app.CheckPlanTargetsButton.Enable    = obj.bool2str(hasPlan);
            app.EditPlanTargetButton.Enable      = obj.bool2str(hasPlan);
            app.EditPlanTargetButton_2.Enable    = obj.bool2str(hasPlan);
            app.EditPlanTargetButton_3.Enable    = obj.bool2str(hasPlan);
            app.EditPlanTargetButton_6.Enable    = obj.bool2str(hasPlan);
        
            % === Approved Targets Panel ===
            app.RefreshApprovedTargetsButton.Enable   = obj.bool2str(hasPlan);
            app.RefreshApprovedTargetsButton_3.Enable = obj.bool2str(hasPlan);
        
            % === Plots ===
            app.UpdateSkyMapButton.Enable       = obj.bool2str(hasPlan);
            app.PlotFlagUniqueCheckBox.Enable   = obj.bool2str(hasPlan);
            app.PlotFlagPlanCheckBox.Enable     = obj.bool2str(hasPlan);
            app.PlotFlagApprovedCheckBox.Enable = obj.bool2str(hasPlan);
            app.PlotFlagCalibrationCheckBox.Enable = obj.bool2str(hasPlan);
            app.PlotFlagExtinctionCheckBox.Enable  = obj.bool2str(hasPlan);
        
            % === Status tab ===
            app.BuildStatusButton.Enable     = obj.bool2str(hasPlan);
            app.ValidationStatusButton.Enable= obj.bool2str(hasPlan);
            app.SubmitStatusButton.Enable    = obj.bool2str(hasPlan);        
        end
        
        
        function s = bool2str(obj, val)
            % Convert logical to 'on'/'off' for Enable property
            if val
                s = 'on';
            else
                s = 'off';
            end
        end

        
        function setLoginButtonStatus(obj, app)
            % Connect button
            if ~isempty(app.MainModule.UserName)
                app.LoginButton.Text = 'Connected';
                app.LoginButton.BackgroundColor = [0.95,1.00,0.95];  % Green
                app.LabelTopUser.Text = ultrasat.planner.guiutils.safeText(app.MainModule.UserName);
            else
                app.LoginButton.Text = 'Login';
                app.LoginButton.BackgroundColor = [1.00,1.00,0.55];  % Yellow
                app.LabelTopUser.Text = 'Please login';
            end

            % Namespace & Username
            if ~isempty(app.MainModule)
                % Set Namespace label colors
                app.LabelTopNamespace.Text = ultrasat.planner.guiutils.safeText(app.MainModule.NamespaceDisplay);
                if strcmp(app.MainModule.NamespaceId, 'OPER')
                    app.LabelTopNamespace.FontColor = [1.00,1.00,1.00];  % White on black
                    app.LabelTopNamespace.BackgroundColor = [0.00,0.00,0.00];
                    app.LabelTopNamespace.Visible = 'on';
                elseif ~isempty(app.MainModule.UserName)
                    app.LabelTopNamespace.FontColor = [0.00,0.00,0.00];  % Black on yellow
                    app.LabelTopNamespace.BackgroundColor = [1.00,1.00,0.55];
                    app.LabelTopNamespace.Visible = 'on';
                else
                    app.LabelTopNamespace.Visible = 'off';
                end
            end
        end


        function showLogger(obj, app)
            % Show log window
            app.msglog('showLogger');

            % Create app windows if not already created
            if isempty(app.LoggerApp) || ~isvalid(app.LoggerApp)
                app.LoggerApp = ultrasat.planner.gui.Logger(app.MainModule);
            end

            % Show the window
            app.LoggerApp.UIFigure.Visible = 'on';
        end


        function showErrorLogger(obj, app)
            % Show error log window
            app.msglog('showErrorLogger');

            % Create app windows if not already created
            if isempty(app.ErrorLogApp) || ~isvalid(app.ErrorLogApp)
                app.ErrorLogApp = ultrasat.planner.gui.ErrorLogger(app.MainModule);
            end

            % Show the window
            app.ErrorLogApp.UIFigure.Visible = 'on';
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        % =================================================================
        %                           HELPERS
        % =================================================================


    end

end
