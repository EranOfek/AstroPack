%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainSessionHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 08/10/2025
% Description : Session Helper for Main Planner (Login, Logout, etc.)
%==========================================================================

classdef PlannerMainSessionHelper < ultrasat.api.Loggable
  
    methods
        
        function obj = PlannerMainSessionHelper()
            % Constructor
            obj.LogPrefix = 'SessionHelper';
            obj.msglog('PlannerMainSessionHelper created successfully');
        end


        function login(obj, app)
            % User login
            app.msglog('login');

            % Do nothing if already connected
            if ~isempty(app.MainModule.UserName)
                return;
            end

            % Check active planner user name
            if app.hasPlanner()
                if ~strcmp(app.AppUtils.askYesNo('Note: you have an active plan, after connection the AstPlanner field will be set to the logged-in user name. Would you like to continue?', 'Confirmation'), 'Yes')
                    return;
                end
            end          

            % Create app
            if isempty(app.LoginApp) || ~isvalid(app.LoginApp)
                app.LoginApp = ultrasat.planner.gui.Login(app.MainModule);
            end            
            LoginStatus = app.showModal(app.LoginApp);
            app.msglog( sprintf('login uiwait returned: isempty: %d, isvalid: %d', isempty(app.LoginApp), isvalid(app.LoginApp)) );

            app.setLoginButtonStatus();
            app.setButtons();

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
            app.setLoginButtonStatus();
            app.setButtons();            
        end        


        function setLoginButtonStatus(obj, app)
            % Connect button
            if ~isempty(app.MainModule.UserName)
                app.LoginButton.Text = 'Connected';
                app.LoginButton.BackgroundColor = [0.00, 1.00, 0.00];  % Green
                app.LabelTopUser.Text = app.MainModule.UserName;
            else
                app.LoginButton.Text = 'Login';
                app.LoginButton.BackgroundColor = [1.00, 1.00, 0.07];  % Yellow
                app.LabelTopUser.Text = 'Please login';
            end            

            % Namespace & Username
            if ~isempty(app.MainModule)
                % Set Namespace label colors
                app.LabelTopNamespace.Text = app.MainModule.NamespaceDisplay;
                if strcmp(app.MainModule.NamespaceId, 'OPER') 
                    app.LabelTopNamespace.FontColor = [1.00,1.00,1.00];  % White on black
                    app.LabelTopNamespace.BackgroundColor = [0.00,0.00,0.00];  
                else
                    app.LabelTopNamespace.FontColor = [0.00,0.00,0.00];  % Black on yellow
                    app.LabelTopNamespace.BackgroundColor = [1.00,1.00,0.07];
                end
            end
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
                app.msglog(sprintf('isAllowed: not allowed: %', Action));
            end            
        end 


        function setButtons(obj, app)
            % Enable/disable buttons and menu options based on current login status.

            enable = app.isLogin();

            % Apply to buttons
            %app.OpenButton.Enable = enable;
            app.SaveButton.Enable = app.hasPlanner();
            app.ParamsButton.Enable = app.hasPlanner();
            app.DuplicateButton.Enable = app.hasPlanner();
            app.ValidateButton.Enable = enable && app.hasPlanner();
            app.SubmitButton.Enable = enable && app.hasPlanner();

            % Apply to menu
            %app.OpenMenu.Enable = enable;
            app.SaveMenu.Enable = app.hasPlanner();
            app.ParamsButton.Enable = app.hasPlanner();
            app.DuplicateMenu.Enable = app.hasPlanner();
            app.ValidateMenu.Enable = enable && app.hasPlanner();
            app.SubmitMenu.Enable = enable && app.hasPlanner();
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
        
        
        function exitPlanner(obj, app)
            % Exit the planner GUI
            %answer = questdlg('Are you sure you want to exit the Observaion Planner?', 'Confirm exit', 'Yes', 'No', 'No');            
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to exit the planner application?', 'Confirmation'), 'Yes')
                return;
            end          

            % Shut down the entire app
            app.delete();
        end
    end

end

