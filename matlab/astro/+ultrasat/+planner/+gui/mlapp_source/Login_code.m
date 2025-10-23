classdef Login < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                  matlab.ui.Figure
        LabelWelcome_2            matlab.ui.control.Label
        Panel_3                   matlab.ui.container.Panel
        EnvironmentDropDown       matlab.ui.control.DropDown
        EnvironmentDropDownLabel  matlab.ui.control.Label
        PasswordEditField         matlab.ui.control.EditField
        PasswordEditFieldLabel    matlab.ui.control.Label
        UsernameEditField         matlab.ui.control.EditField
        UsernameEditFieldLabel    matlab.ui.control.Label
        LabelWelcome              matlab.ui.control.Label
        Panel_2                   matlab.ui.container.Panel
        LoginLabel                matlab.ui.control.Label
        Panel                     matlab.ui.container.Panel
        HelpButton                matlab.ui.control.Button
        CancelButton              matlab.ui.control.Button
        LoginButton               matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % Login App
            %
            % This app provides a user authentication interface for the ULTRASAT 
            % Observation Planner GUI.
            %
            % Features:
            % - Allows users to enter a username and password for authentication.
            % - Displays success or failure messages based on login verification.
            % - Provides options to log in or cancel the operation.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the login attempt, e.g., 'LoginOk' or empty on failure
    end


    methods (Access = public)

        function beforeShow(app)
            % Called from PlannerMain.showModal()
            % Show environment (namespace) drop down only when running in simulation
            % If the planner was loaded in OPER mode (i.e. MainModule.NamespaceId, 'OPER') - 
            % hide the drop down of namespace selection
            if strcmp(app.MainModule.NamespaceId, 'OPER')
                app.EnvironmentDropDown.Visible = "off";
                app.EnvironmentDropDownLabel.Visible = "off";                
                app.EnvironmentDropDown.Items = {'OPER'};
                app.EnvironmentDropDown.Value = 'OPER';                
            else               
               app.EnvironmentDropDown.Visible = "on";
               app.EnvironmentDropDownLabel.Visible = "on";                              
                
               % Set items from NamespaceList
               items = app.MainModule.NamespaceDisplayList;
               app.EnvironmentDropDown.Items = items;
                
               % Determine initial value or default to firt item in the list
               if ~isempty(app.MainModule.NamespaceId) && ismember(app.MainModule.NamespaceId, items)
                   app.EnvironmentDropDown.Value = app.MainModule.NamespaceDisplay;                
               elseif ~isempty(items)
                    app.EnvironmentDropDown.Value = items(1);                
               else
                   % List is empty, set default manually - THIS SHOULD NOT HAPPEN
                   app.EnvironmentDropDown.Items = {'SIM'};
                   app.EnvironmentDropDown.Value = 'SIM - Undefined';
               end               
            end        
        end
    end


    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
            app.MainModule.AppUtils.center(app);
        end

        % Button pushed function: LoginButton
        function LoginButtonPushed(app, event)
            % Handles login attempts based on user input.
            %
            % - Retrieves username and password from input fields.
            % - Calls MainModule's login function to verify credentials.
            % - Displays success or failure messages accordingly.
            % - Resumes UI execution if login is successful.            
            
            app.Status = '';

            % Get username& password
            UserName = app.UsernameEditField.Value;
            Password = app.PasswordEditField.Value;

            % Get Namespace selection (i.e. 'sim-01:Simulator #1')
            Namespace = app.EnvironmentDropDown.Value;
            Result = app.MainModule.login(UserName, Password, Namespace);
            if Result
                app.Status = 'LoginOk';
                app.MainModule.AppUtils.msgOk('Login successful, welcome planner!', 'Success');
                uiresume(app.UIFigure);
            else
                app.MainModule.AppUtils.msgError('Login failed, please try again', 'Failed');
            end
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            uiresume(app.UIFigure);
        end

        % Close request function: UIFigure
        function UIFigureCloseRequest(app, event)

        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('login');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 493 374];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.Resize = 'off';
            app.UIFigure.CloseRequestFcn = createCallbackFcn(app, @UIFigureCloseRequest, true);

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [16 14 465 57];

            % Create LoginButton
            app.LoginButton = uibutton(app.Panel, 'push');
            app.LoginButton.ButtonPushedFcn = createCallbackFcn(app, @LoginButtonPushed, true);
            app.LoginButton.FontWeight = 'bold';
            app.LoginButton.FontColor = [0 0 1];
            app.LoginButton.Position = [101 8 85 39];
            app.LoginButton.Text = 'Login';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [196 8 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [295 8 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [16 329 465 37];

            % Create LoginLabel
            app.LoginLabel = uilabel(app.Panel_2);
            app.LoginLabel.HorizontalAlignment = 'center';
            app.LoginLabel.FontSize = 18;
            app.LoginLabel.FontWeight = 'bold';
            app.LoginLabel.Position = [8 1 448 33];
            app.LoginLabel.Text = 'Login';

            % Create LabelWelcome
            app.LabelWelcome = uilabel(app.UIFigure);
            app.LabelWelcome.WordWrap = 'on';
            app.LabelWelcome.FontSize = 16;
            app.LabelWelcome.FontWeight = 'bold';
            app.LabelWelcome.FontColor = [0 0 1];
            app.LabelWelcome.Position = [27 296 454 22];
            app.LabelWelcome.Text = 'Welcome to ULTRASAT Observation Planner GUI -:)';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.Position = [29 90 444 153];

            % Create UsernameEditFieldLabel
            app.UsernameEditFieldLabel = uilabel(app.Panel_3);
            app.UsernameEditFieldLabel.HorizontalAlignment = 'right';
            app.UsernameEditFieldLabel.Position = [8 101 64 22];
            app.UsernameEditFieldLabel.Text = 'User name';

            % Create UsernameEditField
            app.UsernameEditField = uieditfield(app.Panel_3, 'text');
            app.UsernameEditField.Position = [87 101 341 22];
            app.UsernameEditField.Value = 'yossi';

            % Create PasswordEditFieldLabel
            app.PasswordEditFieldLabel = uilabel(app.Panel_3);
            app.PasswordEditFieldLabel.HorizontalAlignment = 'right';
            app.PasswordEditFieldLabel.Position = [14 60 58 22];
            app.PasswordEditFieldLabel.Text = 'Password';

            % Create PasswordEditField
            app.PasswordEditField = uieditfield(app.Panel_3, 'text');
            app.PasswordEditField.Position = [87 60 341 22];
            app.PasswordEditField.Value = '123';

            % Create EnvironmentDropDownLabel
            app.EnvironmentDropDownLabel = uilabel(app.Panel_3);
            app.EnvironmentDropDownLabel.HorizontalAlignment = 'right';
            app.EnvironmentDropDownLabel.Position = [1 23 72 22];
            app.EnvironmentDropDownLabel.Text = 'Environment';

            % Create EnvironmentDropDown
            app.EnvironmentDropDown = uidropdown(app.Panel_3);
            app.EnvironmentDropDown.Items = {};
            app.EnvironmentDropDown.BackgroundColor = [1 0.9882 0.8196];
            app.EnvironmentDropDown.Position = [83 23 345 22];
            app.EnvironmentDropDown.Value = {};

            % Create LabelWelcome_2
            app.LabelWelcome_2 = uilabel(app.UIFigure);
            app.LabelWelcome_2.WordWrap = 'on';
            app.LabelWelcome_2.FontWeight = 'bold';
            app.LabelWelcome_2.FontColor = [0 0 1];
            app.LabelWelcome_2.Position = [27 257 454 22];
            app.LabelWelcome_2.Text = 'Please login to the system using your user name and password.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Login(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.UIFigure)
        end
    end
end