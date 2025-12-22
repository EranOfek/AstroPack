classdef LcsTargets < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure         matlab.ui.Figure
        Panel_6          matlab.ui.container.Panel
        HelpButton_3     matlab.ui.control.Button
        CancelButton_2   matlab.ui.control.Button
        SaveButton_2     matlab.ui.control.Button
        Panel_5          matlab.ui.container.Panel
        SaveButton       matlab.ui.control.Button
        HelpButton       matlab.ui.control.Button
        Panel_4          matlab.ui.container.Panel
        UITable          matlab.ui.control.Table
        Panel_2          matlab.ui.container.Panel
        LCSTargetsLabel  matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % PlanTargets App
            %
            % This app displays the same Plan Targets table as in PlannerMain,
            % allowing users to view targets in a standalone window.
            %
            % Features:
            % - Provides a larger, resizable view of the Plan Targets table.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Callback function
        function CloseButtonPushed(app, event)

        end

        % Callback function
        function CloseButtonPushed2(app, event)

        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('plan_targets');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1318 786];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.749 0.851 0.949];
            app.Panel_2.Position = [14 747 1142 30];

            % Create LCSTargetsLabel
            app.LCSTargetsLabel = uilabel(app.Panel_2);
            app.LCSTargetsLabel.HorizontalAlignment = 'center';
            app.LCSTargetsLabel.FontSize = 18;
            app.LCSTargetsLabel.FontWeight = 'bold';
            app.LCSTargetsLabel.Position = [14 1 1115 27];
            app.LCSTargetsLabel.Text = 'LCS Targets';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_4.Position = [18 179 1138 555];

            % Create UITable
            app.UITable = uitable(app.Panel_4);
            app.UITable.ColumnName = '';
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = true;
            app.UITable.FontSize = 10;
            app.UITable.Position = [6 10 1119 539];

            % Create Panel_5
            app.Panel_5 = uipanel(app.UIFigure);
            app.Panel_5.BackgroundColor = [0.8 0.8 0.8];
            app.Panel_5.Position = [1188 300 100 434];

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_5, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [7 53 85 39];
            app.HelpButton.Text = 'Help';

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel_5, 'push');
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0 1];
            app.SaveButton.Position = [7 106 85 39];
            app.SaveButton.Text = 'Save';

            % Create Panel_6
            app.Panel_6 = uipanel(app.UIFigure);
            app.Panel_6.BackgroundColor = [0.8 0.8 0.8];
            app.Panel_6.Position = [14 33 1296 57];

            % Create SaveButton_2
            app.SaveButton_2 = uibutton(app.Panel_6, 'push');
            app.SaveButton_2.FontWeight = 'bold';
            app.SaveButton_2.FontColor = [0 0 1];
            app.SaveButton_2.Position = [493 9 85 39];
            app.SaveButton_2.Text = 'Save';

            % Create CancelButton_2
            app.CancelButton_2 = uibutton(app.Panel_6, 'push');
            app.CancelButton_2.Position = [609 9 85 39];
            app.CancelButton_2.Text = 'Cancel';

            % Create HelpButton_3
            app.HelpButton_3 = uibutton(app.Panel_6, 'push');
            app.HelpButton_3.Position = [726 9 85 39];
            app.HelpButton_3.Text = 'Help';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LcsTargets(varargin)

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