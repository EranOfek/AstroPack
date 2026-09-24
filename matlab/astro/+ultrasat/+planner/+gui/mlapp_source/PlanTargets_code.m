classdef PlanTargets < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure    matlab.ui.Figure
        Panel_4     matlab.ui.container.Panel
        UITable     matlab.ui.control.Table
        Panel_2     matlab.ui.container.Panel
        HelpButton  matlab.ui.control.Button
        PlanThistableisupdatedbytheBuildoperationLabel  matlab.ui.control.Label
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
            app.UIFigure.Position = [100 100 1161 617];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.749 0.851 0.949];
            app.Panel_2.Position = [14 578 1142 30];

            % Create PlanThistableisupdatedbytheBuildoperationLabel
            app.PlanThistableisupdatedbytheBuildoperationLabel = uilabel(app.Panel_2);
            app.PlanThistableisupdatedbytheBuildoperationLabel.HorizontalAlignment = 'center';
            app.PlanThistableisupdatedbytheBuildoperationLabel.FontSize = 18;
            app.PlanThistableisupdatedbytheBuildoperationLabel.FontWeight = 'bold';
            app.PlanThistableisupdatedbytheBuildoperationLabel.Position = [14 1 1115 27];
            app.PlanThistableisupdatedbytheBuildoperationLabel.Text = 'Plan (This table is updated by the Build operation)';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [1044 1 64 28];
            app.HelpButton.Text = 'Help';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_4.Position = [18 10 1138 555];

            % Create UITable
            app.UITable = uitable(app.Panel_4);
            app.UITable.ColumnName = '';
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = true;
            app.UITable.FontSize = 10;
            app.UITable.Position = [6 10 1119 539];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlanTargets(varargin)

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