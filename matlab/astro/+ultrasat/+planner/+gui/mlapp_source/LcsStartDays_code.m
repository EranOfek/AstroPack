classdef LcsStartDays < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                matlab.ui.Figure
        Panel_6                 matlab.ui.container.Panel
        HelpButton              matlab.ui.control.Button
        CancelButton            matlab.ui.control.Button
        SelectButton            matlab.ui.control.Button
        Panel_4                 matlab.ui.container.Panel
        UITable                 matlab.ui.control.Table
        Panel_2                 matlab.ui.container.Panel
        LCSAvailableDatesLabel  matlab.ui.control.Label
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

        % Callback function: not associated with a component
        function HelpButtonPushed(app, event)
            
        end

        % Callback function: not associated with a component
        function SelectButtonPushed(app, event)
            %
        end

        % Callback function: not associated with a component
        function CancelButton_3Pushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);            
        end

        % Button pushed function: SelectButton
        function SelectButtonPushed2(app, event)
            %
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed2(app, event)
            app.MainModule.MainApp.showHelp('lcs_start_days');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 849 614];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.HighlightColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.749 0.851 0.949];
            app.Panel_2.Position = [14 575 939 30];

            % Create LCSAvailableDatesLabel
            app.LCSAvailableDatesLabel = uilabel(app.Panel_2);
            app.LCSAvailableDatesLabel.HorizontalAlignment = 'center';
            app.LCSAvailableDatesLabel.FontSize = 18;
            app.LCSAvailableDatesLabel.FontWeight = 'bold';
            app.LCSAvailableDatesLabel.Position = [14 1 917 27];
            app.LCSAvailableDatesLabel.Text = 'LCS Available Dates';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_4.Position = [18 92 817 470];

            % Create UITable
            app.UITable = uitable(app.Panel_4);
            app.UITable.ColumnName = '';
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = true;
            app.UITable.FontSize = 10;
            app.UITable.Position = [6 9 801 455];

            % Create Panel_6
            app.Panel_6 = uipanel(app.UIFigure);
            app.Panel_6.BackgroundColor = [0.8 0.8 0.8];
            app.Panel_6.Position = [18 20 817 57];

            % Create SelectButton
            app.SelectButton = uibutton(app.Panel_6, 'push');
            app.SelectButton.ButtonPushedFcn = createCallbackFcn(app, @SelectButtonPushed2, true);
            app.SelectButton.FontWeight = 'bold';
            app.SelectButton.FontColor = [0 0 1];
            app.SelectButton.Position = [263 9 85 39];
            app.SelectButton.Text = 'Select';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel_6, 'push');
            app.CancelButton.Position = [374 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_6, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed2, true);
            app.HelpButton.Position = [478 9 85 39];
            app.HelpButton.Text = 'Help';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LcsStartDays(varargin)

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