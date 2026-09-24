classdef LcsFields < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure             matlab.ui.Figure
        Panel_8              matlab.ui.container.Panel
        GroupSummaryLabel    matlab.ui.control.Label
        UITableGroupSummary  matlab.ui.control.Table
        PanelFieldSummary    matlab.ui.container.Panel
        FieldDetailsLabel    matlab.ui.control.Label
        UITableFieldDates    matlab.ui.control.Table
        Panel_4              matlab.ui.container.Panel
        GroupFieldsLabel     matlab.ui.control.Label
        UITableGroupFields   matlab.ui.control.Table
        Panel_2              matlab.ui.container.Panel
        HelpButton           matlab.ui.control.Button
        LCSFieldsLabel       matlab.ui.control.Label
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
            app.MainModule.MainApp.showHelp('lcs_targets');
        end

        % Callback function
        function HelpButtonPushed2(app, event)
            
        end

        % Selection changed function: UITableGroupSummary
        function UITableGroupSummarySelectionChanged(app, event)
            selection = app.UITableGroupSummary.Selection;
            app.MainModule.MainApp.LcsHelper.onGroupSummarySelectionChanged(app.MainModule.MainApp, app, selection);
        end

        % Selection changed function: UITableGroupFields
        function UITableGroupFieldsSelectionChanged(app, event)
            selection = app.UITableGroupFields.Selection;
            app.MainModule.MainApp.LcsHelper.onGroupFieldsSelectionChanged(app.MainModule.MainApp, app, selection);
        end

        % Selection changed function: UITableFieldDates
        function UITableFieldDatesSelectionChanged(app, event)
            %selection = app.UITableFieldDates.Selection;
            %
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1318 717];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.HighlightColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.749 0.851 0.949];
            app.Panel_2.Position = [14 678 1294 30];

            % Create LCSFieldsLabel
            app.LCSFieldsLabel = uilabel(app.Panel_2);
            app.LCSFieldsLabel.HorizontalAlignment = 'center';
            app.LCSFieldsLabel.FontSize = 18;
            app.LCSFieldsLabel.FontWeight = 'bold';
            app.LCSFieldsLabel.Position = [14 1 1190 27];
            app.LCSFieldsLabel.Text = 'LCS Fields';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [1220 3 64 26];
            app.HelpButton.Text = 'Help';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_4.Position = [18 11 935 487];

            % Create UITableGroupFields
            app.UITableGroupFields = uitable(app.Panel_4);
            app.UITableGroupFields.ColumnName = '';
            app.UITableGroupFields.RowName = {};
            app.UITableGroupFields.ColumnEditable = true;
            app.UITableGroupFields.SelectionChangedFcn = createCallbackFcn(app, @UITableGroupFieldsSelectionChanged, true);
            app.UITableGroupFields.FontSize = 10;
            app.UITableGroupFields.Position = [11 12 902 436];

            % Create GroupFieldsLabel
            app.GroupFieldsLabel = uilabel(app.Panel_4);
            app.GroupFieldsLabel.HorizontalAlignment = 'center';
            app.GroupFieldsLabel.FontSize = 14;
            app.GroupFieldsLabel.FontWeight = 'bold';
            app.GroupFieldsLabel.Position = [8 450 900 27];
            app.GroupFieldsLabel.Text = 'Group Fields';

            % Create PanelFieldSummary
            app.PanelFieldSummary = uipanel(app.UIFigure);
            app.PanelFieldSummary.BackgroundColor = [0.902 0.902 0.902];
            app.PanelFieldSummary.Position = [974 11 334 653];

            % Create UITableFieldDates
            app.UITableFieldDates = uitable(app.PanelFieldSummary);
            app.UITableFieldDates.ColumnName = '';
            app.UITableFieldDates.RowName = {};
            app.UITableFieldDates.ColumnEditable = true;
            app.UITableFieldDates.SelectionChangedFcn = createCallbackFcn(app, @UITableFieldDatesSelectionChanged, true);
            app.UITableFieldDates.FontSize = 10;
            app.UITableFieldDates.Position = [10 12 314 599];

            % Create FieldDetailsLabel
            app.FieldDetailsLabel = uilabel(app.PanelFieldSummary);
            app.FieldDetailsLabel.HorizontalAlignment = 'center';
            app.FieldDetailsLabel.FontSize = 14;
            app.FieldDetailsLabel.FontWeight = 'bold';
            app.FieldDetailsLabel.Position = [11 622 313 27];
            app.FieldDetailsLabel.Text = 'Field Details';

            % Create Panel_8
            app.Panel_8 = uipanel(app.UIFigure);
            app.Panel_8.TitlePosition = 'centertop';
            app.Panel_8.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_8.Position = [14 517 935 151];

            % Create UITableGroupSummary
            app.UITableGroupSummary = uitable(app.Panel_8);
            app.UITableGroupSummary.ColumnName = '';
            app.UITableGroupSummary.RowName = {};
            app.UITableGroupSummary.ColumnEditable = true;
            app.UITableGroupSummary.SelectionChangedFcn = createCallbackFcn(app, @UITableGroupSummarySelectionChanged, true);
            app.UITableGroupSummary.FontSize = 10;
            app.UITableGroupSummary.Position = [9 10 902 111];

            % Create GroupSummaryLabel
            app.GroupSummaryLabel = uilabel(app.Panel_8);
            app.GroupSummaryLabel.HorizontalAlignment = 'center';
            app.GroupSummaryLabel.FontSize = 14;
            app.GroupSummaryLabel.FontWeight = 'bold';
            app.GroupSummaryLabel.Position = [8 120 900 27];
            app.GroupSummaryLabel.Text = 'Group Summary';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LcsFields(varargin)

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