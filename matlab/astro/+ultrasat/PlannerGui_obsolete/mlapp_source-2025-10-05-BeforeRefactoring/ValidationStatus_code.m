classdef ValidationStatus < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                        matlab.ui.Figure
        ValidationHistoryMostRecentFirstPanel  matlab.ui.container.Panel
        UITableHistory                  matlab.ui.control.Table
        TabGroup                        matlab.ui.container.TabGroup
        ResultsTextTab                  matlab.ui.container.Tab
        TextArea                        matlab.ui.control.TextArea
        Label                           matlab.ui.control.Label
        ResultsHTMLTab                  matlab.ui.container.Tab
        HTML                            matlab.ui.control.HTML
        ResultsTable                    matlab.ui.container.Tab
        HTML_2                          matlab.ui.control.HTML
        UITable                         matlab.ui.control.Table
        Validation120250101121212Panel  matlab.ui.container.Panel
        StatusEditField                 matlab.ui.control.EditField
        StatusEditFieldLabel            matlab.ui.control.Label
        ElapsedEditField                matlab.ui.control.EditField
        ElapsedEditFieldLabel           matlab.ui.control.Label
        StartedEditField                matlab.ui.control.EditField
        StartedEditFieldLabel           matlab.ui.control.Label
        Panel_2                         matlab.ui.container.Panel
        ValidationStatusHistoryLabel    matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % ValidationStatus App
            %
            % This app displays validation results and history for observation plans.
            %
            % Features:
            % - Provides multiple views: text, HTML, and table formats.
            % - Displays validation history in reverse chronological order.
            % - Shows status, start time, and elapsed time for each validation entry.
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
            uiresume(app.UIFigure);
        end

        % Callback function
        function ValidateButtonPushed(app, event)

        end

        % Selection changed function: UITableHistory
        function UITableHistorySelectionChanged(app, event)
            app.MainModule.MainApp.validationHistorySelected();
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1263 819];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.FontWeight = 'bold';
            app.Panel_2.FontSize = 14;
            app.Panel_2.Position = [11 779 1245 31];

            % Create ValidationStatusHistoryLabel
            app.ValidationStatusHistoryLabel = uilabel(app.Panel_2);
            app.ValidationStatusHistoryLabel.HorizontalAlignment = 'center';
            app.ValidationStatusHistoryLabel.FontSize = 18;
            app.ValidationStatusHistoryLabel.FontWeight = 'bold';
            app.ValidationStatusHistoryLabel.Position = [8 1 1237 27];
            app.ValidationStatusHistoryLabel.Text = 'Validation Status & History';

            % Create Validation120250101121212Panel
            app.Validation120250101121212Panel = uipanel(app.UIFigure);
            app.Validation120250101121212Panel.Title = 'Validation #1: 2025-01-01 12:12:12';
            app.Validation120250101121212Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Validation120250101121212Panel.Position = [15 474 1240 93];

            % Create StartedEditFieldLabel
            app.StartedEditFieldLabel = uilabel(app.Validation120250101121212Panel);
            app.StartedEditFieldLabel.HorizontalAlignment = 'right';
            app.StartedEditFieldLabel.Position = [30 36 44 22];
            app.StartedEditFieldLabel.Text = 'Started';

            % Create StartedEditField
            app.StartedEditField = uieditfield(app.Validation120250101121212Panel, 'text');
            app.StartedEditField.Editable = 'off';
            app.StartedEditField.BackgroundColor = [1 0.9882 0.8196];
            app.StartedEditField.Position = [89 36 174 22];

            % Create ElapsedEditFieldLabel
            app.ElapsedEditFieldLabel = uilabel(app.Validation120250101121212Panel);
            app.ElapsedEditFieldLabel.HorizontalAlignment = 'right';
            app.ElapsedEditFieldLabel.Position = [329 36 48 22];
            app.ElapsedEditFieldLabel.Text = 'Elapsed';

            % Create ElapsedEditField
            app.ElapsedEditField = uieditfield(app.Validation120250101121212Panel, 'text');
            app.ElapsedEditField.Editable = 'off';
            app.ElapsedEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ElapsedEditField.Position = [392 36 174 22];

            % Create StatusEditFieldLabel
            app.StatusEditFieldLabel = uilabel(app.Validation120250101121212Panel);
            app.StatusEditFieldLabel.HorizontalAlignment = 'right';
            app.StatusEditFieldLabel.Position = [617 36 39 22];
            app.StatusEditFieldLabel.Text = 'Status';

            % Create StatusEditField
            app.StatusEditField = uieditfield(app.Validation120250101121212Panel, 'text');
            app.StatusEditField.Editable = 'off';
            app.StatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.StatusEditField.Position = [671 36 174 22];

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [11 10 1245 450];

            % Create ResultsTextTab
            app.ResultsTextTab = uitab(app.TabGroup);
            app.ResultsTextTab.Title = 'Results (Text)';

            % Create Label
            app.Label = uilabel(app.ResultsTextTab);
            app.Label.HorizontalAlignment = 'right';
            app.Label.FontName = 'Courier New';
            app.Label.Position = [-25 391 25 22];
            app.Label.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.ResultsTextTab);
            app.TextArea.Editable = 'off';
            app.TextArea.FontName = 'Courier New';
            app.TextArea.Position = [15 12 1219 403];

            % Create ResultsHTMLTab
            app.ResultsHTMLTab = uitab(app.TabGroup);
            app.ResultsHTMLTab.Title = 'Results (HTML)';

            % Create HTML
            app.HTML = uihtml(app.ResultsHTMLTab);
            app.HTML.Position = [13 12 737 402];

            % Create ResultsTable
            app.ResultsTable = uitab(app.TabGroup);
            app.ResultsTable.Title = 'Results (Table)';

            % Create UITable
            app.UITable = uitable(app.ResultsTable);
            app.UITable.ColumnName = {'Column 1'; 'Column 2'; 'Column 3'; 'Column 4'};
            app.UITable.RowName = {};
            app.UITable.SelectionType = 'row';
            app.UITable.Multiselect = 'off';
            app.UITable.Position = [13 164 1221 248];

            % Create HTML_2
            app.HTML_2 = uihtml(app.ResultsTable);
            app.HTML_2.Position = [13 12 1221 141];

            % Create ValidationHistoryMostRecentFirstPanel
            app.ValidationHistoryMostRecentFirstPanel = uipanel(app.UIFigure);
            app.ValidationHistoryMostRecentFirstPanel.Title = 'Validation History (Most Recent First)';
            app.ValidationHistoryMostRecentFirstPanel.Position = [16 582 1239 189];

            % Create UITableHistory
            app.UITableHistory = uitable(app.ValidationHistoryMostRecentFirstPanel);
            app.UITableHistory.ColumnName = {'Column 1'; 'Column 2'; 'Column 3'; 'Column 4'};
            app.UITableHistory.RowName = {};
            app.UITableHistory.SelectionType = 'row';
            app.UITableHistory.SelectionChangedFcn = createCallbackFcn(app, @UITableHistorySelectionChanged, true);
            app.UITableHistory.Multiselect = 'off';
            app.UITableHistory.Position = [8 12 1221 147];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = ValidationStatus(varargin)

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