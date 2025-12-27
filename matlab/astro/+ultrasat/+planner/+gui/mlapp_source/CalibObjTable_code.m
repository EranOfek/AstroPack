classdef CalibObjTable < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure             matlab.ui.Figure
        Panel_5              matlab.ui.container.Panel
        RowEditFieldLabel_2  matlab.ui.control.Label
        NameEditField        matlab.ui.control.EditField
        NameEditFieldLabel   matlab.ui.control.Label
        IndexEditField       matlab.ui.control.EditField
        IndexEditFieldLabel  matlab.ui.control.Label
        RowEditField         matlab.ui.control.EditField
        RowEditFieldLabel    matlab.ui.control.Label
        Panel_4              matlab.ui.container.Panel
        UITableData          matlab.ui.control.Table
        Panel_2              matlab.ui.container.Panel
        HelpButton           matlab.ui.control.Button
        CalibObjectsLabel    matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % CalibObjTable App
            %
            % This app displays the calibration objects table returned by 
            % Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', false).
            %
            % It allows users to view and analyze calibration data associated 
            % with unique astronomical targets.
            %
            % Features:
            % - Displays a tabular view of calibration objects.
            % - See PlannerMain.plotCalibObj().
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

        % Selection changed function: UITableData
        function UITableDataSelectionChanged(app, event)
           
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('calib_obj');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1335 574];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 532 1316 33];

            % Create CalibObjectsLabel
            app.CalibObjectsLabel = uilabel(app.Panel_2);
            app.CalibObjectsLabel.HorizontalAlignment = 'center';
            app.CalibObjectsLabel.FontSize = 18;
            app.CalibObjectsLabel.FontWeight = 'bold';
            app.CalibObjectsLabel.Position = [8 1 1298 33];
            app.CalibObjectsLabel.Text = 'Calib Objects';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [1219 3 64 26];
            app.HelpButton.Text = 'Help';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.BorderColor = [0.4902 0.4902 0.4902];
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Panel_4.Position = [11 8 1316 475];

            % Create UITableData
            app.UITableData = uitable(app.Panel_4);
            app.UITableData.ColumnName = '';
            app.UITableData.RowName = {};
            app.UITableData.SelectionChangedFcn = createCallbackFcn(app, @UITableDataSelectionChanged, true);
            app.UITableData.FontSize = 10;
            app.UITableData.Position = [8 9 1298 457];

            % Create Panel_5
            app.Panel_5 = uipanel(app.UIFigure);
            app.Panel_5.BorderType = 'none';
            app.Panel_5.BackgroundColor = [0.8 0.8 0.8];
            app.Panel_5.Position = [15 492 1315 34];

            % Create RowEditFieldLabel
            app.RowEditFieldLabel = uilabel(app.Panel_5);
            app.RowEditFieldLabel.HorizontalAlignment = 'right';
            app.RowEditFieldLabel.Position = [112 7 29 22];
            app.RowEditFieldLabel.Text = 'Row';

            % Create RowEditField
            app.RowEditField = uieditfield(app.Panel_5, 'text');
            app.RowEditField.Editable = 'off';
            app.RowEditField.BackgroundColor = [1 0.9882 0.8196];
            app.RowEditField.Position = [156 7 47 22];

            % Create IndexEditFieldLabel
            app.IndexEditFieldLabel = uilabel(app.Panel_5);
            app.IndexEditFieldLabel.HorizontalAlignment = 'right';
            app.IndexEditFieldLabel.Position = [226 7 34 22];
            app.IndexEditFieldLabel.Text = 'Index';

            % Create IndexEditField
            app.IndexEditField = uieditfield(app.Panel_5, 'text');
            app.IndexEditField.Editable = 'off';
            app.IndexEditField.BackgroundColor = [1 0.9882 0.8196];
            app.IndexEditField.Position = [275 7 47 22];

            % Create NameEditFieldLabel
            app.NameEditFieldLabel = uilabel(app.Panel_5);
            app.NameEditFieldLabel.HorizontalAlignment = 'right';
            app.NameEditFieldLabel.Position = [334 7 37 22];
            app.NameEditFieldLabel.Text = 'Name';

            % Create NameEditField
            app.NameEditField = uieditfield(app.Panel_5, 'text');
            app.NameEditField.Editable = 'off';
            app.NameEditField.BackgroundColor = [1 0.9882 0.8196];
            app.NameEditField.Position = [386 7 196 22];

            % Create RowEditFieldLabel_2
            app.RowEditFieldLabel_2 = uilabel(app.Panel_5);
            app.RowEditFieldLabel_2.HorizontalAlignment = 'right';
            app.RowEditFieldLabel_2.FontWeight = 'bold';
            app.RowEditFieldLabel_2.Position = [9 7 89 22];
            app.RowEditFieldLabel_2.Text = 'Unique Target:';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = CalibObjTable(varargin)

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