classdef FieldObjTable < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                    matlab.ui.Figure
        Panel_6                     matlab.ui.container.Panel
        TableDropDown               matlab.ui.control.DropDown
        TableDropDownLabel          matlab.ui.control.Label
        SmallEditField              matlab.ui.control.EditField
        SmallEditFieldLabel         matlab.ui.control.Label
        BlazarsEditField            matlab.ui.control.EditField
        BlazarsEditFieldLabel       matlab.ui.control.Label
        ClustersEditField           matlab.ui.control.EditField
        ClustersEditFieldLabel      matlab.ui.control.Label
        MassiveStarsEditField       matlab.ui.control.EditField
        MassiveStarsEditFieldLabel  matlab.ui.control.Label
        TransPlanetsEditField       matlab.ui.control.EditField
        TransPlanetsEditFieldLabel  matlab.ui.control.Label
        Panel_5                     matlab.ui.container.Panel
        RowEditFieldLabel_2         matlab.ui.control.Label
        NameEditField               matlab.ui.control.EditField
        NameEditFieldLabel          matlab.ui.control.Label
        IndexEditField              matlab.ui.control.EditField
        IndexEditFieldLabel         matlab.ui.control.Label
        RowEditField                matlab.ui.control.EditField
        RowEditFieldLabel           matlab.ui.control.Label
        Panel_4                     matlab.ui.container.Panel
        UITableData                 matlab.ui.control.Table
        Panel_2                     matlab.ui.container.Panel
        HelpButton                  matlab.ui.control.Button
        FieldObjectsLabel           matlab.ui.control.Label
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
            app.MainModule.MainApp.showHelp('field_obj');
        end

        % Value changed function: TableDropDown
        function TableDropDownValueChanged(app, event)
            app.MainModule.MainApp.TablesHelper.FieldObjTableDropDownValueChanged(app.MainModule.MainApp);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1204 585];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 543 1186 33];

            % Create FieldObjectsLabel
            app.FieldObjectsLabel = uilabel(app.Panel_2);
            app.FieldObjectsLabel.HorizontalAlignment = 'center';
            app.FieldObjectsLabel.FontSize = 18;
            app.FieldObjectsLabel.FontWeight = 'bold';
            app.FieldObjectsLabel.Position = [8 1 1167 33];
            app.FieldObjectsLabel.Text = 'Field Objects';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [1102 3 64 26];
            app.HelpButton.Text = 'Help';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.BorderColor = [0.4902 0.4902 0.4902];
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Panel_4.Position = [18 11 1178 441];

            % Create UITableData
            app.UITableData = uitable(app.Panel_4);
            app.UITableData.ColumnName = '';
            app.UITableData.RowName = {};
            app.UITableData.SelectionChangedFcn = createCallbackFcn(app, @UITableDataSelectionChanged, true);
            app.UITableData.FontSize = 10;
            app.UITableData.Position = [9 13 1158 418];

            % Create Panel_5
            app.Panel_5 = uipanel(app.UIFigure);
            app.Panel_5.BorderType = 'none';
            app.Panel_5.BackgroundColor = [0.8 0.8 0.8];
            app.Panel_5.Position = [14 501 1186 34];

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

            % Create Panel_6
            app.Panel_6 = uipanel(app.UIFigure);
            app.Panel_6.BorderColor = [0.8 0.8 0.8];
            app.Panel_6.BorderType = 'none';
            app.Panel_6.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_6.Position = [15 459 1185 37];

            % Create TransPlanetsEditFieldLabel
            app.TransPlanetsEditFieldLabel = uilabel(app.Panel_6);
            app.TransPlanetsEditFieldLabel.HorizontalAlignment = 'right';
            app.TransPlanetsEditFieldLabel.Position = [223 6 75 22];
            app.TransPlanetsEditFieldLabel.Text = 'TransPlanets';

            % Create TransPlanetsEditField
            app.TransPlanetsEditField = uieditfield(app.Panel_6, 'text');
            app.TransPlanetsEditField.Editable = 'off';
            app.TransPlanetsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.TransPlanetsEditField.Position = [313 6 50 22];

            % Create MassiveStarsEditFieldLabel
            app.MassiveStarsEditFieldLabel = uilabel(app.Panel_6);
            app.MassiveStarsEditFieldLabel.HorizontalAlignment = 'right';
            app.MassiveStarsEditFieldLabel.Position = [380 6 77 22];
            app.MassiveStarsEditFieldLabel.Text = 'MassiveStars';

            % Create MassiveStarsEditField
            app.MassiveStarsEditField = uieditfield(app.Panel_6, 'text');
            app.MassiveStarsEditField.Editable = 'off';
            app.MassiveStarsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MassiveStarsEditField.Position = [472 6 50 22];

            % Create ClustersEditFieldLabel
            app.ClustersEditFieldLabel = uilabel(app.Panel_6);
            app.ClustersEditFieldLabel.HorizontalAlignment = 'right';
            app.ClustersEditFieldLabel.Position = [546 6 49 22];
            app.ClustersEditFieldLabel.Text = 'Clusters';

            % Create ClustersEditField
            app.ClustersEditField = uieditfield(app.Panel_6, 'text');
            app.ClustersEditField.Editable = 'off';
            app.ClustersEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ClustersEditField.Position = [610 6 50 22];

            % Create BlazarsEditFieldLabel
            app.BlazarsEditFieldLabel = uilabel(app.Panel_6);
            app.BlazarsEditFieldLabel.HorizontalAlignment = 'right';
            app.BlazarsEditFieldLabel.Position = [683 6 45 22];
            app.BlazarsEditFieldLabel.Text = 'Blazars';

            % Create BlazarsEditField
            app.BlazarsEditField = uieditfield(app.Panel_6, 'text');
            app.BlazarsEditField.Editable = 'off';
            app.BlazarsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BlazarsEditField.Position = [743 6 50 22];

            % Create SmallEditFieldLabel
            app.SmallEditFieldLabel = uilabel(app.Panel_6);
            app.SmallEditFieldLabel.HorizontalAlignment = 'right';
            app.SmallEditFieldLabel.Position = [808 6 35 22];
            app.SmallEditFieldLabel.Text = 'Small';

            % Create SmallEditField
            app.SmallEditField = uieditfield(app.Panel_6, 'text');
            app.SmallEditField.Editable = 'off';
            app.SmallEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SmallEditField.Position = [858 6 50 22];

            % Create TableDropDownLabel
            app.TableDropDownLabel = uilabel(app.Panel_6);
            app.TableDropDownLabel.HorizontalAlignment = 'right';
            app.TableDropDownLabel.Position = [14 6 34 22];
            app.TableDropDownLabel.Text = 'Table';

            % Create TableDropDown
            app.TableDropDown = uidropdown(app.Panel_6);
            app.TableDropDown.Items = {'TransPlanets', 'MassiveStars', 'Clusters', 'Blazars', 'Small'};
            app.TableDropDown.ValueChangedFcn = createCallbackFcn(app, @TableDropDownValueChanged, true);
            app.TableDropDown.Position = [63 6 143 22];
            app.TableDropDown.Value = 'TransPlanets';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = FieldObjTable(varargin)

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