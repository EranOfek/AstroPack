classdef FieldObjTable < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                    matlab.ui.Figure
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
            app.MainModule.MainApp.PlotHelper.FieldObjTableDropDownValueChanged(app.MainModule.MainApp);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1026 543];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 501 1004 33];

            % Create FieldObjectsLabel
            app.FieldObjectsLabel = uilabel(app.Panel_2);
            app.FieldObjectsLabel.HorizontalAlignment = 'center';
            app.FieldObjectsLabel.FontSize = 18;
            app.FieldObjectsLabel.FontWeight = 'bold';
            app.FieldObjectsLabel.Position = [8 1 995 33];
            app.FieldObjectsLabel.Text = 'Field Objects';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [928 3 64 26];
            app.HelpButton.Text = 'Help';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.BorderColor = [0.4902 0.4902 0.4902];
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Panel_4.Position = [14 14 1003 441];

            % Create UITableData
            app.UITableData = uitable(app.Panel_4);
            app.UITableData.ColumnName = '';
            app.UITableData.RowName = {};
            app.UITableData.SelectionChangedFcn = createCallbackFcn(app, @UITableDataSelectionChanged, true);
            app.UITableData.FontSize = 10;
            app.UITableData.Position = [9 13 983 418];

            % Create TransPlanetsEditFieldLabel
            app.TransPlanetsEditFieldLabel = uilabel(app.UIFigure);
            app.TransPlanetsEditFieldLabel.HorizontalAlignment = 'right';
            app.TransPlanetsEditFieldLabel.Position = [234 467 75 22];
            app.TransPlanetsEditFieldLabel.Text = 'TransPlanets';

            % Create TransPlanetsEditField
            app.TransPlanetsEditField = uieditfield(app.UIFigure, 'text');
            app.TransPlanetsEditField.Editable = 'off';
            app.TransPlanetsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.TransPlanetsEditField.Position = [324 467 50 22];

            % Create MassiveStarsEditFieldLabel
            app.MassiveStarsEditFieldLabel = uilabel(app.UIFigure);
            app.MassiveStarsEditFieldLabel.HorizontalAlignment = 'right';
            app.MassiveStarsEditFieldLabel.Position = [391 467 77 22];
            app.MassiveStarsEditFieldLabel.Text = 'MassiveStars';

            % Create MassiveStarsEditField
            app.MassiveStarsEditField = uieditfield(app.UIFigure, 'text');
            app.MassiveStarsEditField.Editable = 'off';
            app.MassiveStarsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MassiveStarsEditField.Position = [483 467 50 22];

            % Create ClustersEditFieldLabel
            app.ClustersEditFieldLabel = uilabel(app.UIFigure);
            app.ClustersEditFieldLabel.HorizontalAlignment = 'right';
            app.ClustersEditFieldLabel.Position = [557 467 49 22];
            app.ClustersEditFieldLabel.Text = 'Clusters';

            % Create ClustersEditField
            app.ClustersEditField = uieditfield(app.UIFigure, 'text');
            app.ClustersEditField.Editable = 'off';
            app.ClustersEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ClustersEditField.Position = [621 467 50 22];

            % Create BlazarsEditFieldLabel
            app.BlazarsEditFieldLabel = uilabel(app.UIFigure);
            app.BlazarsEditFieldLabel.HorizontalAlignment = 'right';
            app.BlazarsEditFieldLabel.Position = [694 467 45 22];
            app.BlazarsEditFieldLabel.Text = 'Blazars';

            % Create BlazarsEditField
            app.BlazarsEditField = uieditfield(app.UIFigure, 'text');
            app.BlazarsEditField.Editable = 'off';
            app.BlazarsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BlazarsEditField.Position = [754 467 50 22];

            % Create SmallEditFieldLabel
            app.SmallEditFieldLabel = uilabel(app.UIFigure);
            app.SmallEditFieldLabel.HorizontalAlignment = 'right';
            app.SmallEditFieldLabel.Position = [819 467 35 22];
            app.SmallEditFieldLabel.Text = 'Small';

            % Create SmallEditField
            app.SmallEditField = uieditfield(app.UIFigure, 'text');
            app.SmallEditField.Editable = 'off';
            app.SmallEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SmallEditField.Position = [869 467 50 22];

            % Create TableDropDownLabel
            app.TableDropDownLabel = uilabel(app.UIFigure);
            app.TableDropDownLabel.HorizontalAlignment = 'right';
            app.TableDropDownLabel.Position = [25 467 34 22];
            app.TableDropDownLabel.Text = 'Table';

            % Create TableDropDown
            app.TableDropDown = uidropdown(app.UIFigure);
            app.TableDropDown.Items = {'TransPlanets', 'MassiveStars', 'Clusters', 'Blazars', 'Small'};
            app.TableDropDown.ValueChangedFcn = createCallbackFcn(app, @TableDropDownValueChanged, true);
            app.TableDropDown.Position = [74 467 143 22];
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