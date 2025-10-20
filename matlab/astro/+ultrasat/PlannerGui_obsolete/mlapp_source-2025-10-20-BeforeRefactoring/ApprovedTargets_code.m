classdef ApprovedTargets < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure              matlab.ui.Figure
        Panel_4               matlab.ui.container.Panel
        UITable               matlab.ui.control.Table
        Panel_2               matlab.ui.container.Panel
        ApprovedTargetsLabel  matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % ApprovedTargets App
            %
            % This app displays the same Approved Targets table as in PlannerMain,
            % allowing users to view targets in a standalone window.
            %
            % Features:
            % - Provides a larger, resizable view of the Approved Targets table.
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
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1085 620];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.8588 0.9294 0.7608];
            app.Panel_2.Position = [14 582 1065 30];

            % Create ApprovedTargetsLabel
            app.ApprovedTargetsLabel = uilabel(app.Panel_2);
            app.ApprovedTargetsLabel.HorizontalAlignment = 'center';
            app.ApprovedTargetsLabel.FontSize = 18;
            app.ApprovedTargetsLabel.FontWeight = 'bold';
            app.ApprovedTargetsLabel.Position = [8 5 1049 24];
            app.ApprovedTargetsLabel.Text = 'Approved Targets';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.BorderColor = [0.4902 0.4902 0.4902];
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Panel_4.Position = [14 11 1065 559];

            % Create UITable
            app.UITable = uitable(app.Panel_4);
            app.UITable.ColumnName = '';
            app.UITable.RowName = {};
            app.UITable.FontSize = 10;
            app.UITable.Position = [8 10 1049 540];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = ApprovedTargets(varargin)

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