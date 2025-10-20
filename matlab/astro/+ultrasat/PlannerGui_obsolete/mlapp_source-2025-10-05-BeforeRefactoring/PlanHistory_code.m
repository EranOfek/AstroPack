classdef PlanHistory < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure             matlab.ui.Figure
        Panel_4              matlab.ui.container.Panel
        UITable              matlab.ui.control.Table
        Panel_2              matlab.ui.container.Panel
        PlanHistoryLogLabel  matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % PlanHistory App
            %
            % This app displays the history and log of an observation plan.
            %
            % Features:
            % - Presents a table of recorded plan events.
            % - Allows users to review past modifications and statuses.
            % - Can be closed without affecting the current plan state.
        end
    end

    properties (Access = public)
        MainModule          % Reference to the main application module
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

        % Callback function: not associated with a component
        function UITableSelectionChanged(app, event)
           
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 843 541];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 499 821 33];

            % Create PlanHistoryLogLabel
            app.PlanHistoryLogLabel = uilabel(app.Panel_2);
            app.PlanHistoryLogLabel.HorizontalAlignment = 'center';
            app.PlanHistoryLogLabel.FontSize = 18;
            app.PlanHistoryLogLabel.FontWeight = 'bold';
            app.PlanHistoryLogLabel.Position = [8 1 802 33];
            app.PlanHistoryLogLabel.Text = 'Plan History & Log';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.BorderColor = [0.4902 0.4902 0.4902];
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Panel_4.Position = [15 12 821 476];

            % Create UITable
            app.UITable = uitable(app.Panel_4);
            app.UITable.ColumnName = '';
            app.UITable.RowName = {};
            app.UITable.FontSize = 10;
            app.UITable.Position = [8 10 802 457];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlanHistory(varargin)

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