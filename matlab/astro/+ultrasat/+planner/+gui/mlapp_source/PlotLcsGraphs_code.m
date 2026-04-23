classdef PlotLcsGraphs < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure            matlab.ui.Figure
        Panel_2             matlab.ui.container.Panel
        ButtonGroup         matlab.ui.container.ButtonGroup
        CatBButton          matlab.ui.control.RadioButton
        ScheduleButton      matlab.ui.control.RadioButton
        HelpButton          matlab.ui.control.Button
        PlotLCSGraphsLabel  matlab.ui.control.Label
        AxesGraphsPlot      matlab.ui.control.UIAxes
    end

    methods (Static)
        function about()
            % PlotGraphs App
            %
            % This app displays the same graphs as in PlannerMain,
            % allowing users to resize them in a standalone window.
            %
            % Features:
            % - Provides a larger, resizable view of plotted graphs.
            % - Displays plots using MATLAB UIAxes.
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

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('lcs_graphs_plot');
        end

        % Selection changed function: ButtonGroup
        function ButtonGroupSelectionChanged(app, event)
            app.MainModule.MainApp.PlotHelper.plotLcsGraphs(app.MainModule.MainApp, app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 200 1221 862];
            app.UIFigure.Name = 'MATLAB App';

            % Create AxesGraphsPlot
            app.AxesGraphsPlot = uiaxes(app.UIFigure);
            title(app.AxesGraphsPlot, 'Title')
            xlabel(app.AxesGraphsPlot, 'X')
            ylabel(app.AxesGraphsPlot, 'Y')
            zlabel(app.AxesGraphsPlot, 'Z')
            app.AxesGraphsPlot.FontName = 'Helvetica';
            app.AxesGraphsPlot.Position = [15 15 1199 789];

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.HighlightColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [7 823 1207 33];

            % Create PlotLCSGraphsLabel
            app.PlotLCSGraphsLabel = uilabel(app.Panel_2);
            app.PlotLCSGraphsLabel.HorizontalAlignment = 'center';
            app.PlotLCSGraphsLabel.FontSize = 18;
            app.PlotLCSGraphsLabel.FontWeight = 'bold';
            app.PlotLCSGraphsLabel.Position = [1 0 1197 33];
            app.PlotLCSGraphsLabel.Text = 'Plot - LCS Graphs';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [1113 2 64 28];
            app.HelpButton.Text = 'Help';

            % Create ButtonGroup
            app.ButtonGroup = uibuttongroup(app.Panel_2);
            app.ButtonGroup.SelectionChangedFcn = createCallbackFcn(app, @ButtonGroupSelectionChanged, true);
            app.ButtonGroup.ForegroundColor = [0 0 1];
            app.ButtonGroup.Position = [845 2 174 30];

            % Create ScheduleButton
            app.ScheduleButton = uiradiobutton(app.ButtonGroup);
            app.ScheduleButton.Text = 'Schedule';
            app.ScheduleButton.FontWeight = 'bold';
            app.ScheduleButton.FontColor = [0 0 1];
            app.ScheduleButton.Position = [8 3 75 22];
            app.ScheduleButton.Value = true;

            % Create CatBButton
            app.CatBButton = uiradiobutton(app.ButtonGroup);
            app.CatBButton.Text = 'CatB';
            app.CatBButton.FontWeight = 'bold';
            app.CatBButton.FontColor = [0 0 1];
            app.CatBButton.Position = [108 4 50 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlotLcsGraphs(varargin)

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