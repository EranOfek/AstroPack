classdef PlotGraphs < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure         matlab.ui.Figure
        Label            matlab.ui.control.Label
        Panel_2          matlab.ui.container.Panel
        PlotGraphsLabel  matlab.ui.control.Label
        AxesGraphsPlot   matlab.ui.control.UIAxes
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
            app.AxesGraphsPlot.Position = [15 14 1199 789];

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [7 823 1207 33];

            % Create PlotGraphsLabel
            app.PlotGraphsLabel = uilabel(app.Panel_2);
            app.PlotGraphsLabel.HorizontalAlignment = 'center';
            app.PlotGraphsLabel.FontSize = 18;
            app.PlotGraphsLabel.FontWeight = 'bold';
            app.PlotGraphsLabel.Position = [1 0 965 33];
            app.PlotGraphsLabel.Text = 'Plot - Graphs';

            % Create Label
            app.Label = uilabel(app.UIFigure);
            app.Label.FontWeight = 'bold';
            app.Label.FontColor = [0 0 1];
            app.Label.Position = [15 802 310 22];
            app.Label.Text = 'Use the controls in the main form to update this plot.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlotGraphs(varargin)

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