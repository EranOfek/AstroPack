classdef PlotSkyMap < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure         matlab.ui.Figure
        Label            matlab.ui.control.Label
        Panel_2          matlab.ui.container.Panel
        PlotSkyMapLabel  matlab.ui.control.Label
        AxesSkymapPlot   matlab.ui.control.UIAxes
    end

    methods (Static)
        function about()
            % PlotSkyMap App
            %
            % This app displays the same sky map plot as in PlannerMain,
            % allowing users to resize it in a standalone window.
            %
            % Features:
            % - Provides a larger, resizable view of the sky map.
            % - Displays the plot using MATLAB UIAxes.
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
            app.UIFigure.Position = [100 100 1249 853];
            app.UIFigure.Name = 'MATLAB App';

            % Create AxesSkymapPlot
            app.AxesSkymapPlot = uiaxes(app.UIFigure);
            title(app.AxesSkymapPlot, 'Title')
            xlabel(app.AxesSkymapPlot, 'X')
            ylabel(app.AxesSkymapPlot, 'Y')
            zlabel(app.AxesSkymapPlot, 'Z')
            app.AxesSkymapPlot.FontName = 'Helvetica';
            app.AxesSkymapPlot.Position = [9 14 1229 778];

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [8 815 1230 31];

            % Create PlotSkyMapLabel
            app.PlotSkyMapLabel = uilabel(app.Panel_2);
            app.PlotSkyMapLabel.HorizontalAlignment = 'center';
            app.PlotSkyMapLabel.FontSize = 18;
            app.PlotSkyMapLabel.FontWeight = 'bold';
            app.PlotSkyMapLabel.Position = [0 -1 1220 33];
            app.PlotSkyMapLabel.Text = 'Plot - Sky Map';

            % Create Label
            app.Label = uilabel(app.UIFigure);
            app.Label.FontWeight = 'bold';
            app.Label.FontColor = [0 0 1];
            app.Label.Position = [19 791 310 22];
            app.Label.Text = 'Use the controls in the main form to update this plot.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlotSkyMap(varargin)

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