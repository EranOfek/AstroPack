classdef SubmitStatus < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure           matlab.ui.Figure
        Panel_2            matlab.ui.container.Panel
        SubmitStatusLabel  matlab.ui.control.Label
    end

    
    properties (Access = public)
        MainModule      %
        Status          %
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 624 479];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [15 434 599 37];

            % Create SubmitStatusLabel
            app.SubmitStatusLabel = uilabel(app.Panel_2);
            app.SubmitStatusLabel.HorizontalAlignment = 'center';
            app.SubmitStatusLabel.FontSize = 18;
            app.SubmitStatusLabel.FontWeight = 'bold';
            app.SubmitStatusLabel.Position = [8 1 568 33];
            app.SubmitStatusLabel.Text = 'Submit Status';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = SubmitStatus(varargin)

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