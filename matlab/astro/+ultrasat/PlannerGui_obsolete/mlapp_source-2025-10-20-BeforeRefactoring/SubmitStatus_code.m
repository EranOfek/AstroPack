classdef SubmitStatus < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure           matlab.ui.Figure
        Panel_2            matlab.ui.container.Panel
        SubmitStatusLabel  matlab.ui.control.Label
        Panel              matlab.ui.container.Panel
        CancelButton       matlab.ui.control.Button
        SubmitButton       matlab.ui.control.Button
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

        % Button pushed function: SubmitButton
        function SubmitButtonPushed(app, event)
            app.Status = 'Submit';
            uiresume(app.UIFigure);                        
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);                        
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 640 480];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [15 11 603 57];

            % Create SubmitButton
            app.SubmitButton = uibutton(app.Panel, 'push');
            app.SubmitButton.ButtonPushedFcn = createCallbackFcn(app, @SubmitButtonPushed, true);
            app.SubmitButton.FontWeight = 'bold';
            app.SubmitButton.FontColor = [0 0.4471 0.7412];
            app.SubmitButton.Position = [186 8 85 39];
            app.SubmitButton.Text = 'Submit';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [321 8 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [15 435 599 37];

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