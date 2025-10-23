classdef MsgBox < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure       matlab.ui.Figure
        TextArea       matlab.ui.control.TextArea
        TextAreaLabel  matlab.ui.control.Label
        Panel_2        matlab.ui.container.Panel
        TitleLabel     matlab.ui.control.Label
        Panel          matlab.ui.container.Panel
        CloseButton    matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % About App
            %
            % This app displays information about the ULTRASAT Observation Planner,
            % including version details, update history, developers, and support links.
            %
            % Features:
            % - Shows the current version and last update date.
            % - Lists the developers of the project.
            % - Provides direct hyperlinks to the project website and support email.
            % - Displays the project and institution logos.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Title           % Form title
        Msg             % Message text to display in box
        Status          % Required by PlannerMain.showModal() but not used
    end
    
 methods (Access = public)

        function beforeShow(app)
            % Called from PlannerMain.showModal()
            app.TitleLabel.Text = app.Title;
            app.TextArea.Value = app.Msg;
        end
    end



    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
            app.UIFigure.Name = 'Message';
            app.MainModule.AppUtils.center(app);
        end

        % Button pushed function: CloseButton
        function CloseButtonPushed(app, event)
            uiresume(app.UIFigure);
        end

        % Callback function
        function HyperlinkWebsiteClicked(app, event)
            web(app.HyperlinkWebsite.Value);
        end

        % Callback function
        function HyperlinkEmailClicked(app, event)
            web(app.HyperlinkEmail.Value);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 479 351];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [17 14 452 57];

            % Create CloseButton
            app.CloseButton = uibutton(app.Panel, 'push');
            app.CloseButton.ButtonPushedFcn = createCallbackFcn(app, @CloseButtonPushed, true);
            app.CloseButton.Position = [183 9 85 39];
            app.CloseButton.Text = 'Close';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.7176 0.2745 1];
            app.Panel_2.Position = [9 307 460 37];

            % Create TitleLabel
            app.TitleLabel = uilabel(app.Panel_2);
            app.TitleLabel.HorizontalAlignment = 'center';
            app.TitleLabel.FontSize = 18;
            app.TitleLabel.FontWeight = 'bold';
            app.TitleLabel.FontColor = [1 1 1];
            app.TitleLabel.Position = [7 1 453 33];
            app.TitleLabel.Text = 'Title';

            % Create TextAreaLabel
            app.TextAreaLabel = uilabel(app.UIFigure);
            app.TextAreaLabel.HorizontalAlignment = 'right';
            app.TextAreaLabel.Position = [41 266 25 22];
            app.TextAreaLabel.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.UIFigure);
            app.TextArea.Editable = 'off';
            app.TextArea.FontSize = 18;
            app.TextArea.Position = [16 82 453 208];
            app.TextArea.Value = {'Message'};

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = MsgBox(varargin)

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