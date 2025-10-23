classdef About < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                 matlab.ui.Figure
        Image2                   matlab.ui.control.Image
        Image                    matlab.ui.control.Image
        Panel_3                  matlab.ui.container.Panel
        HyperlinkWebsite         matlab.ui.control.Hyperlink
        WebsiteLabel             matlab.ui.control.Label
        SupportLabel             matlab.ui.control.Label
        HyperlinkEmail           matlab.ui.control.Hyperlink
        DevelopersTextArea       matlab.ui.control.TextArea
        DevelopersTextAreaLabel  matlab.ui.control.Label
        UpdatedEditField         matlab.ui.control.EditField
        UpdatedEditFieldLabel    matlab.ui.control.Label
        VersionEditField         matlab.ui.control.EditField
        VersionEditFieldLabel    matlab.ui.control.Label
        Panel_2                  matlab.ui.container.Panel
        ULTRASATObservationPlannerLabel  matlab.ui.control.Label
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
        DataModule      % Reference to the main application module
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, DataModule)
            app.DataModule = DataModule;
            app.UIFigure.Name = 'About';
        end

        % Callback function: HyperlinkWebsite
        function HyperlinkWebsiteClicked(app, event)
            web(app.HyperlinkWebsite.Value);
        end

        % Callback function: HyperlinkEmail
        function HyperlinkEmailClicked(app, event)
            web(app.HyperlinkEmail.Value);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Get the file path for locating images
            pathToMLAPP = fileparts(mfilename('fullpath'));

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 643 441];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [9 397 627 37];

            % Create ULTRASATObservationPlannerLabel
            app.ULTRASATObservationPlannerLabel = uilabel(app.Panel_2);
            app.ULTRASATObservationPlannerLabel.HorizontalAlignment = 'center';
            app.ULTRASATObservationPlannerLabel.FontSize = 18;
            app.ULTRASATObservationPlannerLabel.FontWeight = 'bold';
            app.ULTRASATObservationPlannerLabel.Position = [7 1 620 33];
            app.ULTRASATObservationPlannerLabel.Text = 'ULTRASAT Observation Planner';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.TitlePosition = 'centertop';
            app.Panel_3.Position = [9 9 362 379];

            % Create VersionEditFieldLabel
            app.VersionEditFieldLabel = uilabel(app.Panel_3);
            app.VersionEditFieldLabel.HorizontalAlignment = 'right';
            app.VersionEditFieldLabel.Position = [30 332 45 22];
            app.VersionEditFieldLabel.Text = 'Version';

            % Create VersionEditField
            app.VersionEditField = uieditfield(app.Panel_3, 'text');
            app.VersionEditField.Editable = 'off';
            app.VersionEditField.Position = [90 332 249 22];
            app.VersionEditField.Value = '0.1c';

            % Create UpdatedEditFieldLabel
            app.UpdatedEditFieldLabel = uilabel(app.Panel_3);
            app.UpdatedEditFieldLabel.HorizontalAlignment = 'right';
            app.UpdatedEditFieldLabel.Position = [30 300 50 22];
            app.UpdatedEditFieldLabel.Text = 'Updated';

            % Create UpdatedEditField
            app.UpdatedEditField = uieditfield(app.Panel_3, 'text');
            app.UpdatedEditField.Editable = 'off';
            app.UpdatedEditField.Position = [90 300 249 22];
            app.UpdatedEditField.Value = '20/10/2025';

            % Create DevelopersTextAreaLabel
            app.DevelopersTextAreaLabel = uilabel(app.Panel_3);
            app.DevelopersTextAreaLabel.HorizontalAlignment = 'right';
            app.DevelopersTextAreaLabel.Position = [7 265 66 22];
            app.DevelopersTextAreaLabel.Text = 'Developers';

            % Create DevelopersTextArea
            app.DevelopersTextArea = uitextarea(app.Panel_3);
            app.DevelopersTextArea.Editable = 'off';
            app.DevelopersTextArea.Position = [88 195 251 94];
            app.DevelopersTextArea.Value = {'Chen Tishler'; 'Yossi Shvartzvald'; 'Sasha Krassilchtchikov'; 'Eran Ofek'};

            % Create HyperlinkEmail
            app.HyperlinkEmail = uihyperlink(app.Panel_3);
            app.HyperlinkEmail.HyperlinkClickedFcn = createCallbackFcn(app, @HyperlinkEmailClicked, true);
            app.HyperlinkEmail.URL = 'mailto:planner@ultrasatsoc.org';
            app.HyperlinkEmail.Position = [95 165 186 22];
            app.HyperlinkEmail.Text = 'mailto:planner@ultrasatsoc.org';

            % Create SupportLabel
            app.SupportLabel = uilabel(app.Panel_3);
            app.SupportLabel.Position = [29 164 47 22];
            app.SupportLabel.Text = 'Support';

            % Create WebsiteLabel
            app.WebsiteLabel = uilabel(app.Panel_3);
            app.WebsiteLabel.Position = [25 129 48 22];
            app.WebsiteLabel.Text = 'Website';

            % Create HyperlinkWebsite
            app.HyperlinkWebsite = uihyperlink(app.Panel_3);
            app.HyperlinkWebsite.HyperlinkClickedFcn = createCallbackFcn(app, @HyperlinkWebsiteClicked, true);
            app.HyperlinkWebsite.URL = 'https://www.weizmann.ac.il/ultrasat/';
            app.HyperlinkWebsite.Position = [95 131 211 22];
            app.HyperlinkWebsite.Text = 'https://www.weizmann.ac.il/ultrasat/';

            % Create Image
            app.Image = uiimage(app.UIFigure);
            app.Image.Position = [381 -3 255 334];
            app.Image.ImageSource = fullfile(pathToMLAPP, 'images', 'PlannerLogo1.png');

            % Create Image2
            app.Image2 = uiimage(app.UIFigure);
            app.Image2.Position = [323 330 371 58];
            app.Image2.ImageSource = fullfile(pathToMLAPP, 'images', 'WisLogo1.png');

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = About(varargin)

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