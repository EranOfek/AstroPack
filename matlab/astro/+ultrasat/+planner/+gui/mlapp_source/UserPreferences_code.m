classdef UserPreferences < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                     matlab.ui.Figure
        GeneralUserPreferencesPanel  matlab.ui.container.Panel
        RADecformatDropDown          matlab.ui.control.DropDown
        RADecformatDropDownLabel     matlab.ui.control.Label
        Panel_2                      matlab.ui.container.Panel
        UserPreferencesLabel         matlab.ui.control.Label
        Panel                        matlab.ui.container.Panel
        CancelButton                 matlab.ui.control.Button
        SaveButton                   matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % User preferences window for selecting and saving application settings.
            % Includes a dropdown for choosing the RA & Dec format and buttons to save or cancel changes.
            % The Save and Cancel buttons update the Status property and resume the UI.
            % Not yet functional beyond basic UI interactions.            
        end
    end


    properties (Access = public)
        MainModule          % Reference to the main application module
        Status              % Status of the operation ('Save' or 'Cancel')
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            app.Status = 'Save';
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
            app.UIFigure.Position = [100 100 627 461];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [17 11 603 57];

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0 1];
            app.SaveButton.Position = [187 8 85 39];
            app.SaveButton.Text = 'Save';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [301 8 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [16 421 599 33];

            % Create UserPreferencesLabel
            app.UserPreferencesLabel = uilabel(app.Panel_2);
            app.UserPreferencesLabel.HorizontalAlignment = 'center';
            app.UserPreferencesLabel.FontSize = 18;
            app.UserPreferencesLabel.FontWeight = 'bold';
            app.UserPreferencesLabel.Position = [8 1 562 33];
            app.UserPreferencesLabel.Text = 'User Preferences';

            % Create GeneralUserPreferencesPanel
            app.GeneralUserPreferencesPanel = uipanel(app.UIFigure);
            app.GeneralUserPreferencesPanel.TitlePosition = 'centertop';
            app.GeneralUserPreferencesPanel.Title = 'General User Preferences';
            app.GeneralUserPreferencesPanel.Position = [16 84 599 325];

            % Create RADecformatDropDownLabel
            app.RADecformatDropDownLabel = uilabel(app.GeneralUserPreferencesPanel);
            app.RADecformatDropDownLabel.HorizontalAlignment = 'right';
            app.RADecformatDropDownLabel.Position = [25 252 94 22];
            app.RADecformatDropDownLabel.Text = 'RA & Dec format';

            % Create RADecformatDropDown
            app.RADecformatDropDown = uidropdown(app.GeneralUserPreferencesPanel);
            app.RADecformatDropDown.Items = {'Degrees', 'Sexagesimal'};
            app.RADecformatDropDown.Position = [134 252 175 22];
            app.RADecformatDropDown.Value = 'Degrees';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = UserPreferences

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

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