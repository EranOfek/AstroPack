classdef LcsStartTimes < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                     matlab.ui.Figure
        Title3                       matlab.ui.control.Label
        Title2                       matlab.ui.control.Label
        Title1                       matlab.ui.control.Label
        Panel_6                      matlab.ui.container.Panel
        HelpButton                   matlab.ui.control.Button
        CancelButton                 matlab.ui.control.Button
        OKButton                     matlab.ui.control.Button
        Panel_4                      matlab.ui.container.Panel
        UITable                      matlab.ui.control.Table
        Panel_2                      matlab.ui.container.Panel
        SuggestedLCSStartTimesLabel  matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % PlanTargets App
            %
            % This app displays the same Plan Targets table as in PlannerMain,
            % allowing users to view targets in a standalone window.
            %
            % Features:
            % - Provides a larger, resizable view of the Plan Targets table.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation, e.g., 'Cancel' or 'Ok'
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

        % Callback function
        function CloseButtonPushed2(app, event)

        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('lcs_start_times');            
        end

        % Callback function
        function SelectButtonPushed(app, event)

        end

        % Callback function
        function CancelButton_3Pushed(app, event)

        end

        % Callback function
        function OKButtonPushed2(app, event)
            %
        end

        % Callback function
        function HelpButtonPushed2(app, event)

        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);                        
        end

        % Button pushed function: OKButton
        function OKButtonPushed(app, event)
            app.Status = 'Ok';
            uiresume(app.UIFigure);                                    
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1209 581];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.HighlightColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.749 0.851 0.949];
            app.Panel_2.Position = [14 542 1189 30];

            % Create SuggestedLCSStartTimesLabel
            app.SuggestedLCSStartTimesLabel = uilabel(app.Panel_2);
            app.SuggestedLCSStartTimesLabel.HorizontalAlignment = 'center';
            app.SuggestedLCSStartTimesLabel.FontSize = 18;
            app.SuggestedLCSStartTimesLabel.FontWeight = 'bold';
            app.SuggestedLCSStartTimesLabel.Position = [14 1 1166 27];
            app.SuggestedLCSStartTimesLabel.Text = 'Suggested LCS Start Times';

            % Create Panel_4
            app.Panel_4 = uipanel(app.UIFigure);
            app.Panel_4.TitlePosition = 'centertop';
            app.Panel_4.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_4.Position = [20 84 1183 341];

            % Create UITable
            app.UITable = uitable(app.Panel_4);
            app.UITable.ColumnName = '';
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = true;
            app.UITable.FontSize = 10;
            app.UITable.Position = [11 5 1163 325];

            % Create Panel_6
            app.Panel_6 = uipanel(app.UIFigure);
            app.Panel_6.BackgroundColor = [0.8 0.8 0.8];
            app.Panel_6.Position = [21 14 1182 57];

            % Create OKButton
            app.OKButton = uibutton(app.Panel_6, 'push');
            app.OKButton.ButtonPushedFcn = createCallbackFcn(app, @OKButtonPushed, true);
            app.OKButton.FontWeight = 'bold';
            app.OKButton.FontColor = [0 0 1];
            app.OKButton.Position = [463 9 85 39];
            app.OKButton.Text = 'OK';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel_6, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [590 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_6, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [717 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Title1
            app.Title1 = uilabel(app.UIFigure);
            app.Title1.BackgroundColor = [1 1 0.549];
            app.Title1.HorizontalAlignment = 'center';
            app.Title1.FontSize = 16;
            app.Title1.FontWeight = 'bold';
            app.Title1.FontColor = [1 0 0];
            app.Title1.Position = [16 505 1178 28];
            app.Title1.Text = 'LCS cannot be scheduled on the requested start date:';

            % Create Title2
            app.Title2 = uilabel(app.UIFigure);
            app.Title2.BackgroundColor = [1 1 0.549];
            app.Title2.HorizontalAlignment = 'center';
            app.Title2.FontSize = 16;
            app.Title2.FontWeight = 'bold';
            app.Title2.FontColor = [0.102 0.102 0.4];
            app.Title2.Position = [15 472 1179 28];
            app.Title2.Text = 'After searching, these are the nearest available start date options found.';

            % Create Title3
            app.Title3 = uilabel(app.UIFigure);
            app.Title3.BackgroundColor = [1 1 0.549];
            app.Title3.HorizontalAlignment = 'center';
            app.Title3.FontSize = 16;
            app.Title3.FontWeight = 'bold';
            app.Title3.FontColor = [0.102 0.102 0.4];
            app.Title3.Position = [16 440 1179 26];
            app.Title3.Text = 'Please choose an alternative start date from the list and click Ok.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LcsStartTimes(varargin)

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