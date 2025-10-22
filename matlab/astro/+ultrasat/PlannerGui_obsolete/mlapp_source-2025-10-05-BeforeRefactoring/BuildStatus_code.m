classdef BuildStatus < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure               matlab.ui.Figure
        TabGroup               matlab.ui.container.TabGroup
        ValidationHTMLTab      matlab.ui.container.Tab
        StatuHTML              matlab.ui.control.HTML
        StatusTextTab          matlab.ui.container.Tab
        StatusTextArea         matlab.ui.control.TextArea
        StatusPanel            matlab.ui.container.Panel
        StatusEditField        matlab.ui.control.EditField
        StatusEditFieldLabel   matlab.ui.control.Label
        ElapsedEditField       matlab.ui.control.EditField
        ElapsedEditFieldLabel  matlab.ui.control.Label
        StartedEditField       matlab.ui.control.EditField
        StartedEditFieldLabel  matlab.ui.control.Label
        Panel_2                matlab.ui.container.Panel
        BuildStatusLabel       matlab.ui.control.Label
        Panel                  matlab.ui.container.Panel
        OKButton               matlab.ui.control.Button
    end

    
    properties (Access = public)
        MainModule      %
        Status          %
    end
    

    methods (Access = public)
        function setData(app, Data)
            app.StatusEditField.Value = '';
            app.StartedEditField.Value = '';
            app.StatusTextArea.Value = '';
            app.StatuHTML.HtmlSource = '';
        end
    end

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: OKButton
        function OKButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Callback function
        function ValidateButtonPushed(app, event)
            
            app.MainModule

            %uiresume(app.UIFigure);            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 640 585];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [17 8 603 57];

            % Create OKButton
            app.OKButton = uibutton(app.Panel, 'push');
            app.OKButton.ButtonPushedFcn = createCallbackFcn(app, @OKButtonPushed, true);
            app.OKButton.Position = [260 9 85 39];
            app.OKButton.Text = 'OK';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.FontWeight = 'bold';
            app.Panel_2.FontSize = 14;
            app.Panel_2.Position = [8 544 625 33];

            % Create BuildStatusLabel
            app.BuildStatusLabel = uilabel(app.Panel_2);
            app.BuildStatusLabel.HorizontalAlignment = 'center';
            app.BuildStatusLabel.FontSize = 18;
            app.BuildStatusLabel.FontWeight = 'bold';
            app.BuildStatusLabel.Position = [8 5 606 22];
            app.BuildStatusLabel.Text = 'Build Status';

            % Create StatusPanel
            app.StatusPanel = uipanel(app.UIFigure);
            app.StatusPanel.Title = 'Status';
            app.StatusPanel.Position = [11 392 619 142];

            % Create StartedEditFieldLabel
            app.StartedEditFieldLabel = uilabel(app.StatusPanel);
            app.StartedEditFieldLabel.HorizontalAlignment = 'right';
            app.StartedEditFieldLabel.Position = [20 85 44 22];
            app.StartedEditFieldLabel.Text = 'Started';

            % Create StartedEditField
            app.StartedEditField = uieditfield(app.StatusPanel, 'text');
            app.StartedEditField.Editable = 'off';
            app.StartedEditField.Position = [79 85 174 22];

            % Create ElapsedEditFieldLabel
            app.ElapsedEditFieldLabel = uilabel(app.StatusPanel);
            app.ElapsedEditFieldLabel.HorizontalAlignment = 'right';
            app.ElapsedEditFieldLabel.Position = [21 54 48 22];
            app.ElapsedEditFieldLabel.Text = 'Elapsed';

            % Create ElapsedEditField
            app.ElapsedEditField = uieditfield(app.StatusPanel, 'text');
            app.ElapsedEditField.Editable = 'off';
            app.ElapsedEditField.Position = [84 54 174 22];

            % Create StatusEditFieldLabel
            app.StatusEditFieldLabel = uilabel(app.StatusPanel);
            app.StatusEditFieldLabel.HorizontalAlignment = 'right';
            app.StatusEditFieldLabel.Position = [31 13 39 22];
            app.StatusEditFieldLabel.Text = 'Status';

            % Create StatusEditField
            app.StatusEditField = uieditfield(app.StatusPanel, 'text');
            app.StatusEditField.Editable = 'off';
            app.StatusEditField.Position = [85 13 174 22];

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [12 64 621 317];

            % Create ValidationHTMLTab
            app.ValidationHTMLTab = uitab(app.TabGroup);
            app.ValidationHTMLTab.Title = 'Validation HTML';

            % Create StatuHTML
            app.StatuHTML = uihtml(app.ValidationHTMLTab);
            app.StatuHTML.Position = [13 12 597 269];

            % Create StatusTextTab
            app.StatusTextTab = uitab(app.TabGroup);
            app.StatusTextTab.Title = 'Status Text';

            % Create StatusTextArea
            app.StatusTextArea = uitextarea(app.StatusTextTab);
            app.StatusTextArea.Position = [14 12 596 269];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = BuildStatus(varargin)

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