classdef AdjustGroupStartTime < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                      matlab.ui.Figure
        Label                         matlab.ui.control.Label
        AdjustButtonGroup             matlab.ui.container.ButtonGroup
        StartTimeEditField            matlab.ui.control.EditField
        YYYYMMDDHHMMSSEditFieldLabel  matlab.ui.control.Label
        ShiftTimeEditField            matlab.ui.control.EditField
        HHMMSSLabel                   matlab.ui.control.Label
        NewStartTimeButton            matlab.ui.control.RadioButton
        ShiftTimeButton               matlab.ui.control.RadioButton
        RelativeButton                matlab.ui.control.RadioButton
        GroupDropDown                 matlab.ui.control.DropDown
        GroupDropDownLabel            matlab.ui.control.Label
        Panel_2                       matlab.ui.container.Panel
        AdjustGroupStartTimeLabel     matlab.ui.control.Label
        Panel                         matlab.ui.container.Panel
        HelpButton                    matlab.ui.control.Button
        CancelButton                  matlab.ui.control.Button
        OKButton                      matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % AdjustGroupStartTime App
            %
            % This app allows users to adjust the start time of observation groups.
            % Users can choose between relative adjustments, shifting time, or 
            % setting a new absolute start time. 
            % 
            % Features:
            % - Supports group-based start time modifications.
            % - Provides a dropdown for selecting specific groups or 'All'.
            % - Modal dialog ensures user confirmation before applying changes.            
        end
    end

    properties (Access = public)
        MainModule          % Reference to the main application module
        Status              % Status of the operation, e.g., 'OK' or 'Cancel'
        Mode                % Selected adjustment mode: 'Relative', 'StartTime', or 'Shift'
        GroupList           % List of selected groups for adjustment
        StartTime           % New start time if using 'StartTime' mode
        ShiftTime           % Time shift duration if using 'Shift' mode
    end


    methods (Access = public)
        function beforeShow(app)
            % Initialize UI components before displaying the modal window, 
            % called from PlannerMain.showModal()
            %
            % - Resets the GroupDropDown selection to 'All'.
            % - Ensures the correct default settings before user input.            
            app.GroupDropDown.Value = 'All';
            %app.GroupDropDownValueChanged();            
        end        
    end
   

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
            app.MainModule.AppUtils.center(app);
        end

        % Button pushed function: OKButton
        function SaveButtonPushed(app, event)
            % Apply the selected adjustment and close the app.
            %
            % - Determines the adjustment mode based on selected radio button.
            % - Converts input values to appropriate datetime or duration formats.
            % - Sets the Status to 'OK' and resumes the UI.            
            app.Status = [];
            try
                % Set Mode based on seleted radio button
                if app.RelativeButton.Value
                    app.Mode = 'Relative';
                elseif app.ShiftTimeButton.Value
                    app.Mode = 'Shift';
                    app.ShiftTime = duration(app.ShiftTimeEditField.Value);
                elseif app.NewStartTimeButton.Value
                    app.Mode = 'StartTime';                    
                    app.StartTime = datetime(app.StartTimeEditField.Value, 'TimeZone', 'UTC');
                end
                app.Status = 'OK';
                uiresume(app.UIFigure);
            catch ME
                app.MainModule.AppUtils.msgError(ME.message);
            end
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Value changed function: GroupDropDown
        function GroupDropDownValueChanged(app, event)
            % Update UI based on the selected group.
            %
            % - Enables or disables NewStartTime mode based on the selection.
            % - Clears GroupList when 'All' is selected, otherwise stores selected group.            
            value = app.GroupDropDown.Value;
            if strcmp(value, 'All')
                app.GroupList = [];
                app.NewStartTimeButton.Enable = 'off';
                app.StartTimeEditField.Enable = 'off';
            else
                app.GroupList = [str2double(value)];
                app.NewStartTimeButton.Enable = 'on';
                app.StartTimeEditField.Enable = 'on';
            end
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('adjust_group_start_time');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 517 388];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.Resize = 'off';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [10 10 501 57];

            % Create OKButton
            app.OKButton = uibutton(app.Panel, 'push');
            app.OKButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.OKButton.FontWeight = 'bold';
            app.OKButton.FontColor = [0 0 1];
            app.OKButton.Position = [107 9 85 39];
            app.OKButton.Text = 'OK';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [212 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [310 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [10 347 501 33];

            % Create AdjustGroupStartTimeLabel
            app.AdjustGroupStartTimeLabel = uilabel(app.Panel_2);
            app.AdjustGroupStartTimeLabel.HorizontalAlignment = 'center';
            app.AdjustGroupStartTimeLabel.FontSize = 18;
            app.AdjustGroupStartTimeLabel.FontWeight = 'bold';
            app.AdjustGroupStartTimeLabel.Position = [9 7 480 22];
            app.AdjustGroupStartTimeLabel.Text = 'Adjust Group Start Time';

            % Create GroupDropDownLabel
            app.GroupDropDownLabel = uilabel(app.UIFigure);
            app.GroupDropDownLabel.HorizontalAlignment = 'right';
            app.GroupDropDownLabel.Position = [31 274 38 22];
            app.GroupDropDownLabel.Text = 'Group';

            % Create GroupDropDown
            app.GroupDropDown = uidropdown(app.UIFigure);
            app.GroupDropDown.Items = {'All', '1', '2', '3'};
            app.GroupDropDown.ValueChangedFcn = createCallbackFcn(app, @GroupDropDownValueChanged, true);
            app.GroupDropDown.Position = [84 274 144 22];
            app.GroupDropDown.Value = 'All';

            % Create AdjustButtonGroup
            app.AdjustButtonGroup = uibuttongroup(app.UIFigure);
            app.AdjustButtonGroup.Title = 'Adjust';
            app.AdjustButtonGroup.Position = [19 80 492 180];

            % Create RelativeButton
            app.RelativeButton = uiradiobutton(app.AdjustButtonGroup);
            app.RelativeButton.Text = 'Relative to mission approved targets (requires Retreive Approved Targets)';
            app.RelativeButton.Position = [11 116 422 22];
            app.RelativeButton.Value = true;

            % Create ShiftTimeButton
            app.ShiftTimeButton = uiradiobutton(app.AdjustButtonGroup);
            app.ShiftTimeButton.Text = 'Shift time by';
            app.ShiftTimeButton.Position = [11 76 88 22];

            % Create NewStartTimeButton
            app.NewStartTimeButton = uiradiobutton(app.AdjustButtonGroup);
            app.NewStartTimeButton.Text = 'New start time';
            app.NewStartTimeButton.Position = [11 30 99 22];

            % Create HHMMSSLabel
            app.HHMMSSLabel = uilabel(app.AdjustButtonGroup);
            app.HHMMSSLabel.HorizontalAlignment = 'right';
            app.HHMMSSLabel.Position = [227 76 88 22];
            app.HHMMSSLabel.Text = '+/- HH:MM:SS';

            % Create ShiftTimeEditField
            app.ShiftTimeEditField = uieditfield(app.AdjustButtonGroup, 'text');
            app.ShiftTimeEditField.Position = [127 76 101 22];
            app.ShiftTimeEditField.Value = '00:05:00';

            % Create YYYYMMDDHHMMSSEditFieldLabel
            app.YYYYMMDDHHMMSSEditFieldLabel = uilabel(app.AdjustButtonGroup);
            app.YYYYMMDDHHMMSSEditFieldLabel.HorizontalAlignment = 'right';
            app.YYYYMMDDHHMMSSEditFieldLabel.Position = [287 30 145 22];
            app.YYYYMMDDHHMMSSEditFieldLabel.Text = 'YYYY-MM-DD HH:MM:SS';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.AdjustButtonGroup, 'text');
            app.StartTimeEditField.Position = [127 30 151 22];
            app.StartTimeEditField.Value = '2028-01-01 12:00:00';

            % Create Label
            app.Label = uilabel(app.UIFigure);
            app.Label.BackgroundColor = [1 1 0.549];
            app.Label.HorizontalAlignment = 'center';
            app.Label.FontWeight = 'bold';
            app.Label.FontAngle = 'italic';
            app.Label.FontColor = [0.102 0.102 0.4];
            app.Label.Position = [10 317 501 22];
            app.Label.Text = 'When selecting All Groups, only Relative and Shift options are applicable.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = AdjustGroupStartTime(varargin)

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