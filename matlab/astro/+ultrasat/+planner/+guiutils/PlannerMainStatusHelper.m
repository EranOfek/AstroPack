%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainStatusHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 08/10/2025
% Description : Status Helper for Main Planner (Update, Clear, etc.)
%==========================================================================

classdef PlannerMainStatusHelper < ultrasat.api.Loggable
    % Helper class for PlannerMain.mlapp
    %
    % All methods require the PlannerMain instance as the first argument, named 'app'.
    % This is NOT implicit: even when calling from PlannerMain.mlapp, pass 'app'
    % explicitly to the helper method.
    %
    % Internal call example (from PlannerMain.mlapp):
    %   app.UniqueTargetsHelper.setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp);
    %
    % External call example (from another window/module):
    %   app.MainModule.MainApp.PlanParamsHelper.applyCheckTimes(app.MainModule.MainApp, ParamsApp);
    %
    % Notes:
    %   - 'app' always refers to the PlannerMain instance.
    %   - Additional parameters (e.g., ParamsApp) are the calling window/modules as needed.
    %

    methods

        function obj = PlannerMainStatusHelper()
            % Constructor
            obj.LogPrefix = 'StatusHelper';
            obj.msglog('PlannerMainStatusHelper created successfully');
        end


        function applyPlanStatus(obj, app)
            % Helper:
            if app.hasPlanner()
                % Only draft plans can be editted, otherwise read-only
                if ~strcmp(planData.status, '') && ~strcmp(planData.status, 'draft')
                    app.setReadOnly(true);
                else
                    app.setReadOnly(false);
                end
            else
            end
        end


        function setReadOnly(obj, app, ReadOnly)
            % Helper: Setc/clear read-only status of the current plan
            app.AllowEdit = ~ReadOnly;
        end


        function Result = isReadOnly(obj, app)
            % Helper: Return true if currently in read-only mode
            Result = ~app.AllowEdit;
        end


        function Result = isReadOnlyMsg(obj, app)
            % Helper: Return true if currently in read-only mode, show popup message
            Result = ~app.AllowEdit;
            if Result
                uialert(app.UIFigure, sprintf('Plan is read-only: %s', app.AllowEditMsg), 'Message', 'Icon', 'success');
            end
        end


        function setModified(obj, app, logText)
            % Helper: Mark the plan as modified (i.e. required to be saved/discarded)
            % if ~app.MainModule.Modified
                if nargin < 2 || isempty(logText)
                    logText = '';
                end
                app.msglog(sprintf('setModified: modified - %s', logText));
                app.MainModule.setModified();
                app.ModifiedLabel.Text = 'Modified';

                app.SaveButton.Enable = 'on';
            % end
        end


        function clearModified(obj, app)
            % Helper: Clear the Modified flag and status
            if app.MainModule.Modified
                app.msglog('clearModified')
            end
            app.MainModule.clearModified();
            app.ModifiedLabel.Text = '';
            app.SaveButton.Enable = 'off';
        end


        function Result = needSave(obj, app, AskSave)
            % Helper: Check if current plan has been modified and need to be saved
            if app.MainModule.Modified
                if AskSave
                    if strcmp(app.AppUtils.askYesNo('Your changes are not saved. Save or discard?', 'Save or discard'), 'Yes')
                        try
                            app.savePlan();
                        catch ME
                            app.msgex('needSave', ME);
                        end
                        Result = true;
                    else
                        Result = true;
                    end
                else
                    Result = true;
                end
            else
                Result = false;
            end
        end


        function setStatus(obj, app, Status, Text)
            % Helper: Update the status panel with new status
            app.msglog(sprintf('setStatus: %s - %s', Status, Text));
            app.MainModule.setStatus(Status, Text);
            app.updateStatus();
        end


        function setStatusEx(obj, app, Title, ME)
            % Helper: Update the status panel with exception message
            app.MainModule.setStatus('Error', sprintf('%s - %s', Title, ME.message));
            app.updateStatus();
        end


        function updateStatus(obj, app)
            % @Todo - ??

            app.setStatusField(app.StatusTextArea, app.MainModule.CurrentStatus, app.MainModule.StatusText);

            PlanData = app.MainModule.PlanData;
            Planner = app.MainModule.Planner;

            % Planner is not empty, set fields
            if ~isempty(Planner)
                app.BuildTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ScheduledTime);
                app.ValidationTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ValidatedTime);
                app.SubmitTimeEditField.Value = app.MainModule.DateTime2Str(Planner.SubmittedTime);

                %
                app.setStatusField(app.BuildShortStatusEditField, PlanData.metadata.BuildStatus.Status, PlanData.metadata.BuildStatus.Status);
                app.setStatusField(app.ValidationShortStatusEditField, PlanData.metadata.ValidationStatus.Status, PlanData.metadata.ValidationStatus.Status);
                app.setStatusField(app.SubmitShortStatusEditField, PlanData.metadata.SubmitStatus.Status, PlanData.metadata.SubmitStatus.Status);

                if strcmp(Planner.Status, 'submitted')
                    app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07]);
                else
                    app.setTopLabel('', [], []);
                end

            % Planner is empty, clear fields
            else
                app.BuildTimeEditField.Value = '';
                app.ValidationTimeEditField.Value = '';
                app.SubmitTimeEditField.Value = '';

                app.setStatusField(app.BuildShortStatusEditField, '', '');
                app.setStatusField(app.ValidationShortStatusEditField, '', '');
                app.setStatusField(app.SubmitShortStatusEditField, '', '');

                app.setTopLabel('', [], []);
            end

        end


        function setStatusField(obj, app, EditField, Status, StatusText)
            % Helper: Set the background color of the EditField based on the Status value.
            % Valid values for Status: OK, Warning, Error, (empty)
            app.MainModule.GuiHelper.setStatusField(app, EditField, Status, StatusText);
        end


        function setTopLabel(obj, app, Text, FontColor, BackgroundColor)
            % Helper: Set text and colors of LabelTopStatus (located just below the main toolbar)
            % Hide the label if Text is empty.
            % Colors: Font: Blue: [0.00,0.00,1.00], Background: Yellow: [1.00,1.00,0.07]
            % Example: app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07])
            if isempty(Text)
                app.LabelTopStatus.Visible = false;
            else
                app.LabelTopStatus.Text = Text;
                app.LabelTopStatus.FontColor = FontColor;
                app.LabelTopStatus.BackgroundColor = BackgroundColor;
                app.LabelTopStatus.Visible = true;
            end
        end

    end
end
