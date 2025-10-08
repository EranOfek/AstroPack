%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainPlanParamsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 06/10/2025
% Description : Plan Parameters Helper for Main Planner
%==========================================================================

classdef PlannerMainPlanParamsHelper < ultrasat.api.Loggable

    methods
        
        function obj = PlannerMainPlanParamsHelper()
            % Constructor
            obj.LogPrefix = 'PlanParamsHelper';
            obj.msglog('PlannerMainPlanParamsHelper created successfully');
        end

        
        function showPlanParamsWindow(obj, app)
            % Show window with Plan Parameters
            app.msglog('showPlanParamsWindow');
            if ~app.hasPlanner(), return; end            

            Planner = app.MainModule.Planner;

            % Create app
            if isempty(app.PlanParamsApp) || ~isvalid(app.PlanParamsApp)
                app.PlanParamsApp = ultrasat.planner.gui.PlanParams(app.MainModule);
            end
            app.PlanParamsApp.UIFigure.Visible = 'on';
            ParamsApp = app.PlanParamsApp;            

            % Make the form read-only if plan is already built
            app.MainModule.AfterBuild = height(Planner.Plan);
            obj.setPlanParamsFields(app, ParamsApp);

            % Show app
            if strcmp(app.showModal(ParamsApp), 'Save')
                % Apply the parameters
                try
                    % Do we need to call it? it is called from PlanParams's
                    % Save button @Todo
                    app.applyPlanParams(ParamsApp);
                catch ME
                    app.msgex('showPlanParamsWindow', ME);
                end                    
            end
        end


        function setPlanParamsFields(obj, app, ParamsApp)
            % Helper: Set PlanParams app fields from current planner
            % Called from showPlanParamsWindow

            % Get the Planner instance from the main module
            PlanData = app.MainModule.PlanData;
            Planner = app.MainModule.Planner;
        
            % Assign values to UI fields
            ParamsApp.TitleEditField.Value = Planner.Title;
            ParamsApp.PlanTypeDropDown.Value = Planner.Type;
            ParamsApp.StartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.StartTime);
            ParamsApp.EndTimeEditField.Value = app.MainModule.DateTime2Str(Planner.EndTime);
            ParamsApp.ExposureEditField.Value = num2str(seconds(Planner.Exptime));
            ParamsApp.EpochsPerVisitEditField.Value = num2str(Planner.DefEpochsPerVisit);
            
            % Assign tile checkboxes
            tileNumbers = '1234';
            checkBoxes = [ParamsApp.Tile1CheckBox, ParamsApp.Tile2CheckBox, ParamsApp.Tile3CheckBox, ParamsApp.Tile4CheckBox];
            
            for i = 1:length(tileNumbers)
                checkBoxes(i).Value = ismember(tileNumbers(i), char(Planner.Tiles));
            end
            
            % Assign folders and files
            ParamsApp.BaseDataDirEditField.Value = Planner.BaseDataDir;
            ParamsApp.CalSubDirEditField.Value = Planner.CalibDir;
            ParamsApp.CalObjFileEditField.Value = '@TODO';  %Planner.CalibObj;
        
            % Assign Unique Targets & Plan Targets
            ParamsApp.PlanTargetsEditField.Value = num2str(Planner.N_planTargets);
            ParamsApp.UniqueTargetsEditField.Value = num2str(Planner.N_uniqueTargets);
        
            % Assign Check Times
            ParamsApp.CheckStartTimeEditField.Value = app.MainModule.DateTime2Str( Planner.CheckTimes(1) );
            ParamsApp.CheckEndTimeEditField.Value = app.MainModule.DateTime2Str( Planner.CheckTimes(2) );
        
            % Assign System Parameters
            ParamsApp.FieldOfViewRadiusEditField.Value = num2str(Planner.Rfov);
            ParamsApp.TileReadTimeEditField.Value = num2str(seconds(Planner.FullTileReadTime));
            ParamsApp.SlewBufferEditField.Value = num2str(seconds(Planner.DefSlewBuffer));
        
            % Assign LCSTab Parameters
            ParamsApp.LcsDailyWindowStartTimeEditField.Value = char(Planner.DailyWindowStartTime);
            ParamsApp.LcsDailyWindowMaxDurationEditField.Value = char(Planner.DailyWindowMaxDuration);
        
            % Assign AllSkyTab Parameters
            ParamsApp.AllSkyDailyWindowStartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.DailyWindowStartTime);
            ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value = num2str(hours(Planner.DailyWindowMaxDuration));
            ParamsApp.AllSkyGalacticLatTresholdEditField.Value = Planner.AllSSHighLatThresh;

            % @Yossi @Todo ??
            ParamsApp.AllSkyLatVisitsEditField.Value = Planner.LowLatVisits;
            ParamsApp.AllSkyLowLatVisitsEditField.Value = Planner.HighLatVisits;
            ParamsApp.AllSkyHighGalacticLatDitherPatternDropDown.Value = num2str(Planner.DitherPattern);
        
            % Assign TOOTab Parameters
            ParamsApp.TooStartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.TOOStartTime);
            ParamsApp.TooWindowDurationEditField.Value = num2str(hours(Planner.TOOWindowDuration));
        
            % Assign Mission Status Fields
            ParamsApp.PlanStatusEditField.Value = Planner.Status;
            ParamsApp.AstPlannerEditField.Value = Planner.AstPlanner;

            % Status text
            app.setStatusField(ParamsApp.BuildStatusEditField, PlanData.metadata.BuildStatus.Status, PlanData.metadata.BuildStatus.Status);
            app.setStatusField(ParamsApp.ValidationStatusEditField, PlanData.metadata.ValidationStatus.Status, PlanData.metadata.ValidationStatus.Status);
            app.setStatusField(ParamsApp.SubmitStatusEditField, PlanData.metadata.SubmitStatus.Status, PlanData.metadata.SubmitStatus.Status);

            % Status times
            ParamsApp.BuildTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ScheduledTime);            
            ParamsApp.ValidationTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ValidatedTime);
            ParamsApp.SubmitTimeEditField.Value = app.MainModule.DateTime2Str(Planner.SubmittedTime);           
        
            % Assign Mission Distance Constraints            
            ParamsApp.SunMinDistObsEditField.Value = num2str(Planner.ObsSunDist);
            ParamsApp.MoonMinDistObsEditField.Value = num2str(Planner.ObsMoonDist);
            ParamsApp.EarthMinDistObsEditField.Value = num2str(Planner.ObsEarthDist);
            
            ParamsApp.SunMinDistSlewEditField.Value = '@Todo';
            ParamsApp.MoonMinDistSlewEditField.Value = '@Todo';
            ParamsApp.EarthMinDistSlewEditField.Value = '@Todo';

            % Assign Plan Buttons
            ParamsApp.SaveButton.Enable = true;
            ParamsApp.CancelButton.Enable = true;
        end


        function applyPlanParams(obj, app, ParamsApp)
            % Helper: Apply plan parameters in current planner from PlanParams app
            % Called from showPlanParamsWindow            
            try
                obj.doApplyPlanParams(app, ParamsApp);
            catch ME
                app.msgex('applyPlanParams', ME);
            end
        end


        function doApplyPlanParams(obj, app, ParamsApp)
            % Helper: Apply plan parameters in current planner from PlanParams app
            % Called from showPlanParamsWindow

            Planner = app.MainModule.Planner;
            %try
                % General parameters to all plan types
                Planner.Title = ParamsApp.TitleEditField.Value;
                
                % Start & End times
                app.setPlanStartEndTime(ParamsApp.StartTimeEditField.Value, ParamsApp.EndTimeEditField.Value);

                % Other general parameters
                Planner.DefEpochsPerVisit = ParamsApp.EpochsPerVisitEditField.Value;
                Planner.Exptime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.ExposureEditField.Value);
                
                % Apply per-type parameters
                if strcmp(Planner.Type, 'LCS')
                    Planner.DailyWindowStartTime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.LcsDailyWindowStartTimeEditField.Value);
                    Planner.DailyWindowMaxDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.LcsDailyWindowMaxDurationEditField.Value);
                elseif strcmp(Planner.Type, 'AllSS')                
                    Planner.DailyWindowStartTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.AllSkyDailyWindowStartTimeEditField.Value);
                    Planner.DailyWindowMaxDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value);
                    Planner.AllSSHighLatThresh = ParamsApp.AllSkyGalacticLatTresholdEditField.Value;
                    Planner.LowLatVisits = ParamsApp.AllSkyLatVisitsEditField.Value;

                    % Future
                    %Planner.= ParamsApp.AllSkyLowLatVisitsEditField.Value;
                    %Planner.= ParamsApp.AllSkyHighGalacticLatDitherPatternDropDown.Value;
                elseif strcmp(Planner.Type, 'TOO')                
                    Planner.TOOStartTime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.TooStartTimeEditField.Value);
                    Planner.TOOWindowDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.TooWindowMaxDurationEditField.Value);
                end        

                % Apply check times
                app.applyCheckTimes(ParamsApp);                

                % @Future: Apply system constants from ParamsApp

            %catch ME
            %    app.msgex('applyPlanParams', ME);
            %end
        end


        function setPlanStartEndTime(obj, app, StartTimeValue, EndTimeValue)
            %
            app.msglog('setPlanStartEndTime')            
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end

            try
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(StartTimeValue);
                EndTime = app.MainModule.GuiHelper.getFieldDateTime(EndTimeValue);

                Planner = app.MainModule.Planner;
                Planner.StartTime = StartTime;
                Planner.EndTime = EndTime;
            catch ME
                app.msgex('setPlanStartEndTime', ME);
            end
        end


        function applyCheckTimes(obj, app, ParamsApp)
            % Helper: Update Planner.CheckTimes with values from the edit fields
            % Note: Called from applyPlanParams() above
            % Note: REMOVED: Called from PlanParams.CheckTimesUpdateButtonPushed()
            app.msglog('applyCheckTimes')            
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end

            app.showPleaseWait('Updating CheckTimes...');
            try                                
                Planner = app.MainModule.Planner;
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.CheckStartTimeEditField.Value);
                EndTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.CheckEndTimeEditField.Value);

                % Call adjustCheckTimes() only if values have been changed
                if StartTime ~= Planner.CheckTimes(1) || EndTime ~= Planner.CheckTimes(2)
                    Planner.adjustCheckTimes(StartTime, EndTime);
                end
            catch ME
                app.msgex('applyCheckTimes', ME);
            end
            app.closePleaseWait();
        end        
        

        function showPlanHistory(obj, app)
            app.msglog('showPlanHistory');
            if ~app.hasPlanner(), return; end            

            % Create app
            if isempty(app.PlanHistoryApp) || ~isvalid(app.PlanHistoryApp)
                app.PlanHistoryApp = ultrasat.planner.gui.PlanHistory(app.MainModule);
            end

            % Todo - set the table
            if true
                History = app.MainModule.PlanData.history;
                Data = struct2table(History, 'AsArray', true);
                Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
                app.PlanHistoryApp.UITable.Data = Data;
                if ~isempty(Data)
                    app.PlanHistoryApp.UITable.ColumnName = Data.Properties.VariableNames;
                end
            end            

            % Show app
            app.showModal(app.PlanHistoryApp);
        end


        function updatePlanParams(obj, app)
            % Helper: Update fields in top panel of with window with values from Plan parameters
            app.msglog('updatePlanParams');
            if ~app.hasPlanner(), return; end

            % Set fields
            Planner = app.MainModule.Planner;
            app.PlanTypeEditField.Value = Planner.Type;
            app.UserNameEditField.Value = Planner.AstPlanner;
            app.PlanTitleEditField.Value = Planner.Title;
            app.StartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.StartTime);
            app.EndTimeEditField.Value = app.MainModule.DateTime2Str(Planner.EndTime);

            if app.isReadOnly()
                app.StartTimeEditField.Editable = "off";
                app.EndTimeEditField.Editable = "off";
                app.PlanTitleEditField.Editable = "off";
            else
                app.StartTimeEditField.Editable = "on";
                app.EndTimeEditField.Editable = "on";                
                app.PlanTitleEditField.Editable = "on";                
            end

            % Show message if plan was already submitted and cannot be modified
            if strcmp(Planner.Status, 'submitted')
                app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07]);
            else
                app.setTopLabel('', [], []);
            end
        end

        
        function Result = checkPlanSelfConsistency(obj, app)
            % Check plan for self consistency, update status display
            app.msglog('checkPlan')
            Result = false;
            try
                % Perform the check
                if height(app.MainModule.Planner.Plan) > 0
                    CheckStatus = app.MainModule.Planner.planSelfConsistencyCheck();
                end

                % Update display with status
                if CheckStatus
                    app.MainModule.setStatus('OK', 'self consistency: OK');
                    Result = true;
                else
                    app.MainModule.setStatus('Error', 'self consistency: issues found');
                end
            catch ME
                app.msgex('planSelfConsistencyCheck failed', ME);
                app.MainModule.setStatus('Error', sprintf('self consistency: exception: %s', ME.message));
            end            
        end
        
    end
end

