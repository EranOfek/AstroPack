%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainPlanParamsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 11/11/2025
% Description : Plan Parameters Helper for Main Planner
%==========================================================================

classdef PlannerMainPlanParamsHelper < ultrasat.api.core.Loggable
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

    methods (Access = public)

        function obj = PlannerMainPlanParamsHelper()
            % Constructor
            obj.LogPrefix = 'PlanParamsHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function showPlanParamsWindow(obj, app)
            % Show window with Plan Parameters

            app.msglog('showPlanParamsWindow');
            if ~app.hasPlanner(), return; end

            Planner = app.MainModule.Planner;

            % Create PlanParamsApp and show it
            if isempty(app.PlanParamsApp) || ~isvalid(app.PlanParamsApp)
                app.PlanParamsApp = ultrasat.planner.gui.PlanParams(app.MainModule);
            end
            app.PlanParamsApp.UIFigure.Visible = 'on';
            ParamsApp = app.PlanParamsApp;

            % Make the form read-only if plan is already built
            app.MainModule.AfterBuild = height(Planner.Plan) > 0;
            obj.setPlanParamsFields(app, ParamsApp);

            % Show app
            if strcmp(app.showModal(ParamsApp), 'Save')
                % PlanParams.mlapp calls applyPlanParams from the 'Save' button

                % Update values in the main form fields
                obj.updatePlanParams(app);
            end
        end


        function Result = applyCheckTimes(obj, app, ParamsApp)
            % Update Planner.CheckTimes with values from the edit fields

            % Note: Called from applyPlanParams() above
            % Note: REMOVED: Called from PlanParams.CheckTimesUpdateButtonPushed()
            app.msglog('applyCheckTimes');
            Result = false;
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            app.showPleaseWait('Updating CheckTimes, this may take a while. Please wait...');
            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);            
            try
                Planner = app.MainModule.Planner;

                % Get start/end times from PlanParams.mlapp dialog
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.CheckStartTimeEditField.Value);
                EndTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.CheckEndTimeEditField.Value);

                if isempty(StartTime) || isempty(EndTime)
                    app.msglog('applyCheckTimes: Invalid StartTime or EndTime');
                    AppUtils.msgError('Invalid StartTime or EndTime', 'applyCheckTimes');
                    return;
                end

                % Call adjustCheckTimes() only if values have been changed
                if StartTime ~= Planner.CheckTimes(1) || EndTime ~= Planner.CheckTimes(2)
                    app.msglog('applyCheckTimes: Adjusting CheckTimes');
                    Planner.adjustCheckTimes(StartTime, EndTime);
                    Result = true;
                else
                    app.msglog('applyCheckTimes: CheckTimes are the same');
                    Result = true;
                end
            catch ME
                app.msgex('applyCheckTimes', ME);
                AppUtils.msgError(ME.message, 'applyCheckTimes');
            end
            app.closePleaseWait();
        end


        function Result = checkPlanSelfConsistency(obj, app)
            % Check plan for self consistency, update status display

            app.msglog('checkPlan')            
            Result = false;
            if ~app.hasPlanner(), return; end
            if height(app.MainModule.Planner.Plan) == 0, return; end            

            CheckStatus = false;
            try
                % Perform the check
                [CheckStatus, BadPlanRow, BadPlanRowIndex, Message] = app.MainModule.Planner.planSelfConsistencyCheck();

                % Update display with status
                if CheckStatus
                    app.MainModule.setStatus('OK', 'self consistency: OK');
                    app.AppUtils.msgOk('Self consistency check passed OK.');
                    Result = true;
                else
                    app.MainModule.setStatus('Error', sprintf('self consistency: issues found, BadPlanRow: %d, Message: %s', BadPlanRowIndex, Message));
                    app.AppUtils.msgError(sprintf('Self consistency check failed, BadPlanRow: %d, Message: %s', BadPlanRowIndex, Message));
                end
            catch ME
                app.msgex('planSelfConsistencyCheck failed', ME);
                app.MainModule.setStatus('Error', sprintf('self consistency: exception: %s', ME.message));
            end
        end


        function Result = applyPlanParams(obj, app, ParamsApp)
            % Apply plan parameters in current planner from PlanParams app, called from showPlanParamsWindow

            app.msglog('applyPlanParams');
            Result = false;
            Planner = app.MainModule.Planner;

            % Create AppUtils instance for PlanParams app
            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);
            try
                % Apply common parameters
                if ~obj.doApplyPlanParamsCommon(app, ParamsApp)
                    return;
                end

                % Apply HCS parameters
                if strcmp(Planner.Type, 'HCS')
                    if ~obj.doApplyPlanParamsHCS(app, ParamsApp)
                        return;
                    end

                % Apply LCS parameters
                elseif strcmp(Planner.Type, 'LCS')
                    if ~obj.doApplyPlanParamsLCS(app, ParamsApp)
                        return;
                    end

                % Apply DDT parameters
                elseif strcmp(Planner.Type, 'DDT')
                    if ~obj.doApplyPlanParamsDDT(app, ParamsApp)
                        return;
                    end

                % Apply AllSky parameters
                elseif strcmp(Planner.Type, 'AllSS')
                    if ~obj.doApplyPlanParamsAllSS(app, ParamsApp)
                        return;
                    end

                % Apply TOO parameters
                elseif strcmp(Planner.Type, 'TOO')
                    if ~obj.doApplyPlanParamsTOO(app, ParamsApp)
                        return;
                    end
                end

                % Success
                Result = true;
            catch ME
                app.msgex('applyPlanParams', ME);
                AppUtils.msgError(ME.message, 'applyPlanParams');
            end
        end
        
        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

        function showPlanHistory(obj, app)
            % Show Plan History window

            app.msglog('showPlanHistory');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.PlanHistoryApp) || ~isvalid(app.PlanHistoryApp)
                app.PlanHistoryApp = ultrasat.planner.gui.PlanHistory(app.MainModule);
            end

            % Todo - set the table
            try
                History = app.MainModule.PlanData.history;
                Data = struct2table(History, 'AsArray', true);
                Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
                app.PlanHistoryApp.UITable.Data = Data;
                if ~isempty(Data)
                    app.PlanHistoryApp.UITable.ColumnName = Data.Properties.VariableNames;
                end

                % Show the history window
                app.showModal(app.PlanHistoryApp);
            catch ME
                app.msgex('showPlanHistory', ME);
            end
        end


        function updatePlanParams(obj, app)
            % Update fields in top panel of with window with values from Plan parameters

            app.msglog('updatePlanParams');
            if ~app.hasPlanner(), return; end

            try
                % Set fields
                Planner = app.MainModule.Planner;
                app.PlanTypeEditField.Value = Planner.Type;
                app.UserNameEditField.Value = Planner.AstPlanner;
                app.PlanPkEditField.Value = num2str(Planner.Pk);
                app.PlanTitleEditField.Value = Planner.Title;
                app.StartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.StartTime);
                app.EndTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.EndTime);

                % Set editability of fields based on read-only status
                if app.isEditable()
                    app.PlanTitleEditField.Editable = "on";
                else
                    app.PlanTitleEditField.Editable = "off";
                end

                % Show message if plan was already submitted and cannot be modified
                if strcmp(Planner.Status, 'submitted')
                    app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07]);
                else
                    app.setTopLabel('', [], []);
                end
            catch ME
                app.msgex('updatePlanParams failed', ME);
            end
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        % =================================================================
        %                         UTILITY HELPERS
        % =================================================================

        function setPlanParamsFields(obj, app, ParamsApp)
            % Set PlanParams app fields from current planner, called from showPlanParamsWindow

            app.msglog('setPlanParamsFields');
            try
                % Get the Planner instance from the main module
                PlanData = app.MainModule.PlanData;
                Planner = app.MainModule.Planner;

                % Assign values to UI fields
                ParamsApp.PkEditField.Value = num2str(Planner.Pk);
                ParamsApp.TitleEditField.Value = Planner.Title;
                ParamsApp.PlanTypeDropDown.Value = Planner.Type;
                ParamsApp.StartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.StartTime);
                ParamsApp.EndTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.EndTime);
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
                ParamsApp.CheckStartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str( Planner.CheckTimes(1) );
                ParamsApp.CheckEndTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str( Planner.CheckTimes(2) );

                % Assign System Parameters
                ParamsApp.SlewBufferEditField.Value = num2str(seconds(Planner.DefSlewBuffer));                
                ParamsApp.TileReadTimeEditField.Value = num2str(seconds(Planner.FullTileReadTime));
                ParamsApp.FieldOfViewRadiusEditField.Value = num2str(Planner.Rfov);               

                % ----------------------------------------------- LCS
                % Assign LCSTab Parameters, note that DailyWindowStartTime is duration
                ParamsApp.LcsDailyWindowStartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.Duration2Str(Planner.DailyWindowStartTime, true);
                ParamsApp.LcsDailyWindowMaxDurationEditField.Value = ultrasat.planner.guiutils.FormatUtils.Duration2Str(Planner.DailyWindowMaxDuration);

                % ----------------------------------------------- AllSS                
                % Assign AllSkyTab Parameters, note that DailyWindowStartTime is duration
                ParamsApp.AllSkyDailyWindowStartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.Duration2Str(Planner.DailyWindowStartTime, true);
                ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value = ultrasat.planner.guiutils.FormatUtils.Duration2Str(Planner.DailyWindowMaxDuration);
                ParamsApp.AllSkyGalacticLatThresholdEditField.Value = Planner.AllSSHighLatThresh;  % Numeric field component
                ParamsApp.AllSkyLowLatVisitsEditField.Value = Planner.LowLatVisits;  % Numeric field component
                ParamsApp.AllSkyHighLatVisitsEditField.Value = Planner.HighLatVisits;  % Numeric field component
                ParamsApp.AllSkyHighGalacticLatDitherPatternDropDown.Value = Planner.DitherPattern;

                % ----------------------------------------------- TOO
                % Assign TOOTab Parameters
                ParamsApp.TooStartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.Duration2Str(Planner.TOOStartTime);
                ParamsApp.TooWindowDurationEditField.Value = ultrasat.planner.guiutils.FormatUtils.Duration2Str(Planner.TOOWindowDuration);

                % Assign Mission Status Fields
                ParamsApp.PlanStatusEditField.Value = Planner.Status;
                ParamsApp.AstPlannerEditField.Value = Planner.AstPlanner;

                % Status text
                app.setStatusField(ParamsApp.BuildStatusEditField, PlanData.metadata.BuildStatus.Status, PlanData.metadata.BuildStatus.Status);
                app.setStatusField(ParamsApp.ValidationStatusEditField, PlanData.metadata.ValidationStatus.Status, PlanData.metadata.ValidationStatus.Status);
                app.setStatusField(ParamsApp.SubmitStatusEditField, PlanData.metadata.SubmitStatus.Status, PlanData.metadata.SubmitStatus.Status);

                % Status times
                ParamsApp.BuildTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.ScheduledTime);
                ParamsApp.ValidationTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.ValidatedTime);
                ParamsApp.SubmitTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(Planner.SubmittedTime);

                % Distance Constraints
                ParamsApp.SunMinDistObsEditField.Value = num2str(Planner.ObsSunDist);
                ParamsApp.MoonMinDistObsEditField.Value = num2str(Planner.ObsMoonDist);
                ParamsApp.EarthMinDistObsEditField.Value = num2str(Planner.ObsEarthDist);

                % Slew
                ParamsApp.SunMinDistSlewEditField.Value = num2str(Planner.SlewSunDist);
                ParamsApp.MoonMinDistSlewEditField.Value = num2str(Planner.SlewMoonDist);
                ParamsApp.EarthMinDistSlewEditField.Value = num2str(Planner.SlewEarthDist);

                % Enable buttons
                ParamsApp.SaveButton.Enable = true;
                ParamsApp.CancelButton.Enable = true;
            catch ME
                app.msgex('setPlanParamsFields', ME);
            end
        end


        function Result = setPlanStartEndTime(obj, app, StartTimeValue, EndTimeValue, ParamsApp)
            % Set Plan Start and End times in current planner

            app.msglog(sprintf('setPlanStartEndTime: StartTimeValue=%s, EndTimeValue=%s', StartTimeValue, EndTimeValue));
            Result = false;
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);            
            try
                % Convert strings to datetime
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(StartTimeValue);
                EndTime = app.MainModule.GuiHelper.getFieldDateTime(EndTimeValue);
                if isempty(StartTime) || isempty(EndTime)
                    app.msglog('setPlanStartEndTime: Invalid StartTime or EndTime');
                    AppUtils.msgError('Invalid StartTime or EndTime', 'setPlanStartEndTime');
                    return;
                end

                Planner = app.MainModule.Planner;
                Planner.StartTime = StartTime;
                Planner.EndTime = EndTime;
                Result = true;
            catch ME
                app.msgex('setPlanStartEndTime', ME);
                AppUtils.msgError(ME.message, 'setPlanStartEndTime');
            end
        end

        % =================================================================
        %               APPLY PLANNER PARAMETERS HELPERS
        % =================================================================

        function Result = doApplyPlanParamsCommon(obj, app, ParamsApp)
            % Apply plan parameters for all plan types

            app.msglog('doApplyPlanParamsAll');
            Result = false;
            Planner = app.MainModule.Planner;            
            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);
            try

                % --------------------------------- Plan Type (char)
                if strcmp(ParamsApp.PlanTypeDropDown.Enable, "on")
                    NewType = char(ParamsApp.PlanTypeDropDown.Value);
                    if ~strcmp(NewType, Planner.Type)
                        app.msglog(sprintf('applyPlanParams: Changing Plan Type from %s to %s', Planner.Type, NewType));
                        Planner.Type = NewType;
                    end                    
                end

                % --------------------------------- Title (char)
                NewTitle = strtrim(ParamsApp.TitleEditField.Value);
                if isempty(NewTitle)
                    app.msglog('applyPlanParams: Invalid Title');
                    AppUtils.msgError('Title cannot be empty', 'doApplyPlanParamsCommon');
                    return;
                end
                if ~strcmp(NewTitle, Planner.Title)
                    app.msglog(sprintf('applyPlanParams: Changing Title from %s to %s', Planner.Title, NewTitle));
                    Planner.Title = NewTitle;
                end

                % --------------------------------- Start & End times
                TmpResult = obj.setPlanStartEndTime(app, ParamsApp.StartTimeEditField.Value, ParamsApp.EndTimeEditField.Value, ParamsApp);
                if ~TmpResult, return; end

                % --------------------------------- EpochsPerVisit (int)
                NewEpochsPerVisit = app.MainModule.GuiHelper.getFieldNum(ParamsApp.EpochsPerVisitEditField.Value);
                if isnan(NewEpochsPerVisit) || NewEpochsPerVisit <= 1 || NewEpochsPerVisit > 60
                    app.msglog('applyPlanParams: Invalid EpochsPerVisit');
                    AppUtils.msgError('EpochsPerVisit must be a number between 1 and 60', 'doApplyPlanParamsCommon');
                    return;
                end
                if NewEpochsPerVisit ~= Planner.DefEpochsPerVisit
                    app.msglog(sprintf('applyPlanParams: Changing EpochsPerVisit from %d to %d', Planner.DefEpochsPerVisit, NewEpochsPerVisit));
                    Planner.DefEpochsPerVisit = NewEpochsPerVisit;
                end

                % --------------------------------- Exposure Time (seconds)
                NewExposureTime = app.MainModule.GuiHelper.getFieldNum(ParamsApp.ExposureEditField.Value);
                if isnan(NewExposureTime) || NewExposureTime <= 30 || NewExposureTime > 600
                    app.msglog('applyPlanParams: Invalid Exposure Time');
                    AppUtils.msgError('Exposure Time must be a number between 30 and 600', 'doApplyPlanParamsCommon');
                    return;
                end
                if seconds(NewExposureTime) ~= Planner.Exptime
                    app.msglog(sprintf('applyPlanParams: Changing Exposure Time from %s to %s', char(Planner.Exptime), char(seconds(NewExposureTime)) ));
                    Planner.Exptime = seconds(NewExposureTime);
                end

                % --------------------------------- Tiles
                tileNumbers = '1234';
                checkBoxes = [ParamsApp.Tile1CheckBox, ParamsApp.Tile2CheckBox, ParamsApp.Tile3CheckBox, ParamsApp.Tile4CheckBox];
                
                selectedTiles = '';  % must be char, not numeric!
                for i = 1:numel(tileNumbers)
                    if checkBoxes(i).Value
                        selectedTiles(end+1) = tileNumbers(i); %#ok<AGROW>
                    end
                end
                if ~strcmp(selectedTiles, Planner.Tiles)
                    app.msglog(sprintf('applyPlanParams: Changing Tiles from %s to %s', Planner.Tiles, selectedTiles));
                    Planner.Tiles = selectedTiles;
                end

                % --------------------------------- Check times
                tmpResult = obj.applyCheckTimes(app, ParamsApp);
                if ~tmpResult, return; end

                % --------------------------------- DefSlewBuffer (seconds)
                NewDefSlewBuffer = app.MainModule.GuiHelper.getFieldNum(ParamsApp.SlewBufferEditField.Value);
                if isnan(NewDefSlewBuffer) || NewDefSlewBuffer <= 0 || NewDefSlewBuffer > 60
                    app.msglog('applyPlanParams: Invalid DefSlewBuffer');
                    AppUtils.msgError('DefSlewBuffer must be a number between 0 and 60', 'doApplyPlanParamsCommon');
                    return;
                end
                if seconds(NewDefSlewBuffer) ~= Planner.DefSlewBuffer
                    app.msglog(sprintf('applyPlanParams: Changing DefSlewBuffer from %s to %s', char(Planner.DefSlewBuffer), char(seconds(NewDefSlewBuffer)) ));
                    Planner.DefSlewBuffer = seconds(NewDefSlewBuffer);
                end

                % --------------------------------- FullTileReadTime (seconds)
                NewFullTileReadTime = app.MainModule.GuiHelper.getFieldNum(ParamsApp.TileReadTimeEditField.Value);
                if isnan(NewFullTileReadTime) || NewFullTileReadTime <= 1 || NewFullTileReadTime > 60
                    app.msglog('applyPlanParams: Invalid FullTileReadTime');
                    AppUtils.msgError('FullTileReadTime must be a number between 1 and 60', 'doApplyPlanParamsCommon');
                    return;
                end
                if seconds(NewFullTileReadTime) ~= Planner.FullTileReadTime
                    app.msglog(sprintf('applyPlanParams: Changing FullTileReadTime from %s to %s', char(Planner.FullTileReadTime), char(seconds(NewFullTileReadTime)) ));
                    Planner.FullTileReadTime = seconds(NewFullTileReadTime);
                end

                % --------------------------------- Rfov (degrees)
                NewRfov = app.MainModule.GuiHelper.getFieldNum(ParamsApp.FieldOfViewRadiusEditField.Value);
                if isnan(NewRfov) || NewRfov <= 0 || NewRfov > 180
                    app.msglog('applyPlanParams: Invalid Rfov');
                    AppUtils.msgError('Rfov must be a number between 0 and 180', 'doApplyPlanParamsCommon');
                    return;
                end
                if NewRfov ~= Planner.Rfov
                    app.msglog(sprintf('applyPlanParams: Changing Rfov from %d to %d', Planner.Rfov, NewRfov));
                    Planner.Rfov = NewRfov;
                end

                % @Future: Apply more system constants from ParamsApp

                % Success
                Result = true;
            catch ME
                app.msgex('applyPlanParams', ME);
                AppUtils.msgError(ME.message, 'doApplyPlanParamsCommon');
            end
        end

        % ======================================================= HCS        

        function Result = doApplyPlanParamsHCS(obj, app, ParamsApp)
            % Apply plan parameters for HCS

            app.msglog('doAplyPlanParamsHCS - Nothing to apply for HCS');
            Result = true;
        end

        % ======================================================= LCS        

        function Result = doApplyPlanParamsLCS(obj, app, ParamsApp)
            % Apply plan parameters for LCS

            app.msglog('doApplyPlanParamsLCS');
            Result = false;
            Planner = app.MainModule.Planner;            
            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);
            try
                % --------------------------------- DailyWindowStartTime (duration)
                NewDailyWindowStartTime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.LcsDailyWindowStartTimeEditField.Value);
                if isempty(NewDailyWindowStartTime)
                    app.msglog('applyPlanParams: Invalid DailyWindowStartTime');
                    AppUtils.msgError('DailyWindowStartTime cannot be empty', 'doApplyPlanParamsLCS');
                    return;
                end
                if NewDailyWindowStartTime ~= Planner.DailyWindowStartTime
                    app.msglog(sprintf('applyPlanParams: Changing DailyWindowStartTime from %s to %s', Planner.DailyWindowStartTime, NewDailyWindowStartTime));
                    Planner.DailyWindowStartTime = NewDailyWindowStartTime; 
                end 

                % --------------------------------- DailyWindowMaxDuration
                NewDailyWindowMaxDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.LcsDailyWindowMaxDurationEditField.Value);
                if isempty(NewDailyWindowMaxDuration)
                    app.msglog('applyPlanParams: Invalid DailyWindowMaxDuration');
                    AppUtils.msgError('DailyWindowMaxDuration must be a number between 0 and 24', 'doApplyPlanParamsLCS');
                    return;
                end
                if NewDailyWindowMaxDuration ~= Planner.DailyWindowMaxDuration
                    app.msglog(sprintf('applyPlanParams: Changing DailyWindowMaxDuration from %s to %s', Planner.DailyWindowMaxDuration, NewDailyWindowMaxDuration));
                    Planner.DailyWindowMaxDuration = NewDailyWindowMaxDuration;
                end

                % Success
                Result = true;
            catch ME
                app.msgex('applyPlanParams', ME);
                AppUtils.msgError(ME.message, 'doApplyPlanParamsLCS');
            end
        end

        % ======================================================= DDT        

        function Result = doApplyPlanParamsDDT(obj, app, ParamsApp)
            % Apply plan parameters in current planner from PlanParams app, called from showPlanParamsWindow

            app.msglog('doAplyPlanParamsDDT - Nothing to apply for DDT');
            Result = true;
        end

        % ======================================================= AllSS        

        function Result = doApplyPlanParamsAllSS(obj, app, ParamsApp)
            % Apply plan parameters for AllSS

            app.msglog('doApplyPlanParamsAllSS');
            Result = false;
            Planner = app.MainModule.Planner;            
            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);
            try
                % --------------------------------- DailyWindowStartTime (datetime) 
                NewDailyWindowStartTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.AllSkyDailyWindowStartTimeEditField.Value);
                if isempty(NewDailyWindowStartTime)
                    app.msglog('applyPlanParams: Invalid DailyWindowStartTime');
                    AppUtils.msgError('DailyWindowStartTime cannot be empty', 'doApplyPlanParamsAllSS');
                    return;
                end
                if NewDailyWindowStartTime ~= Planner.DailyWindowStartTime
                    app.msglog(sprintf('applyPlanParams: Changing DailyWindowStartTime from %s to %s', Planner.DailyWindowStartTime, NewDailyWindowStartTime));
                    Planner.DailyWindowStartTime = NewDailyWindowStartTime;
                end

                % --------------------------------- DailyWindowMaxDuration (duration)
                NewDailyWindowMaxDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value);
                if isempty(NewDailyWindowMaxDuration)
                    app.msglog('applyPlanParams: Invalid DailyWindowMaxDuration');
                    AppUtils.msgError('DailyWindowMaxDuration cannot be empty', 'doApplyPlanParamsAllSS');
                    return;
                end
                if NewDailyWindowMaxDuration ~= Planner.DailyWindowMaxDuration
                    app.msglog(sprintf('applyPlanParams: Changing DailyWindowMaxDuration from %s to %s', Planner.DailyWindowMaxDuration, NewDailyWindowMaxDuration));
                    Planner.DailyWindowMaxDuration = NewDailyWindowMaxDuration;
                end

                % --------------------------------- AllSSHighLatThresh (degrees)
                NewAllSSHighLatThresh = app.MainModule.GuiHelper.getFieldNum(ParamsApp.AllSkyGalacticLatThresholdEditField.Value);
                if isnan(NewAllSSHighLatThresh) || NewAllSSHighLatThresh <= 0 || NewAllSSHighLatThresh > 180
                    app.msglog('applyPlanParams: Invalid AllSSHighLatThresh');
                    AppUtils.msgError('AllSSHighLatThresh must be a number between 0 and 180', 'doApplyPlanParamsAllSS');
                    return;
                end
                if NewAllSSHighLatThresh ~= Planner.AllSSHighLatThresh
                    app.msglog(sprintf('applyPlanParams: Changing AllSSHighLatThresh from %d to %d', Planner.AllSSHighLatThresh, NewAllSSHighLatThresh));
                    Planner.AllSSHighLatThresh = NewAllSSHighLatThresh;
                end

                % --------------------------------- LowLatVisits (number)
                NewLowLatVisits = app.MainModule.GuiHelper.getFieldNum(ParamsApp.AllSkyLowLatVisitsEditField.Value);
                if isnan(NewLowLatVisits) || NewLowLatVisits <= 0 || NewLowLatVisits > 100
                    app.msglog('applyPlanParams: Invalid LowLatVisits');
                    AppUtils.msgError('LowLatVisits must be a number between 0 and 100', 'doApplyPlanParamsAllSS');
                    return;
                end
                if NewLowLatVisits ~= Planner.LowLatVisits
                    app.msglog(sprintf('applyPlanParams: Changing LowLatVisits from %d to %d', Planner.LowLatVisits, NewLowLatVisits));
                    Planner.LowLatVisits = NewLowLatVisits;
                end

                % --------------------------------- HighLatVisits (number)
                NewHighLatVisits = app.MainModule.GuiHelper.getFieldNum(ParamsApp.AllSkyLowLatVisitsEditField.Value);
                if isnan(NewHighLatVisits) || NewHighLatVisits <= 0 || NewHighLatVisits > 100
                    app.msglog('applyPlanParams: Invalid HighLatVisits');
                    AppUtils.msgError('HighLatVisits must be a number between 0 and 100', 'doApplyPlanParamsAllSS');
                    return;
                end
                if NewHighLatVisits ~= Planner.HighLatVisits
                    app.msglog(sprintf('applyPlanParams: Changing HighLatVisits from %d to %d', Planner.HighLatVisits, NewHighLatVisits));
                    Planner.HighLatVisits = NewHighLatVisits;
                end
           

                % --------------------------------- LowLatVisits (number)
                NewLowLatVisits = app.MainModule.GuiHelper.getFieldNum(ParamsApp.AllSkyLatVisitsEditField.Value);
                if isnan(NewLowLatVisits) || NewLowLatVisits <= 0 || NewLowLatVisits > 100
                    app.msglog('applyPlanParams: Invalid LowLatVisits');
                    app.AppUtils.msgError('LowLatVisits must be a number between 0 and 100');
                    return;
                end
                if NewLowLatVisits ~= Planner.LowLatVisits
                    app.msglog(sprintf('applyPlanParams: Changing LowLatVisits from %d to %d', Planner.LowLatVisits, NewLowLatVisits));
                    Planner.LowLatVisits = NewLowLatVisits;
                end

                % --------------------------------- HighLatVisits (number)
                NewHighLatVisits = app.MainModule.GuiHelper.getFieldNum(ParamsApp.AllSkyLowLatVisitsEditField.Value);
                if isnan(NewHighLatVisits) || NewHighLatVisits <= 0 || NewHighLatVisits > 100
                    app.msglog('applyPlanParams: Invalid HighLatVisits');
                    app.AppUtils.msgError('HighLatVisits must be a number between 0 and 100');
                    return;
                end
                if NewHighLatVisits ~= Planner.HighLatVisits
                    app.msglog(sprintf('applyPlanParams: Changing HighLatVisits from %d to %d', Planner.HighLatVisits, NewHighLatVisits));
                    Planner.HighLatVisits = NewHighLatVisits;
                end

                % Success
                Result = true;
            catch ME
                app.msgex('applyPlanParams', ME);
                AppUtils.msgError(ME.message);
            end
        end
        
        % ======================================================= TOO

        function Result = doApplyPlanParamsTOO(obj, app, ParamsApp)
            % Apply plan parameters in current planner from PlanParams app, called from showPlanParamsWindow

            app.msglog('doApplyPlanParamsTOO');
            Result = false;
            Planner = app.MainModule.Planner;            
            AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, ParamsApp);
            try
                % --------------------------------- TOOStartTime (duration)
                NewTOOStartTime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.TooStartTimeEditField.Value);
                if isempty(NewTOOStartTime)
                    app.msglog('applyPlanParams: Invalid TOOStartTime');
                    AppUtils.msgError('TOOStartTime cannot be empty', 'doApplyPlanParamsTOO');
                    return;
                end
                if NewTOOStartTime ~= Planner.TOOStartTime
                    app.msglog(sprintf('applyPlanParams: Changing TOOStartTime from %s to %s', Planner.TOOStartTime, NewTOOStartTime));
                    Planner.TOOStartTime = NewTOOStartTime;
                end

                % --------------------------------- TOOWindowDuration (duration)
                NewTOOWindowDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.TooWindowDurationEditField.Value);
                if isempty(NewTOOWindowDuration)
                    app.msglog('applyPlanParams: Invalid TOOWindowDuration');
                    AppUtils.msgError('TOOWindowDuration cannot be empty', 'doApplyPlanParamsTOO');
                    return;
                end
                if NewTOOWindowDuration ~= Planner.TOOWindowDuration
                    app.msglog(sprintf('applyPlanParams: Changing TOOWindowDuration from %s to %s', Planner.TOOWindowDuration, NewTOOWindowDuration));
                    Planner.TOOWindowDuration = NewTOOWindowDuration;
                end

                % Success
                Result = true;
            catch ME
                app.msgex('applyPlanParams', ME);
                AppUtils.msgError(ME.message, 'doApplyPlanParamsTOO');
            end
        end

    end

end

