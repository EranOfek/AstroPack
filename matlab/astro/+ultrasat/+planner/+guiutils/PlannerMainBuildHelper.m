%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainBuildHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 06/10/2025
% Description : Build Helper for Main Planner
%==========================================================================

classdef PlannerMainBuildHelper < ultrasat.api.Loggable
    
    methods

        function obj = PlannerMainBuildHelper()
            % Constructor
            obj.LogPrefix = 'BuildHelper';
            obj.msglog('PlannerMainBuildHelper created successfully');
        end


        function build(obj, app)
            % Build plan according to plan type, calls doBuild...() below
            app.msglog('build');            
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end            

            %
            app.MainModule.AfterBuild = height(app.MainModule.Planner.Plan) > 0;
            if app.MainModule.AfterBuild
                if ~strcmp(app.AppUtils.askYesNo('Build was already executed, this will override you existing plan. Are you sure you want to execute build?', 'Confirm'), 'Yes')
                    return;
                end
            end

            app.showPleaseWait('Building your plan. This may take a while. Please wait....');
            try
                PlanType = app.MainModule.PlanType;
                app.MainModule.clearStatus();
                app.updateStatus();
                app.msglog(sprintf('build: PlanType: %s', PlanType));

                if strcmp(PlanType, 'HCS')
                    obj.doBuildHCS(app);
                elseif strcmp(PlanType, 'LCS')
                    obj.doBuildLCS(app);
                elseif strcmp(PlanType, 'DDT')
                    obj.doBuildDDT(app);
                elseif strcmp(PlanType, 'TOO')
                    obj.doBuildTOO(app);
                elseif strcmp(PlanType, 'AllSS')
                    obj.doBuildAllSS(app);
                end                    

                % Set AfterBuild=true for all plan types except DDT
                if ~strcmp(PlanType, 'DDT') && ~isempty(app.MainModule.Planner.Plan)
                    app.MainModule.AfterBuild = true;
                end
            catch ME
                app.MainModule.setStatus('Error', ME.message);
                app.msgex('build', ME);
            end

            % Close the "Please Wait" dialog
            app.closePleaseWait();

            % Update display
            app.setModified('build');  % Move call to other place?
            app.updateStatus();
            app.showPlanTargets();                                
        end


        function setBuildStatus(obj, app, Status)
            app.MainModule.PlanData.setStatus('BuildStatus', Status);
        end


        function doBuildHCS(obj, app)
            % Helper: Build HCS
            app.msglog('doBuildHCS started');
            if ~app.hasPlanner(), return; end
            
            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = obj.getUniqueTargetsIndexByOrderColumn(app, app.UITableUniqueTargets.Data);
            if numel(SelectedRows) ~= 1
                app.AppUtils.msgError('HCS requires single unique target');
                return;
            end

            upHCS = app.MainModule.Planner;            
            upHCS.buildHCS('HCS_UniqTarg', SelectedRows);
            app.addHistory('BuildHCS Ok');
            app.setBuildStatus('OK');
            app.MainModule.setStatus('OK', 'Build: self consistency: OK');
            %app.debugSave('upHCS.mat', upHCS);
            app.msglog('doBuildHCS done');
        end


        function doBuildLCS(obj, app)
            % Helper: Build LCS
            app.msglog('doBuildLCS started');
            if ~app.hasPlanner(), return; end

            upLCS = app.MainModule.Planner;

            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = obj.getUniqueTargetsIndexByOrderColumn(app, app.UITableUniqueTargets.Data);
            upLCS.buildLCS('TargetList', SelectedRows);
          
            app.addHistory('BuildLCS Ok');
            app.setBuildStatus('OK');
            %app.debugSave('upLCS.mat', upLCS);
            app.msglog('doBuildLCS done');
        end


        function doBuildDDT(obj, app)
            % Helper: Build DDT
            app.msglog('doBuildDDT started');
            if ~app.hasPlanner(), return; end

            upDDT = app.MainModule.Planner;
            
            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = obj.getUniqueTargetsIndexByOrderColumn(app, app.UITableUniqueTargets.Data);
            if isempty(SelectedRows)
                return;
            end

            % Create app
            if isempty(app.EnterStartTimeApp) || ~isvalid(app.EnterStartTimeApp)
                app.EnterStartTimeApp = ultrasat.planner.gui.EnterStartTime(app.MainModule);
            end            

            % Set start time field from the planner
            app.EnterStartTimeApp.GroupStartTimeEditField.Value = app.MainModule.DateTime2Str(app.MainModule.Planner.StartTime);

            % Extract the selected Unique Targets and show them in the dialog
            SelectedData = app.UITableUniqueTargets.Data(SelectedRows, :);
            Data = SelectedData(:, {'Order', 'Name', 'RA', 'Dec'});
            app.EnterStartTimeApp.UITable.Data = Data;
            if ~isempty(Data)
                app.EnterStartTimeApp.UITable.ColumnName = Data.Properties.VariableNames;
            end

            % Generate group number from current plan
            if ~isempty(upDDT.Plan.Group)
                Group = max(upDDT.Plan.Group) + 1;
            else
                Group = 1;
            end
            app.EnterStartTimeApp.GroupEditField.Value = num2str(Group);

            % Show app
            if strcmp(app.showModal(app.EnterStartTimeApp), 'OK')

                % Get start time
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(app.EnterStartTimeApp.GroupStartTimeEditField.Value);
                app.msglog(sprintf('doBuildDDT: StartTime: %s ....', StartTime));
                try
                    % This is the actual 'build' of DDT
                    upDDT.addDDT2Plan(SelectedRows, StartTime, 'Group', Group);
                    app.addHistory('addDDT2Plan Ok');
                    app.setStatus('OK', 'build: addDDT2Plan successfully');
                catch ME
                    app.msgex('addDDT2Plan', ME);
                end
            end

            %app.debugSave('upDDT.mat', upDDT);
            app.msglog('doBuildDDT done');
        end


        function doBuildTOO(app)
            % Helper: Build TOO - @Todo @Yossi
            app.msglog('doBuildTOO started');
            if ~app.hasPlanner(), return; end

            try
                upTOO = app.MainModule.Planner;
    
                Fields = upTOO.UniqTarg(1);
                upTOO.buildTOO('RA', Fields.RA, 'Dec', Fields.Dec, 'Name', HCS_fields.Name);   
                %app.debugSave('upTOO.mat', upTOO);

            catch ME
                app.msgex('doBuildTOO', ME);
            end                
            app.msglog('doBuildTOO done');
        end


        function doBuildAllSS(obj, app)
            % Helper: Build AllSS - @Todo @Yossi
            app.msglog('doBuildAllSS started');
            if ~app.hasPlanner(), return; end

            try

            catch ME
                app.msgex('doBuildAllSS', ME);
            end                

            app.msglog('doBuildAllSS done');
        end        
        

        function showBuildStatusWindow(obj, app)
            % Show window with last build status
            app.msglog('showBuildStatusWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.BuildStatusApp) || ~isvalid(app.BuildStatusApp)
                app.BuildStatusApp = ultrasat.planner.gui.BuildStatus(app.MainModule);
            end

            % Set fields and show the app
            %app.BuildStatusApp.setData(app.MainModule.BuildStatus);
            app.showModal(app.BuildStatusApp);
        end


        function Result = getUniqueTargetsIndexByOrderColumn(obj, app, Data)
            % Returns the row indices sorted by 'Order' column.
            % If only one row exists, returns 1.
            % If only one row has a non-empty 'Order' value, returns its index.
            % Otherwise, returns indices of rows with non-empty 'Order', sorted by value.
        
            try
                % If only one row in the table, return index 1
                if height(Data) == 1
                    Result = 1;
                    return;
                end
            
                % Convert to string array for uniform processing
                OrderColumn = string(Data.Order);
            
                % Identify non-empty rows (ignoring whitespace and empty strings)
                trimmedOrder = strtrim(OrderColumn);
                isValid = ~(trimmedOrder == "" | trimmedOrder == " ");
                
                % If only one valid row with non-empty 'Order', return its index
                if sum(isValid) == 1
                    Result = find(isValid);
                    return;
                end
            
                % Handle case: all values are invalid or non-numeric
                validNumbers = str2double(trimmedOrder(isValid));
                if all(isnan(validNumbers))
                    OrderColumn = string(1:height(Data))';
                    trimmedOrder = OrderColumn;
                    isValid = true(height(Data), 1);
                end
                
                % Now safely convert all to numbers, keeping invalid as NaN
                numericOrder = NaN(height(Data), 1);
                numericOrder(isValid) = str2double(trimmedOrder(isValid));
                
                % Get non-empty rows and sort
                nonEmptyRows = find(~isnan(numericOrder));
                [~, sortedIdx] = sort(numericOrder(nonEmptyRows));
                Result = nonEmptyRows(sortedIdx)';          
            catch ME
                app.msgex('getUniqueTargetsIndexByOrderColumn', ME);
                Result = 1;
            end                                
        end


        function Result = getUniqueTargetsIndexByOrderColumn0(obj, app, Data)
            % Extract row indices for non-empty 'Order' values, sorted by 'Order'.
            % If only one row exists, returns 1.
            % If only one row has a non-empty 'Order' value, returns its index.
            % Otherwise, returns indices of rows with non-empty 'Order', sorted by value.
            
            % If only one row in the table, return index 1
            if height(Data) == 1
                Result = 1;
                return;
            end

            % Check if all values in 'Order' column are empty and replace with row numbers if needed
            if all(cellfun(@(x) isempty(strtrim(x)), Data.Order)) || all(isnan(str2double(Data.Order)))
                Data.Order = string(1:height(Data))'; 
            end            

            % Convert to cell array if necessary (handles both strings and chars)
            if iscell(Data.Order) || isstring(Data.Order)
                % Trim whitespace and convert empty strings to NaN for filtering
                trimmedOrder = strtrim(Data.Order);
                isValid = ~strcmp(trimmedOrder, "") & ~strcmp(trimmedOrder, " ");  % Check for truly empty strings
                Data.Order(~isValid) = NaN;  % Replace empty strings with NaN
                Data.Order = str2double(Data.Order); % Convert valid numeric strings to doubles
            end
        
            % Find non-empty (non-NaN) rows
            nonEmptyRows = find(~isnan(Data.Order));
        
            % Sort by 'Order' column
            [~, sortedIdx] = sort(Data.Order(nonEmptyRows));
        
            % Return sorted row indices
            Result = nonEmptyRows(sortedIdx);
            Result = Result';
        end
        
    end
end
