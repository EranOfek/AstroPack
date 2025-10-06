%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/GuiHelper.m
% Author:  Chen Tishler
% Created: 07/01/2025
% Updated: 06/10/2025
% Title:   
%==========================================================================

classdef PlannerMainNewPlanHelper < ultrasat.api.Loggable

     < ultrasat.api.Loggable
    % This class serves like a DataModule in Delphi.
    
    properties  
    end
    
    % =====================================================================
    %             Create New Plan - HCS, LCS, DDT, AllSS, ToO
    % =====================================================================    
    methods

        function obj = PlannerMainNewPlanHelper()
            % Constructor
            obj.msglog('PlannerMainNewPlanHelper created successfully');
        end
        

        function createNewPlan(obj, app)
            % Create new plan
            app.msglog('createNewPlan');            

            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('Your changes are not saved. Do you want to discard them and create a new plan?', 'Save or discard'), 'Yes')
                    return;
                end
            end

            % Close existing plan
            app.closePlan();

            % Create app
            if isempty(app.NewPlanApp) || ~isvalid(app.NewPlanApp)
                app.NewPlanApp = ultrasat.planner.gui.NewPlan(app.MainModule);                
            end

            % Set PlannerName field value
            if app.isLogin()
                app.NewPlanApp.PlannerNameEditField.Value = app.MainModule.UserName;
                app.NewPlanApp.PlannerNameEditField.Enable = false;
            else
                app.NewPlanApp.PlannerNameEditField.Value = '';
                app.NewPlanApp.PlannerNameEditField.Enable = true;
            end

            if ~strcmp(app.showModal(app.NewPlanApp), 'Create')
                return;
            end

            app.msglog(sprintf('New plan type: %s ....', app.MainModule.PlanType));
            try
                obj.doCreateNewPlan(app);
            catch ME
                app.msgex('createNewPlan', ME);
            end

            %
            app.setButtons();
        end


        function doCreateNewPlan(obj, app)

            % Create new plan according to parameters in app.NewPlanApp
            PlanType = app.NewPlanApp.PlanType;
            app.msglog(sprintf('doCreateNewPlan: PlanType: %s', PlanType));
            
            % Create new PlanData instance
            app.MainModule.createPlanData();

            % Call the designated function according to PlanType
            if strcmp(PlanType, 'HCS')
                obj.doCreateNewPlanHCS(app);
            elseif strcmp(PlanType, 'LCS')
                obj.doCreateNewPlanLCS(app);
            elseif strcmp(PlanType, 'DDT')
                obj.doCreateNewPlanDDT(app);                
            elseif strcmp(PlanType, 'AllSS')
                obj.doCreateNewPlanAllSS(app);
            elseif strcmp(PlanType, 'TOO')
                obj.doCreateNewPlanTOO(app);
            else
                app.msglog(sprintf('doCreateNewPlan: Unknown PlanType: %s', PlanType));
            end

            % Update data and references
            app.MainModule.PlanData.planner = app.MainModule.Planner;
            app.MainModule.AfterBuild = false;

            % Update GUI
            app.SaveButton.Enable = 'off';
            if strcmp(PlanType, 'DDT')            
                app.BuildButton.Text = 'Add';
            else
                app.BuildButton.Text = 'Build';
            end

            %
            app.setModified('doCreateNewPlan');
            app.showUniqueTargets();
            app.showPlanTargets();
            app.setStatus('OK', 'New plan created successfully');
            app.msglog('doCreateNewPlan done');
        end


        function doCreateNewPlanHCS(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanHCS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance            
            upHCS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'HCS', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upHCS);

            app.MainModule.setPlanner(upHCS);
            app.setModified('doCreateNewPlanHCS');
            app.updatePlanParams();
            %app.debugSave('upHCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanHCS done');
        end


        function doCreateNewPlanLCS(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanLCS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName();
            
            % Create new uplanner instance
            upLCS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'LCS', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upLCS);

            app.MainModule.setPlanner(upLCS);
            app.setModified('doCreateNewPlanLCS');
            app.updatePlanParams();
            %app.debugSave('upLCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanLCS done');
        end


        function doCreateNewPlanDDT(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanDDT started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName();            

            % Create new uplanner instance            
            upDDT = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'DDT', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upDDT);

            app.MainModule.setPlanner(upDDT);
            app.setModified('doCreateNewPlanDDT');
            app.updatePlanParams();
            %app.debugSave('upDDT.mat', 'app.MainModule.Planner');
            app.msglog('doCreateNewPlanDDT done');
        end


        function doCreateNewPlanTOO(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanTOO started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName();            

            % Create new uplanner instance            
            upTOO = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'TOO', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upHCS);            

            app.MainModule.setPlanner(upTOO);
            app.setModified('doCreateNewPlanDDT');
            app.updatePlanParams();
            %app.debugSave('upTOO.mat', 'app.MainModule.Planner');
            app.msglog('doCreateNewPlanTOO done');
        end


        function doCreateNewPlanAllSS(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanAllSS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance            
            upAllSS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'AllSS', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upHCS);

            app.MainModule.setPlanner(upAllSS);
            app.setModified('doCreateNewPlanDDT');
            app.updatePlanParams();
            %app.debugSave('upLCS.mat', 'app.MainModule.Planner');
            app.msglog('doCreateNewPlanAllSS done');
        end        


        function UserName = getNewPlanUserName(obj, app)
            % Helper: Get logged-in user name, or user name entered in NewPlanApp dialog            
            if app.isLogin()
                UserName = app.MainModule.UserName;
            else
                UserName = app.NewPlanApp.PlannerNameEditField.Value;
            end            
        end


        function setNewPlanDataFromCreateDialog(obj, app, Planner)
            % Helper: Set planner data from the create dialog: PlanTitle, StartTime, EndTime
            PlanTitle = app.MainModule.GuiHelper.getFieldTitle( app.NewPlanApp.TitleEditField.Value );
            StartTime = app.MainModule.GuiHelper.getFieldDateTime( app.NewPlanApp.StartTimeEditField.Value );
            EndTime = app.MainModule.GuiHelper.getFieldDateTime( app.NewPlanApp.EndTimeEditField.Value );            

            Planner.Title = PlanTitle;
            Planner.StartTime = StartTime;
            Planner.EndTime = EndTime;            
        end

    end

end

