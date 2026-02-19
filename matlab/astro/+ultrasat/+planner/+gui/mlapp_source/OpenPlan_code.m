classdef OpenPlan < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                  matlab.ui.Figure
        FilterPanel               matlab.ui.container.Panel
        PlanTitleEditField        matlab.ui.control.EditField
        PlanTitleEditFieldLabel   matlab.ui.control.Label
        SearchButton              matlab.ui.control.Button
        EndTimeEditField          matlab.ui.control.EditField
        EndTimeEditFieldLabel     matlab.ui.control.Label
        StartTimeEditField        matlab.ui.control.EditField
        StartTimeEditFieldLabel   matlab.ui.control.Label
        ClearButton               matlab.ui.control.Button
        Panel_3                   matlab.ui.container.Panel
        UITable                   matlab.ui.control.Table
        Panel_2                   matlab.ui.container.Panel
        OpenObservationPlanLabel  matlab.ui.control.Label
        Panel                     matlab.ui.container.Panel
        HelpButton                matlab.ui.control.Button
        CancelButton              matlab.ui.control.Button
        OpenButton                matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % OpenPlan App
            %
            % This app allows users to browse and open observation plans stored in the system.
            % Users can filter plans based on time range and title, and select a plan to open.
            % The actual open operation is done in PlannerMain.openPlan()
            %
            % Features:
            % - Displays a list of observation plans in a table.
            % - Filters plans by start time, end time, and title substring.
            % - Allows users to search, clear filters, and select a plan.
            % - Retrieves plan data from the main application module.
        end
    end    


    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation ('Open' or 'Cancel')
        Pk              % Primary key of the selected plan
    end


    methods (Access = public)

        function beforeShow(app)
            % Initializes the table and loads the initial list of observation plans.
            %
            % - Sets the selection mode to single-row.
            % - Calls getList() to populate the table.            
            app.UITable.SelectionType = "row";
            app.UITable.Multiselect = "off";            
            app.UITable.RowName = "numbered";

            % Load the initial list
            app.getList();
        end
        

        function getList(app)
            % Retrieves a filtered list of observation plans from the API.
            %
            % - Reads filter parameters from the UI fields.
            % - Sends a request to the API client to fetch plans.
            % - Updates the table with the retrieved plans or clears it if no results are found.
            % - Displays an alert if the request fails.

            % Get filters value
            start_time = app.StartTimeEditField.Value;
            end_time = app.EndTimeEditField.Value;
            title_subtext = app.PlanTitleEditField.Value;
            
            % Convert empty fields to [] so API gets empty values if not provided
            if isempty(start_time)
                start_time = [];
            end
            if isempty(end_time)
                end_time = [];
            end
            if isempty(title_subtext)
                title_subtext = [];
            end
        
            % Fetch the plans list from API
            try
                response = app.PlannerMainStorageHelper.getPlansList(start_time, end_time, title_subtext);            
            catch ME
                uialert(app.UIFigure, sprintf('Failed to retrieve plans list: %s', ME.message), 'Error');
                return;
            end

            if ~response.ok
                % @Todo Show alert (use msgbox or uialert)
                uialert(app.UIFigure, 'Failed to retrieve plans list', 'Error');
                return;
            end
            
            % Convert struct array to table if not empty
            if ~isempty(response.plans)
                %Data = struct2table(response.plans);
                Data = app.MainModule.TableHelper.plansToTopLevelTable(response.plans);
                Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
                Data = app.MainModule.TableHelper.selectTableColumns(Data, {'pk','plan_type', 'ast_planner', 'title','status', 'create_time', 'update_time', 'start_time', 'end_time'});

                % Sort table by update_time or create_time
                % Safely detect if all update_time cells are empty
                
                % Convert update_time cells into strings (empty cells become "")
                update_str = cell(size(Data.update_time));
                for i = 1:numel(Data.update_time)
                    if isempty(Data.update_time{i})
                        update_str{i} = "";
                    else
                        update_str{i} = string(Data.update_time{i});
                    end
                end
                update_str = string(update_str);
                
                % If all update times are empty, sort by create_time
                if all(update_str == "")
                    Data = sortrows(Data, 'create_time', 'descend');
                else
                    % Replace column temporarily for sorting
                    Data.update_time = update_str;
                    Data = sortrows(Data, 'update_time', 'descend');
                end



                app.UITable.Data = Data;
                app.UITable.ColumnName = Data.Properties.VariableNames;  
                app.UITable.ColumnSortable = true;
            else
                % Clear the table if no plans are found
                app.UITable.Data = [];
            end
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: OpenButton
        function OpenButtonPushed(app, event)
            % Handles the Open button click event to select a plan.
            %
            % - Retrieves the selected row index from the table.
            % - Extracts the primary key (Pk) of the selected plan.
            % - Updates the status to 'Open' and resumes UI execution.         
            Index = app.UITable.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Get the pk of the selected row
            app.Pk = app.UITable.Data.pk(Index);
            app.Status = 'Open';
            uiresume(app.UIFigure);                        
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);            
        end

        % Button pushed function: SearchButton
        function SearchButtonPushed(app, event)
            % Searches for observation plans based on user input.
            %
            % - Calls getList() to update the table with the filtered results.
            app.getList();
        end

        % Button pushed function: ClearButton
        function ClearButtonPushed(app, event)
            % Clears all search filters and resets the table.
            %
            % - Resets the Start Time, End Time, and Plan Title fields            

            app.StartTimeEditField.Value = '';
            app.EndTimeEditField.Value = '';
            app.PlanTitleEditField.Value = '';
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('open_plan');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1024 746];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [17 11 1002 57];

            % Create OpenButton
            app.OpenButton = uibutton(app.Panel, 'push');
            app.OpenButton.ButtonPushedFcn = createCallbackFcn(app, @OpenButtonPushed, true);
            app.OpenButton.FontWeight = 'bold';
            app.OpenButton.FontColor = [0 0 1];
            app.OpenButton.Position = [355 9 85 39];
            app.OpenButton.Text = 'Open';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [453 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [546 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 705 1000 33];

            % Create OpenObservationPlanLabel
            app.OpenObservationPlanLabel = uilabel(app.Panel_2);
            app.OpenObservationPlanLabel.HorizontalAlignment = 'center';
            app.OpenObservationPlanLabel.FontSize = 18;
            app.OpenObservationPlanLabel.FontWeight = 'bold';
            app.OpenObservationPlanLabel.Position = [9 0 988 31];
            app.OpenObservationPlanLabel.Text = 'Open Observation Plan';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.Position = [16 79 1003 543];

            % Create UITable
            app.UITable = uitable(app.Panel_3);
            app.UITable.ColumnName = {'Title'; 'User'; 'Updated'; 'Status'};
            app.UITable.RowName = {};
            app.UITable.Position = [17 16 973 513];

            % Create FilterPanel
            app.FilterPanel = uipanel(app.UIFigure);
            app.FilterPanel.Title = 'Filter';
            app.FilterPanel.Position = [13 631 1001 63];

            % Create ClearButton
            app.ClearButton = uibutton(app.FilterPanel, 'push');
            app.ClearButton.ButtonPushedFcn = createCallbackFcn(app, @ClearButtonPushed, true);
            app.ClearButton.Position = [904 11 85 23];
            app.ClearButton.Text = 'Clear';

            % Create StartTimeEditFieldLabel
            app.StartTimeEditFieldLabel = uilabel(app.FilterPanel);
            app.StartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.StartTimeEditFieldLabel.Position = [8 11 60 22];
            app.StartTimeEditFieldLabel.Text = 'Start Time';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.FilterPanel, 'text');
            app.StartTimeEditField.Editable = 'off';
            app.StartTimeEditField.Tooltip = {'Enter plan start time (i.e. 2024-12-04 00:00:00)'};
            app.StartTimeEditField.Placeholder = 'YYYY-MM-DD HH:MM:SS';
            app.StartTimeEditField.Position = [83 11 159 22];

            % Create EndTimeEditFieldLabel
            app.EndTimeEditFieldLabel = uilabel(app.FilterPanel);
            app.EndTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.EndTimeEditFieldLabel.Position = [253 11 56 22];
            app.EndTimeEditFieldLabel.Text = 'End Time';

            % Create EndTimeEditField
            app.EndTimeEditField = uieditfield(app.FilterPanel, 'text');
            app.EndTimeEditField.Editable = 'off';
            app.EndTimeEditField.Placeholder = 'YYYY-MM-DD HH:MM:SS';
            app.EndTimeEditField.Position = [324 11 155 22];

            % Create SearchButton
            app.SearchButton = uibutton(app.FilterPanel, 'push');
            app.SearchButton.ButtonPushedFcn = createCallbackFcn(app, @SearchButtonPushed, true);
            app.SearchButton.Position = [808 11 85 23];
            app.SearchButton.Text = 'Search';

            % Create PlanTitleEditFieldLabel
            app.PlanTitleEditFieldLabel = uilabel(app.FilterPanel);
            app.PlanTitleEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanTitleEditFieldLabel.Position = [497 11 54 22];
            app.PlanTitleEditFieldLabel.Text = 'Plan Title';

            % Create PlanTitleEditField
            app.PlanTitleEditField = uieditfield(app.FilterPanel, 'text');
            app.PlanTitleEditField.Editable = 'off';
            app.PlanTitleEditField.Position = [566 11 226 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = OpenPlan(varargin)

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