%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/AppUtils.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 06/10/2025
% Description : App Utils for Main Planner
%==========================================================================

classdef AppUtils < handle
    % This class serves as DataModule in Delphi.
    
    properties
        MainModule              % Handle of MainModule instance
        App                     % Reference to PlannerMain
    end


    methods
        function obj = AppUtils(AMainModule)
            % Constructor
            disp('AppUtils');
            obj.MainModule = AMainModule;
            obj.App = obj.MainModule.MainApp;
        end


        function msglog(obj, msg)
            % Write message to log, @Todo - Console / LogFile
            fprintf('%s\n', msg);
        end


        function msgOk(obj, Msg, Title)
            if nargin < 3
                Title = '';
            end

            if isempty(obj.App.MsgBoxApp) || ~isvalid(obj.App.MsgBoxApp)
                obj.App.MsgBoxApp = ultrasat.planner.gui.MsgBox(obj.MainModule);                
            end
            obj.App.MsgBoxApp.Msg = Msg;            
            obj.App.MsgBoxApp.Title = Title;
            obj.App.showModal(obj.App.MsgBoxApp);      
        end


        function msgError(obj, Msg, Title)
            % Show popup window with error message and Ok button
            if nargin < 3
                Title = 'Error'; % Default title
            end                        
            obj.msgOk(Msg, Title);
        end        


        function msgDebug(obj, Msg, Title)
            % Show debug message
            if nargin < 3
                Title = 'Debug'; % Default title
            end                        
            %obj.MsgBox(Msg, Title);
        end


        function Result = askYesNo(obj, Msg, Title)
            % Show modal dialog with Yes/No buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end            
           Result = uiconfirm(obj.App.UIFigure, Msg, Title, ...
                'Options', {'Yes', 'No'}, ...
                'Icon', 'question', ...
                'DefaultOption', 2, ...
                'CancelOption', 2);  % 'No' is default and cancel
        end


        function Result = askYesNoCancel(obj, Msg, Title)
            % Show modal dialog with Yes/No/Cancel buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end        
            Result = uiconfirm(obj.App.UIFigure, Msg, Title, ...
                'Options', {'Yes', 'No', 'Cancel'}, ...
                'Icon', 'question', ...
                'DefaultOption', 1, ...
                'CancelOption', 3); % 'Cancel' is the default cancel option            
        end        


        function Result = askSaveDiscard(obj, Msg, Title)
            % Show modal dialog with Save/Discard buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end                        
            % Use modern UI confirmation
            Result = uiconfirm(obj.App.UIFigure, Msg, Title, ...
                'Options', {'Save', 'Discard'}, ...
                'Icon', 'warning', ...
                'DefaultOption', 1, ...
                'CancelOption', 2); % 'Discard' as the default cancel option            
        end        


        function Result = askSaveDiscardCancel(obj, Msg, Title)
            % Show modal dialog with Save/Discard/Cancel buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end                        
            % Use modern UI confirmation
            Result = uiconfirm(obj.App.UIFigure, Msg, Title, ...
                'Options', {'Save', 'Discard', 'Cancel'}, ...
                'Icon', 'warning', ...
                'DefaultOption', 1, ...
                'CancelOption', 3); % 'Cancel' is the cancel option            
        end                


        function center(obj, App)
            % Center the App window on the screen
            movegui(App.UIFigure, 'center');
        end
        
    end
    
end
