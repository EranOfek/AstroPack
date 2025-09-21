%==========================================================================
% ULTRASAT Planner
%
% File:   AppUtils.m
% Author: Chen Tishler
% Created: 07/01/2025
% Updated: 12/03/2025
%
%==========================================================================

classdef AppUtils < handle
    % This class serves as DataModule in Delphi.
    
    properties
        App                     % Handle of main/current AppDesigner window - @TDB
        MainApp                 % Handle of main AppDesigner window - @TBD
    end
    

    methods
        function obj = AppUtils(App)
            % Constructor
            disp('AppUtils');
            obj.App = App;
        end


        function msglog(obj, msg)
            % Write message to log, @Todo - Console / LogFile
            fprintf('%s\n', msg);
        end
    end


    methods (Static)
        function msgOk(App, Msg, Title)
            % Show popup window with message and Ok button
            if nargin < 3
                Title = 'Message'; % Default title
            end                        
            uialert(App.UIFigure, Msg, Title, 'Icon', 'success');            
        end


        function msgError(App, Msg, Title)
            % Show popup window with error message and Ok button
            if nargin < 3
                Title = 'Error'; % Default title
            end                        
            uialert(App.UIFigure, Msg, Title, 'Icon', 'error');
        end        


        function msgDebug(App, Msg, Title)
            % Show debug message
            if nargin < 3
                Title = 'Debug'; % Default title
            end                        
            %uialert(App.UIFigure, Msg, Title, 'Icon', 'success');
            %disp(Msg);
        end


        function Result = askYesNo(App, Msg, Title)
            % Show modal dialog with Yes/No buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end            
           Result = uiconfirm(App.UIFigure, Msg, Title, ...
                'Options', {'Yes', 'No'}, ...
                'Icon', 'question', ...
                'DefaultOption', 2, ...
                'CancelOption', 2);  % 'No' is default and cancel
            %Result = questdlg(Msg, Title, 'Yes', 'No', []);
        end


        function Result = askYesNoCancel(App, Msg, Title)
            % Show modal dialog with Yes/No/Cancel buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end        
            Result = uiconfirm(App.UIFigure, Msg, Title, ...
                'Options', {'Yes', 'No', 'Cancel'}, ...
                'Icon', 'question', ...
                'DefaultOption', 1, ...
                'CancelOption', 3); % 'Cancel' is the default cancel option            
            %Result = questdlg(Msg, Title, 'Yes', 'No', 'Cance', []);
        end        


        function Result = askSaveDiscard(App, Msg, Title)
            % Show modal dialog with Save/Discard buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end                        
            % Use modern UI confirmation
            Result = uiconfirm(App.UIFigure, Msg, Title, ...
                'Options', {'Save', 'Discard'}, ...
                'Icon', 'warning', ...
                'DefaultOption', 1, ...
                'CancelOption', 2); % 'Discard' as the default cancel option            
            %Result = questdlg(Msg, Title, 'Save', 'Discard', []);
        end        


        function Result = askSaveDiscardCancel(App, Msg, Title)
            % Show modal dialog with Save/Discard/Cancel buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end                        
            % Use modern UI confirmation
            Result = uiconfirm(App.UIFigure, Msg, Title, ...
                'Options', {'Save', 'Discard', 'Cancel'}, ...
                'Icon', 'warning', ...
                'DefaultOption', 1, ...
                'CancelOption', 3); % 'Cancel' is the cancel option            
            %Result = questdlg(Msg, Title, 'Save', 'Discard', 'Cancel', []);
        end                


        function center(App)
            % Center the App window on the screen
            movegui(App.UIFigure, 'center');
        end
    end
    
end
