%==========================================================================
% ULTRASAT Planner
%
% File:   AppUtils.m
% Author: Chen Tishler
% Created: 07/01/2025
% Updated: 28/01/2025
%
%==========================================================================

classdef AppUtils < handle
    % This class serves as DataModule in Delphi.
    
    properties
        App                     %
        MainApp                 %
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
            if nargin < 3
                Title = 'Message'; % Default title
            end                        
            uialert(App.UIFigure, Msg, Title, 'Icon', 'success');            
        end


        function msgError(App, Msg, Title)
            % Show popup window with error message
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
            disp(Msg);
        end


        function Result = askYesNo(App, Msg, Title)
            % Show modal dialog with Yes/No buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end            
            Result = questdlg(Msg, Title, 'Yes', 'No', []);
        end

        function Result = askSaveDiscard(App, Msg, Title)
            % Show modal dialog with Save/Discard buttons
            if nargin < 3
                Title = 'Confirmation'; % Default title
            end                        
            Result = questdlg(Msg, Title, 'Save', 'Discard', []);
        end        

        function center(App)
            % Center the App window on the screen
            movegui(App.UIFigure, 'center');
        end
    end
    
end


