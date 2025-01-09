%==========================================================================
% ULTRASAT Planner
%
% File:   .m
% Author: Chen Tishler
% Created: 07/01/2025
% Updated: 07/01/2025
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
            fprintf('%s\n', msg);
        end
    end

    methods (Static)
        function msgOk(App, Msg, Title)
            uialert(App.UIFigure, Msg, Title, 'Icon', 'success');
        end


        function msgError(App, Msg, Title)
            uialert(App.UIFigure, Msg, Title, 'Icon', 'error');
        end        

        function center(App)
            movegui(App.UIFigure, 'center');                        
        end
    end
    
end


