% Automatic scope/function entry/exit logger
% Currently unused because MATLAB scope behavious is not clear yet.

% #functions (autogen)
% FuncLog - Constructor
% delete - Destructor
% unitTest -
% unitTestHelper1 - Test auto-destructor
% #/functions (autogen)
%

classdef FuncLog < handle
    
    % Properties
    properties (SetAccess = public)
        Title       % Function/scope title
    end
    
    %--------------------------------------------------------
    methods
        function Obj = FuncLog(Title)
            % Constructor, display termination message using io.msgStyle()
            % Input:    Title - char with description of the function
            % Example:  F1 = FuncLog('MyFuncName');
            Obj.Title = Title;
            io.msgStyle(LogLevel.Debug, '@start', 'Func Start: %s', Obj.Title);
        end
        
        
        function delete(Obj)
            % Destructor, display termination message using io.msgStyle()
            io.msgStyle(LogLevel.Debug, '@done', 'Func Done: %s', Obj.Title);
        end
    end
   
    
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'FuncLog test started')
           
            % Explicit delete
            f = io.FuncLog('testing explicit delete');
            io.msgLog(LogLevel.Test, 'Doing something...');
            delete(f);
            
            % Function
            io.msgLog(LogLevel.Test, 'testing inside function');
            io.FuncLog.unitTestHelper1();
            
            % @Todo: Scope - object is destructed on function end, not on out of
            % scope, to be fixed?
            a = 1;
            if a == 1
                f_ = io.FuncLog('testing scope true');
                
                % Explicit delete
                delete(f_);
            else
                % @Todo: Here it will not be called automatically, only
                % when function is done
                f_ = io.FuncLog('testing scope false');
            end
            
            io.msgStyle(LogLevel.Test, '@passed', 'FuncLog test done')
            Result = true;
        end
        
        
        function unitTestHelper1()
            % Test auto-destructor
            io.FuncLog('unitTestHelper test');
        end
    end
    
end
