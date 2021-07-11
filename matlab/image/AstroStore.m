% Data storge manager (with AstroDb and ImagePath)

% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2

classdef AstroStore < Component
    
    properties (Hidden, SetAccess = public)
        ManageTimer = []
        StorePath
    end
    

    methods % Constructor    
        
        function Obj = AstroStore
            Obj.setName('AstroStore')
           
            % Setup and start timer
            % https://www.mathworks.com/help/matlab/ref/timer-class.html
            Obj.ManageTimer = timer;
            Obj.ManageTimer.StartDelay = 3;
            Obj.ManageTimer.Period = 1;
            Obj.ManageTimer.TimerFcn = @(myTimerObj, Obj.ManageTimerEvent);
            start(Obj.ManageTimer)
        end

    end
    
    
    methods
        
        function Result = copyFileToStore(Obj, SrcFileName, DstFileName)
            %
            Result = true;
        end
  
        
        function Result = getImagePath(Obj, ImPath)
            %
            Result = '';
        end
        
    end
             
    %
    methods(Static)
        function Result = get()
            persistent Obj
            if isempty(Obj)
                Obj = AstroStore;
            end
            Result = Obj;
        end
        
    end
    
    % 
    
    % setters/getters
    methods
                
    end
    
    
    % setters/getters
    methods
    
        function manageTimerEvent()
        end
    end

    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroDbStorge test started')
               
            % Create db adaptor
            db = AstroDb;                                   
   
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDbStore test passed')
            Result = true;
        end
    end    
             
end

            