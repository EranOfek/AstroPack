% Data storge manager (with AstroDb and ImagePath)

% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2

classdef AstroStore < Component
    
    properties (Hidden, SetAccess = public)
        ManageTimer = [];
        DataPath = '';
    end
    

    methods % Constructor    
        
        function Obj = AstroStore
            Obj.setName('AstroStore')            
            Obj.setup();
            
            %Obj.startTimer();
        end
        
        
        function Result = setup(Obj)
            % Load config
            if tools.os.iswindows()
                Obj.DataPath = Obj.Config.Data.Pipeline.AstroStore.DataPath_Win;
            else
                Obj.DataPath = Obj.Config.Data.Pipeline.AstroStore.DataPath;
            end
                       
            % Create folder
            if ~isfolder(Obj.DataPath)
                Obj.msgLog(LogLevel.Info, 'Creating path: %d', Obj.DataPath);
                mkdir(Obj.DataPath);
            end
            Result = true;
        end
        
        
        function Result = startTimer(Obj)
            % Setup and start timer
            % https://www.mathworks.com/help/matlab/ref/timer-class.html
            % https://www.mathworks.com/help/matlab/matlab_prog/timer-callback-functions.html
            Obj.ManageTimer = timer;
            %Obj.ManageTimer.StartDelay = 1;
            Obj.ManageTimer.Period = 1;
            Obj.ManageTimer.ExecutionMode = 'fixedRate';
            Obj.ManageTimer.TimerFcn = @Obj.manageTimerEvent;
            %Obj.ManageTimer.UserData = Obj;
            start(Obj.ManageTimer);
            Result = true;
        end

        
        function Obj = stopTimer(Obj)
            % Stop timer
            stop(Obj.ManageTimer);
        end
        
        
        function manageTimerEvent(Obj, Timer, Event)
            % Timer callback function
            disp(datestr(Event.Data.time,'dd-mmm-yyyy HH:MM:SS.FFF'));
        end        

    end
    
    
    methods
        
        function Result = copyFileToStore(Obj, SrcFileName, DstFileName, Args)
            % Copy file to storage
            arguments
                Obj
                SrcFileName
                DstFileName
                Args.Move = false;
            end                
                
            %
            try
                if Args.Move
                    Obj.msgLog(LogLevel.Info, 'moving file: %s -> %s', SrcFileName, DstFileName);
                    movefile(SrcFileName, DstFileName);
                else                    
                    Obj.msgLog(LogLevel.Info, 'copying file: %s -> %s', SrcFileName, DstFileName);
                    copyfile(SrcFileName, DstFileName)
                end
            catch
            end
            Result = true;
        end
  
        
        function Result = insertFile(Obj, SrcFileName, DstFileName, Args)
            % Insert file record to database
            arguments
                Obj
                SrcFileName
                DstFileName
                Args.Move = false;
                Args.TableName
            end                
            
        end
        
        function Result = getImagePath(Obj, ImPath)
            % Get full path to image
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

            