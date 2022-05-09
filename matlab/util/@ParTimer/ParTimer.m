% Parallel timer class - Create a timer object on a parallel worker

NOT WORKING

classdef ParTimer < handle
    % ParTimer class
    
    % Properties
    properties (SetAccess = public)
        Pool            = [];
        ExecutionMode   = 'singleShot';   % 'singleShot' (default) | 'fixedRate' | 'fixedDelay' | 'fixedSpacing'
        Period          = 1;
        BusyMode        = 'drop';
        Running         = 'off';
        
        TimerFcn        = '';
        ErrorFcn        = '';
        StartFcn        = '';
        StopFcn         = '';
        
        
    end
    
    %--------------------------------------------------------
    methods % constructor
        function Obj = ParTimer(Pool, Args)
            %
           
            arguments
                Pool                  = [];    %parpool('threads');
                Args.PoolType         = 'threads';    % 'threads' | 'local'
                Args.TimerArgs cell   = {};
                Args.TimerObjName     = 'Timer';
            end
            
            if isempty(Pool)
                Obj.Pool = gcp('nocreate');  % retrieve existing pool
                if isempty(Obj.Pool)
                    % create a new pool
                    Obj.Pool = parpool(Args.PoolType);
                end
            end
            
            FunStr = sprintf('%s = timer;',Args.TimerObjName);
            Nta    = numel(Args.TimerArgs);
            for Ita=1:2:Nta
                FunStr = sprintf('%s  %s=%s;', FunStr, Args.TimerArgs{Ita}, Args.TimerArgs{Ita+1});
            end
            F = parfeval(Obj.Pool, FunStr, numout,X1,...,Xm)
            
        end
    end
    
    
    
    methods(Static) % unitTest
        Result = unitTest(Obj)
            % Unit test
            
   end
    
end
