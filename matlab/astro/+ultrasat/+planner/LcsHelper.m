%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of functions:
% - ultrasat.LcsHelper.uplanner(Args): Constructor
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Additional functions to be considered:
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classdef LcsHelper < Component 
    % 
    properties(Access = public)
        Planner                                 % uplanner

    end

    % 
    properties (Hidden, Constant)

    end 

    % 
    methods  % Constructor
        function Obj = LcsHelper(Args)
            % object constructor
            % example: up = ultrasat.planner.uplanner('AstPlanner','YS');
            arguments                
                Args.UPlanner               %
                Args.Param = 0;             % plan type: HCS, LCS, AllSS, DDT, TOO  
            end

            %          
            Obj.Planner = Args.UPlanner;
        end

    end 


    %
    methods % Building the plan
        %
        function buildLcs(Obj, Args)
            % Build a plan for a HCS field, using a single selected UniqTarget 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTarg) >=1)
            arguments
                Obj
                Args.HCS_UniqTarg = 1; % Default is the first line if not selected
            end               
        end

    end


    methods % unitTest, Debug      
        function Result = unitTest(Obj)
            Result = true;
        end

    end
end
