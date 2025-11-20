% uplanner helper class for LCS - create it from uplanner buildLCS etc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of functions:
% - ultrasat.LcsHelper.LcsHelper(Args): Constructor
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
                Args.Param = 0;             % 
            end

            %          
            Obj.Planner = Args.UPlanner;
        end

    end 


    %
    methods % Building the plan

        % === Modify, add functions, etc. ===
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


    methods (Static) % unitTest, Debug

        % === Write here unit test of the class ===
        function Result = unitTest()

            % From uplanner.unitTest() etc
            upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');

            % ...

            Result = true;
        end

    end
end
