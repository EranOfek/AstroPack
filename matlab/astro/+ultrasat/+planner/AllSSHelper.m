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

classdef AllSSHelper < Component 
    % 
    properties(Access = public)
        Planner                                 % uplanner

    end

    % 
    properties (Hidden, Constant)

    end 

    % 
    methods  % Constructor
        function Obj = AllSSHelper(UPlanner, Args)
            % object constructor
            % example: up = ultrasat.planner.uplanner('AstPlanner','YS');
            arguments                
                UPlanner               		%
                Args.Param = 0;             % Whatever you need in constructor
            end

            %          
            Obj.Planner = UPlanner;
        end

    end 


    %
    methods % Building the plan

        % === Modify, add functions, etc. ===
        function buildAllSS(Obj, Args)
            % Build a plan for a HCS field, using a single selected UniqTarget 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTarg) >=1)
            arguments
                Obj
                Args.HCS_UniqTarg = 1; % Default is the first line if not selected
            end
			
			fprintf('AllSSHelper.buildAllSS: UT table height: %d\n', height(Obj.Planner.UniqTarg));
        end

    end


    methods (Static) % unitTest, Debug

        % === Write here unit test of the class ===
        function Result = unitTest()

            % From uplanner.unitTest() etc
            % upAllSS = ultrasat.planner.uplanner('AstPlanner','YS','Type','AllSS','ExtragalDitherLeg',DitherLeg,...
            %    'Load','~/matlab/data/ULTRASAT/alss_uniq_targ.mat');

            % ...

            Result = true;
        end

    end
end
