classdef UltrasatPerf2GUI < Base
    % UP2GUI Summary of this class goes here
    % Detailed explanation goes here
    %
    % This file is required in source folder:
    %
    % P90_UP_test_60_ZP_Var_Cern_21.mat
    %
    % Download link:
    % https://drive.google.com/file/d/1PI4AjxjYod8vtUbDc7XyQWrgQ2ou1V6j/view?usp=sharing
    %
    % GDrive: pipeline/Deployment/snr/P90_UP_test_60_ZP_Var_Cern_21.mat
    %
    
    properties
        MatFileName = 'P90_UP_test_60_ZP_Var_Cern_21.mat';    % File name 
        UP              % UltrasatPerf object loaded from file
    end
    
    properties(Hidden)
        Sources         % Loaded from UP
    end
    
    methods
        function Obj = UltrasatPerf2GUI()
            % Constructor
            % Input   : -
            % Output  : New object
            % Author  : Arie Blumenzweig (2023)
            % Example : UG = UltrasatPerf2GUI();
            
            Obj.UP = load(Obj.MatFileName, 'UP');
            Obj.Sources = string({Obj.UP.UP.Specs.ObjName});
        end
        
        
        function index = sourceIndex(Obj, Args)
            %
            % Input   : - Object
            %           - 
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             '...' -
            %                      
            % Output  : 
            % Author  : Arie Blumenzweig (2023)
            % Example : 
            %           
            
            arguments
                Obj
                Args.Source string
            end
            
            index = find(ismember(Obj.Sources, Args.Source));
        end
    
        
        function Result = calcSNR(Obj, Args)
            %
            % Input   : - Object
            %           - 
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             '...' -
            %                      
            % Output  : 
            % Author  : Arie Blumenzweig (2023)
            % Example : 
            %           
            
            arguments
                Obj                         %
                Args.ExpTime                %
                Args.NumImages              %
                Args.R                      % This is the Index and not the value
                Args.Source                 %
                Args.SnrMagnitude           %
                Args.LimitingMagnitude      %
            end

            %
            Result.ResultSnr = [];
            Result.ResultLimitingMagnitude = [];
            Result.message = ''; %string(nan);
            
            %
            args.ExpTime = Args.ExpTime;
            args.Nim = Args.NumImages;
            sourceIndex = Obj.sourceIndex('Source', Args.Source);
            if isempty(sourceIndex)
                Result.message = sprintf("error: unknown source '%s'", Args.Source);
                return;
            end
            args.SrcINd = sourceIndex;
            args.R = str2double(string(Args.R));
            args.SN = Args.SnrMagnitude;

            % Call UltrasatPerf.CalcSNR
            try
                ArgsCell = namedargs2cell(args);
                out = Obj.UP.UP.calcSNR(ArgsCell{:});
            catch ex
                Result.message = sprintf("error: calcSNR threw exception identifier='%s' with message='%s'", ex.identifier, ex.message);
                return;
            end
            
            % Put output results
            Result.ResultSnr = round(out.SNRm, 2);
            Result.ResultLimitingMagnitude = round(out.LimMag, 2);
        end
        
        
        function [Pickles, BlackBodyTemperature] = getSources(Obj)
            % Get lists of sources
            % Input   : - Object
            % Output  : - Pickles - cell
            %           - BlackBodyTemperature - cell
            % Author  : Arie Blumenzweig (2023)
            % Example : [Pickles, BlackBodyTemperature] = UG.getSources()

            Pickles = {};
            BlackBodyTemperature = {};
            
            for i = 1:numel(Obj.Sources)
                if startsWith(Obj.Sources(i), 'Planck')
                    S = split(Obj.Sources(i), '=');
                    if numel(S) == 2
                        V = split(S(2), '.');
                        BlackBodyTemperature{end+1} = V(1);
                        continue;
                    end
                end

                %
                Pickles{end+1} = Obj.Sources(i);
            end
        end
        
        
        function Result = getRdeg(Obj)
            % Get list of Rdeg
            % Input   : - Object
            % Output  : Cell array
            % Author  : Arie Blumenzweig (2023)
            % Example : Rdeg = UG.getRdeg();
            
            Result = Obj.UP.UP.Rdeg;
        end        
    end
    

    methods(Static) % Unit test

        Result = unitTest()
            % Unit test
    end
    
end

