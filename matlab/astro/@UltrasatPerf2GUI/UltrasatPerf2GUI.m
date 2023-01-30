classdef UltrasatPerf2GUI < Base
    %UP2GUI Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        UP
    end
    
    properties(Hidden)
        Sources
    end
    
    methods
        function Obj = UltrasatPerf2GUI()
            Obj.UP = load('P90_UP_test_60_ZP_Var_Cern_21.mat', 'UP');
            Obj.Sources = string({Obj.UP.UP.Specs.ObjName});
        end
        
        function index = sourceIndex(Obj, Args)
            arguments
                Obj
                Args.Source string
            end
            
            index = find(ismember(Obj.Sources, Args.Source));
        end
    
        function Result = calcSNR(Obj, Args)
            arguments
                Obj
                Args.ExpTime
                Args.NumImages
                Args.R
                Args.Source
                Args.SnrMagnitude
                Args.LimitingMagnitude
            end

            Result.ResultSnr = [];
            Result.ResultLimitingMagnitude = [];
            Result.message = string(nan);
            
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

            try
                ArgsCell = namedargs2cell(args);
                out = Obj.UP.UP.calcSNR(ArgsCell{:});
            catch ex
                Result.message = sprintf("error: calcSNR threw exception identifier='%s' with message='%s'", ex.identifier, ex.message);
                return;
            end
            
            Result.ResultSnr = out.SNRm;
            Result.ResultLimitingMagnitude = out.LimMag;
        end
        
        function Sources = getSources(Obj)
            Sources = Obj.Sources;
        end
    end
end

