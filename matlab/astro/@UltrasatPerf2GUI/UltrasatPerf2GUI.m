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
    
        function jout = calcSNR(Obj, Args)
            arguments
                Obj
                Args.InFile
                Args.OutFile
            end
            Stack = dbstack();
            Func = Stack(1);

            if ~isfield(Args, 'InFile') || ~isfield(Args, 'OutFile')
                error("%s: Missing either 'Infile' or 'OutFile' argument", Func);
            end
            
            try
                inargs = jsondecode(fileread(Args.InFile));
            catch ex
                rethrow(ex);
            end

            result.ResultSnr = [];
            result.ResultLimitingMagnitude = [];
            result.message = string(nan);
            
            args.ExpTime = inargs.ExpTime;
            args.Nim = inargs.NumImages;
            sourceIndex = Obj.sourceIndex('Source', inargs.PicklesModels);
            if isempty(sourceIndex)
                result.message = sprintf("error: unknown source '%s'", inargs.PicklesModels);
                jout = jsonencode(result);
                return;
            end
            args.SrcINd = sourceIndex;
            args.R = str2double(string(inargs.R));
            args.SN = inargs.SnrMagnitude;

            try
                ArgsCell = namedargs2cell(args);
                out = Obj.UP.UP.calcSNR(ArgsCell{:});
            catch ex
                result.message = sprintf("error: calcSNR threw exception identifier='%s' with message='%s'", ex.identifier, ex.message);
                jout = jsonencode(result);
                return;
            end
            
            result.ResultSnr = out.SNRm;
            result.ResultLimitingMagnitude = out.LimMag;
            
            fid = fopen(Args.OutFile, 'w');
            fprintf(fid, jsonencode(result, 'ConvertInfAndNaN', true));
            fclose(fid);
        end
    end
end

