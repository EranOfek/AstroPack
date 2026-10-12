classdef AstroStreak < handle
    properties
        X % 2xN % abscissae of the extremal points of the streak(s), pixel coordinates
        Y % 2xN % ordinatae of the extremal points of the streak, pixel coordinates
        RA % 2xN % same, in RA,Dec coordinates
        Dec % 2xN
        JD % 2xN % time bounds for the streak duration (may include rolling shutter effects)
        IsEdge % 2xN - flags if the extremes of the streak are at the image edge
        Flux % 1xN % photometric flux estimation, (sigma units)
        FitPar % 3xN % a,b,c coefficients of the fitted sagittal deviation
        Curve = struct('X',[],'Y',[],'Flux',[],'TransverseSigma',[],...
            'Hmean',[],'Acceptable',false(0,0),'TransversePSF',[]);...
                                % coordinates and fluxes of streak slices
        ID % Telescope, Epoch, Crop ID.
    end

    methods
        function [Json, St] = convert2json(Obj, Args)
            % Convert AstroStreak object(s) to a JSON string.
            %   Property names are discovered at run time, so added or
            %   renamed public properties, and extra fields in nested
            %   structs such as Curve, are serialized without changing
            %   this method.
            % Input  : - An AstroStreak object (scalar or array).
            %          * ...,key,val,...
            %            'PrettyPrint' - Pretty-print JSON. Default is false.
            %            'ConvertInfAndNaN' - Convert Inf/NaN to JSON null.
            %                   Default is true.
            %            'OmitEmpty' - Skip empty properties/fields.
            %                   Default is false.
            %            'Skip' - Cell of property names to omit.
            %                   Default is {}.
            %            'IncludeClass' - Store class name in Class.
            %                   Default is true.
            %            'StructArrayAsCell' - Encode struct arrays
            %                   (including 1-element) as JSON arrays.
            %                   Default is true. Set false to keep a 1x1
            %                   struct as a JSON object.
            %            'FileName' - If non-empty, write JSON to this file.
            %                   Default is [].
            % Output : - JSON string.
            %          - Struct (or cell array of structs) that was encoded.
            % Author : Eran Ofek (Oct 2026)
            % Example: S = imUtil.streaks.AstroStreak;
            %          Json = S.convert2json;
            %          Json = S.convert2json('PrettyPrint',true,'OmitEmpty',true);
            arguments
                Obj
                Args.PrettyPrint logical        = false
                Args.ConvertInfAndNaN logical   = true
                Args.OmitEmpty logical          = false
                Args.Skip cell                  = {}
                Args.IncludeClass logical       = true
                Args.StructArrayAsCell logical  = true
                Args.FileName                   = []
            end

            Nobj = numel(Obj);
            StCell = cell(Nobj, 1);
            for Iobj = 1:Nobj
                StCell{Iobj} = AstroStreak.obj2struct(Obj(Iobj), Args);
            end

            if Nobj == 1
                St = StCell{1};
            else
                St = StCell;
            end

            Json = jsonencode(St, 'PrettyPrint', Args.PrettyPrint, ...
                'ConvertInfAndNaN', Args.ConvertInfAndNaN);

            if ~isempty(Args.FileName)
                FID = fopen(Args.FileName, 'w');
                if FID < 0
                    error('Cannot open file %s for writing', Args.FileName);
                end
                cleaner = onCleanup(@() fclose(FID));
                fwrite(FID, Json, 'char');
            end
        end
    end

    methods (Static, Access=private)
        function St = obj2struct(Obj, Args)
            % Copy public properties of one object into a JSON-safe struct.

            PropList = properties(Obj);
            St = struct();
            if Args.IncludeClass
                St.Class = class(Obj);
            end

            for Iprop = 1:numel(PropList)
                Name = PropList{Iprop};
                if any(strcmp(Args.Skip, Name))
                    continue
                end
                Val = Obj.(Name);
                if Args.OmitEmpty && AstroStreak.isEmptyVal(Val)
                    continue
                end
                St.(Name) = AstroStreak.jsonValue(Val, Args);
            end
        end

        function Val = jsonValue(Val, Args)
            % Recursively convert a value to a jsonencode-safe type.

            if isempty(Val) && (isnumeric(Val) || islogical(Val) || ischar(Val) || isstring(Val) || iscell(Val))
                return
            end

            if isobject(Val) && ~isenum(Val) && ~isdatetime(Val) && ~isduration(Val) && ~istimetable(Val)
                if numel(Val) == 1
                    Val = AstroStreak.obj2struct(Val, Args);
                else
                    C = cell(numel(Val), 1);
                    for I = 1:numel(Val)
                        C{I} = AstroStreak.obj2struct(Val(I), Args);
                    end
                    Val = C;
                end
                return
            end

            if isstruct(Val)
                N = numel(Val);
                if N == 0
                    Val = [];
                    return
                end
                C = cell(N, 1);
                for I = 1:N
                    C{I} = AstroStreak.structFields(Val(I), Args);
                end
                if N == 1 && ~Args.StructArrayAsCell
                    Val = C{1};
                else
                    Val = C;
                end
                return
            end

            if iscell(Val)
                for I = 1:numel(Val)
                    Val{I} = AstroStreak.jsonValue(Val{I}, Args);
                end
                return
            end

            if istable(Val) || istimetable(Val)
                Val = table2struct(Val);
                Val = AstroStreak.jsonValue(Val, Args);
                return
            end

            if isdatetime(Val)
                Val = string(Val);
                return
            end

            if isduration(Val)
                Val = seconds(Val);
                return
            end

            if isa(Val, 'function_handle')
                Val = func2str(Val);
                return
            end

            if issparse(Val)
                Val = full(Val);
            end

            if isnumeric(Val) && ~isreal(Val)
                Val = struct('real', real(Val), 'imag', imag(Val));
            end
        end

        function St = structFields(S, Args)
            % Convert one struct (all current fields) to a JSON-safe struct.

            if isempty(S) && isstruct(S)
                St = struct();
                return
            end

            FN = fieldnames(S);
            St = struct();
            for I = 1:numel(FN)
                Name = FN{I};
                Val = S.(Name);
                if Args.OmitEmpty && AstroStreak.isEmptyVal(Val)
                    continue
                end
                St.(Name) = AstroStreak.jsonValue(Val, Args);
            end
        end

        function TF = isEmptyVal(Val)
            % True if a property/field has no content to serialize.

            if isstruct(Val)
                TF = isempty(Val) || (isscalar(Val) && isempty(fieldnames(Val)));
            else
                TF = isempty(Val);
            end
        end
    end
end
