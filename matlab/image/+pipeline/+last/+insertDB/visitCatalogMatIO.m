classdef visitCatalogMatIO
    % Compact v2 save/load for visit-level proc _Cat.mat and _Hdr.mat files.
    methods (Static)
        function saveVisitCatalogMat(CatPath, Cat)
            % Save catalog numeric payload (InsertDbMatVersion 2).
            visitCatalogMatIO.assertColNamesMatch_(Cat);
            InsertDbMatVersion = visitCatalogMatIO.MatVersion_();
            Catalog = arrayfun(@(C) C.Catalog, Cat, 'UniformOutput', false);
            ColNames = Cat(1).ColNames;
            ColUnits = Cat(1).ColUnits;
            save(CatPath, 'InsertDbMatVersion', 'Catalog', 'ColNames', 'ColUnits', '-v7.3');
        end

        function saveVisitHeaderMat(HdrPath, AH)
            % Save HDU-3 keyword cells only (InsertDbMatVersion 2).
            InsertDbMatVersion = visitCatalogMatIO.MatVersion_();
            HdrData = arrayfun(@(H) H.Data, AH, 'UniformOutput', false);
            save(HdrPath, 'InsertDbMatVersion', 'HdrData', '-v7.3');
        end

        function Cat = loadVisitCatalogMat(CatPath)
            % Load v2 _Cat.mat and reconstruct AstroCatalog array.
            S = load(CatPath);
            visitCatalogMatIO.assertMatVersion_(S, CatPath, '_Cat');
            Catalog = S.Catalog;
            Cat = AstroCatalog(Catalog, 'ColNames', S.ColNames, 'ColUnits', S.ColUnits);
        end

        function AH = loadVisitHeaderMat(HdrPath)
            % Load v2 _Hdr.mat and reconstruct AstroHeader array.
            S = load(HdrPath);
            visitCatalogMatIO.assertMatVersion_(S, HdrPath, '_Hdr');
            HdrData = S.HdrData;
            N = numel(HdrData);
            AH = AstroHeader([1, N]);
            KeyDict = Dictionary.getDict('Header.Synonyms.KeyNames');
            ValDict = Dictionary.getDict('Header.Synonyms.KeyVal.IMTYPE');
            CommentDict = Dictionary.getDict('Header.Comments.Default');
            TimeDict = Dictionary.getDict('Header.Time.KeyNames');
            TimeDict = Dictionary.string2funHandle(TimeDict);
            for Ih = 1:N
                AH(Ih).Data = HdrData{Ih};
                AH(Ih).File = '';
                AH(Ih).KeyDict = KeyDict;
                AH(Ih).ValDict = ValDict;
                AH(Ih).CommentDict = CommentDict;
                AH(Ih).TimeDict = TimeDict;
                AH(Ih).IsKeyUpToDate = false;
            end
        end

        function Ok = colNamesMatch(Cat)
            Ok = visitCatalogMatIO.assertColNamesMatch_(Cat, true);
        end
    end

    methods (Static, Access = private)
        function V = MatVersion_()
            V = 2;
        end

        function Ok = assertColNamesMatch_(Cat, Silent)
            if nargin < 2
                Silent = false;
            end
            N = numel(Cat);
            Ok = true;
            if N < 1
                Ok = false;
                if ~Silent
                    error('visitCatalogMatIO:EmptyCat', 'Catalog array is empty.');
                end
                return
            end
            Ref = Cat(1).ColNames;
            for I = 2:N
                if ~isequal(Cat(I).ColNames, Ref)
                    Ok = false;
                    if ~Silent
                        error('visitCatalogMatIO:ColNamesMismatch', ...
                            'ColNames differ between crops (crop %d vs 1).', I);
                    end
                    return
                end
            end
        end

        function assertMatVersion_(S, MatPath, Label)
            if ~isfield(S, 'InsertDbMatVersion') || S.InsertDbMatVersion ~= visitCatalogMatIO.MatVersion_()
                if isfield(S, 'Cat') || isfield(S, 'AH')
                    Id = 'visitCatalogMatIO:LegacyMatFormat';
                    Msg = sprintf(['%s is legacy full-object MAT (v1). Re-dump with ', ...
                        'writeVisitCatalogs2Mat_perf and OverWrite=true. Path: %s'], Label, MatPath);
                else
                    Id = 'visitCatalogMatIO:UnsupportedMatVersion';
                    Msg = sprintf('%s unsupported InsertDbMatVersion in %s', Label, MatPath);
                end
                error(Id, '%s', Msg);
            end
        end
    end
end
