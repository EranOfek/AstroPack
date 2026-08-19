function [MatchedS, ResZP] = proc2MatchedSources(AI, Args)
    % Create a MatchedSources object from a set of AstroImage/AstroCatalog objects.
    %   The input object may be array, and the matching process can be done
    %   along one of the dimensions (See DimEpoch argument for details).
    %     Incluidng:
    %       Source matching using: MatchedSources/unifiedCatalogsIntoMatched
    %       Perform and apply relative photometry using lcUtil.relZPfit
    %       Optional steps: fitPolyHyp trends; proper-motion fit
    % Input  : - An AstroImage or AstroCatalog object with photometry and
    %            astrometry. Size may be Nepochs-by-Nfields or a vector.
    %          * ...,key,val,...
    %            'DimEpoch' - The dimension of the input object along which
    %                   to match the sources.
    %                   If DimEpoch is [], then will match all the
    %                   catalogs (AI must be a vector).
    %                   If DimEpoch is 1 or 2 then the input object is a
    %                   matrix, and the matching will be done along this
    %                   dimension.
    %                   In this case the output is a vector of all the
    %                   fields (size 1xNfields).
    %                   Default is [].
    %            'JD' - Vector of Julian Dates per epoch (Nepochs x 1).
    %                   If empty, will be retrieved from AI(:,1) (AstroImage:
    %                   julday; AstroCatalog: .JD). If still empty, uses
    %                   (1:Nepochs)'.
    %                   Default is [].
    %            'FlagGood' - Logical mask of size (Nepochs x Nfields) that
    %                   indicates which epochs/fields to use.
    %                   Default is all true.
    %            'MinGoodEpoch' - Minimum number of good epochs required in
    %                   order to match the sources.
    %                   Default is 5.
    %            'CheckAstrom' - Check astrometric solution quality when AI
    %                   is AstroImage and CooType~='sphere'. If true, bad
    %                   WCS epochs are masked via FlagGood using
    %                   imProc.astrometry.isSuccessWCS.
    %                   Default is false.
    %            'unifyArgs' - A cell array of arguments to pass to: imProc.match.unify
    %                   Default is {}.
    %
    %            'CooType' - Coordinate type by which to match the sources
    %                   I.e., 'pix'|'sphere'.
    %                   Relevant only for MatchMethod=legacy.
    %                   Default is 'sphere'.
    %            'Radius' - Matching radius (in RadiusUnits).
    %                   Default is 1.5.
    %            'RadiusUnits' - Units of matching radius (e.g., 'arcsec'|'pix').
    %                   Default is 'arcsec'.
    %            'MatchedCols' - Cell array of column names to carry into
    %                   the matched catalog (see default in arguments block).
    %            'unifiedSourcesCatalogArgs' - Cell array of additional
    %                   arguments forwarded to MatchedSources.unifiedCatalogsIntoMatched.
    %                   Default is {}.
    %
    %            % Relative photometry (zero-point fit via lcUtil.relZPfit)
    %            'RelPhot' - If true, perform relative photometry (ZP fit)
    %                   and optionally apply ZP to magnitude fields.
    %                   Default is false.
    %            'zp_meddiffArgs' - Cell array of arguments for the median-
    %                   difference stage in lcUtil.relZPfit. Default is {}.
    %            'zp_lsqArgs' - Cell array of arguments for the least-squares
    %                   stage in lcUtil.relZPfit. Default is {}.
    %            'MagCalibColName' - Magnitude column used for ZP fit.
    %                   Default is 'MAG_APER_3'.
    %            'MagCalibErrColName' - Magnitude error column used for ZP fit.
    %                   Default is 'MAGERR_APER_3'.
    %            'ApplyToMagField' - If non-empty (e.g., 'MAG_'), the fitted
    %                   ZP will be applied to all matched magnitude fields
    %                   whose names start with this prefix. If empty, ZP is
    %                   not applied back to MatchedSources.
    %                   Default is 'MAG_'.
    %            % Optional post-processing
    %            'FitPolyHyp' - If true, fit polynomial-in-time trends via
    %                   fitPolyHyp on the matched data.
    %                   Default is false.
    %            'fitPolyHypArgs' - Cell array of arguments forwarded to
    %                   fitPolyHyp. Default is {}.
    %            'FitPM' - If true, fit apparent/proper motion via
    %                   lcUtil.fitMotion. Default is false.
    %            'fitMotionArgs' - Cell array of arguments forwarded to
    %                   lcUtil.fitMotion. Default is {}.
    %
    %            'unifiedSourcesCatalogArgs' - A cell array of arguments to
    %                   pass to: MatchedSources/unifiedCatalogsIntoMatched
    %                   Default is {}.
    %            'MergeMethod' - Matching and merging method: 'unify' | 'legacy'.
    %                   Options are:
    %                       'unify' - use imProc.match.unify including mex.
    %                       'legacy' - Use
    %                           MatchedSources/unifiedCatalogsIntoMatched (old
    %                           code).
    %                   Default is 'unify'.
    %
    % Output : - MatchedS : 1-by-Nfields array of MatchedSources objects.
    %           - ResZP   : 1-by-Nfields struct array with zero-point fit
    %                       results when RelPhot=true; otherwise [].
    % Author : Eran Ofek (2025 Oct) 
    % Example: MS=pipeline.generic.proc2MatchedSources(AI);


    arguments
        AI
        Args.DimEpoch          = [];
        Args.JD                = [];
        Args.FlagGood          = []; % same Dim as AI
        Args.MinGoodEpoch      = 5;
        Args.CheckAstrom       = false;
        %--- imProc.match.unify arguments ---
        Args.unifyArgs         = {};
        Args.ColUse            = [];
        Args.AddUnUse          = false;

        Args.CooType           = 'sphere';
        Args.Radius            = 1.5;
        Args.RadiusUnits       = 'arcsec';
        Args.MatchedCols       = {'RA','Dec',...
                                  'X','Y',...
                                  'X1','Y1','X2','Y2','XY',...
                                  'SN','SN_1','SN_2',...
                                  'MAG_PSF','MAGERR_PSF','PSF_CHI2DOF','FLUX_PSF',...
                                  'MAG_APER_3','MAGERR_APER_3',...
                                  'FLUX_APER_3',...
                                  'FLAGS',...
                                  'BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS',...
                                  'FORCED'};

        Args.RelPhot           = true;
        Args.RelPhotAlgo       = 'meddiff';  % 'meddiff'
        Args.zp_meddiffArgs    = {};
        Args.zp_lsqArgs        = {};
        Args.MagCalibColName   = 'MAG_APER_3';
        Args.MagCalibErrColName= 'MAGERR_APER_3';
        Args.ApplyToMagField   = 'MAG_';  % if empty, do not apply ZP to MatchedSources

        % additional
        Args.FitPolyHyp        = false;
        Args.fitPolyHypArgs    = {};
        Args.FitPM             = false;
        Args.fitMotionArgs     = {};

        Args.unifiedSourcesCatalogArgs = {};
        Args.MergeMethod       = 'unify'; % 'legacy'
        %Args.LogObj            = [];
    end



    if isa(AI, 'AstroImage')
        IsAI = true;
    else
        IsAI = false;
    end

    % Dimensions
    [Nepochs, Nfields] = size(AI);
    if isempty(Args.DimEpoch)
        if Nfields>1 && Nepochs>1
            error('For matrix AI, DimEPoch must be provided');
        end
        AI = AI(:);
        Args.FlagGood = Args.FlagGood(:);
        [Nepochs, Nfields] = size(AI);
    end
    if Args.DimEpoch==2
        AI = AI.';
        Args.FlagGood = Args.FlagGood.';
        [Nepochs, Nfields] = size(AI);
    end

    % retrieve JD
    if isempty(Args.JD)
        if IsAI
            JD  = julday(AI(:,1));     
        else
            JD = [AI(:,1).JD].';
        end
    else
        JD = Args.JD;
    end
    if isempty(JD)
        JD  = (1:1:Nepochs).';
    end

    if isempty(Args.FlagGood)
        Args.FlagGood = true(Nepochs, Nfields);
    end

    % Check quality of astrometry
    if IsAI && Args.CheckAstrom
        FlagAstrom = imProc.astrometry.isSuccessWCS(AI) & ~strcmp(Args.CooType, 'sphere');
        Args.FlagGood = Args.FlagGood & FlagAstrom;
    end

    
    
    % Define an array of MatchedSources object of size: Nfields
    % HERE
    
    ResZP = struct('FitZP',cell(Nfields,1), 'FitStdZP',cell(Nfields,1), 'FitErrZP',cell(Nfields,1), 'Nsrc',cell(Nfields,1)); % depands on the output of lcUtil.relZPfit !!!
    for Ifields=1:1:Nfields
        MatchedS(Ifields)  = MatchedSources;
        
        FlagGood = Args.FlagGood(:,Ifields);   

        if sum(FlagGood)>=Args.MinGoodEpoch

            % source matching
            switch Args.MergeMethod
                case 'unify'
                    % x7 faster
                    [~,~, MatchedS(Ifields)] = imProc.match.unify(AI(FlagGood,Ifields), Args.unifyArgs{:},...
                                                                  'IsSpherical',~strcmp(Args.CooType, 'pix'),...
                                                                  'MatchRadius',Args.Radius,...
                                                                  'Col',Args.MatchedCols,...
                                                                  'ColUse',Args.ColUse,...
                                                                  'AddUnUse',Args.AddUnUse);
                  
                case 'legacy'
                    [MatchedS(Ifields), Matched(Ifields,:)] = MatchedS(Ifields).unifiedCatalogsIntoMatched(AI(FlagGood,Ifields),...
                                                                     'CooType',Args.CooType,...
                                                                     'Radius',Args.Radius,...
                                                                     'RadiusUnits',Args.RadiusUnits,...
                                                                     'MatchedColums',Args.MatchedCols,...
                                                                     'JD',JD(FlagGood),...
                                                                     Args.unifiedSourcesCatalogArgs{:});
                otherwise
                    error('Unknown MatchMethod option');
            end % switch Args.MergeMethod
    
    
            % relative photometry
            if Args.RelPhot
    
                [ResZP0(Ifields), MatchedS(Ifields)] = lcUtil.relZPfit(MatchedS(Ifields),...
                                                                   Args.zp_meddiffArgs{:},...
                                                                   Args.zp_lsqArgs{:},...
                                                                   'RelPhotAlgo',Args.RelPhotAlgo,...
                                                                   'MagCalibColName',Args.MagCalibColName,...
                                                                   'MagCalibErrColName',Args.MagCalibErrColName,...
                                                                   'ApplyToMagField',Args.ApplyToMagField,...
                                                                   'Operator',@minus);
                % check that all epochs exist
                if ~all(FlagGood)
                    FieldName = fieldnames(ResZP0(Ifields)); % {'FitZP','FitStdZP','FitErrZP','Nsrc'};
                    for Ifn=1:1:numel(FieldName)
                        ResZP(Ifields).(FieldName{Ifn})(FlagGood)  = ResZP0(Ifields).(FieldName{Ifn});
                        ResZP(Ifields).(FieldName{Ifn})(~FlagGood) = NaN;
                    end 
                else
                    ResZP(Ifields) = ResZP0(Ifields);
                end
            else
                ResZP = [];
            end % if Args.RelPhot
    
            % fitPolyHyp
            if Args.FitPolyHyp
                [ResVar(Ifields).Result] = fitPolyHyp(MatchedS(Ifields),...
                    'MagFieldNames',Args.MagCalibColName,... 
                    Args.fitPolyHypArgs{:});
                    % 'PolyDeg',Args.PolyDeg,...
                    % 'SubtractMeanT',true,...
                    % 'NormT',true);
            end
    
            % fit proper motion
            if Args.FitPM
                FitMotion = lcUtil.fitMotion(MatchedS(Ifields), Args.fitMotionArgs{:});
            end
        end % if sum(FlagGood)>=MinGoodEpoch
    end    
    

end
