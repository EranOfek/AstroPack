function [Result] = proc2MatchedSources(AI, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Oct) 
    % Example: pipeline.generic.proc2MatchedSources

    arguments
        AI
        Args.DimEpoch          = [];
        Args.JD                = [];
        Args.FlagGood          = []; % same Dim as AI
        Args.CheckAstrom       = false;
        Args.CooType           = 'sphere';
        Args.Radius            = 3;
        Args.RadiusUnits       = 'arcsec';
        Args.MatchedColums     = {'RA','Dec','X1','Y1','X2','Y2','XY','SN_1','SN_2','SN_3','SN_4','MAG_PSF','MAGERR_PSF','PSF_CHI2DOF','MAG_APER_2','MAGERR_APER_2','MAG_APER_3','MAGERR_APER_3','FLUX_APER_3','FLAGS','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS'};

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
    HERE
    
    %ResZP  = [];
    %ResVar = [];
    for Ifields=1:1:Nfields
        MatchedS(Ifields)  = MatchedSources;
                    
        FlagGood = Args.FlagGood(:,Ifields);   

        [MatchedS(Ifields), Matched(Ifields,:)] = MatchedS(Ifields).unifiedCatalogsIntoMatched(Obj(FlagGood,Ifields),...
                                                         'CooType',Args.CooType,...
                                                         'Radius',Args.Radius,...
                                                         'RadiusUnits',Args.RadiusUnits,...
                                                         'MatchedColums',Args.MatchedColums,...
                                                         'JD',JD(FlagGood),...
                                                         Args.unifiedSourcesCatalogArgs{:});

        got here
       
        % relative photometry
        if Args.RelPhot
            switch lower(Args.RelPhotAlgo)
                case 'lsq'
                    warning('apply ZP in this case is partial in code');
                    [ResZP(Ifields), MatchedS(Ifields)] = lcUtil.zp_lsq(MatchedS(Ifields), 'MagField',Args.MagCalibColName, 'MagErrField',Args.MagCalibErrColName);
                case 'meddiff'

                    %error('meddff is not available yet');
                    [ResZP(Ifields)] = lcUtil.zp_meddiff(MatchedS(Ifields), 'MagField',Args.MagCalibColName, 'MagErrField',Args.MagCalibErrColName);
                otherwise
                    error('Unknown RelPhotAlgo option');
            end

            % apply ZP to all Magnitudes...
            [MatchedS(Ifields) ,ApplyToMagField] = applyZP(MatchedS(Ifields), ResZP(Ifields).FitZP, 'FieldZP','FitZP', 'ApplyToMagField','MAG_', 'Operator',@minus);

        else
            ResZP = [];
        end

        % fitPolyHyp
        if Args.fitPolyHyp
            [ResVar(Ifields).Result] = fitPolyHyp(MatchedS(Ifields), 'MagFieldNames',Args.MagCalibColName, 'PolyDeg',Args.PolyDeg, 'SubtractMeanT',true,'NormT',true);
        end

        if Args.FitPM
            FitMotion = lcUtil.fitMotion(MatchedS(Ifields), Args.fitMotionArgs{:});
        end
    end    
    

end
