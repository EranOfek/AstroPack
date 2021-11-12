function [MergedCat, MatchedS, ResVar, FitMotion] = mergeCatalogs(Obj, Args)
    % Merge catalogs of the same field into a merged catalog of all unique sources
    %   This include:
    %       Generate a list of unified sources in all images using
    %       MatchedSources/unifiedCatalogsIntoMatched
    %       Peform relative photometry
    %       Fit proper motions
    %       calculate variability indicators
    % Input  : - An array of AstroImage objects with populated catalogs.
    %            The array has dimensions of (Nepochs X Nfields).
    %          * ...,key,val,...
    %            'CooType' - Coo type by which to match: 'pix' | ['sphere']
    %            'Radius' - Search radius for source matching.
    %                   Default is 3.
    %            'RadiusUnits' - Search radius units. Default is 'arcsec'.
    %            'RelPhot' - Logical indicating if to apply relative phot.
    %                   calibration to the magnitudes. Default is true.
    %            'fitPolyHyp' - 
    %            'PolyDeg'
    %            'FitPM'
    %            'fitMotionArgs'
    %            'MatchedColums'
    %            'ColNameFlags'
    %            'ColNamesStat'
    %            'FunIndStat'
    %            'ColNamesAll'
    %            'MagCalibColName'
    %            'MagCalibErrColName'
    %            'unifiedSourcesCatalogArgs' - A cell array of additional
    %                   arguments to pass to MatchedSources/unifiedCatalogsIntoMatched
    %                   Default is {}.
    % Output : -
    % Author : Eran Ofek (Nov 2021)
    % Example: [MergedCat, MatchedS, Result] = imProc.match.mergeCatalogs(AllSI)
    %          I = 1; AA=MergedCat(I).toTable; Flag = (AA.PM_TdistProb>0.999 & AA.Nobs>5) | (AA.PM_TdistProb>0.9999 & AA.Nobs>3); remove near edge..., check that motion is consistent with Nobs sum(Flag)
    %          ds9(AllSI(1,I), 1); 
    %          ds9(AllSI(end,I), 2); 
    %          ds9.plot(MergedCat(I).Catalog(Flag,1:2),'o','Coo','fk5')
    
    arguments
        Obj AstroImage
        Args.CooType                 = 'sphere';
        Args.Radius                  = 3;
        Args.RadiusUnits             = 'arcsec';
        
        Args.RelPhot logical         = true;
        Args.fitPolyHyp logical      = true;
        Args.PolyDeg cell            = {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]};
        Args.FitPM logical           = true;
        Args.fitMotionArgs cell      = {'Prob',1e-5};
        
        Args.MatchedColums           = {'RA','Dec','X','Y','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
        
        Args.ColNameFlags            = 'FLAGS';
        Args.ColNamesStat            = {'MAG_CONV_2', 'MAG_CONV_3','SN_1','SN_2','SN_3','SN_4'};
        Args.FunIndStat              = {[1:8], [1:8], [1 3], [1 3], [1 3], [1 3]};
        Args.ColNamesAll             = {'MAG_CONV_2','MAGERR_CONV_2'};
        Args.MagCalibColName         = 'MAG_CONV_2';
        Args.MagCalibErrColName      = 'MAGERR_CONV_2';

        Args.unifiedSourcesCatalogArgs cell     = {};
       

    end
    
    % find all unique sources
    [Nepochs, Nfields] = size(Obj);
    JD  = julday(Obj(:,1));     

    Result = [];
    for Ifields=1:1:Nfields
        MatchedS(Ifields) = MatchedSources;
        [MatchedS(Ifields), Matched(Ifields,:)] = MatchedS(Ifields).unifiedCatalogsIntoMatched(Obj(:,Ifields),...
                                                         'CooType',Args.CooType,...
                                                         'Radius',Args.Radius,...
                                                         'RadiusUnits',Args.RadiusUnits,...
                                                         'MatchedColums',Args.MatchedColums,...
                                                         'JD',JD,...
                                                         Args.unifiedSourcesCatalogArgs{:});
                                                     
                                                     
                                                     
        
%         [~, ~, Matched(Ifields,:)] = imProc.match.unifiedSourcesCatalog(Obj(:,Ifields), 'CooType',Args.CooType,...
%                                                          'Radius',Args.Radius,...
%                                                          'RadiusUnits',Args.RadiusUnits,...
%                                                          Args.unifiedSourcesCatalogArgs{:});
%                                                      
%         MatchedS(Ifields) = MatchedSources;
%         MatchedS(Ifields).addMatrix(Matched(Ifields,:), Args.MatchedColums);
%         % populate JD
%         MatchedS(Ifields).JD = JD;  
        
        % image 2 image X shift (pix)
        Summary(Ifields).ShiftX = median(diff(MatchedS(Ifields).Data.X,1,1), 2, 'omitnan');
        Summary(Ifields).ShiftY = median(diff(MatchedS(Ifields).Data.Y,1,1), 2, 'omitnan');
    
        % relative photometry
        if Args.RelPhot
            [ResZP(Ifields), MatchedS(Ifields)] = lcUtil.zp_lsq(MatchedS(Ifields), 'MagField',Args.MagCalibColName, 'MagErrField',Args.MagCalibErrColName);
            
            % apply ZP to all Magnitudes...
            %FFU
            'a'
            
        end
        
        % lcUtil.fitPolyHyp
        if Args.fitPolyHyp
            [ResVar(Ifields).Result] = lcUtil.fitPolyHyp(MatchedS(Ifields), 'MagFieldNames',Args.MagCalibColName, 'PolyDeg',Args.PolyDeg, 'SubtractMeanT',true,'NormT',true);
        end

    end    
    
    % fit proper motion
    if Args.FitPM
        FitMotion = lcUtil.fitMotion(MatchedS, Args.fitMotionArgs{:});
        NumColPM    = 8;
    else
        NumColPM    = 0;
    end
    if Args.fitPolyHyp
        NumColVar    = 2;
    else
        NumColVar    = 0;
    end
    Nstat     = numel(Args.ColNamesStat);
    Nall      = numel(Args.ColNamesAll);
    NstatProp = cellfun(@numel,Args.FunIndStat);
    NumCol    = NumColPM + NumColVar + 1 + sum(NstatProp) + Nall.*Nepochs;
    
    MergedCat = AstroCatalog([Nfields 1]);
    %FunType   = str2func(class(MatchedS(1).getMatrix(Args.ColNameFlags)));
    
    
    for Ifields=1:1:Nfields    
        ColNames = cell(1, NumCol);
        ColUnits = cell(1, NumCol);
        Cat      = zeros(MatchedS(Ifields).Nsrc, NumCol);
        if Args.FitPM
            ColNames(1:NumColPM) = {'RA','Dec','Nobs', 'StdRA','StdDec', 'PM_RA','PM_Dec', 'PM_TdistProb'};
            ColUnits(1:NumColPM) = {'deg','deg','','deg','deg','deg/day','deg/day',''};
            
            Cat(:,1)       = FitMotion(Ifields).RA.ParH1(1,:).';
            Cat(:,2)       = FitMotion(Ifields).Dec.ParH1(1,:).';
            Cat(:,3)       = FitMotion(Ifields).RA.Nobs(:);
            Cat(:,4)       = FitMotion(Ifields).RA.StdResid_H0(:);
            Cat(:,5)       = FitMotion(Ifields).Dec.StdResid_H0(:);
            Cat(:,6)       = FitMotion(Ifields).RA.ParH1(2,:).';
            Cat(:,7)       = FitMotion(Ifields).Dec.ParH1(2,:).';
            Cat(:,8)       = (1 - (1 - FitMotion(Ifields).RA.StudentT_ProbH1).*(1 - FitMotion(Ifields).Dec.StudentT_ProbH1)).';
            
            %Cat(:,8)       = (FitMotion(Ifields).RA.DeltaChi2 + FitMotion(Ifields).Dec.DeltaChi2).';
            Icol = NumColPM;
        else
            Icol = 0;
        end
        
        Icol = Icol + 1;
        Cols(Ifields).Flags = combineFlags(MatchedS(Ifields),'FlagsNameDic',Args.ColNameFlags, 'FlagsType',@uint32);
        Cat(:,Icol) = Cols(Ifields).Flags.(Args.ColNameFlags)(:);
        ColNames{Icol} = Args.ColNameFlags;
        ColUnits{Icol} = '';
        
        
        for Istat=1:1:Nstat
            Cols(Ifields).Stat.(Args.ColNamesStat{Istat}) = statSummary(MatchedS(Ifields),...
                                                                        'FieldNameDic',Args.ColNamesStat{Istat},...
                                                                        'FunInd',Args.FunIndStat{Istat});
            
            StatFieldNames = fieldnames(Cols(Ifields).Stat.(Args.ColNamesStat{Istat}));
            NstatFN = numel(StatFieldNames);
            for IstatFN=1:1:NstatFN
                Icol           = Icol + 1;
                Cat(:,Icol)    = Cols(Ifields).Stat.(Args.ColNamesStat{Istat}).(StatFieldNames{IstatFN})(:);
                ColNames{Icol} = sprintf('%s_%s',StatFieldNames{IstatFN}, Args.ColNamesStat{Istat});
                ColUnits{Icol} = MatchedS(Ifields).getUnits(Args.ColNamesStat{Istat});
            end
        end

        
        for Iall=1:1:Nall
            Cols(Ifields).All.(Args.ColNamesAll{Iall}) = MatchedS(Ifields).getMatrix(Args.ColNamesAll{Iall}).';
            
            Cat(:,Icol+1:Icol+Nepochs) = Cols(Ifields).All.(Args.ColNamesAll{Iall});
            %Icol = Icol + Ncol;
            for IcolEp=1:1:Nepochs
                Icol = Icol + 1;
                ColNames{Icol} = sprintf('Epoch%03d_%s',IcolEp, Args.ColNamesAll{Iall});
                ColUnits{Icol} = MatchedS(Ifields).getUnits(Args.ColNamesAll{Iall});
            end
        end
       
        if Args.fitPolyHyp
            % ResVar
            Icol           = Icol + 1;
            Cat(:,Icol)    = ResVar(Ifields).Result(end).ResidStd(:);
            ColNames{Icol} = 'StdPoly';
            ColUnits{Icol} = 'mag';
            Icol           = Icol + 1;
            Cat(:,Icol)    = ResVar(Ifields).Result(end).DeltaChi2(:);
            ColNames{Icol} = 'PolyDeltaChi2';
            ColUnits{Icol} = '';
        end
        
        MergedCat(Ifields).Catalog  = Cat;
        MergedCat(Ifields).ColNames = ColNames;
        MergedCat(Ifields).ColUnits = ColUnits;
        MergedCat(Ifields).sortrows('Dec');
                
        % FFU: search for asteroids
        % imProc.asteroids.searchAsteroids_pmCat
        
        % FFU: match to external catalogs
        
    end
    
    
    % DEBUG
    %I= 9;
    %semilogy(nanmedian(MatchedS(I).Data.MAG_CONV_2,1),  nanstd(MatchedS(I).Data.MAG_CONV_2,[],1),'.')
    %hold on;
    %semilogy(nanmedian(CorrMS(I).Data.MAG_CONV_2,1),  nanstd(CorrMS(I).Data.MAG_CONV_2,[],1),'.')
    %semilogy(nanmedian(MatchedS(I).Data.MAG_CONV_2,1),  nanstd(MatchedS(I).Data.Dec,[],1).*3600,'.')
    
end