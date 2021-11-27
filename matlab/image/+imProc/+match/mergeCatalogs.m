function [MergedCat, MatchedS, ResZP, ResVar, FitMotion] = mergeCatalogs(Obj, Args)
     % Merge catalogs of the same field into a single unified merged catalog
    %   The program may works AstroImage array in which different columns
    %   corresponds to different fields, and rows corresponds to epochs.
    %   The catalogs of each fields are merged into an AstroTable using 
    %   MatchedSources/unifiedCatalogsIntoMatched.
    %   Next, for each relative photometry is calaculated, and for each
    %   source, proper motion is fitted and variability information is
    %   calculated. The output catalog may contains columns that provide
    %   measurments for all epochs (see 'ColNamesAll' argument), and
    %   columns for which some statstics over all epochs is calculated (see
    %   'ColNamesStat', 'FunIndStat' arguments).
    %   Also generate a MatchedSources object for each field.
    %
    % Input  : - An array of AstroImage objects with populated catalogs.
    %            The array has dimensions of (Nepochs X Nfields).
    %          * ...,key,val,...
    %            'CooType' - Coo type by which to match: 'pix' | ['sphere']
    %            'Radius' - Search radius for source matching.
    %                   Default is 3.
    %            'RadiusUnits' - Search radius units. Default is 'arcsec'.
    %            'JD' - A vector of JD. If empty, use AstroImage header, if
    %                   not AstroImage set to 1:Nepochs.
    %            'RelPhot' - Logical indicating if to apply relative phot.
    %                   calibration to the magnitudes. Default is true.
    %            'fitPolyHyp' - A logical indicating if to fit all light
    %                   curves with polynomials and calculate the delta\chi^2
    %                   between models. Default is true.
    %            'PolyDeg' - A cell array in wich each element contains all
    %                   the degrees of the polynomial to fit.
    %                   E.g., [0:1:2], is a full 2nd deg polynomial.
    %                   The first cell corresponds to the null hypothesis.
    %                   The Delta\chi2^2 is calculated relative to the null
    %                   hypothesis. In addition, the error normalization is
    %                   calculated such that the chi^2/dof of the null
    %                   hypothesis will be 1 (with uniform errors).
    %                   Default is {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]}.
    %            'FitPM' - A logical indicating if to fit a proper motion
    %                   for each source. Default is true.
    %            'fitMotionArgs' - A cell array of additional arguments to
    %                   pass to lcUtil.fitMotion. Default is {}.
    %            'MatchedColums' - A cell array of column names in the
    %                   input AstroCatalog which to propagate into the
    %                   MatchedSources object. Default is
    %                   {'RA','Dec','X','Y','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
    %            'ColNameFlags' - A char array of the column name
    %                   containing a flags (propagated from the bit mask)
    %                   information. Default is 'FLAGS'.
    %            'ColNamesStat' - A cell array of column names for which to
    %                   calculate statistics over all epochs using
    %                   MatchedSources/statSummary. For each requested
    %                   statistics and column name, a new column name will
    %                   be added to the output. For example, if you request
    %                   the median and mean statistics, column names like:
    %                   'Mean_MAG_CONV_2' and 'Median_MAG_CONV_2' will be
    %                   added.
    %                   Default is 
    %                   {'MAG_CONV_2', 'MAG_CONV_3','SN_1','SN_2','SN_3','SN_4'};
    %            'FunIndStat' - A cell array of indices of statistics which
    %                   to calculate for each 'ColNamesStat'.
    %                   The indices are those of the functions in
    %                   MatchedSources/statSummary:
    %                   1 - Mean; 2 - Median; 3 - Std; 4 - RStd; 5 - Range;
    %                   6- Min; 7 - Max; 8 - Nobs.
    %                   Default is 
    %                   {[1:8], [1:8], [1 3], [1 3], [1 3], [1 3]};
    %                   For example, for 'MAG_CONV_3' the Mean and Std will
    %                   be added.
    %            'ColNamesAll' - A cell array of column names for which to
    %                   copy to the output AstroCatalog the data values
    %                   from all epochs (i.e., for each column name, Nepoch
    %                   columns will be added).
    %                   Default is {'MAG_CONV_2','MAGERR_CONV_2'};
    %            'MagCalibColName' - A char array of column name by which to
    %                   calculate the relative photometric calibration.
    %                   Default is 'MAG_CONV_2'.
    %            'MagCalibErrColName' - Error column name corresponding to
    %                   'MagCalibColName'. Default is 'MAGERR_CONV_2'.
    %           
    %            'unifiedSourcesCatalogArgs' - A cell array of additional
    %                   arguments to pass to MatchedSources/unifiedCatalogsIntoMatched
    %                   Default is {}.
    % Output : - MergedCat is an array of AstroCatalog (one per
    %            field/column in the input Astrocatalog). Each AstroCatalog
    %            contains the merged catalog with all the sources and their
    %            properties.
    %          - MatchedS is the MatchedSources object array
    %            with element for each field.
    %            The MatchedSources object contains the matrices of the data
    %            retrieved from 'MatchedColums' columns. 
    %          - ResZP is a structure array with the ZP info, returned by
    %            lcUtil.zp_lsq
    %          - ResVar is a structure array with the variability info,
    %            returned by lcUtil.fitPolyHyp
    %          - FitMotion is a structure array with proper motion info,
    %            returned by lcUtil.fitMotion.
    % Author : Eran Ofek (Nov 2021)
    % Example: [MergedCat, MatchedS] = imProc.match.mergeCatalogs(AllSI)
    %          I = 1; AA=MergedCat(I).toTable; Flag = (AA.PM_TdistProb>0.999 & AA.Nobs>5) | (AA.PM_TdistProb>0.9999 & AA.Nobs>3); remove near edge..., check that motion is consistent with Nobs sum(Flag)
    %          ds9(AllSI(1,I), 1); 
    %          ds9(AllSI(end,I), 2); 
    %          ds9.plot(MergedCat(I).Catalog(Flag,1:2),'o','Coo','fk5')
    
    arguments
        Obj                                 
        Args.CooType                 = 'sphere';
        Args.Radius                  = 3;
        Args.RadiusUnits             = 'arcsec';
        Args.JD                      = [];  % if empty, use header, if no header use 1:N
        
        Args.RelPhot logical         = true;
        Args.fitPolyHyp logical      = true;
        Args.PolyDeg cell            = {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]};
        Args.FitPM logical           = true;
        Args.fitMotionArgs cell      = {'Prob',1e-5};
        
        Args.MatchedColums           = {'RA','Dec','X','Y','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS'};
        
        Args.ColNameFlags            = 'FLAGS';
        Args.ColNamesStat            = {'RA','Dec','X','Y','MAG_CONV_2', 'MAG_CONV_3','SN_1','SN_2','SN_3','SN_4','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS'};  % must be a subset of MatchedColums
        Args.FunIndStat              = {[1 3], [1 3], [1 3], [1 3], [1:8], [1:8], [1 3], [1 3], [1 3], [1 3], [1 3], [1 3], [1 3], [1 3]};
        
        Args.ColNamesAll             = {'MAG_CONV_2','MAGERR_CONV_2'};
        Args.MagCalibColName         = 'MAG_CONV_2';
        Args.MagCalibErrColName      = 'MAGERR_CONV_2';

        Args.unifiedSourcesCatalogArgs cell     = {};
       

    end
    
    % find all unique sources
    [Nepochs, Nfields] = size(Obj);
    if isempty(Args.JD)
        if isa(Obj, 'AstroImage')
            JD  = julday(Obj(:,1));     
        else
            JD  = (1:1:Nepochs).';
        end
    else
        JD = Args.JD;
    end

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
        %Summary(Ifields).ShiftX = median(diff(MatchedS(Ifields).Data.X,1,1), 2, 'omitnan');
        %Summary(Ifields).ShiftY = median(diff(MatchedS(Ifields).Data.Y,1,1), 2, 'omitnan');
    
        % relative photometry
        if Args.RelPhot
            [ResZP(Ifields), MatchedS(Ifields)] = lcUtil.zp_lsq(MatchedS(Ifields), 'MagField',Args.MagCalibColName, 'MagErrField',Args.MagCalibErrColName);
            
            % apply ZP to all Magnitudes...
            %FFU
            warning('Relative photometry implementation is not complete');
        else
            ResZP = [];
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