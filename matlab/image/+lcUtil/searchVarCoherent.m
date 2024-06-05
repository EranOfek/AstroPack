function [Result, MS] = searchVarCoherent(Obj, Args)
    % Search for coherent (period/flaring) variable in a MatchedSources object.
    %     The search include:
    %     Merging MatchedSources objects by coordinates.
    %     Apply medDiffZP.
    %     Add BEST_MAG.
    %     Select good data points.
    %     Select sources with >MinNdet
    %     Calculate general stat (e.g., mean position).
    %     Calculate the power spectra.
    %     Search for flares/eclipses using: timeSeries.filter.runMeanFilter
    %     Calculate internal correlations in data.
    % Input  : - A MatchedSources object.
    %          * ...,key,val,... 
    %            See code for options.
    % Output : - A structure with statistical information about
    %            variability. The following fields are available:
    %            .
    %          - A MatchedSources with good targets only.
    % Author : Eran Ofek (2024 Jun) 
    % Example: R = lcUtil.searchVarCoherent(MS);

    arguments
        Obj
        
        
        Args.SearchRadius        = 3;
        Args.SearchRadiusUnits   = 'arcsec';

        Args.FieldMag            = 'MAG_BEST';
        Args.FieldMagErr         = 'MAGERR_PSF';

        Args.MedDiffZP logical   = true;

        Args.BadFlags            = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};

        Args.MinNdet             = 14;

        Args.FieldRA             = 'RA';
        Args.FieldDec            = 'Dec';
        Args.FieldChi2           = 'PSF_CHI2DOF';
        Args.FieldSN             = 'SN_3'

        Args.MinCorr                = 3./sqrt(14-3);
        Args.runMeanFilterArgs cell = {'Threshold',8};
    end


    Nobj = numel(Obj);

    MS   = Obj.mergeByCoo(Obj(1), 'SearchRadius',Args.SearchRadius, 'SearchRadiusUnits',Args.SearchRadiusUnits);
                  
    % add MAG_BEST
    MS.bestMag;

    if Args.MedDiffZP
        Rzp = lcUtil.zp_meddiff(MS, 'MagField',Args.FieldMag', 'MagErrField',Args.FieldMagErr);
        MS.applyZP(Rzp.FitZP);
    end
                   
    if ~isempty(MS)
        Result.LimMagQuantile = quantile(MS.Data.(Args.FieldMag),0.99,2);
    
        % Flags
        % set to NaN photometry with bad flags
        MSn = MS.setBadPhotToNan('BadFlags',Args.BadFlags, 'MagField',Args.FieldMag, 'CreateNewObj',true);
        
        % Remove sources with NdetGood<MinNdet
        NdetGood = sum(~isnan(MSn.Data.(Args.FieldMag)), 1);
        Fndet    = NdetGood>Args.MinNdet;

        Result.Ndet     = sum(Fndet);
        if sum(Fndet)>0
            % good stars found

            MSn      = MSn.selectBySrcIndex(Fndet, 'CreateNewObj',false);
            MS       = MS.selectBySrcIndex(Fndet, 'CreateNewObj',false);
            
            % General stat
            MSn.addSrcData;
            Result.MeanRA   = MSn.SrcData.(Args.FieldRA);
            Result.MeanDec  = MSn.SrcData.(Args.FieldDec);
            Result.StdRA    = std(MSn.Data.(Args.FieldRA), [], 1, 'omitnan').*cosd(Result.MeanDec);
            Result.StdDec   = std(MSn.Data.(Args.FieldDec), [], 1, 'omitnan');

            Result.MeanChi2 = MSn.SrcData.(Args.FieldChi2);
            Result.MeanSN   = MSn.SrcData.(Args.FieldSN);
            Result.NdetGood = sum(~isnan(MSn.Data.(Args.FieldMag)), 1);
            Result.Ndet     = sum(~isnan(MS.Data.(Args.FieldMag)), 1);
    
            % periodicity
            FreqVec = timeSeries.period.getFreq(MSn.JD, 'OverNyquist',0.4, 'MaxFreq',2000);
            [FreqVec, PS] = period(MSn, FreqVec, 'MagField', Args.FieldMag);
            [Result.MaxPS, MaxI] = max(PS, [], 1);
            Result.MaxFreq = FreqVec(MaxI).';
    
            % flares and transits
            ResFilt = timeSeries.filter.runMeanFilter(MSn.Data.(Args.FieldMag), Args.runMeanFilterArgs{:});
            Result.FlagRunMean = any(ResFilt.FlagCand, 1);
    
            % correlations
            ResCorr = MSn.corrFields([], 'Field1',Args.FieldMag, 'Field2',Args.FieldRA, 'Type','pairs');
            Result.CorrMagRA = ResCorr.Corr;
            Result.CorrPvMagRA = ResCorr.PVal;
    
            ResCorr = MSn.corrFields([], 'Field1',Args.FieldMag, 'Field2',Args.FieldDec, 'Type','pairs');
            Result.CorrMagDec = ResCorr.Corr;
            Result.CorrPvMagDec = ResCorr.PVal;
    
            ResCorr = MSn.corrFields([], 'Field1',Args.FieldMag, 'Field2',Args.FieldChi2, 'Type','pairs');
            Result.CorrMagChi2 = ResCorr.Corr;
            Result.CorrPvMagChi2 = ResCorr.PVal;
    
            ResCorr = MSn.corrFields([], 'Field1',Args.FieldMag, 'Field2',Args.FieldMag, 'Type','all', 'DiagonalNaN',true);
            Result.NcorrAboveTh = sum(triu(ResCorr.Corr>Args.MinCorr, 1), 1);
    
            %Result.CorrMagMag   = ResCorr.Corr;
            %Result.CorrPvMagMag = ResCorr.PVal;
        end
            
        
    else

        Result.LimMagQuantile = NaN;   
    end

    

end
