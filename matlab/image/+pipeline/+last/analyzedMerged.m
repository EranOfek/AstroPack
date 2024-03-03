function [Cand] = analyzedMerged(Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Feb) 
    % Example: pipeline.last.analyzedMerged('List',List);

    arguments
        Args.D                     = [];
        Args.List                  = [];
        Args.BasePath              = '/marvin/LAST.01.10.01';
        Args.FileTemp              = 'LAST*MergedMat*.hdf5';
        Args.YearTemp              = '20*';
        Args.MaxTimeBetweenVisits  = 500./86400;

        Args.Nvisit                = 2;
        Args.StepNvisit            = 1;

        Args.CropID                = (1:1:24).';

        Args.SearchRadius          = 3;
        Args.SearchRadiusUnits     = 'arcsec';

        Args.MagField              = 'MAG_BEST';
        Args.MagErrField           = 'MAGERR_PSF'
        Args.RAField               = 'RA';
        Args.DecField              = 'Dec';

        Args.searchFlaresArgs cell = {};
        Args.ThresholdPS           = 12;
        Args.BinSize               = 1;
        Args.NsigmaRMS             = 5;
        Args.MinDetRMS             = 15;
        Args.SNField               = 'SN_3';
        Args.MinFlareSN            = 8;
        Args.LimMagQuantile        = 0.99;
        Args.BadFlags              = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};

        Args.MaxChi2Dof            = 3;
    end




    RAD = 180./pi;

    OrbEl  = celestial.OrbitalEl.loadSolarSystem('merge');
    INPOP  = celestial.INPOP.init;

    if isempty(Args.D)
        D = pipeline.DemonLAST;
    else
        D = Args.D;
    end
    D.BasePath = Args.BasePath;

    if isempty(Args.List)
        List = D.prepListOfProcVisits('FileTemp',Args.FileTemp, 'YearTemp',Args.YearTemp);
    else
        List = Args.List;
    end

    AllCons = D.searchConsecutiveVisitsOfField('List',List, 'MaxTimeBetweenVisits',Args.MaxTimeBetweenVisits);

    % go over all consecutive fields
    Ncons = numel(AllCons);
    Icand = 0;
    for Icons=1:1:Ncons
        Nvisit = numel(AllCons{Icons});

        % for each group of visits
        for Igroup=1:Args.StepNvisit:Nvisit-1
            % group to analyze:
            Group = AllCons{Icons}(Igroup:Igroup+Args.Nvisit-1);
          
            Ng    = numel(Group);

            % for each CropID
            for Icrop=1:1:numel(Args.CropID)
                [Icons, Icrop]

                Ic = Args.CropID(Icrop);

                clear MS;
                for Ig=1:1:Ng
                    Icc = find(Group(Ig).CropID==Ic);
                    if ~isempty(Icc)
                        cd(Group(Ig).Path);
                        MS(Ig) = MatchedSources.read(Group(Ig).AllFiles{Icc});
                    end
                end

                % get file names 
                MS_FileNames = {MS.FileName};

                if numel(MS)==Args.Nvisit && ~isempty(fieldnames(MS(1).Data))
                    try
                    MS  = MS.mergeByCoo(MS(1), 'SearchRadius',Args.SearchRadius, 'SearchRadiusUnits',Args.SearchRadiusUnits);
                    catch
                        'a'
                    end
                    MS.FileName = MS_FileNames;
                    
                    % add MAG_BEST
                    MS.bestMag;

                    Rzp = lcUtil.zp_meddiff(MS, 'MagField',Args.MagField', 'MagErrField',Args.MagErrField);
                    MS.applyZP(Rzp.FitZP);
                    Nobs = numel(MS.JD);
    
                    % Analyze the MS object
    
                    % Detections
                    FlagDet = ~isnan(MS.Data.(Args.MagField));
                    Ndet    = sum(FlagDet);
    
                    % approximate limiting magnitude
                    LimMagQuantile = quantile(MS.Data.(Args.MagField),0.99,2);

                    % flags
                    [BadFlags] = searchFlags(MS, 'FlagsList',Args.BadFlags);
                    % sources for which all the detections are good:
                    Flag.GoodFlags = any(~BadFlags & FlagDet, 1);

                    % chi2/dof
                    MS.addSrcData;
                    Flag.Chi2 = MS.SrcData.PSF_CHI2DOF<Args.MaxChi2Dof;
                    Flag.GoodFlags = Flag.GoodFlags(:) & Flag.Chi2(:);

                    % search for flares
                    [FlagFlares,FF] = searchFlares(MS, Args.MagField, Args.searchFlaresArgs{:}, 'LimMag',median(LimMagQuantile));
                    Flag.Flares = FlagFlares.Any;
                    
                    FlagMinSN = max(MS.Data.(Args.SNField),[],1)>Args.MinFlareSN;
                    Flag.Flares = Flag.Flares(:) & FlagMinSN(:);
    
                    
    
                    % periodicity
                    FreqVec = timeSeries.period.getFreq(MS.JD, 'OverNyquist',0.4);
                    [FreqVec, PS, Flag.PS] = period(MS, FreqVec, 'MagField', Args.MagField, 'ThresholdPS',Args.ThresholdPS);
                    

                    % rms
                    ResRMS   = MS.rmsMag('MagField',Args.MagField, 'MinDetRmsVar',Args.MinDetRMS, 'Nsigma',Args.NsigmaRMS);
                    Flag.RMS = ResRMS.FlagVarPred;




                    % MeanMag = median(MS.Data.(Args.MagField), 1, 'omitnan');
                    % StdMag  = std(MS.Data.(Args.MagField), [], 1, 'omitnan');
                    % Fn0 = StdMag>1e-10;
                    % 
                    % 
                    % B = timeSeries.bin.binningFast([MeanMag(Fn0).', StdMag(Fn0).'], Args.BinSize, [NaN NaN], {'MidBin', @median, @tools.math.stat.std_mad, @numel});
                    % % Remove points with less than 5 measurments
                    % B = B(B(:,4)>5,:);
                    % 
                    % 
                    % In0  = find(B(:,2)<1e-4,1,'first');
                    % if ~isempty(In0)
                    %     try
                    %     if In0==size(B,1)
                    %         % 0 at faintestr mag
                    %         B(In0,2) = B(In0-1,2);
                    %     else
                    %         B(1:In0,2) = B(In0+1,2);
                    %     end
                    %     catch ME
                    %         'b'
                    %     end
                    % end
                    % 
                    % % adding a bright point
                    % B = [[B(1,1)-10, B(1, 2:end)]; B];
                    % 
                    % Bstd = B(:,2)./sqrt(Nobs);
                    % 
                    % StdThreshold = interp1(B(:,1), B(:,2)+Bstd.*Args.NsigmaRMS, MeanMag(:), 'linear','extrap');
                    % %MeanMagMean = interp1(B(:,1), B(:,2), MeanMag(:), 'linear','extrap');
                    % %MeanMagStd  = interp1(B(:,1), B(:,3), MeanMag(:), 'linear','extrap');
                    % Ndet        = MS.countNotNanEpochs('Field',Args.MagField);
                    % %Flag.RMS    = StdMag(:)>( MeanMagMean(:) + Args.Nsigma.*MeanMagStd(:)) & Ndet(:)>Args.MinDetRMS;
                    % Flag.RMS    = StdMag(:)>StdThreshold & Ndet(:)>Args.MinDetRMS;
    
                    %R=imUtil.calib.fit_rmsCurve(MeanMag, StdMag)

                    % poly std
                    [ResPolyHP, Flag.Poly] = fitPolyHyp(MS, 'PolyDeg',{0, (0:1), (0:1:2)}, 'ThresholdChi2',[Inf, chi2inv(normcdf([4.5 5 6],0,1),2)]);
                    
                    %ThresholdDeltaChi2 = chi2inv(normcdf(6,0,1),2);  % 6 sigma detection of parabola
                    %Flag.Poly = ResPolyHP(3).DeltaChi2>ThresholdDeltaChi2;

                    %ThresholdDeltaChi2 = chi2inv(normcdf(5,0,1),2);  % 5 sigma detection of parabola
                    %Flag.Poly = Flag.Poly & ResPolyHP(2).DeltaChi2>ThresholdDeltaChi2;

                    if sum(Flag.Poly(:) & Flag.GoodFlags(:))>10
                        warning('Too many slope-variables - removing all')
                        Flag.Poly = false(size(Flag.Poly));
                    end
    
    
                    Flag.Interesting = Flag.GoodFlags(:) & ...
                                      (Flag.Flares(:) | Flag.PS(:) | Flag.RMS(:) | Flag.Poly(:));
                    
                    if sum(Flag.Interesting)>0
                        Ind = find(Flag.Interesting);
    
                        Nvar = numel(Ind);
                        for Ivar=1:1:Nvar
                            Ind1 = Ind(Ivar);
    
                            % Basic data
                            VecRA  = MS.Data.(Args.RAField)(:,Ind1);
                            VecDec = MS.Data.(Args.DecField)(:,Ind1);
    
                            RA  = median(VecRA,1,'omitnan');
                            Dec = median(VecDec,1,'omitnan');
                            Mag = median(MS.Data.(Args.MagField)(:,Ind1),1,'omitnan');
                            MeanJD = median(MS.JD);
    
                            % Positional noise [deg]
                            StdRA  = std(VecRA,[],1,'omitnan').*cosd(Dec);
                            StdDec = std(VecDec,[],1,'omitnan');
    
    
                            % external catalogs

    
                            % asteroids
                            [AstTable] = searchMinorPlanetsNearPosition(OrbEl, MeanJD, RA, Dec, 10, 'INPOP',INPOP, 'ConeSearch',true);
                            if AstTable.sizeCatalog>0 || any([StdRA.*3600, StdDec.*3600]>0.1)
                                fprintf('Asteroid found or astrometric jitter\n');
                                %AstTable.Table
                            else
    
                                % Flares, PS, RMS, Poly
                                %[sum(Flag.Flares(:) & Flag.GoodFlags(:)),  sum(Flag.PS(:) & Flag.GoodFlags(:)), sum(Flag.RMS(:) & Flag.GoodFlags(:)), sum(Flag.Poly(:) & Flag.GoodFlags(:))]
                                [sum(Flag.Flares(Ind1) & Flag.GoodFlags(Ind1)),  sum(Flag.PS(Ind1) & Flag.GoodFlags(Ind1)), sum(Flag.RMS(Ind1) & Flag.GoodFlags(Ind1)), sum(Flag.Poly(Ind1) & Flag.GoodFlags(Ind1))]

                                sum(Flag.Interesting)
                                
                                [StdRA, StdDec].*3600                                

                                [SimbadURL]=VO.search.simbad_url(RA./RAD, Dec./RAD)

                                %plot(MS.Data.MAG_PSF(:,205)

                                Args.Plot = false;
                                if Args.Plot
                                    
                                    web(SimbadURL.URL)
                                    
        
                                    % Generate report
                                    figure(1)
                                    cla;
                                    %MS.plotRMS;
                                    semilogy(ResRMS.MeanMag, ResRMS.StdPar,'.')    
                                    hold on
                                    [~,SI] = sort(ResRMS.MeanMag);
                                    semilogy(ResRMS.MeanMag(SI), ResRMS.InterpMeanStd(SI),'-')      
                                    semilogy(ResRMS.MeanMag(SI), ResRMS.InterpMeanStd(SI)+ResRMS.InterpPredStd(SI).*Args.NsigmaRMS,'--')      
                                    plot(ResRMS.MeanMag(Ind1), ResRMS.StdPar(Ind1),'ro')
    
                                    %hold on;
                                    %plot(B(:,1),B(:,2),'b-');
                                    %plot(B(:,1),B(:,2)+Bstd.*Args.NsigmaRMS,'b-');
                                    %plot(MeanMag(Ind1), StdMag(Ind1),'ro')
    
            
                                    figure(2);
                                    cla;
                                    plot(FreqVec,PS(:,Ind1));
            
                                    figure(3);
                                    cla;
                                    plot(MS.JD(:), MS.Data.(Args.MagField)(:,Ind1), 'o','MarkerFaceColor','k');
                                    hold on;
                                    plot(MS.JD(:), LimMagQuantile, 'v')
                                    plot.invy
                                end

                                %lcUtil.reportMatchedSource(MS, Ind1, 'PS',PS, 'FreqVec',FreqVec)
                                
                                Icand = Icand + 1;
                                Cand(Icand).Flags = [Flag.Flares(Ind1), Flag.PS(Ind1), Flag.RMS(Ind1), Flag.Poly(Ind1)];
                                Cand(Icand).Simbad = SimbadURL.URL;
                                Cand(Icand).StdCoo = [StdRA, StdDec].*3600;
                                Cand(Icand).Icons  = Icons;
                                Cand(Icand).Icrop  = Icrop;
                                Cand(Icand).Ind1   = Ind1;
                                Cand(Icand).RA     = RA;
                                Cand(Icand).Dec    = Dec;
                                Cand(Icand).MeanJD = MeanJD;
                                Cand(Icand).Mag    = Mag;
                                Cand(Icand).ResRMS = ResRMS;
                                Cand(Icand).PS     = [FreqVec(:), PS(:,Ind1)];
                                Cand(Icand).ResPolyHP = ResPolyHP;
                                Cand(Icand).MS     = MS;


                                'a'
                            end
                        end
                    end

                    
                end

                %MS
            
            end


        end
    end

end
