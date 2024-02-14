function [Result] = analyzedMerged(Args)
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

        Args.MagField              = 'MAG_PSF';
        Args.MagErrField           = 'MAGERR_PSF'

        Args.searchFlaresArgs cell = {};
        Args.ThresholdPS           = 12;
        Args.BinSize               = 1;
        Args.Nsigma                = 6;
        Args.MinDetRMS             = 15;
        Args.SNField               = 'SN_3';
        Args.MinFlareSN            = 8;
        Args.LimMagQuantile        = 0.99;
        Args.BadFlags              = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};
    end

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
    for Icons=1:1:Ncons
        Nvisit = numel(AllCons{Icons});

        % for each group of visits
        for Igroup=1:Args.StepNvisit:Nvisit
            % group to analyze:
            Group = AllCons{Icons}(Igroup:Igroup+Args.Nvisit-1);
            Ng    = numel(Group);

            % for each CropID
            for Icrop=1:1:numel(Args.CropID)
                [Icons, Icrop]

                Ic = Args.CropID(Icrop);

                for Ig=1:1:Ng
                    Icc = find(Group(Ig).CropID==Ic);
                    cd(Group(Ig).Path);
                    MS(Ig) = MatchedSources.read(Group(Ig).AllFiles{Icc});
                end

                MS  = MS.mergeByCoo(MS(1), 'SearchRadius',Args.SearchRadius, 'SearchRadiusUnits',Args.SearchRadiusUnits);
                Rzp = lcUtil.zp_meddiff(MS, 'MagField',Args.MagField', 'MagErrField',Args.MagErrField);
                MS.applyZP(Rzp.FitZP);


                % Analyze the MS object

                % flags
                [BadFlags] = searchFlags(MS, 'FlagsList',Args.BadFlags);
                Flag.GoodFlags = ~any(BadFlags, 1);

                % search for flares
                [FlagFlares,FF] = searchFlares(MS, Args.MagField, Args.searchFlaresArgs{:});
                Flag.Flares = FlagFlares.Any;
                
                FlagMinSN = max(MS.Data.(Args.SNField),[],1)>Args.MinFlareSN;
                Flag.Flares = Flag.Flares(:) & FlagMinSN(:);

                LimMagQuantile = quantile(MS.Data.(Args.MagField),0.99,2);

                % periodicity
                FreqVec = timeSeries.period.getFreq(MS.JD, 'OverNyquist',0.4);
                [FreqVec, PS] = period(MS, FreqVec, 'MagField', Args.MagField);
                MaxPS = max(PS,[],1);
                Flag.PS = MaxPS>Args.ThresholdPS;

                % rms
                MeanMag = median(MS.Data.(Args.MagField), 1, 'omitnan');
                StdMag  = std(MS.Data.(Args.MagField), [], 1, 'omitnan');
                B = timeSeries.bin.binning([MeanMag(:), StdMag(:)], Args.BinSize, [NaN NaN], {'MidBin', @mean, @tools.math.stat.rstd, @numel});
                MeanMagMean = interp1(B(:,1), B(:,2), MeanMag(:), 'linear','extrap');
                MeanMagStd  = interp1(B(:,1), B(:,3), MeanMag(:), 'linear','extrap');
                Ndet        = MS.countNotNanEpochs('Field',Args.MagField);
                Flag.RMS    = StdMag(:)>( MeanMagMean(:) + Args.Nsigma.*MeanMagStd(:)) & Ndet(:)>Args.MinDetRMS;

                % poly std


                % asteroids
                

                % external catalogs



                Flag.Interesting = Flag.GoodFlags(:) & ...
                                  (Flag.Flares(:) | Flag.PS(:) | Flag.RMS(:));
                
                if sum(Flag.Interesting)>0
                    [sum(Flag.Flares(:) & Flag.GoodFlags(:)),  sum(Flag.PS(:) & Flag.GoodFlags(:)), sum(Flag.RMS(:) & Flag.GoodFlags(:))]
                    sum(Flag.Interesting)
                    Ind = find(Flag.Interesting)
                    %plot(MS.Data.MAG_PSF(:,205)

                    Nvar = numel(Ind);
                    for Ivar=1:1:Nvar
                        Ind1 = Ind(Ivar);

                        % Generate report
                        figure(1)
                        cla;
                        MS.plotRMS;
                        hold on;
                        plot(MeanMag(Ind1), StdMag(Ind1),'ro')

                        figure(2);
                        cla;
                        plot(FreqVec,PS(:,Ind1));

                        figure(3);
                        cla;
                        plot(MS.JD(:), MS.Data.(Args.MagField)(:,Ind1), 'o','MarkerFaceColor','k');
                        hold on;
                        plot(MS.JD(:), LimMagQuantile, 'v')
                        plot.invy

                        'a'
                    end


                    
                end

                %MS
            
            end


        end
    end

end
