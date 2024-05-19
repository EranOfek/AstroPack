% SearchMatchedSources
%   A class for searching and inspecting variable stars in MatchedSources
%   objects.
%
% Example: SMS = pipeline.last.SearchMatchedSources;
%          SMS.populateAllConsecutive;
%          % SMS.prepConsecutive(Icons, Igroup, Icrop)
%
%          [Cand, Summary] = SMS.findVariableAll('Plot',false);
%          [Cand, Summary] = SMS.findVariableAll('Nvisit',9,'Nstep',3,'Plot',false);
%
%          FF=[C.FlagPS]; 
%          FFI=find(FF);
%          SMS.plotVar(C(FFI(1)))



classdef SearchMatchedSources < Component
    properties
        AllConsecutive
        MS    % Matched Sources object to search (latest)

        Cand  % candidaye found
    end

    properties (Hidden)
        INPOP     = celestial.INPOP.init;
        OrbEl     = celestial.OrbitalEl.loadSolarSystem('merge');
    end
    
  
   
    methods % constructor
        
    end


    methods % prep/read consecutive MatchedSources files
        function populateAllConsecutive(Obj, List, Args)
            % populate the AllConsecutive property
            %   Use pipeline.DemonLAST/searchConsecutiveVisitsOfField
            %   to search recursively for all proc/visits directories in a dir
            %   tree and return a list of all fields that were observed
            %   consecutively.
            % Input  : - self.
            %          - Optional List created by
            %            pipeline.DemonLAST/prepListOfProcVisits
            %            If empty, then create.
            %            Default is [].
            %          * ...,key,val,...
            %            See code.
            % Ouput : - A SearchMatchedSources object in which the
            %           AllConsecutive property is populated.
            % Author : Eran Ofek (Mar 2024)
            % Example: SMS = SearchMatchedSources;
            %          SMS.populateAllConsecutive;

            arguments
                Obj
                List           = [];
                Args.BasePath  = '/marvin/LAST.01.01.01';
                Args.FileTemp  = 'LAST*MergedMat*.hdf5';
                Args.YearTemp  = '20*';
            end

            D = pipeline.DemonLAST;
            D.BasePath  = Args.BasePath;

            if isempty(List)
                List = D.prepListOfProcVisits;
            end

            Obj.AllConsecutive = D.searchConsecutiveVisitsOfField('List',List);
        end

        function [Obj, LimMagQuantile]=prepConsecutive(Obj, Icons, Igroup, Icrop, Args)
            % Load one consecutive MatchedSources objects
            %   Given a SearchMatchedSources object in which the AllConsecutive
            %   property is populated, load the MatchedSources files by
            %   they index in the AllConsecutive.
            %   The indices include:
            %       Icons - The index of the AllConsecutive cell.
            %       Igroup - The index of the group of length 'Nvisit'.
            %       Icrop - CropID
            % Input  : - self.
            %          - (Icons) The index of the AllConsecutive cell.
            %               Default is 1.
            %          - (Igroup) The index of the group of length 'Nvisit'.
            %               Default is 1.
            %          - (Icrop) CropID. Default is 1.
            %          * ...,key,val,...
            %            See code.
            % Output : - A SearchMatchedSources object in which the MS
            %            property is populated with the merged (of length
            %            Nvisit) MatchedSources object.
            %          - Vector of estimated (based on quantile) lim. mag
            %            in each epoch.
            % Author : Eran Ofek (Mar 2024)
            % Example: SMS = SearchMatchedSources;
            %          SMS.populateAllConsecutive;
            %          SMS.prepConsecutive;

            arguments
                Obj
                Icons          = 1;
                Igroup         = 1;
                Icrop          = 1;

                Args.Nvisit    = 2;
                Args.Nstep     = 1;
                
                Args.SearchRadius          = 3;
                Args.SearchRadiusUnits     = 'arcsec';

                Args.MagField              = 'MAG_BEST';
                Args.MagErrField           = 'MAGERR_PSF'
                Args.LimMagQuantile        = 0.99;
            end

            PWD = pwd;

            Group = Obj.AllConsecutive{Icons};
            Ng    = numel(Group);
            if Igroup > (Ng - Args.Nvisit + 1)
                % Out of bound
                %Status = false;
                Obj.MS = [];
            else
                %Status = true;

                IndGroup = (Igroup:Args.Nstep:Igroup+Args.Nvisit-1);
                Ngroup   = numel(IndGroup);

                MSm = MatchedSources;
                for Igroup=1:1:Ngroup
                    SingleFile = Obj.AllConsecutive{Icons}(IndGroup(Igroup));
                    IndCrop = find(SingleFile.CropID == Icrop);
                    
                    if ~isempty(IndCrop)
                        cd(SingleFile.Path);
    
                      
                        MSm(Igroup) = MatchedSources.read(SingleFile.AllFiles{IndCrop});
                    end % if ~isempty(IndCrop)
                end % for Igroup=1:1:Ngroup

                if numel(MSm)~=Args.Nvisit || any(MSm.isemptyProperty('Nepoch'))
                    %Status = false;
                    Obj.MS = [];
                else
                    MS_FileNames = {MSm.FileName};
                    Obj.MS  = MSm.mergeByCoo(MSm(1), 'SearchRadius',Args.SearchRadius, 'SearchRadiusUnits',Args.SearchRadiusUnits);
                  
                    Obj.MS.FileName = MS_FileNames;

                    % add MAG_BEST
                    Obj.MS.bestMag;

                    Rzp = lcUtil.zp_meddiff(Obj.MS, 'MagField',Args.MagField', 'MagErrField',Args.MagErrField);
                    Obj.MS.applyZP(Rzp.FitZP);
                   

                end  % if numel(MSm)~=Args.Nvisit

                % get file names 
                
            end % if Igroup > (Ng - Args.Nvisit + 1)

            cd(PWD);

            if nargout>1
                % approximate limiting magnitude
                if ~isempty(Obj.MS)
                    LimMagQuantile = quantile(Obj.MS.Data.(Args.MagField),0.99,2);
                else
                    LimMagQuantile = NaN;
                end
            end

        end

    end


    methods % variable stars search and utilities
        function [Cand, Summary, Rpt] = findVariableAll(Obj, Args)
            % Go over all files in the AllConsecutive property and search for variable sources.
            %   The function works by making the following calls:
            %   prepConsecutive
            %   findVaribleMS
            %   Next for variable candidates it runs:
            %   flagCorr
            %   and the plot* functions.
            % Input  : - A SearchMatchedSources object.
            %          * ...,key,val,...
            %            'Nvisit' - Number of sucessive visits to analyze.
            %                   Default is 2.
            %            'Nstep' - Number of steps between lists.
            %                   Default is 1.
            %            'Ncrop' - Number of sub images (crops).
            %                   Default is 24.
            %            'Plot' - Plot LCs of candidates.
            %                   Default is true.
            %            
            %            See code for additional arguments
            %
            % Output : - A structure array of variable candidates.
            %            Element per candidate.
            %          - Summary generated by findVariableMS
            % Author : Eran Ofek (Mar 2024)
            % Example: SMS = SearchMatchedSources;
            %          SMS.populateAllConsecutive;
            %          [Cand, Summary, Rpt] = SMS.findVariableAll('Nvisit',3,'Plot',true);


            arguments
                Obj
                Args.Nvisit    = 2;
                Args.Nstep     = 1;
                Args.Ncrop     = 24;
                
                Args.MinDet                = 85;
                Args.SearchRadius          = 3;
                Args.SearchRadiusUnits     = 'arcsec';

                Args.MagField              = 'MAG_BEST';
                Args.MagErrField           = 'MAGERR_PSF'
                Args.LimMagQuantile        = 0.99;

                Args.RAField               = 'RA';
                Args.DecField              = 'Dec';

                Args.Plot logical          = true;
            end

            RAD = 180./pi;

            Ncons = numel(Obj.AllConsecutive);
            Icand = 0;
            Cand  = [];
            for Icons=1:1:Ncons
                Ng = numel(Obj.AllConsecutive{Icons});
                for Ig=1:1:Ng
                    [Icons, Ncons, Ig, Ng]

                    for Icrop=1:1:Args.Ncrop
                        [Obj, LimMagQuantile] = Obj.prepConsecutive(Icons, Ig, Icrop, 'Nvisit',Args.Nvisit,...
                                                                                      'Nstep',Args.Nstep,...
                                                                                                  'SearchRadius',Args.SearchRadius,...
                                                                                                  'SearchRadiusUnits',Args.SearchRadiusUnits,...
                                                                                                  'MagField',Args.MagField,...
                                                                                                  'MagErrField',Args.MagErrField,...
                                                                                                  'LimMagQuantile',Args.LimMagQuantile);
                        if ~isempty(Obj.MS)
                            %
                            if (median(diff(Obj.MS.JD)).*86400)>18 && max(diff(Obj.MS.JD))<30

                                %
                                [Flag, FlagInfo, Summary] = findVariableMS(Obj);
    
                                FlagComb = FlagInfo.N(:)>Args.MinDet & Flag.FlagGood(:) & (Flag.PS(:) | Flag.RMS(:) | Flag.Poly(:) | Flag.RunMeanFilt(:));
        
                                if any(FlagComb)
                                    IndCand    = find(FlagComb);
                                    Ncand      = numel(IndCand);
    
                                    
    
                                    for I=1:1:Ncand
                                        IndSrc = IndCand(I);
    
                                        [FlagGood, ResPhotAstCorr] = flagCorr(Obj, IndSrc);
    
                                        if FlagGood
    
                                            RA  = median(Obj.MS.Data.(Args.RAField)(:,IndSrc), 1, 'omitnan');
                                            Dec = median(Obj.MS.Data.(Args.DecField)(:,IndSrc), 1, 'omitnan');
    
                                            Icand = Icand + 1;
                                            Cand(Icand).IndSrc          = IndSrc;
                                            Cand(Icand).NcandInSubImage = Ncand;
                                            Cand(Icand).Flag        = Flag;
                                            Cand(Icand).MS          = Obj.MS;
                                            Cand(Icand).FlagGood    = Flag.FlagGood(IndSrc);
                                            Cand(Icand).FlagPS      = Flag.PS(IndSrc);
                                            Cand(Icand).FlagRMS     = Flag.RMS(IndSrc);
                                            Cand(Icand).FlagPoly    = Flag.Poly(IndSrc);
                                            Cand(Icand).MaxPS       = Summary.MaxPS(IndSrc);
                                            Cand(Icand).MaxFreq     = Summary.MaxFreq(IndSrc);
                                            Cand(Icand).RA          = RA;
                                            Cand(Icand).Dec         = Dec;
        
                                            if Args.Plot
                                                
                                                % if Icand==1
                                                %     Fig = figure;
                                                % else
                                                %     clf;
                                                % end
                                                FigLC  = Obj.plotLC(IndSrc);
                                                FigRMS = Obj.plotRMS(IndSrc, 'NsigmaPredRMS',7);
                                                H = gca;
                                                H.YLim(1) = 1e-3;
                                                [FigPS, FigPh] = Obj.plotPS(IndSrc);
                                                drawnow;
    
                                                [SimbadURL]=VO.search.simbad_url(RA./RAD, Dec./RAD)
                                                SDSS_URL=VO.SDSS.navigator_link(RA./RAD, Dec./RAD);
                                                PS1_URL=VO.PS1.navigator_link(RA./RAD,Dec./RAD);
                                                AC=catsHTM.cone_search('GAIADR3',RA./RAD, Dec./RAD, 3,'OutType','AstroCatalog');
    
    
                                                AbsMag = AC.Table.phot_bp_mean_mag - (5.*log10(1000./AC.Table.Plx)-5);
                                                Color  = AC.Table.phot_bp_mean_mag - AC.Table.phot_rp_mean_mag;
                                                fprintf('AbsMag: %5.2f    Color: %5.2f\n', AbsMag,Color);
    
                                                if AbsMag>8
                                                    'a'
                                                end
    
                                                Args.Report = true;
                                                if Args.Report
                                                    if Icand==1
                                                        import mlreportgen.report.* 
                                                        import mlreportgen.dom.* 
                                                        Rpt = Report('/home/eran/Variables','pdf'); 
                                                        tp = TitlePage; 
                                                        tp.Title = 'Variables'; 
                                                        tp.Subtitle = '';
                                                        tp.Author = 'SearchMatchedSources/findVariableAll'; 
                                                        
                                                        append(Rpt,tp); 
                                                        append(Rpt,TableOfContents); 
        
                                                        ch1 = Chapter; 
                                                        ch1.Title = 'Variable candidates'; 
                                                    end
                                                    %---
                                                    
                                                    sec1 = Section; 
                                                    %br = PageBreak();
                                                    %append(sec1,br);
    
                                                    FlagsType = sprintf('%d %d %d %d', Flag.PS(IndSrc),  Flag.RMS(IndSrc), Flag.Poly(IndSrc), Flag.RunMeanFilt(IndSrc));
                                                    sec1.Title = sprintf('LAST J%s%s - %s',celestial.coo.convertdms(RA,'d','SH'), celestial.coo.convertdms(Dec,'d','SD'), FlagsType);
    
                                                    para = Text(sprintf('RA=%10.6f Dec=%10.6f\n Lim Mag: %5.1f\n MeanMag=%6.3f   StdMag=%6.3f\n AbsMag=%6.2f   Color=%6.2f\n',...
                                                        RA, Dec,...
                                                        median(LimMagQuantile,1,'omitnan'),...
                                                        mean(Obj.MS.Data.(Args.MagField)(:,IndSrc), 1, 'omitnan'),...
                                                        std(Obj.MS.Data.(Args.MagField)(:,IndSrc), [], 1, 'omitnan'),...
                                                        AbsMag, Color)); 
                                                    append(sec1,para) 
    
                                                    fig = Figure(gcf);
                                                    fig.Snapshot.Height  = '6in'; 
                                                    fig.Snapshot.Width   = '7.5in'; 
                                                    fig.Snapshot.Caption = sprintf('');
                                                    append(sec1,fig); 
                                                    append(ch1, sec1);
    
                                                    %clf;  % close figure
                                                    close;
    
                                                end
    
                                                %web(SimbadURL.URL)
                                                if Flag.RunMeanFilt(IndSrc)
                                                'a'
                                                end
                                                
                                                % if Icand==300
                                                %     append(Rpt, ch1);
                                                % 
                                                %     close(Rpt)
                                                %     return;
                                                % end
                                            end
                                        end
                                    end
                                end
                                

                            end
                        end
                    end
                end
            end

            if Args.Report
                append(Rpt, ch1);
                
                close(Rpt)
                
                %rptview(Rpt)
            end


        end


        function [Flag, FlagInfo, Summary]=findVariableMS(Obj, Args)
            % Find variable stars in a single MatchedSources object.
            %   The function do the the following steps:
            %   Set bad flags to NaN using: setBadPhotToNan
            %   Flag stars by data quality using: flagByQuality
            %       Remove soources with bad photometry.
            %   Select by power spectrum thresholding.
            %   Select by rms plot thresholding.
            %   Select by polynomial fitting hypothesis testing.
            %
            % Input  : - A SearchMatchedSources object.
            %          * ...,key,val,...
            %            
            %           
            %            See code for additional arguments
            %
            % Output : - A structure array of variable candidates.
            %            Element per candidate.
            %          - Summary generated by findVariableMS
            % Author : Eran Ofek (Mar 2024)
            % Example: SMS = SearchMatchedSources;
            %          SMS.populateAllConsecutive;
            %          [Cand, Summary] = SMS.findVariableAll('Nvisit',3,'Plot',true);

            arguments
                Obj
               
                Args.BadFlags              = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};

                Args.MagField              = 'MAG_BEST';
                Args.MaxChi2Dof            = 3;

                Args.MinNdet               = 2;
                Args.MaxOverlapFrac        = 0.5;

                Args.ThresholdPS           = 12;

                Args.NsigmaPredRMS         = 10;
                Args.NsigmaStdRMS          = 7;
                Args.MinDetRMS             = 15;
                Args.MinNptRMS             = 10;
                Args.MinRMS4poly           = 5;
            end

            % set to NaN photometry with bad flags
            Obj.setBadPhotToNan('BadFlags',Args.BadFlags, 'MagField',Args.MagField);

            
            [Flag.FlagGood, FlagInfo]=flagByQuality(Obj, 'MagField',Args.MagField, 'MaxChi2Dof',Args.MaxChi2Dof, 'MinNdet',Args.MinNdet, 'MaxOverlapFrac',Args.MaxOverlapFrac);
           
            % periodicity
            FreqVec = timeSeries.period.getFreq(Obj.MS.JD, 'OverNyquist',0.4, 'MaxFreq',2000);
            [FreqVec, PS, Flag.PS] = period(Obj.MS, FreqVec, 'MagField', Args.MagField, 'ThresholdPS',Args.ThresholdPS);
                    
            % rms
            ResRMS   = Obj.MS.rmsMag('MagField',Args.MagField, 'MinDetRmsVar',Args.MinDetRMS, 'NsigmaPred',Args.NsigmaPredRMS, 'NsigmaStd',Args.NsigmaStdRMS, 'MinNpt',Args.MinNptRMS);
            Flag.RMS = ResRMS.FlagVarPred;


            % std filter
            % FilterBox = true(5,1);
            % IsNaN   = isnan(Obj.MS.Data.MAG_BEST);
            % IsNaN   = imdilate(IsNaN, FilterBox);
            % StdFilt = stdfilt(Obj.MS.Data.MAG_BEST, FilterBox);
            % StdFilt(IsNaN) = NaN;
            % StdVec  = std(Obj.MS.Data.MAG_BEST,[],1, 'omitnan');

            % poly std
            [ResPolyHP, Flag.Poly] = fitPolyHyp(Obj.MS, 'PolyDeg',{0, (0:1), (0:1:2)}, 'ThresholdChi2',[Inf, chi2inv(normcdf([5 6 7],0,1),2)]);
            Flag.Poly = Flag.Poly(:);
            Flag.Poly(ResRMS.NsigmaStd<Args.MinRMS4poly & Flag.Poly) = false;

            % running mean filter
            RMFilt = timeSeries.filter.runMeanFilter(Obj.MS.Data.(Args.MagField));
            Flag.RunMeanFilt = any(RMFilt.FlagCand, 1);

            Summary.FreqVec   = FreqVec;
            Summary.PS        = PS;
            [MaxPS, MaxInd]   = max(PS,[],1);
            Summary.MaxPS     = MaxPS;
            Summary.MaxFreq   = FreqVec(MaxInd);
            Summary.ResPolyHP = ResPolyHP;
            Summary.ResRMS    = ResRMS;
        end


        function [FlagGood, FlagInfo]=flagByQuality(Obj, Args)
            % Select stars with good quality photometry
            %   Select stars with good photometric quality.
            %   Including chi2 of PSF fitting, jitter in coordinates, and
            %   difference between APER2 and APER3.
            % Input  : - A searchMatchedSources object.
            %          * ...,key,val,...
            %            'MaxAstStd' - Maximum astrometric std allowed.
            %                   Default is 0.4./3600 [deg].
            %            'AperPhotPair' - Cell array of aperture phot. for
            %                   which to calculate difference.
            %                   Default is {'MAG_APER_2','MAG_APER_3'}
            %            'AperPhotPairQuantile' - Lower and upper quantile
            %                   for aper phot, diff. selection.
            %                   Default is [0.02 0.98].
            %
            %            See code for additional arguments.
            %
            % Output : - A vector of logicals indicating stars with good
            %            photometric data.
            %          - Structure with specific flags data.
            % Author : Eran Ofek (Mar 2024)
            

            arguments
                Obj
                
                Args.MagField              = 'MAG_BEST';
                Args.MaxChi2Dof            = 3;

                Args.MinNdet               = 2;
                Args.MaxOverlapFrac        = 0.5;
                
                Args.RAField               = 'RA';
                Args.DecField              = 'Dec';
                Args.MaxAstStd             = 0.5./3600;  % deg

                Args.AperPhotPair          = {'MAG_APER_2','MAG_APER_3'};
                Args.AperPhotPairQuantile  = [0.02 0.98];
            end

            % Detections
            FlagDet = ~isnan(Obj.MS.Data.(Args.MagField));
            Ndet    = sum(FlagDet, 1);

            FlagInfo.N    = Ndet;
            FlagInfo.Ndet = Ndet>=Args.MinNdet;
            
            % remove near edge - even one
            FlagNearEdge = searchFlags(Obj.MS, 'FlagsList',{'NearEdge'});
            FlagAll.NotNearEdge = all(~FlagNearEdge,1);

            % remove ovelap- if > Args.MaxOverlapFrac
            FlagOverlap = searchFlags(Obj.MS, 'FlagsList',{'Overlap'});
            FlagInfo.NotOverlap = sum(FlagOverlap, 1)./Obj.MS.Nepoch < Args.MaxOverlapFrac;

            % chi2/dof
            Obj.MS.addSrcData;
            FlagInfo.Chi2 = Obj.MS.SrcData.PSF_CHI2DOF<Args.MaxChi2Dof;

            % astrometric jitter
            Dec = median(Obj.MS.Data.(Args.DecField), 1, 'omitnan');
            % Positional noise [deg]
            StdRA  = std(Obj.MS.Data.(Args.RAField), [],1,'omitnan').*cosd(Dec);
            StdDec = std(Obj.MS.Data.(Args.DecField), [],1,'omitnan');
            FlagInfo.NoAstJitter = ~(StdRA>Args.MaxAstStd | StdDec>Args.MaxAstStd);
   
            % aperture photometry diff
            DiffAper = median(Obj.MS.Data.(Args.AperPhotPair{1}) - Obj.MS.Data.(Args.AperPhotPair{2}), 1, 'omitnan');
            QR = quantile(DiffAper, Args.AperPhotPairQuantile);
            FlagInfo.AperDiff = DiffAper>QR(1) & DiffAper<QR(2);


            % summarize all flags
            FlagGood  = FlagInfo.Ndet(:) & FlagAll.NotNearEdge(:) & FlagInfo.NotOverlap(:) & FlagInfo.Chi2(:) & FlagInfo.NoAstJitter(:) & FlagInfo.AperDiff(:);


        end

        function [FlagGood, Res] = flagCorr(Obj, IndSrc, Args)
            % Look for correlations for a given star in MatchedSources object
            %   Specifically will search for correlations between
            %       astrometry + photometry
            %       chi^2 + photometry
            %       background + photometry
            %   and return logical flags of good sources (no correlations).
            % Input  : - A SearchMatchedSources object, with populated MS.
            %          - Star index in MS.
            %          * ...,key,val,...
            %            'PosProbThresh' - Correlation threshold for
            %                   position. Default is 0.9.
            %            'Chi2ProbThresh' - Correlation threshold for chi^2
            %                   Default is 0.99.
            %            'BackProbThresh' - Correlation threshold gor back.
            %                   Default is 0.99.
            %
            %            See code for additional arguments.
            %
            % Output : - A scalar logical indicating in the star
            %            photometry is good.
            %          - Structure with correlation results.
            % Author : Eran Ofek (Mar 2024)


            arguments
                Obj
                IndSrc
                Args.MagField              = 'MAG_BEST';
                Args.RAField               = 'RA';
                Args.DecField              = 'Dec';
                Args.PosProbThresh         = 0.9;

                Args.Chi2Field             = 'PSF_CHI2DOF';
                Args.Chi2ProbThresh        = 0.99;

                Args.BackField             = 'BACK_ANNULUS';
                Args.BackProbThresh        = 0.99;
            end

            [Res.C_RA, Res.Pc_RA]    = tools.math.stat.corrsim(Obj.MS.Data.(Args.MagField)(:,IndSrc), Obj.MS.Data.(Args.RAField)(:,IndSrc));
            [Res.C_Dec,Res.Pc_Dec]   = tools.math.stat.corrsim(Obj.MS.Data.(Args.MagField)(:,IndSrc), Obj.MS.Data.(Args.DecField)(:,IndSrc));
            [Res.C_Chi2,Res.Pc_Chi2] = tools.math.stat.corrsim(Obj.MS.Data.(Args.MagField)(:,IndSrc), Obj.MS.Data.(Args.Chi2Field)(:,IndSrc));
            [Res.C_Back,Res.Pc_Back] = tools.math.stat.corrsim(Obj.MS.Data.(Args.MagField)(:,IndSrc), Obj.MS.Data.(Args.BackField)(:,IndSrc));

            if Res.Pc_RA>Args.PosProbThresh || Res.Pc_Dec>Args.PosProbThresh || Res.Pc_Chi2>Args.Chi2ProbThresh || Res.Pc_Back>Args.BackProbThresh || ...
                Res.Pc_RA<(1-Args.PosProbThresh) || Res.Pc_Dec<(1-Args.PosProbThresh) || Res.Pc_Chi2<(1-Args.Chi2ProbThresh) || Res.Pc_Back<(1-Args.BackProbThresh)
               
                FlagGood = false;
            else
                FlagGood = true;
            end
        end


        function Obj=setBadPhotToNan(Obj, Args)
            % set to NaN photometry with bad flags
            % Input  : - A searchMatchedSources object.
            %          * ...,key,val,...
            %            'BadFlags' - A cell array of flags that if
            %                   present, then photometry will be replaced with
            %                   NaN. Default is {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'}
            %            'MagField' - Magnitude field to set to NaN.
            %                   Default is 'MAG_BEST'.
            % Output : - A searchMatchedSources object, in which the mag.
            %            field in the MS property is modified.
            % Author : Eran Ofek (Mar 2024)

            arguments
                Obj
                Args.BadFlags              = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};
                Args.MagField              = 'MAG_BEST';
            end

            [BadFlags] = searchFlags(Obj.MS, 'FlagsList',Args.BadFlags);
            Obj.MS.Data.(Args.MagField)(BadFlags) = NaN;
        end

    end
    
    methods % orphans
        function Result=searchOrphansMS(Obj, Args)
            % Search orphans in MatchedSources object
            %   Orphans are defined as sources with <=MinNdet good detections
            %   possibly consecutive.
            %   For such sources some mean properties are returned.
            % Input  : - An pipeline.last.SearchMatchedSources object.
            %          * ...,key,val,...
            %            See code.
            % Output : - A structure array, with element per MatchedSources
            %            element, and the following fields:
            %            .Ind - Vector of indices of orphan candidates.
            %            .Norphan - Number of orphans.
            %            .Ndet - Vector of number of detections.
            %            .JD - Vector of mean JD per orphan.
            %            .RA - Vector of mean RA per orphan.
            %            .Dec - Vector of mean Dec per orphan.
            %            .SN - Vector of mean SN per orphan.
            %            .Mag - Vector of mean Mag per orphan.
            %            .DistGAIA -
            %            .N_GAIA - 
            %            .Bp - 
            %            .Rp - 
            % 
            % Author : Eran Ofek (Mar 2024)
            % Example: RR=SMS.searchOrphans
            %
            %          SMS=pipeline.last.SearchMatchedSources;
            %          SMS.populateAllConsecutive;         
            %          SMS.prepConsecutive(1,1,4,'Nvisit',1)
            %          RR=SMS.searchOrphansMS;
            %          

            
            arguments
                Obj
                Args.MinNdet        = 3;
                Args.ThreshDiffSN   = 0;
                Args.FieldSN        = 'SN_2';
                Args.FieldDeltaSN   = {'SN_1','SN_2'};
                Args.MinSN          = 8;
                Args.BadFlags       = {'CR_DeltaHT','NaN','NearEdge'};                
                Args.OnlyConsecutive logical  = true;
                
                Args.MeanFun        = @median;
                Args.MeanFunArgs    = {'omitnan'};
                Args.FieldRA        = 'RA';
                Args.FieldDec       = 'Dec';
                Args.FieldMag       = 'MAG_PSF';
                
                Args.MatchCatName   = 'GAIADR3';
                Args.matchcatsHTMArgs cell = {};
                Args.MagCols        = {'phot_bp_mean_mag','phot_rp_mean_mag'}

                Args.DetStreak logical        = true;
                Args.orphansOnStreakArgs cell = {};
                Args.StreakNonGAIA logical    = true;  % search streaks only for non-GAIA stars
            end
            
            
            CellArgs  = namedargs2cell(Args);
            Result    = Obj.MS.searchOrphans(CellArgs{:});
            
        end
       

        function searchOrphans(Obj, Args)
            %
            % Example: SMS=pipeline.last.SearchMatchedSources
            %          SMS.populateAllConsecutive
            %          SMS.searchOrphans

            arguments
                Obj
                Args.Ncrop              = 24;
                Args.Nvisit             = 1;
                Args.Nstep              = 1;
                Args.SearchRadius       = 3;
                Args.SearchRadiusUnits  = 'arcsec';
                Args.MagField           = 'MAG_PSF';
                Args.MagErrField        = 'MAGERR_PSF';
                Args.LimMagQuantile     = 0.99;

                Args.CooUnits           = 'deg';
                % we are looking for fast moving asteroids
                % with ang. speed of ~5"/60s ~ 2 deg/day
                % error on velocity is about 20%
                % error on position ~10"
                Args.HalfRangeVec       = [10./3600, 10./3600, 2.*0.2, 2.*0.2];
                Args.MinNpairs          = 5;
            end

            Ncons = numel(Obj.AllConsecutive);
            for Icons=1:1:Ncons
                Ng = numel(Obj.AllConsecutive{Icons});
                for Ig=1:1:Ng
                    [Icons, Ncons, Ig, Ng]

                    for Icrop=1:1:Args.Ncrop
                        [Obj, LimMagQuantile] = Obj.prepConsecutive(Icons, Ig, Icrop, 'Nvisit',Args.Nvisit,...
                                                                                      'Nstep',Args.Nstep,...
                                                                                                  'SearchRadius',Args.SearchRadius,...
                                                                                                  'SearchRadiusUnits',Args.SearchRadiusUnits,...
                                                                                                  'MagField',Args.MagField,...
                                                                                                  'MagErrField',Args.MagErrField,...
                                                                                                  'LimMagQuantile',Args.LimMagQuantile);
                        if ~isempty(Obj.MS)
                            %
                            Icrop

                            ResOrphan = Obj.searchOrphansMS;

                            % Rempve streaks from orphan list
                            % indices if non-streaks
                            IndNS = ResOrphan.StreakInd==0;

                            % Convert RA and Dec Dec to X/Y gnomonic
                            % projection
                            % assume input is spherical coordinates
                            % get mean RA/Dec
                            
                            JD  = [ResOrphan.JD(IndNS)].';
                            RA  = [ResOrphan.RA(IndNS)].';
                            Dec = [ResOrphan.Dec(IndNS)].';
                
                            [Lon0, Lat0] = celestial.coo.funOnCosDir(RA, Dec, @mean, 'FunArgs',{1,'omitnan'}, 'InUnits',Args.CooUnits, 'OutUnits','rad');
                            Conv  = convert.angular(Args.CooUnits, 'rad');
                            RA    = RA.*Conv;
                            Dec   = Dec.*Conv;

                            % project coordinates on plane
                            Scale  = convert.angular('rad', 'deg');
                            [X, Y] = celestial.proj.pr_gnomonic(RA, Dec, Scale, [Lon0, Lat0]);
                            % X/Y is the projected position in units of
                            % deg.


                            Nsrc = numel(X);
                            % make sure that Nsrc is not too large

                            if Nsrc>Args.MinNpairs
                           
                                Cand = imUtil.asteroids.pairsMotionMatchKDTree(JD, X, Y, "HalfRangeVec",Args.HalfRangeVec, "MinNpairs",Args.MinNpairs);
                            
                                if ~isempty(Cand)
                                    'a'
                                end
                            end

                        end
                    end
                end
            end
        end


    end
    
    methods % plot


        function Result=plotVar(Obj, IndSrc, Args)
            % Variability plot for a source in MatchedSources object.
            % Input  : - A SearchMatchedSources object, including a
            %            populatred MS.
            %          - Source index in the MS.
            %          * ...,key,val,...
            %            See code for options.
            %
            % Output : - A structure containing web links to source.
            % Author : Eran Ofek (Mar 2024)

            arguments
                Obj
                IndSrc
                Args.MS                    = []; 

                Args.RAField               = 'RA';
                Args.DecField              = 'Dec';
                Args.MagField              = 'MAG_BEST';
                Args.UnitsTime             = 'day';
                Args.DispUnitsTime         = 'min';
                Args.SubT0 logical         = true;

                Args.BD                    = BitDictionary;
                Args.FlagsField            = 'FLAGS';
                Args.DefaultSymbol         = {'ko','MarkerFaceColor','k'};
                Args.ListFlags             = {'Saturated',{'b^'}; ...
                                              'NaN',{'r>'}; ...
                                              'Negative',{'rv'}; ...
                                              'CR_DeltaHT',{'b<'}};

            end
            RAD = 180./pi;

            if isstruct(IndSrc)
                Cand = IndSrc;
                Args.MS = Cand.MS;
                IndSrc = Cand.IndSrc;
            end


            if ~isempty(Args.MS)
                Obj.MS = Args.MS;
            end

            Obj.plotLC(IndSrc)
            Obj.plotRMS(IndSrc, 'NsigmaPredRMS',7);
            Obj.plotPS(IndSrc);
            
            RA  = median(Obj.MS.Data.(Args.RAField)(:,IndSrc), 1, 'omitnan');
            Dec = median(Obj.MS.Data.(Args.DecField)(:,IndSrc), 1, 'omitnan');



            [Result.SimbadURL]=VO.search.simbad_url(RA./RAD, Dec./RAD);
            Result.SDSS_URL=VO.SDSS.navigator_link(RA./RAD, Dec./RAD);
            Result.PS1_URL=VO.PS1.navigator_link(RA./RAD,Dec./RAD);
            %Result.AC=catsHTM.cone_search('GAIADR3',RA./RAD, Dec./RAD, 5,'OutType','AstroCatalog');


        end


       
        function Fig=plotLC(Obj, IndSrc, Args)
            %

            arguments
                Obj
                IndSrc
                Args.MS                    = []; 

                Args.SubPlot logical       = true;
                Args.FigN                  = 1;
                Args.MagField              = 'MAG_BEST';
                Args.UnitsTime             = 'day';
                Args.DispUnitsTime         = 'min';
                Args.SubT0 logical         = true;

                Args.BD                    = BitDictionary;
                Args.FlagsField            = 'FLAGS';
                Args.DefaultSymbol         = {'ko','MarkerFaceColor','k','MarkerSize',4};
                Args.ListFlags             = {'Saturated',{'b^'}; ...
                                              'NaN',{'r>'}; ...
                                              'Negative',{'rv'}; ...
                                              'CR_DeltaHT',{'b<'}};

            end

            if ~isempty(Args.MS)
                Obj.MS = Args.MS;
            end
            
            JD = Obj.MS.JD;
            if Args.SubT0
                JD = JD - min(JD);
            end
            Time = convert.timeUnits(Args.UnitsTime, Args.DispUnitsTime, JD);

            if Args.SubPlot
                subplot(2,2, Args.FigN);
                Fig = Args.FigN;
                cla;
                box on;
            else
                Fig=figure(Args.FigN);
                cla;
                box on;
            end

            Nflag = size(Args.ListFlags,1);
            FlagPlot = false(Obj.MS.Nepoch, Nflag);
            for Iflag=1:1:Nflag
                VecFlag = Obj.MS.Data.(Args.FlagsField)(:,IndSrc);
                VecFlag(isnan(VecFlag)) = 0;
                FlagPlot(:,Iflag) = Args.BD.findBit(VecFlag, Args.ListFlags(Iflag,1), 'Method','any');

                plot(Time(FlagPlot(:,Iflag)), Obj.MS.Data.(Args.MagField)(FlagPlot(:,Iflag),IndSrc))
                hold on;

            end

            FlagG = all(~FlagPlot, 2);
            plot(Time(FlagG), Obj.MS.Data.(Args.MagField)(FlagG,IndSrc), Args.DefaultSymbol{:});

            
            %plot(Time, Obj.MS.Data.(Args.MagField)(:,IndSrc))
            plot.invy;

            H = xlabel(sprintf('Time [%s]',Args.DispUnitsTime));
            H.FontSize = 18;
            H.Interpreter = 'latex';
            H = ylabel('Magnitude');
            H.FontSize = 18;
            H.Interpreter = 'latex';

            hold off;


        end

        function Fig=plotRMS(Obj, IndSrc, Args)
            %

            arguments
                Obj
                IndSrc

                Args.SubPlot logical  = true;
                Args.FigN    = 2;
                Args.ResRMS  = [];

                Args.MagField              = 'MAG_BEST';
                
                Args.NsigmaPredRMS         = 5;
                Args.MinDetRMS             = 15;
            end

            if isempty(Args.ResRMS)
                ResRMS   = Obj.MS.rmsMag('MagField',Args.MagField, 'MinDetRmsVar',Args.MinDetRMS, 'NsigmaPred',Args.NsigmaPredRMS);
            else
                ResRMS = Args.ResRMS;
            end

            if Args.SubPlot
                subplot(2,2, Args.FigN);
                Fig = Args.FigN;
                cla;
                box on;
            else
                Fig=figure(Args.FigN)
                cla;
                box on;
            end
            %MS.plotRMS;
            semilogy(ResRMS.MeanMag, ResRMS.StdPar,'.')    
            hold on
            [~,SI] = sort(ResRMS.MeanMag);
            semilogy(ResRMS.MeanMag(SI), ResRMS.InterpMeanStd(SI),'-')      
            semilogy(ResRMS.MeanMag(SI), ResRMS.InterpMeanStd(SI)+ResRMS.InterpPredStd(SI).*Args.NsigmaPredRMS,'--')      
            plot(ResRMS.MeanMag(IndSrc), ResRMS.StdPar(IndSrc),'ro')

            H = xlabel('Magnitude');
            H.FontSize = 18;
            H.Interpreter = 'latex';
            H = ylabel('RMS [mag]');
            H.FontSize = 18;
            H.Interpreter = 'latex';

        end
    
        function [FigPS, FigPh, FreqVec, PS]=plotPS(Obj, IndSrc, Args)
            %

            arguments
                Obj
                IndSrc

                Args.SubPlot logical   = true;
                Args.FigN      = 3;
                Args.PhaseFigN = 4;
                Args.FreqVec   = [];
                Args.PS        = [];

                Args.MagField              = 'MAG_BEST';
                
                Args.PlotPhase logical     = true;
                Args.PlotFreq              = [];
            end

            if isempty(Args.FreqVec) && isempty(Args.PS)
                % calc PS
                FreqVec = timeSeries.period.getFreq(Obj.MS.JD, 'OverNyquist',0.4);
                [PS] = timeSeries.period.period([Obj.MS.JD, Obj.MS.Data.(Args.MagField)(:,IndSrc)], FreqVec);
                PS   = PS(:,2);
            else
                FreqVec = Args.FreqVec;
                PS      = Args.PS;
            end

            if Args.SubPlot
                subplot(2,2, Args.FigN)
                FigPS = Args.FigN;
                cla;
                box on;
            else
                FigPS=figure(Args.FigN);
                cla;
                box on;
            end
            plot(FreqVec,PS);
            H = xlabel('Frequency [1/day]');
            H.FontSize = 18;
            H.Interpreter = 'latex';
            H = ylabel('Power');
            H.FontSize = 18;
            H.Interpreter = 'latex';

            
            %
            FigPh = [];
            if Args.PlotPhase

                if Args.SubPlot
                    subplot(2,2, Args.PhaseFigN);
                    FigPh = Args.PhaseFigN;
                    cla
                    box on;
                else
                    FigPh=figure(Args.PhaseFigN);
                    cla
                    box on;
                end

                if isempty(Args.PlotFreq)
                    [~,IndMaxPS] = max(PS);
                    Period       = 1./FreqVec(IndMaxPS);
                else
                    Period       = 1./Args.PlotFreq;
                end
                FF = timeSeries.fold.folding([Obj.MS.JD, Obj.MS.Data.MAG_BEST(:,IndSrc)], Period);
                B  = timeSeries.bin.binning(FF, 0.1, [0 1],{'MidBin',@median,@std,@numel});
                plot(FF(:,1),FF(:,2),'.');
                hold on
                plot.invy;
                errorbar(B(:,1), B(:,2), B(:,3)./sqrt(B(:,4)), 'o');
                hold off;
                H = xlabel(sprintf('Phase [P=%8.4f]',Period.*1440));  % [min]
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('Mag');
                H.FontSize = 18;
                H.Interpreter = 'latex';

            end


        end
            
    end

end