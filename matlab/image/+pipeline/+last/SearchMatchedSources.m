% SearchMatchedSources


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
            % load one consecutive MatchedSources objects

            arguments
                Obj
                Icons          = 1;
                Igroup         = 1;
                Icrop          = 1;

                Args.Nvisit    = 2;
                
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

                IndGroup = (Igroup:1:Igroup+Args.Nvisit-1);
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

                if numel(MSm)~=Args.Nvisit
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


    methods
        function [Cand, Summary] = findVariableAll(Obj, Args)
            %

            arguments
                Obj
                Args.Nvisit    = 2;
                Args.Ncrop     = 24;
                
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
            for Icons=1:1:Ncons
                Ng = numel(Obj.AllConsecutive{Icons});
                for Ig=1:1:Ng
                    [Icons, Ncons, Ig, Ng]

                    for Icrop=1:1:Args.Ncrop
                        [Obj, LimMagQuantile] = Obj.prepConsecutive(Icons, Ig, Icrop, 'Nvisit',Args.Nvisit,...
                                                                                                  'SearchRadius',Args.SearchRadius,...
                                                                                                  'SearchRadiusUnits',Args.SearchRadiusUnits,...
                                                                                                  'MagField',Args.MagField,...
                                                                                                  'MagErrField',Args.MagErrField,...
                                                                                                  'LimMagQuantile',Args.LimMagQuantile);
                        if ~isempty(Obj.MS)
                            %
                            [Flag, FlagInfo, Summary] = findVariableMS(Obj);
                            FlagComb = Flag.FlagGood(:) & (Flag.PS(:) | Flag.RMS(:) | Flag.Poly(:));
    
                            if any(FlagComb)
                                Icand = Icand + 1;
                                Cand(Icand).N_FlagComb = sum(FlagComb);
                                Cand(Icand).IndCand    = find(FlagComb);
                                Cand(Icand).Flag       = Flag;

                                for I=1:1:Cand(Icand).N_FlagComb
                                    IndSrc = Cand(Icand).IndCand(I);

                                    [FlagGood, ResPhotAstCorr] = flagAstPhotCorr(Obj, IndSrc);

                                    if FlagGood

                                        if Args.Plot
                                            
                                            Obj.plotLC(IndSrc);
                                            Obj.plotRMS(IndSrc);
                                            Obj.plotPS(IndSrc);
                                            
    
                                            RA  = median(Obj.MS.Data.(Args.RAField)(:,IndSrc), 1, 'omitnan');
                                            Dec = median(Obj.MS.Data.(Args.DecField)(:,IndSrc), 1, 'omitnan');
    
    
    
                                            [SimbadURL]=VO.search.simbad_url(RA./RAD, Dec./RAD)
    
                                            web(SimbadURL.URL)
                                            'a'
                                        end
                                    end
                                    
                                end
                                

                            end
                        end
                    end
                end
            end


        end


        function [Flag, FlagInfo, Summary]=findVariableMS(Obj, Args)
            % Find variable stars

            arguments
                Obj
               
                Args.BadFlags              = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};

                Args.MagField              = 'MAG_BEST';
                Args.MaxChi2Dof            = 3;

                Args.MinNdet               = 2;
                Args.MaxOverlapFrac        = 0.5;

                Args.ThresholdPS           = 12;

                Args.NsigmaRMS             = 5;
                Args.MinDetRMS             = 15;
                Args.MinNptRMS             = 10;
            end

            % set to NaN photometry with bad flags
            Obj.setBadPhotToNan('BadFlags',Args.BadFlags, 'MagField',Args.MagField);

            
            [Flag.FlagGood, FlagInfo]=flagByQuality(Obj, 'MagField',Args.MagField, 'MaxChi2Dof',Args.MaxChi2Dof, 'MinNdet',Args.MinNdet, 'MaxOverlapFrac',Args.MaxOverlapFrac);
           
            % periodicity
            FreqVec = timeSeries.period.getFreq(Obj.MS.JD, 'OverNyquist',0.4);
            [FreqVec, PS, Flag.PS] = period(Obj.MS, FreqVec, 'MagField', Args.MagField, 'ThresholdPS',Args.ThresholdPS);
                    
            % rms
            ResRMS   = Obj.MS.rmsMag('MagField',Args.MagField, 'MinDetRmsVar',Args.MinDetRMS, 'Nsigma',Args.NsigmaRMS, 'MinNpt',Args.MinNptRMS);
            Flag.RMS = ResRMS.FlagVarPred;

            % poly std
            [ResPolyHP, Flag.Poly] = fitPolyHyp(Obj.MS, 'PolyDeg',{0, (0:1), (0:1:2)}, 'ThresholdChi2',[Inf, chi2inv(normcdf([4.5 5 6],0,1),2)]);

            Summary.FreqVec   = FreqVec;
            Summary.PS        = PS;
            Summary.ResPolyHP = ResPolyHP;
        end


        function [FlagGood, FlagInfo]=flagByQuality(Obj, Args)
            % Select stars with good quality photometry

            arguments
                Obj
                
                Args.MagField              = 'MAG_BEST';
                Args.MaxChi2Dof            = 3;

                Args.MinNdet               = 2;
                Args.MaxOverlapFrac        = 0.5;
                
                Args.RAField               = 'RA';
                Args.DecField              = 'Dec';
                Args.MaxAstStd             = 0.5./3600;  % deg
            end

            % Detections
            FlagDet = ~isnan(Obj.MS.Data.(Args.MagField));
            Ndet    = sum(FlagDet, 1);

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
   
            % summarize all flags
            FlagGood  = FlagInfo.Ndet(:) & FlagAll.NotNearEdge(:) & FlagInfo.NotOverlap(:) & FlagInfo.Chi2(:) & FlagInfo.NoAstJitter(:);


        end

        function [FlagGood, Res] = flagAstPhotCorr(Obj, IndSrc, Args)
            % astrometry-photometry correltaions

            arguments
                Obj
                IndSrc
                Args.MagField              = 'MAG_BEST';
                Args.RAField               = 'RA';
                Args.DecField              = 'Dec';
                Args.ProbThresh            = 0.9;
            end

            [Res.C_RA, Res.Pc_RA]  = tools.math.stat.corrsim(Obj.MS.Data.(Args.MagField)(:,IndSrc), Obj.MS.Data.(Args.RAField)(:,IndSrc));
            [Res.C_Dec,Res.Pc_Dec] = tools.math.stat.corrsim(Obj.MS.Data.(Args.MagField)(:,IndSrc), Obj.MS.Data.(Args.DecField)(:,IndSrc));

            if Res.Pc_RA>Args.ProbThresh || Res.Pc_Dec>Args.ProbThresh
                FlagGood = false;
            else
                FlagGood = true;
            end
        end


        function Obj=setBadPhotToNan(Obj, Args)
            % set to NaN photometry with bad flags

            arguments
                Obj
                Args.BadFlags              = {'Overlap','NearEdge','CR_DeltaHT','Saturated','NaN','Negative'};
                Args.MagField              = 'MAG_BEST';
            end

            [BadFlags] = searchFlags(Obj.MS, 'FlagsList',Args.BadFlags);
            Obj.MS.Data.(Args.MagField)(BadFlags) = NaN;
        end

    end
    
    methods % plot

        function plotLC(Obj, IndSrc, Args)
            %

            arguments
                Obj
                IndSrc
                Args.FigN                  = 1;
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

            
            JD = Obj.MS.JD;
            if Args.SubT0
                JD = JD - min(JD);
            end
            Time = convert.timeUnits(Args.UnitsTime, Args.DispUnitsTime, JD);

            figure(Args.FigN);
            cla;

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

        function plotRMS(Obj, IndSrc, Args)
            %

            arguments
                Obj
                IndSrc
                Args.FigN    = 2;
                Args.ResRMS  = [];

                Args.MagField              = 'MAG_BEST';
                
                Args.NsigmaRMS             = 5;
                Args.MinDetRMS             = 15;
            end

            if isempty(Args.ResRMS)
                ResRMS   = Obj.MS.rmsMag('MagField',Args.MagField, 'MinDetRmsVar',Args.MinDetRMS, 'Nsigma',Args.NsigmaRMS);
            else
                ResRMS = Args.ResRMS;
            end

            figure(Args.FigN)
            cla;
            %MS.plotRMS;
            semilogy(ResRMS.MeanMag, ResRMS.StdPar,'.')    
            hold on
            [~,SI] = sort(ResRMS.MeanMag);
            semilogy(ResRMS.MeanMag(SI), ResRMS.InterpMeanStd(SI),'-')      
            semilogy(ResRMS.MeanMag(SI), ResRMS.InterpMeanStd(SI)+ResRMS.InterpPredStd(SI).*Args.NsigmaRMS,'--')      
            plot(ResRMS.MeanMag(IndSrc), ResRMS.StdPar(IndSrc),'ro')

            H = xlabel('Magnitude');
            H.FontSize = 18;
            H.Interpreter = 'latex';
            H = ylabel('RMS [mag]');
            H.FontSize = 18;
            H.Interpreter = 'latex';

        end
    
        function [FreqVec, PS]=plotPS(Obj, IndSrc, Args)
            %

            arguments
                Obj
                IndSrc
                Args.FigN      = 3;
                Args.FreqVec   = [];
                Args.PS        = [];

                Args.MagField              = 'MAG_BEST';
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

            figure(Args.FigN);
            cla;
            plot(FreqVec,PS);
            H = xlabel('Frequency [1/day]');
            H.FontSize = 18;
            H.Interpreter = 'latex';
            H = ylabel('Power');
            H.FontSize = 18;
            H.Interpreter = 'latex';
        end
            
    end

end