function [MS1s, Flags] = genLC(RA, Dec, Args)
    % Generated relative photometric LCs for a field.
    % Input  : - J2000 RA [deg], or Table of visit images, of a
    %            MatchedSources object.
    %          - J2000.0 Dec [deg].
    %          * ...,key,val,... 
    %            
    % Output : - 
    % Author : Eran Ofek (2025 May) 
    % Example:
    % MS1s = pipeline.last.queryDB.genLC(88.1157006,15.8858523,'FieldID','WD0549');
    % MS2s = pipeline.last.queryDB.genLC(274.654678,+30.923869,'FieldID','Nagi1b','CamNum',1)
    % MS3s = pipeline.last.queryDB.genLC(117.248162870746,31.4201026297819,'FieldID','1346','CamNum',3)
    % MS3s = pipeline.last.queryDB.genLC(64.2218833203206,    26.4047462995035,'FieldID','1254','CamNum',3)
    % MS3s = pipeline.last.queryDB.genLC(119.720893587354 ,   16.2792466747635,'FieldID','1097','CamNum',1)
    % MS3s = pipeline.last.queryDB.genLC(107.857988831646  ,  44.0678803271926,'FieldID','1489','CamNum',2)
    % MS3s = pipeline.last.queryDB.genLC(60.2176197876583  ,  34.0772710251479 ,'FieldID','MasterOT','CamNum',4)
    % MS3s = pipeline.last.queryDB.genLC(60.2176197876583  ,  34.0772710251479 ,'FieldID','MasterOT','CamNum',4)
    % MS3s = pipeline.last.queryDB.genLC(236.236624284269 ,   45.2560870113117  ,'FieldID','1513','CamNum',4)
    %
    % RA=233.970883040643; Dec=-14.22005886708; FieldID='686'; CamNum=1; CropID=2;  % HP Lib
    % RA=212.952668267765; Dec=-18.584465563234; FieldID='213-18'; CamNum=2; CropID=5;  %
    % RA=67.4798599922836; Dec=34.7928892561292; FieldID='1335'; CamNum=4; CropID=22;   % cand - one eclipse
    % RA=64.2218415434159; Dec=26.404713118121; FieldID='1254'; CamNum=2; CropID=5; % new var % new -brighning /many / interesting
    % RA=182.925919255858; Dec=38.3066889437051; FieldID='1338.WDM4'; CamNum=2; CropID=14; % new / possible 22.545888 min period, but phase...
    % MS = pipeline.last.queryDB.genLC(RA,Dec,'FieldID',FieldID,'CamNum',CamNum,'CropID',CropID); 
    % R=MS.coneSearch(RA,Dec);
    % JD = MS.JD;
    % LC=MS.Data.MAG_BEST(:,R.Ind);
    % IN = celestial.INPOP.init;
    % BJD = celestial.time.barycentricJD(JD,RA./RAD,Dec./RAD,'GeoPos',[35./RAD, 30./RAD, 415], 'INPOP',IN);
    % Freq = timeSeries.period.getFreq(BJD, 'MaxFreq',1440);
    % [PS]=timeSeries.period.period([BJD, LC], Freq);
    % MS.plotLC(R.Ind);
    % plot(PS(:,1), PS(:,2))
    % [MaxPS,MaxI] = max(PS(:,2));
    % F = timeSeries.fold.folding([JD,LC],1./Freq(MaxI));
    % plot(F(:,1), F(:,2),'o')
    % B=timeSeries.bin.binning(F,0.05,[0 1]);
    % hold on; plot(B(:,1),B(:,3),'o')

    arguments
        RA
        Dec
        Args.FieldID           = "WD0549";
        Args.CamNum            = 1;
        Args.CropID            = [];

        Args.MinNotNanFrac     = 0.1; %0.95;
        Args.UseMagMinRMS      = false;
        Args.Nsysrem           = 2;

        Args.MagField          = {'MAG_APER_3','MAG_PSF'};
        Args.GeoCoo            = [35.0407331 30.0529838];
        Args.AddExtColor       = true;
        Args.AddAM             = true;
        
        Args.MaxDAper          = -0.2;
        Args.MinNeighFlux      = 0.001;

        Args.IsBadFlags        = {'Saturated', 'NearEdge'};

        Args.DB                = [];
    end

    RAD = 180./pi;

    MagField = Args.MagField;

    if isa(RA, 'MatchedSources')
        MS = RA;

    else
        if istable(RA)
            TmpT = RA;
        else
            TmpT =pipeline.last.queryDB.searchVisitsByCoo(RA, Dec, 'QueryMethod','radec','DB',Args.DB);
        end

        if isempty(Args.FieldID)
            TT = TmpT{1};
        else
            Flag = TmpT{1}.fieldid == Args.FieldID;
            TT   = TmpT{1}(Flag,:);
        end
    
        if ~isempty(Args.CamNum)
            Flag = TT.camnum == Args.CamNum;
            TT   = TT(Flag,:);
        end

        if ~isempty(Args.CropID)
            Flag = TT.cropid == Args.CropID;
            TT   = TT(Flag,:);
        end

        

        MS=pipeline.last.queryDB.loadProducts(TT,'merged','MergedMat'); 
    end

    Rrms = MS.calcRMS('FieldX',{'MAG_APER_3'});
    %plot([Rrms.MagMinRMS],[Rrms.MinRMS], '.')
    Igood = find([Rrms.MinRMS]<0.005);
    fprintf('Use %d MS out of %d MS\n', numel(Igood), numel(Rrms));

    MS = MS(Igood);

    Args.CleanMissingX2 = true;
    if Args.CleanMissingX2
        Nms = numel(MS);
        IsF=false(Nms,1);
        
        for I=1:1:numel(MS)
            IsF(I)=isfield(MS(I).Data,'X2');
        end
        MS = MS(IsF);
    end
    MS1 = MS.mergeByCoo(MS(1));

    % remove bad:
    IsBad = imProc.cat.findBit(uint32(MS1.Data.FLAGS), Args.IsBadFlags, [], 'Image');
    Nmagf    = numel(MagField);
    for Imagf=1:1:Nmagf
        MS1.Data.(MagField{Imagf})(IsBad) = NaN;
    end

    if Args.AddExtColor
        MS1.addExtMagColor;
    end
    if Args.AddAM
        MS1.addAirMassPA('GeoCoo',Args.GeoCoo);
    end

    %[~,MagCross]=MS1.bestMag;
    Rrms = MS1.calcRMS('FieldX',{'MAG_APER_3'});

    
    Nnn = sum(~isnan(MS1.Data.(MagField{1})), 1);
    [Nepoch, Nsrc] = size(MS1.Data.(MagField{1}));
    MedMag = median(MS1.Data.(MagField{1}), 1, 'omitnan');

    if Args.UseMagMinRMS
        MagMinRMS = Rrms.MagMinRMS;
    else
        MagMinRMS = -Inf;
    end
    FlagSrc = MedMag>MagMinRMS & (Nnn./Nepoch)>=Args.MinNotNanFrac;
    
    MS1s = MS1.selectBySrcIndex(FlagSrc, 'CreateNewObj',true);
    
    for Imagf=1:1:Nmagf
        %MS1s.plotRMS('FieldX',MagField(Imagf));

        RRR=lcUtil.zp_meddiff(MS1s,'MagField',MagField{Imagf},'MagErrField','MAGERR_PSF');
        MS1s.applyZP(RRR, 'ApplyToMagField',MagField(Imagf));

        %hold on;
        %MS1s.plotRMS('FieldX',MagField(Imagf), 'PlotColor','b');

        Rrms    = MS1s.calcRMS('FieldX', MagField(Imagf));
        MedMag  = median(MS1s.Data.(MagField{Imagf}), 1, 'omitnan');
        RealErr = interp1(Rrms.B(:,1),Rrms.B(:,2), MedMag, 'linear','extrap');
        
        if Args.Nsysrem>0

            Resid{Imagf} = MS1s.Data.(MagField{Imagf}) - MedMag;
            [S2,Res{Imagf}]=timeSeries.detrend.sysrem(Resid{Imagf}, RealErr, 'Niter',Args.Nsysrem);
    
            CalibMag{Imagf} = Res{Imagf}(2).Resid + MedMag;
    
            %semilogy(MedMag, std(MS1.Data.(MagField{Imagf})),'.');
            %hold on
            %semilogy(MedMag, std(CalibMag{Imagf}), 'r.'); %MedMag(ii), std(Res(2).Resid),'.')
    
            MS1s.Data.(MagField{Imagf}) = CalibMag{Imagf};
        end

        %MS1s = lcUtil.zp_fit2D(MS1s, 'FieldMag',MagField{Imagf});

    end
    MS1s.bestMag;
    
    MS1s.addSrcData;


    ResidZ    = MS1s.Data.MAG_BEST-MS1s.SrcData.MAG_BEST;
    RStd      = tools.math.stat.rstd(ResidZ);
    Z         = ResidZ./RStd;
    EpochChi2 = sum(Z.^2,2,'omitnan')./size(ResidZ,2);
    Flag      = EpochChi2<3;
    MS1s      = MS1s.selectByEpoch(Flag);
    Rrms      = MS1s.calcRMS('FieldX','MAG_BEST');

    if nargout>1
        Nsrc = MS1s.Nsrc;
        Corr_AM_Mag = nan(Nsrc,1);
        Prob_AM_Mag = nan(Nsrc,1);
        for Isrc=1:1:Nsrc
            [Corr_AM_Mag(Isrc), Prob_AM_Mag(Isrc)]=tools.math.stat.corrsim(MS1s.Data.AM(:,Isrc), MS1s.Data.MAG_BEST(:,Isrc));
        end
    
    
    
        Dist   = celestial.coo.sphere_dist_fast(MS1s.SrcData.RA(:)./RAD, MS1s.SrcData.Dec(:)./RAD, MS1s.SrcData.RA(:).'./RAD, MS1s.SrcData.Dec(:).'./RAD).*RAD.*3600;
        Nsrc   = size(Dist,1);
        Dist   = Dist + diag(nan(1,Nsrc));
        FluxRatio = MS1s.SrcData.FLUX_APER_3(:)./MS1s.SrcData.FLUX_APER_3(:).';
        SDF = sum(exp(-(Dist./3).^2).*FluxRatio, 1, 'omitnan');
    
        Flags.BadNeighFlux = SDF>Args.MinNeighFlux;
        
        %MS1s.plotRMS
        %hold on;
        %plot(MS1s.SrcData.MAG_BEST(ii), std(MS1s.Data.MAG_BEST(:,ii)), 'ro')
    
        Flags.BadDAper = median(MS1s.Data.MAG_APER_3-MS1s.Data.MAG_APER_2, 1, 'omitnan')<Args.MaxDAper;

        


    end

end
