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

    arguments
        RA
        Dec
        Args.FieldID           = "WD0549";
        Args.CamNum            = 1;

        Args.MinNotNanFrac     = 0.95;
        Args.UseMagMinRMS      = false;
        Args.Nsysrem           = 2;

        Args.MagField          = {'MAG_APER_3','MAG_PSF'};
        Args.GeoCoo            = [35.0407331 30.0529838];
        Args.AddExtColor       = true;
        Args.AddAM             = true;
        
        Args.MaxDAper          = -0.2;
        Args.MinNeighFlux      = 0.001;

        Args.IsBadFlags        = {'Saturated', 'NearEdge'};
    end

    RAD = 180./pi;

    MagField = Args.MagField;

    if isa(RA, 'MatchedSources')
        MS = RA;

    else
        if istable(RA)
            TmpT = RA;
        else
            TmpT =pipeline.last.queryDB.searchVisitsByCoo(RA, Dec);
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

        MS=pipeline.last.queryDB.loadProducts(TT,'merged','MergedMat'); 
    end

    Rrms = MS.calcRMS('FieldX',{'MAG_APER_3'});
    %plot([Rrms.MagMinRMS],[Rrms.MinRMS], '.')
    Igood = find([Rrms.MinRMS]<0.005);
    fprintf('Use %d MS out of %d MS\n', numel(Igood), numel(Rrms));

    MS = MS(Igood);

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
