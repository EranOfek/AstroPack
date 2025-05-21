function [Result] = genLC(X, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 May) 
    % Example: 

    arguments
        RA
        Dec
        Args.FieldID           = "WD0549";
        Args.CamNum            = 1;
    end


    TmpT =pipeline.last.queryDB.searchVisitsByCoo(RA, Dec);
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
    
    Rrms = MS.calcRMS;
    plot([Rrms.MagMinRMS],[Rrms.MinRMS], '.')
    Igood = find([Rrms.MinRMS]<0.02);
    MS = MS(Igood);
    MS1=MS.mergeByCoo(MS(1));
    MS1.bestMag;
    MS1.plotRMS('FieldX','MAG_BEST');
    MS1.addExtMagColor;
    MS1.addAirMassPA('GeoCoo',[35.0407331 30.0529838]);

    RRR=lcUtil.zp_meddiff(MS1,'MagField','MAG_BEST','MagErrField','MAGERR_PSF');
    MS1.applyZP(RRR);

    MS1.plotRMS('FieldX','MAG_BEST');
    Rrms = MS1.calcRMS;
   
    MaxMag = 20;

    MagRange = [10, Rrms.MagMinRMS;
                Rrms.MagMinRMS Rrms.MagMinRMS+3;
                Rrms.MagMinRMS+3 MaxMag];
    Nmagrange = size(MagRange,1);

    MagField = 'MAG_PSF'; %'MAG_APER_3';
    %MagField = 'MAG_APER_3';

    Mag = MS1.Data.(MagField);
    MagErr = MS1.Data.(MagField);
    

    


    MedMag = median(MS1.Data.(MagField), 1, 'omitnan');
    Nnn = sum(~isnan(MS1.Data.(MagField)), 1);
    [Nepoch, Nsrc] = size(MS1.Data.(MagField));
    %ii = find(MedMag<19 & MedMag>12 & Nnn>=(Nepoch.*1));
    %MagBright = MS1.Data.(MagField)(:,ii);

    RealErr = interp1(Rrms.B(:,1),Rrms.B(:,2), MedMag, 'linear','extrap');

    %CC=corrcoef(MagBright);
    %F=triu(ones(size(CC)),1)==0;
    %CC(F) = NaN;
    % there is a clear relation between corr and color
    %plot(MS1.SrcData.ExtColor(ii),CC(1,:),'.')

    %DeltaColor = MS1.SrcData.ExtColor(ii) - MS1.SrcData.ExtColor(ii).';
    %DeltaColor(F) = NaN;

    %plot(DeltaColor(:), CC(:),'.')


    % svd
    Resid = cell(1,Nmagrange);
    Res   = cell(1,Nmagrange);
    CalibMag = nan(size(Mag));
    for Imagrange=1:1:Nmagrange
        IndSrc = find(MedMag>MagRange(Imagrange,1) & MedMag<=MagRange(Imagrange,2));
        Resid{Imagrange} = Mag(:,IndSrc) - MedMag(IndSrc);
        [S2,Res{Imagrange}]=timeSeries.detrend.sysrem(Resid{Imagrange}, RealErr(IndSrc), 'Niter',3);

        CalibMag(:,IndSrc) = Res{Imagrange}(end).Resid + MedMag(IndSrc);
    end

    %Resid = MagBright - MedMag(ii);
    %[S2,Res]=timeSeries.detrend.sysrem(Resid, RealErr(ii), 'Niter',3);
    semilogy(MedMag, std(Mag),'.');
    hold on
    semilogy(MedMag, std(CalibMag), '.'); %MedMag(ii), std(Res(2).Resid),'.')






    TmpR = lcUtil.zp_fit2D(MS1,'FieldMag','MAG_BEST', 'RefEpochID',30);
    
    RRR=lcUtil.zp_meddiff(MS1,'MagField','MAG_BEST','MagErrField','MAGERR_PSF');
    MS1.applyZP(RRR);
    Ind = MS1.coneSearch(RA, Dec);
    [JD, Mag] = getLC_ind(MS1, Ind.Ind);
    plot(JD - floor(min(JD)),Mag,'.');
    plot.invy;

end
