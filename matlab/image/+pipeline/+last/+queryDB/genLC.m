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
    MS1=MS.mergeByCoo(MS(1));
    MS1.bestMag;
    MS1.plotRMS('FieldX','MAG_BEST');
    TmpR = lcUtil.zp_fit2D(MS1,'FieldMag','MAG_BEST', 'RefEpochID',30);
    
    RRR=lcUtil.zp_meddiff(MS1,'MagField','MAG_BEST','MagErrField','MAGERR_PSF');
    MS1.applyZP(RRR);
    Ind = MS1.coneSearch(RA, Dec);
    [JD, Mag] = getLC_ind(MS1, Ind.Ind);
    plot(JD - floor(min(JD)),Mag,'.');
    plot.invy;
    
end
