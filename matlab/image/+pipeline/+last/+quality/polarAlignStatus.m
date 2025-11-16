function [Summary,PerMnt] = polarAlignStatus(Args)
    % Fit polar alignment shift to drift data collected during LAST visits.
    % Input  : * ...,key,val,...
    %            See code for options.
    % Output : - Summary poer camera
    %          - Summary per mount [Mny, DeltaAz, DeltaAlt]
    % Author : Eran Ofek (2025 Nov) 
    % Example: [S,PerMnt]=pipeline.last.quality.polarAlignStatus('DB',DB);

   
    arguments
        Args.DB                = [];
        Args.Scale             = 1.25;   % [arcsec/pix]
        Args.MountNumVec       = (1:10);   % mounts to check
        Args.CamNumVec         = (1:4);    % cam to check
        Args.CropIdVec         = (1:24);
        Args.MaxFWHM           = 4;
        Args.MaxAirmass        = 1.5;
        Args.RangeJD           = [celestial.time.julday([1 6 2025]), celestial.time.julday([1 10 2025])];   % JD range to check
        
    end

    ARCSEC_DEG = 3600;

    if isempty(Args.DB)
        Args.DB = db.Db;
        Args.DB.User = 'last_user';
        Args.DB.Password = 'physics';
        Args.DB.Conn;
        Args.DB.useDB('last');
        Args.DB.connect;
    end

    Nmnt = numel(Args.MountNumVec);
    Ncam = numel(Args.CamNumVec);

    ColNames = {'mountnum','camnum','BestDAz','BestDAlt', 'MedianAbsGM_Alpha','MedianAbsGM_Delta'};
    %[CellEmpty{1:numel(ColNames)}]=deal([]);
    %Summary.MedianTable = table(CellEmpty{:}, 'VariableNames',ColNames);
    Summary = zeros(Nmnt.*Ncam, numel(ColNames));
    PerMnt  = zeros(Nmnt, 3);
    K = 0;
    for Imnt=1:1:Nmnt
        for Icam=1:1:Ncam
            K = K + 1;
            Query = sprintf('SELECT * FROM visit_images WHERE mountnum=%d and camnum=%d AND jd_start>%10.1f AND jd_start<%10.1f AND fwhm<%4.1f AND airmass<%5.2f', Args.MountNumVec(Imnt), Args.CamNumVec(Icam), Args.RangeJD, Args.MaxFWHM, Args.MaxAirmass);
            T = Args.DB.query(Query);

            % scatter(T.m_ha,T.m_dec, 30, T.gm_ratex.*1.25.*60, 'filled');  % gm_ratex [pix/sec]
            RateAlpha = -T.gm_ratex.*Args.Scale.*86400./3600;   % [deg/day]
            RateDelta = T.gm_ratey.*Args.Scale.*86400./3600;

            R=celestial.polarAlign.polarAlign_fitDrift(T.m_ha, T.m_dec, RateAlpha, RateDelta);
            Summary(K,:) = [Args.MountNumVec(Imnt), Args.CamNumVec(Icam), R(end).BestDAz, R(end).BestDAlt, median(RateAlpha,1,'omitnan'), median(RateDelta,1,'omitnan')];
            
        end

        F = Summary(:,1)==Args.MountNumVec(Imnt);
        PerMnt(Imnt,1:3) = [Args.MountNumVec(Imnt), median(Summary(F,3:4))];
        
        [DA, DD] = celestial.polarAlign.trackingErrorRates(PerMnt(Imnt,2), PerMnt(Imnt,3));
        [median(abs(DA(:)),1,'omitnan'), median(abs(DD(:)),1,'omitnan')]

    end
    Summary = array2table(Summary, 'VariableNames',ColNames);




end
