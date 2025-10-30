function [Summary] = telescopeAlignmentStatus(Args)
    % Measure LAST telescope relative position (alignment) using the DB.
    %     For each mount, go over all telescopes and calculate the
    %     difference in astrometric pointing between different telescopes.
    % Input  : * ...,key,val,...
    %            See code for options.
    % Output : - A structure array with elemnt per mount and the following
    %            information:
    %            .Data
    %            .Median
    %            .Std
    % Author : Eran Ofek (2025 Oct) 
    % Example: Summary=pipeline.last.quality.telescopeAlignmentStatus;

    arguments
        Args.MountNumVec       = (1:10);   % mounts to check
        Args.CamNumVec         = (1:4);    % cam to check
        Args.CropID            = 10;       % cropID - don't change
        Args.MaxFWHM           = 3;
        Args.MaxAirmass        = 1.4;
        Args.RangeJD           = [celestial.time.julday([1 9 2025]), celestial.time.julday([1 10 2025])];   % JD range to check
        Args.MinPointing       = 30;   % min number of observations per field to use.
        Args.DB                = [];   % DB object, if empty generate
    end
    ARCSEC_DEG = 3600;

    if isempty(Args.DB)
        Args.DB = db.Db;
        Args.DB.connect;
        Args.useDB('last');
    end

    Nmnt = numel(Args.MountNumVec);
    Ncam = numel(Args.CamNumVec);

    ColNames = {'Mount', 'CamNum', 'CropID', 'FieldID','medAirMass','minAirMass','maxAirMass', 'stdRA', 'stdDec', 'rstdRA', 'rstdDec', 'rangeRA', 'rangeDec'};
    All    = nan(1e4, numel(ColNames));
    PerCam = nan(10.*4, numel(ColNames));
    K    = 0;
    Kcam = 0;


    for Imnt=1:1:Nmnt
        [Imnt]
        Query = sprintf('SELECT * FROM visit_images WHERE mountnum=%d and cropid=%d AND jd_start>%10.1f AND jd_start<%10.1f AND fwhm<%4.1f AND airmass<%5.2f', Args.MountNumVec(Imnt), Args.CropID, Args.RangeJD, Args.MaxFWHM, Args.MaxAirmass);
        T = Args.DB.query(Query);
        % select unique id_visit
        %[~,Iun] = unique(T.id_visit);
        %T = T(Iun,:);

        K = 0;
        Summary(Imnt).Data = nan(1e5,11);
        Nt = size(T,1);
        for It=1:1:Nt
            Igroup = find(abs(T.jd_start(It) - T.jd_start)<1./1440);
            [~,SI]=sort(T.camnum(Igroup));
            Igroup = Igroup(SI);  % sortd by camnum

            if numel(Igroup)==4 && all(T.camnum(Igroup)==Args.CamNumVec(:))
                % found group of 4 visits taken by all telescopes on mount
                Cam1 = 1; % ref cam
                MeanDec = T.dec(Igroup(Cam1));
                DeltaRA  = (T.ra(Igroup)-T.ra(Igroup(Cam1))).*cosd(MeanDec);
                DeltaDec = T.dec(Igroup)-T.dec(Igroup(Cam1));
                K = K + 1;
                Summary(Imnt).Data(K,:) = [T.airmass(Igroup(Cam1)), T.m_ha(Igroup(Cam1)), T.m_dec(Igroup(Cam1)), DeltaRA.', DeltaDec.'];
            end
        end
        Summary(Imnt).Data   = Summary(Imnt).Data(1:K,:);
        Summary(Imnt).Median = median(Summary(Imnt).Data);
        Summary(Imnt).Std    = std(Summary(Imnt).Data);

        figure(Imnt);
        Ref0 = median(Summary(Imnt).Data(:,4:11).*60);
        plot(Summary(Imnt).Data(:,1), Summary(Imnt).Data(:,4:11).*60-Ref0,'.')
    end

end
