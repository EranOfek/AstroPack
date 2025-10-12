function [All, PerCam] = pointingRMS(Args)
    % Estimate the pointing accuracy of LAST
    % Input  : * ...,key,val,... 
    %            See code for options.
    % Output : - A table with the median std, airmass, etc, per each
    %            mount/cam/field.
    %          - The same as above, but in which each line is the median
    %            over all fieldsID in a specific mount/cam.
    % Author : Eran Ofek (2025 Oct) 
    % Example: [All,PerCam]=pipeline.last.quality.pointingRMS('DB',DB);

    arguments
        Args.MountNumVec       = (1:10);   % mounts to check
        Args.CamNumVec         = (1:4);    % cam to check
        Args.CropID            = 10;       % cropID - don't change
        Args.RangeJD           = [celestial.time.julday([1 8 2025]), celestial.time.julday([1 10 2025])];   % JD range to check
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
        for Icam=1:1:Ncam
            Kcam = Kcam + 1;
            [Imnt, Icam]
            Query = sprintf('SELECT * FROM visit_images WHERE mountnum=%d AND camnum=%d and cropid=%d AND jd_start>%10.1f AND jd_start<%10.1f', Args.MountNumVec(Imnt), Args.CamNumVec(Icam), Args.CropID, Args.RangeJD);
            T = Args.DB.query(Query);

            UnFid=unique(T.fieldid);
            NumberedFields = UnFid(~isnan(str2double(UnFid)));
            Nf = numel(NumberedFields);
            FirstField = true;
            for If=1:1:Nf
                IndSel = find(T.fieldid==NumberedFields(If));
                if numel(IndSel)>Args.MinPointing
                    K = K + 1;
                    if FirstField
                        I1 = K;
                        FirstField = false;
                    end
                    Tsel = T(IndSel,:);
                    FieldID = str2double(NumberedFields(If));
                    
                    CosDec = cosd(median(Tsel.dec));
                    All(K,:) = [Args.MountNumVec(Imnt), Args.CamNumVec(Icam), Args.CropID, FieldID, median(Tsel.airmass), min(Tsel.airmass), max(Tsel.airmass), [std(Tsel.ra).*CosDec, std(Tsel.dec), tools.math.stat.rstd(Tsel.ra).*CosDec, tools.math.stat.rstd(Tsel.dec),  range(Tsel.ra).*CosDec, range(Tsel.dec)].*ARCSEC_DEG];

                end
            end
            I2 = K;
            PerCam(Kcam,:) = median(All(I1:I2,:));
        end
    end
   
    All = All(1:K,:);
    PerCam = PerCam(1:Kcam,:);

    All = array2table(All, 'VariableNames',ColNames);
    PerCam = array2table(PerCam, 'VariableNames',ColNames);


end
