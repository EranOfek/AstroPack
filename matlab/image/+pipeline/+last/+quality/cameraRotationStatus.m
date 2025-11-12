function [Summary] = cameraRotationStatus(Args)
    % Summarize the LAST camera on sky rotations as measured from DB information.
    % Input  : * ...,key,val,... 
    %            See code for details.
    % Output : - A structute with a table with median rotation per camera
    % Author : Eran Ofek (2025 Nov) 
    % Example: S=pipeline.last.quality.cameraRotationStatus


    arguments
        Args.DB                = [];
        Args.MountNumVec       = (1:10);   % mounts to check
        Args.CamNumVec         = (1:4);    % cam to check
        Args.MaxFWHM           = 5;
        Args.MaxAirmass        = 1.4;
        Args.RangeJD           = [celestial.time.julday([1 9 2025]), celestial.time.julday([1 10 2025])];   % JD range to check
      
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

    ColNames = {'mountnum','camnum','rotation'};
    %[CellEmpty{1:numel(ColNames)}]=deal([]);
    %Summary.MedianTable = table(CellEmpty{:}, 'VariableNames',ColNames);
    Summary = zeros(Nmnt.*Ncam, numel(ColNames));
    K = 0;
    for Imnt=1:1:Nmnt
        for Icam=1:1:Ncam
            K = K + 1;
            Query = sprintf('SELECT * FROM visit_images WHERE mountnum=%d and camnum=%d AND jd_start>%10.1f AND jd_start<%10.1f AND fwhm<%4.1f AND airmass<%5.2f', Args.MountNumVec(Imnt), Args.CamNumVec(Icam), Args.RangeJD, Args.MaxFWHM, Args.MaxAirmass);
            T = Args.DB.query(Query);

            [Result] = imUtil.astrometry.cdmatrix2rotScale(T.cd1_1, T.cd1_2, T.cd2_1, T.cd2_2);

            Summary(K,:) = [Args.MountNumVec(Imnt), Args.CamNumVec(Icam), median(Result.PA_deg)];
           
        end
    end
    Summary = array2table(Summary, 'VariableNames',ColNames);

end
