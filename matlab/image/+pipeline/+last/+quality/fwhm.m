function [Summary] = fwhm(Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: Summary=pipeline.last.quality.fwhm;
    %          Summary=pipeline.last.quality.fwhm('DB',DB,'MountNumVec',1,'CamNumVec',1);

    arguments
        Args.MountNumVec       = (1:10);   % mounts to check
        Args.CamNumVec         = (1:4);    % cam to check
        Args.CropIdVec         = (1:24);
        Args.MaxFWHM           = 5;
        Args.MaxAirmass        = 2;
        Args.RangeJD           = [celestial.time.julday([1 9 2025]), celestial.time.julday([1 10 2025])];   % JD range to check
        Args.MinPointing       = 30;        % min number of observations per field to use.
        Args.DB                = [];        % DB object, if empty generate
        Args.RefTel            = 3;         % The reference telescope
        Args.RAdiff            = 61.28./60; % Required RA  shift, Deg
        Args.Decdiff           = 90./60;    % required Dec shigt, Deg
        Args.Parallel          = false;     % Are the mount in parallel or open mode


        Args.CropMap = [1 7 13 19; 2 8 14 20; 3 9 15 21; 4 10 16 22; 5 11 17 23; 6 12 18 24];
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
    Ncrop = numel(Args.CropIdVec);

    ColNames = {'mountnum','camnum','cropid','crop_x','crop_y','fwhm','med_a','med_b','med_th','elon'};
    %[CellEmpty{1:numel(ColNames)}]=deal([]);
    %Summary.MedianTable = table(CellEmpty{:}, 'VariableNames',ColNames);
    Summary.MedianTable = zeros(Nmnt.*Ncam.*Ncrop, numel(ColNames));
    K = 0;
    for Imnt=1:1:Nmnt
        for Icam=1:1:Ncam
            for Icrop=1:1:Ncrop
                K = K + 1;
                Query = sprintf('SELECT * FROM visit_images WHERE mountnum=%d and camnum=%d and cropid=%d AND jd_start>%10.1f AND jd_start<%10.1f AND fwhm<%4.1f AND airmass<%5.2f', Args.MountNumVec(Imnt), Args.CamNumVec(Icam), Args.CropIdVec(Icrop), Args.RangeJD, Args.MaxFWHM, Args.MaxAirmass);
                T = Args.DB.query(Query);

                Flag = T.airmass<1.2;
                
                Elon =  T.med_a./T.med_b;

                Ilin = find(Args.CropIdVec(Icrop)==Args.CropMap(:));
                [Y,X] = ind2sub(size(Args.CropMap),Ilin);


                Summary.MedianTable(K,:) = [Args.MountNumVec(Imnt), Args.CamNumVec(Icam), Args.CropIdVec(Icrop),...
                                            X, Y,...
                                            median(T.fwhm(Flag)),...
                                            median(T.med_a(Flag)),...
                                            median(T.med_b(Flag)),...
                                            median(T.med_th(Flag)),...
                                            median(Elon(Flag))];




            end

        end
    end
    Summary.MedianTable = array2table(Summary.MedianTable, 'VariableNames',ColNames);


end
