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
        Args.MaxFWHM           = 5;
        Args.MaxAirmass        = 1.4;
        Args.RangeJD           = [celestial.time.julday([1 9 2025]), celestial.time.julday([1 10 2025])];   % JD range to check
        Args.MinPointing       = 30;        % min number of observations per field to use.
        Args.DB                = [];        % DB object, if empty generate
        Args.RefTel            = 3;         % The reference telescope
        Args.RAdiff            = 61.28./60; % Required RA  shift, Deg
        Args.Decdiff           = 90./60;    % required Dec shigt, Deg
        Args.Parallel          = false;     % Are the mount in parallel or open mode
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

    % If the telescopes in a parallel mode zero the gap value
    if (Args.Parallel)
       Args.RAdiff = 0;
       Args.Decdiff = 0;
    end

    Nmnt = numel(Args.MountNumVec);
    Ncam = numel(Args.CamNumVec);

    ColNames = {'Mount', 'CamNum', 'CropID', 'FieldID','medAirMass','minAirMass','maxAirMass', 'stdRA', 'stdDec', 'rstdRA', 'rstdDec', 'rangeRA', 'rangeDec'};
    All    = nan(1e4, numel(ColNames));
    PerCam = nan(10.*4, numel(ColNames));
    K    = 0;
    Kcam = 0;

    % Telescopes shift direction
    Multiplier = [1, 1; 1, -1; -1, -1; -1, 1];
    % Add a shift to telescope determined by the reference
    RefFlag = [ 0  0 1 1  0 1 1  0;
                0  0 1 1 -1 0 0 -1;
               -1 -1 0 0 -1 0 0 -1;
               -1 -1 0 0  0 1 1  0];

    for Imnt=1:1:Nmnt
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
                MeanDec = T.dec(Igroup(Args.RefTel));
                DeltaRA  = (T.ra(Igroup)-T.ra(Igroup(Args.RefTel))).*cosd(MeanDec);
                DeltaDec = T.dec(Igroup)-T.dec(Igroup(Args.RefTel));
                K = K + 1;
                Summary(Imnt).Data(K,:) = [T.airmass(Igroup(Args.RefTel)), T.m_ha(Igroup(Args.RefTel)), T.m_dec(Igroup(Args.RefTel)), DeltaRA.', DeltaDec.'];
            end
        end
        Summary(Imnt).Data   = Summary(Imnt).Data(1:K,:);
        Summary(Imnt).Median = nanmedian(Summary(Imnt).Data);
        Summary(Imnt).Std    = std(Summary(Imnt).Data);

%        figure(Imnt);
        Ref0 = median(Summary(Imnt).Data(:,4:11).*60);
%        plot(Summary(Imnt).Data(:,1), Summary(Imnt).Data(:,4:11).*60-Ref0,'.')
    end

    % Plot median offset
    figure
    set(gcf, 'Position', [1725           3         740        1059]);
    plot(0,0,'+k','MarkerSize',20)
    hold on
    plot([61.28, 61.28, -61.28, -61.28], [90 -90 -90 90], 'k+', 'MarkerSize',15)
    for Imnt=1:1:Nmnt
       plot(Summary(Imnt).Median(4:7).*60 + Multiplier(Args.RefTel,1).*61.28,     Summary(Imnt).Median(8:11).*60 + Multiplier(Args.RefTel,2).*90, '.r','LineWidth',2)
       text(Summary(Imnt).Median(4:7).*60 + Multiplier(Args.RefTel,1).*61.28 + 2, Summary(Imnt).Median(8:11).*60 + Multiplier(Args.RefTel,2).*90, num2str(Args.MountNumVec(Imnt)),'FontSize',10)
    end
    axis equal
    axis auto
    axis([-90 90 -120 120])
    xlabel('RA [arcmin]')
    ylabel('Dec [arcmin]')
    title(strcat('Ref telescope: T', num2str(Args.RefTel)))

    % Plot Summary in a printable table
    fprintf('\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    fprintf('Shifts [deg]:\t\tRight Ascension\t\t||\t\tDeclination\n')
    fprintf('\t\tCam1\tCam2\tCam3\tCam4\t||\tCam1\tCam2\tCam3\tCam4\n')
    for Imnt=1:1:Nmnt
       fprintf('Mount %d:\t%.2f\t%.2f\t%.2f\t%.2f\t||\t%.2f\t%.2f\t%.2f\t%.2f\n', Args.MountNumVec(Imnt), ...
               ((Summary(Imnt).Median([4:7,8:11]) + ...
               [Args.RAdiff, Args.RAdiff, Args.RAdiff, Args.RAdiff, ...
                Args.Decdiff, Args.Decdiff, Args.Decdiff, Args.Decdiff].* ...
                RefFlag(Args.RefTel,:).*2)).*[-1 -1 -1 -1 -1 -1 -1 -1])
    end
    fprintf('\nPositive value - move telescope east or north\n')
    fprintf('Negative value - move telescope west or south\n')


end
