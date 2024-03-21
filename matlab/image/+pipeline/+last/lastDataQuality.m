function [Result] = lastDataQuality(Res, T, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Mar) 
    % Example: 

    arguments
        Res         = [];
        T           = [];

        Args.BasePath = '/marvin';
        Args.DirTemp  = 'LAST.01.*';
        Args.Lon      = 35;
        Args.Lat      = 30;
    end
    RAD = 180./pi;

    if isempty(Res) || isempty(T)
        D = pipeline.DemonLAST;

        PWD = pwd;
        cd(Args.BasePath);
        DL = io.files.dirDir(Args.DirTemp);
        for I=1:1:numel(DL)
            I
            D.BasePath=fullfile(DL(I).folder,DL(I).name);
            [Res,T]=D.findAllVisits('Result',Res,'ReadHeader',0);
        end
    end

    Nvisit = numel(Res);

    % add information to table
    [MoonRA,MoonDec] = celestial.SolarSys.mooncool(T.JD,[Args.Lon Args.Lat]./RAD);
    MoonHA = celestial.convert.convert_ha(MoonRA, T.JD, 'InUnits','rad','OutUnits','rad','Long',Args.Lon, 'LongUnits','deg');
    [MoonAz,MoonAlt]=celestial.coo.hadec2azalt(MoonHA, MoonDec, Args.Lat./RAD);

    T.MoonAlt = MoonAlt.*RAD;  % [deg]

    %% Median Lim. mag.
    FlagGood  = T.Airmass<1.1 & T.MoonAlt<0;
    MagVec = [17:0.1:21.5];
    MagC   = (MagVec(1:end-1) + MagVec(2:end)).*0.5;
    Nall   = histcounts(T.MedLimM, MagVec);
    Ngood  = histcounts(T.MedLimM(FlagGood), MagVec);
    plot(MagC, Nall./sum(Nall));
    hold on;
    plot(MagC, Ngood./sum(Ngood))

    xlabel('Median Lim Mag');
    ylabel('Prob. per 0.1 mag bin')

    legend('All','AM<1.1 & MoonAlt<0', 'Location','NorthWest')

    print Median_LimMag_All.jpg -djpeg100

    %% Median seeing
    FWHMVec = [1.5:0.1:7].';
    FWHMC   = (FWHMVec(1:end-1) + FWHMVec(2:end)).*0.5;
    Nmed    = histcounts(T.MedFWHM, FWHMVec);
    Nmin    = histcounts(T.MinFWHM, FWHMVec);
    Nmax    = histcounts(T.MaxFWHM, FWHMVec);

    plot(FWHMC, Nmed./sum(Nmed));
    hold on;
    plot(FWHMC, Nmin./sum(Nmin));
    plot(FWHMC, Nmax./sum(Nmax));

    xlabel('Seeing [arcsec]');
    ylabel('Prob. per 0.1 arcsec bin')

    legend('Median','Min','Max', 'Location','NorthEast')

    print Median_Seeing_All.jpg -djpeg100

    %%

    

    %%
end
