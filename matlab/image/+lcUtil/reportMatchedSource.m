function [Result] = reportMatchedSource(MS, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Feb) 
    % Example: 

    arguments
        MS MatchedSources
        Ind1                   = 1;
        Args.CalcZP logical    = true;
        Args.FreqVec           = [];
        Args.PS                = [];
        Args.MagField          = 'MAG_PSF';
        Args.MagErrField       = 'MAGERR_PSF';
        Args.TimeUnitsLC       = 'min'; 
    end

    if Args.CalcZP
        Rzp = lcUtil.zp_meddiff(MS, 'MagField',Args.MagField', 'MagErrField',Args.MagErrField);
        MS.applyZP(Rzp.FitZP);
    end
    
    Nobs = numel(MS.JD);
    
    % rms
    MeanMag = median(MS.Data.(Args.MagField), 1, 'omitnan');
    StdMag  = std(MS.Data.(Args.MagField), [], 1, 'omitnan');
    Fn0 = StdMag>1e-10;


    B = timeSeries.bin.binningFast([MeanMag(Fn0).', StdMag(Fn0).'], Args.BinSize, [NaN NaN], {'MidBin', @median, @tools.math.stat.std_mad, @numel});
    % Remove points with less than 5 measurments
    B = B(B(:,4)>5,:);

    
    In0  = find(B(:,2)<1e-4,1,'first');
    if ~isempty(In0)
        if In0==size(B,1)
            % 0 at faintestr mag
            B(In0,2) = B(In0-1,2);
        else
            B(1:In0,2) = B(In0+1,2);
        end
       
    end

    % adding a bright point
    B = [[B(1,1)-10, B(1, 2:end)]; B];

    Bstd = B(:,2)./sqrt(Nobs);

    StdThreshold = interp1(B(:,1), B(:,2)+Bstd.*Args.NsigmaRMS, MeanMag(:), 'linear','extrap');
    
    
    % Basic data
    VecRA  = MS.Data.(Args.RAField)(:,Ind1);
    VecDec = MS.Data.(Args.DecField)(:,Ind1);

    RA  = median(VecRA,1,'omitnan');
    Dec = median(VecDec,1,'omitnan');
    Mag = median(MS.Data.(Args.MagField)(:,Ind1),1,'omitnan');
    MeanJD = median(MS.JD);

    % Positional noise [deg]
    StdRA  = std(VecRA,[],1,'omitnan').*cosd(Dec);
    StdDec = std(VecDec,[],1,'omitnan');

    % poly std
    ResPolyHP = lcUtil.fitPolyHyp(MS, 'PolyDeg',{0, (0:1:2)});
    
    % external catalogs

    
    [SimbadURL]=VO.search.simbad_url(RA./RAD, Dec./RAD);
    SDSSURL    = VO.SDSS.navigator_link(RA./RAD, Dec./RAD);
    PS1URL     = VO.PS1.navigator_link(RA./RAD, Dec./RAD);
    
    % start report
    Report = Document("mydoc","html");
    
    TitleText = mlreportgen.dom.Text("Light curve variability report");
    TitleText.FontSize = "32pt";
    
    add(Report, TitleText);
    
    add(Report, sprintf("First MatchedSources file name : %s",MS.FileNames{1});
    add(Report, sprintf("Src Id in MatchedSources: %d",Ind1));
    add(Report, sprintf("RA  = %11.7f        %s",RA, celestial.coo.convertdms(RA,'d','SH')));
    add(Report, sprintf("Dec = %11.7f        %s",Dec, celestial.coo.convertdms(Dec,'d','SD')));
    add(Report, sprintf("RMS RA  = %6.3f arcsec", StdRA.*3600));
    add(Report, sprintf("RMS Dec = %6.3f arcsec", StdDec.*3600));
    add(Report, sprintf("Magnitude median    = %7.3f", median(MS.Data.(Args.MagField)(:,Ind1),1,'omitnan')));
    add(Report, sprintf("Magnitude std       = %7.3f", std(MS.Data.(Args.MagField)(:,Ind1),[],1,'omitnan')));
    
    add(Report, sprintf("Magnitude poly-std  = %7.3f", NaN);
    add(Report, sprintf("Magnitude poly-chi2 = %7.3f/%d", NaN, NaN);
    
    % write external catalog info
    
    % URLs
    add(Report, sprintf("SIMBAD : %s",SimbadURL.URL));
    add(Report, sprintf("SDSS   : %s",SDSSURL{1}));
    add(Report, sprintf("PS1    : %s",PS1URL{1}));
    
     
    %figure(1)
    cla;
    MS.plotRMS;
    hold on;
    plot(B(:,1),B(:,2),'b-');
    plot(B(:,1),B(:,2)+Bstd.*Args.NsigmaRMS,'b-');
    plot(MeanMag(Ind1), StdMag(Ind1),'rh')
    H = xlabel('Mag');
    H.FontSize = 16;
    H.Interpreter = 'latex';
    H = ylabel('rms (mag)');
    H.FontSize = 16;
    H.Interpreter = 'latex';
    
    add(Report, Figure);

    
        
    %figure(2);
    cla;
    plot(Args.FreqVec,Args.PS(:,Ind1));
    H = xlabel('Frequency [1/day]');
    H.FontSize = 16;
    H.Interpreter = 'latex';
    H = ylabel('Power');
    H.FontSize = 16;
    H.Interpreter = 'latex';
    
    add(Report, Figure);
    
    %figure(3);
    cla;
    TimeVec = MS.JD(:) - min(MS.JD(:);
    TimeVec = convert.timeUnits('day', Args.TimeUnitsLC, TimeVec);
    plot(TimeVec, MS.Data.(Args.MagField)(:,Ind1), 'o','MarkerFaceColor','k');
    %hold on;
    %plot(MS.JD(:), LimMagQuantile, 'v')
    plot.invy
    %
    H = xlabel(sprintf('Time [%s]',Args.TimeUnitsLC));
    H.FontSize = 16;
    H.Interpreter = 'latex';
    H = ylabel('Mag');
    H.FontSize = 16;
    H.Interpreter = 'latex';
    
    add(Report, Figure);

end
