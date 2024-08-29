function LCS_placement(Args) 
    %
    arguments             
            Args.StartDate = '2028-01-01 00:00:00';
            Args.NumDays   =  360; % [days]
            Args.TimeBin   = 0.01; % [days] 0.01 day = 864 s ~ 3 x 300 s              
    end
    % 
    RAD = 180/pi;

    R = 7; % deg

    io.files.load1('~/matlab/data/ULTRASAT/extinction_ULTRASAT_aver_7deg.mat');
    
    LimitType = {'SunLimits','EarthLimits','MoonLimits','PowerLimits'};
    NType = numel(LimitType);

    figure(1)
    plot.ungridded_image(RA_st,Dec_st,Stat); caxis([0,1])

    % add fields
    hold on; xlabel '\lambda, deg'; ylabel '\beta, deg';

    Lng = linspace(0,360,1000);
    border = 50*ones(numel(Lng),1);
    plot(Lng,border,'Color','black');
    plot(Lng,-border,'Color','black');

    [Lon,Lat]=celestial.coo.celestial_circ(10/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(33.5/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(57/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(80.5/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(104/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(346.5/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(323/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(299.5/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(276/RAD,-50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(10/RAD,-64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(44/RAD,-64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(78/RAD,-64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(112/RAD,-64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(336/RAD,-64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(302/RAD,-64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(330/RAD,-79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(51/RAD,-79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(133/RAD,-79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    %   18

    [Lon,Lat]=celestial.coo.celestial_circ(88/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(111.5/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(134/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(157.5/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(181/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(204.5/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(228/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(251.5/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(275/RAD,50/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(98/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(132/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(166/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(200/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(234/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(268/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(302/RAD,64/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(75/RAD,79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(157/RAD,79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(239/RAD,79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    [Lon,Lat]=celestial.coo.celestial_circ(321/RAD,79/RAD,R/RAD,300);
    Lon(Lon<0) = Lon(Lon<0)+2*pi;
    plot(Lon*RAD,Lat*RAD,'.','Color','red')

    l = readtable('~/matlab/data/ULTRASAT/LCS_v3_list.txt');
    plot(l.Var1,l.Var2,'*','Color','white')
    
    % 20 + 18 = 38 
    
    % probe visibility for Args.TimeBin bins during Args.NumDays:
    
    [Grid(:,1), Grid(:,2)] = celestial.coo.convert_coo(l.Var1./RAD,l.Var2./RAD,'e','j2000.0');    
    
    JD = celestial.time.julday(Args.StartDate) + (0:Args.TimeBin:Args.NumDays)';
    Nt = length(JD); 
    
    Vis = ultrasat.ULTRASAT_restricted_visibility(JD,Grid,'MinSunDist',(70)./RAD,'MinMoonDist',(34)./RAD,'MinEarthDist',(56)./RAD);
    
    LimitsCombined = Vis.PowerLimits .* Vis.SunLimits .* Vis.MoonLimits .* Vis.EarthLimits;
    Limits2 =  Vis.PowerLimits .* Vis.SunLimits .* Vis.MoonLimits;  % observations only when the Sun is behind the Earth

    Month = 30 / Args.TimeBin; % number of bins in a month
    Nm    = 12;                % number of months
    
    for iM = 1:Nm
        bin1 = (iM-1)*Month+1; bin2 = iM*Month;
        LimMonth(iM,:)  = prod(LimitsCombined(bin1:bin2,:));
        LimMonth2(iM,:) = prod(Limits2(bin1:bin2,:));
    end
    
    figure(2); subplot(1,2,1); imagesc(LimMonth); xlabel 'Field number'; ylabel 'Month number'; title 'full limits'; colorbar
    subplot(1,2,2); imagesc(LimMonth2); xlabel 'Field number'; ylabel 'Month number'; title 'w/o Earth limits'; colorbar
    
    [(1:Nm)' sum(LimMonth2,2)] % number of fields visible in each month
    
    [(1:height(l))' sum(LimMonth2,1)] % number of months when each of the fields is visible
            
%     for IType = 1:NType     
%         Limits = Vis.(LimitType{IType});
% %         MaxLen.(LimitType{IType}) = uninterruptedLength(Limits, Np, Nt).* Args.TimeBin; % convert to [days]    
%     end

end