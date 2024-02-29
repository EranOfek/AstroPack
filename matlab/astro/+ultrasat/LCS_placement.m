function LCS_placement

    RAD = 180/pi;
    
    R = 7; % deg

    io.files.load1('~/matlab/data/ULTRASAT/extinction_ULTRASAT_aver_7deg.mat');
    
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
end