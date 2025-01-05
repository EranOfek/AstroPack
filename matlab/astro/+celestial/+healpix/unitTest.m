function Result = unitTest()
    % unitTest for celestial.healpix
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % cone search
    RA  = 50.; 
    Dec = 30.;
    Nside = 16;
    Rad   = 10; %deg
    
    R1 = celestial.healpix.coneSearchRecur(Nside,RA,Dec,Rad,'RadiusUnits','deg','CooUnits','deg');
    R2 = celestial.healpix.coneSearch(Nside,RA,Dec,Rad,'RadiusUnits','deg','CooUnits','deg');
    
    [Lon1, Lat1] = celestial.healpix.pix2ang(Nside, R1, 'CooUnits','deg');
    [Lon2, Lat2] = celestial.healpix.pix2ang(Nside, R2, 'CooUnits','deg');
     
    figure(1); clf; hold off
    plot(RA,Dec,'+','Color','black','LineWidth',3);
    xlabel 'RA'; ylabel 'Dec'
    hold on    
    plot(Lon1,Lat1,'*','Color','red');
    plot(Lon2,Lat2,'o','Color','blue');
    
    try
        Command = sprintf('python3 ~/healpix_cone_search.py %d %.2f %.2f %.2f', Nside, RA, Dec, Rad);
        [Status, R0] = system(Command);    
        [Lon0, Lat0] = celestial.healpix.pix2ang(Nside, str2num(R0), 'CooUnits','deg');
        plot(Lon0,Lat0,'d','Color','cyan','LineWidth',1);
    catch
    end
    
    Result = true;
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
end
