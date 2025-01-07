function Result = unitTest()
    % unitTest for celestial.healpix
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    RAD = 180./pi;
        
    % test celestial.healpix.ang2pix and celestial.healpix.pix2ang
        
    
    % cone search
    RA  = 200.67; % 50.; 
    Dec = 50.4; % 30.;
    Nside = 2^8; % 16;
    Rad   = 10; %deg
    
    R1 = celestial.healpix.coneSearchRecur(Nside,RA,Dec,Rad,'RadiusUnits','deg','CooUnits','deg');
    R2 = celestial.healpix.coneSearch(Nside,RA,Dec,Rad,'RadiusUnits','deg','CooUnits','deg');    
%     R2 = celestial.healpix.coneSearch(Nside,RA/RAD,Dec/RAD,Rad/RAD);
    
    [Lon1, Lat1] = celestial.healpix.pix2ang(Nside, R1, 'CooUnits','deg');
    [Lon2, Lat2] = celestial.healpix.pix2ang(Nside, R2, 'CooUnits','deg');
     
    figure(1); clf; hold off
    plot(RA,Dec,'+','Color','black','LineWidth',3);
    xlabel 'RA'; ylabel 'Dec'
    hold on    
    plot(Lon1,Lat1,'*','Color','red');
    plot(Lon2,Lat2,'o','Color','blue');
    
    try
        Command = sprintf('python3 ~/matlab/AstroPack/matlab/astro/+celestial/+healpix/healpix_cone_search.py %d %.2f %.2f %.2f', Nside, RA, Dec, Rad);
        [Status, R00] = system(Command);    
        R0 = unique(str2num(R00));
        [Lon0, Lat0] = celestial.healpix.pix2ang(Nside, R0, 'CooUnits','deg');
        plot(Lon0,Lat0,'d','Color','cyan','LineWidth',1);
    catch
        fprintf('astropy error: %s \n',Status);
    end
    
    Result = true;
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
end
