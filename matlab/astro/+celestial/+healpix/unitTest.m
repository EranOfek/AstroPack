function Result = unitTest()
    % unitTest for celestial.healpix
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    RAD = 180./pi;
        
    % test celestial.healpix.ang2pix and celestial.healpix.pix2ang
      
    %% celestial.healpix  nest2xyf & xyf2nest
    NSide = 8;         
    Pix=(0:767); 
    [X,Y,F] = celestial.healpix.nest2xyf(NSide, Pix);
    [p] = celestial.healpix.xyf2nest(NSide,X,Y,F);
    if max(abs(Pix-double(p)))>0
        error('Problem with celestial.healpix.nest2xyf or celestial.healpix.xyf2nest');
    end

    %% celestial.healpix.pixRadius
    NSide = [1 2 4 8 16 64 256];
    [R, MaxR] = celestial.healpix.pixRadius(NSide);
    % cap area equals pixel area
    if max(abs(2.*pi.*(1-cos(R)) ./ (4.*pi./(12.*NSide.^2)) - 1)) > 1e-10
        error('Problem with celestial.healpix.pixRadius: equal-area radius');
    end
    % healpy max_pixrad values
    MaxRHealpy = [0.8410686705679302 0.48146047147606696 0.2543334045529589 ...
                  0.13042543553450464 0.06601476143251347 0.01665302521141051 0.004172560737300081];
    if max(abs(MaxR./MaxRHealpy - 1)) > 1e-12 || ~isequal(size(MaxR), size(NSide))
        error('Problem with celestial.healpix.pixRadius: max radius');
    end

    %% celestial.healpix.coneSearch: cone smaller than a pixel returns its pixel
    Pix0 = celestial.healpix.mex.ang2pix_nested(4, 1, 0.5);
    Pix  = celestial.healpix.coneSearch(4, 1, 0.5, 1e-4);
    if ~any(Pix==Pix0)
        error('Problem with celestial.healpix.coneSearch: small cone misses the centre pixel');
    end

    %% celestial.healpix.findNeighbors
    RAD = 180./pi;
    Pix=(1:1e5);
    In1 =celestial.healpix.findNeighbors(2.^16,Pix);
    In2 = celestial.healpix.mex.neighbors_nested(2.^16, int64(Pix));

    [Lon0,Lat0]=celestial.healpix.pix2ang(2.^16, Pix);
    Lon0 = Lon0(:).';
    Lat0 = Lat0(:).';
    [Lon,Lat]=celestial.healpix.pix2ang(2.^16,In1(:)); 

    Lon=reshape(Lon,size(In1));
    Lat=reshape(Lat,size(In1));
    D=celestial.coo.sphere_dist_fast(Lon,Lat,Lon0,Lat0);
    MinD = min(D.*180./pi.*3600, [], 'all');
    MaxD = max(D.*180./pi.*3600, [],'all');

    if MinD<3 || MaxD>5 || sum(In1-In2 > 0,'all') > 0
        error('Problem with celestial.healpix.findNeighbors');
    end


    %% nestedNeighbors(NSide, Pix)
    % NSide = 8;
    % Pix   = (0:1:767);
    % PN    = celestial.healpix.nestedNeighbors(NSide, Pix);
    % 
    % [Lon0, Lat0] = celestial.healpix.pix2ang(NSide, Pix);
    % [Lon, Lat] = celestial.healpix.pix2ang(NSide, PN);
    % Lon = reshape(Lon, size(PN));
    % Lat = reshape(Lat, size(PN));
    % 
    % D = celestial.coo.sphere_dist_fast(Lon0.', Lat0.', Lon, Lat);

    %%
    
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
