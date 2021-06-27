function Result = unitTest()
    % unitTest for +imProc.astrometry
    % Example: imProc.astrometry.unitTest
   
    % astrometryCore (with catalog data)
    RA     = 1;     % rad
    Dec    = 1;     % rad
    Radius = 1000;  % arcsec
    % get catalog
    Ref    = catsHTM.cone_search('GAIAEDR3', RA, Dec, Radius, 'OutType','AstroCatalog');
    % project catalog with pix scale of 1"/pix
    Scale = 3600.*180./pi;  % ./PixScale ["/pix]
    Cat    = imProc.trans.projection(Ref, RA, Dec, Scale, 'TAN', 'Coo0Units','rad');
    Cat = queryRange(Cat, AstroCatalog.DefNamesMag, [11 19]);
    % plot(Cat.Catalog(:,31),Cat.Catalog(:,32),'.')
    Cat.Catalog(:,31) = Cat.Catalog(:,31) + 10;
    Cat.Catalog(:,32) = Cat.Catalog(:,32) + 20;
    
    Result = imProc.astrometry.astrometryCore(Cat, 'RA', RA, 'Dec', Dec, 'CooUnits','rad');
    
    % astrometryCore (with real data)
    cd /home/eran/matlab/images
    AI = AstroImage('PTF_201211203837_i_p_scie_t091230_u014655064_f02_p100037_c02.fits');
    imProc.sources.findMeasureSources(AI);
    
    
    
end
   