function Result = unitTest()
    % unitTest for +imProc.astrometry
    % Example: imProc.astrometry.unitTest
   
    % astrometryCore
    RA     = 1;     % rad
    Dec    = 1;     %rad
    Radius = 1000;  % arcsec
    % get catalog
    Ref    = catsHTM.cone_search('GAIAEDR3', RA, Dec, Radius, 'OutType','AstroCatalog');
    % project catalog with pix scale of 1"/pix
    Scale = 3600.*180./pi;  % ./PixScale ["/pix]
    Cat    = imProc.trans.projection(Ref, RA, Dec, Scale, 'TAN', 'Coo0Units','rad');
    % plot(Cat.Catalog(:,31),Cat.Catalog(:,32),'.')
    
    Result = imProc.astrometry.astrometryCore(Cat, 'RA', RA, 'Dec', Dec)
    
end
   