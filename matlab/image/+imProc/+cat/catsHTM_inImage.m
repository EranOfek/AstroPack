function [Cat] = catsHTM_inImage(CatName, Obj, Args)
    % Query catsHTM catalog for all sources within an AstroImage footprint (corners).
    %   The footprint of the AstroImage is contained either in the image
    %   header or calculated from the WCS.
    %     See also: imProc.cat.withinPoly, imProc.astrometry.getCooCorners
    % Input  : - catsHTM catalog name. E.g., 'GAIADR3'.
    %          - An AstroImage/AstroDiff/AstroZOGY object from which to get
    %            the image corners. If this is a multi elemnt object, then
    %            the output has multiple elements.
    %          * ...,key,val,... 
    %            'CenterFromWCS' - A logical indicating if to get image
    %                   center from WCS or to measure it by fitting a circle that
    %                   enclose the sources in the catalog.
    %                   Default is true.
    %            'CornersFromWCS' - A logical indicating if to use the WCS (true)
    %                   or to try and read the corner keywords from the
    %                   header (false). If truem then, will use the image
    %                   size along with xy2sky WCS method to evaluate the
    %                   corners RA/Dec coordinates.
    %                   Default is true.
    %            'KeyCornersRA' - A cell array of header keywords corners
    %                   for RA. Default is {'RA1','RA2','RA3','RA4'}.
    %            'KeyCornersDec' - A cell array of header keywords corners
    %                   for Dec. Default is {'DEC1','DEC2','DEC3','DEC4'}.
    %            'PolyUnits' - Units of the polygon (corners).
    %                   Default is 'deg'.
    % Output : - An AstroCatalog object containing a catalog of sources
    %            inside the footprint for each image.
    %            The number of elements (catalogs) equal to the number of
    %            images in the second input argument.
    % Author : Eran Ofek (2025 Dec) 
    % Example: Cat=imProc.cat.catsHTM_inImage('GAIADR3',AllSI(1));

    arguments
        CatName
        Obj
        Args.CenterFromWCS     = true;

        Args.CornersFromWCS    = true;
        Args.KeyCornersRA      = {'RA1','RA2','RA3','RA4'};
        Args.KeyCornersDec     = {'DEC1','DEC2','DEC3','DEC4'};

        Args.PolyUnits         = 'deg';
    end
    RAD = 180./pi;
    ARCSEC_DEG = 3600;


    % get corners
    [CornersRA, CornersDec] = imProc.astrometry.getCooCorners(Obj, 'UseWCS',Args.CornersFromWCS, 'KeyCornersRA',Args.KeyCornersRA, 'KeyCornersDec',Args.KeyCornersDec);

    % get center and FoV
    CenterFov = imProc.astrometry.getCooCenter(Obj, 'UseWCS',Args.CenterFromWCS);
        

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
    
        Cat(Iobj) = catsHTM.cone_search(CatName, CenterFov(Iobj,1)./RAD, CenterFov(Iobj,2)./RAD, CenterFov(Iobj,3).*ARCSEC_DEG, 'OutType','AstroCatalog');        
        Cat(Iobj) = imProc.cat.withinPoly(Cat(Iobj), CornersRA(:,Iobj), CornersDec(:,Iobj), 'CooTYpe','sphere', 'PolyUnits',Args.PolyUnits);

    end

end
