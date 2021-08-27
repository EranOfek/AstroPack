function [Result, ZP, PhotCat] = photometricZP(Obj, Args)
    % Apply an absolute photometric calibration to AstroCatalog
    %       Given an AstroCatalog or AstroImage with a catalog, match the
    %       sources against a photometric catalog, and calculate the zero
    %       point (ZP) of the catalog.
    % Inout  : -
    %          * ...,key,val,...
    
    %            'CatName' - Either an astrometric catalog name (char
    %                   array) to query around the requested coordinates,
    %                   or an AstroCatalog object containing such a
    %                   catalaog.
    %                   Default is 'GAIAEDR3'.
    %            'CatOrigin' - Catalog origin (relevant if CatName is a
    %                   char array).
    %                   Default is 'catsHTM'.
    %            'CatRadius' - Catalog query radius.
    %                   If empty will attempt to estimate automatically
    %                   from the diagonal of the image in pixels, and the
    %                   max(scale).
    %                   Default is 1400.
    %            'CatRadiusUnits' - CatRadius units.
    %                   Default is 'arcsec'.
    %            'Con' - Additional constraints for the catalog query.
    %                   See catsHTM.cone_search. Default is {}.
    
    %            'ColNameMag' - Column name containing mag.
    %                   Default is {'Mag_BP','Mag'}.
    %            'RangeMag' - Magnitude range to retrieve.
    %                   Default is [12 19.5].
    %            'ColNamePlx' - Parallax column name.
    %                   Default is {'Plx'}.
    %            'RangePlx' - Parllax range to retrieve.
    %                   Default is [-Inf 50].
    
    % Output :
    % Author :
    % Example: 
    
    arguments
        Obj           % AstroCatalaog | AstroImage
        
        Args.Radius                   = 3;
        Args.RadiusUnits              = 'arcsec';
        Args.Method                   = 'simple';
        
        Args.CatColNameMag            = 'PSF_MAG';
        Args.CatColNameMagErr         = 'PSF_MAGERR';
        
        Args.RefColNameMag            = 'Mag_BP';
        Args.RefColNameMagErr         = 'ErrMag_BP';
        Args.RefColNameMagBands       = {'Mag_RP'};
        Args.RefColNameMagBandsErr    = {'ErrMag_RP'};
        
        Args.CatName                  = 'GAIAEDR3';   % or AstroCatalog
        Args.CatOrigin                = 'catsHTM';
        Args.Radius                   = 1400;
        Args.RadiusUnits              = 'arcsec';
        Args.CooUnits                 = 'deg';
        Args.Shape
        Args.OutUnits                 = 'rad';
        Args.Con cell                 = {};
        Args.UseIndex(1,1) logical    = false;
        
        % queryRange
        Args.ColNameMag                = {'Mag_BP','Mag'};
        Args.RangeMag                  = [12 19.5];
        Args.ColNamePlx                = {'Plx'};
        Args.RangePlx                  = [0.1 100];  % remove galaxies
        
        Args.CreateNewObj          = [];
        
    end
    
    [Result, CreateNewObj] = Obj.createNewObj(Args.CreateNewObj, nargout, 0);
    
    % add RA/Dec to all catalogs
    Result = addCoordinates2catalog(Result, Args.addCoordinates2catalogArgs{:},...
                                            'CreateNewObj',false);
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            error('Unknown input object type - first input arg must be AstroCatalog or AstroImage');
        end
        
        % RA/Dec bounding box
        [RA, Dec, CircleRadius] = boundingCircle(Result, 'OutUnits','rad', 'CooType','sphere');
        
        % get photometric catalog
        [PhotCat] = imProc.cat.getAstrometricCatalog(RA, Dec, 'CatName',Args.CatName,...
                                                              'CatOrigin',Args.CatOrigin,...
                                                              'Radius',CircleRadius,...
                                                              'CooUnits','rad',...
                                                              'OutUnits','rad',...
                                                              'Con',Args.Con,...
                                                              'UseIndex',Args.UseIndex,...
                                                              'ColNameMag',Args.ColNameMag,...
                                                              'RangeMag',Args.RangeMag,...
                                                              'ColNamePlx',Args.ColNamePlx,...
                                                              'RangePlx',Args.RangePlx);
                                                          
        % match Cat against reference (photometric) catalog
        PhotCat.sortrows('Dec');
        Cat.sortrows('Dec');
        [MatchedPhotCat] = match(PhotCat, Cat, 'Radius',Args.Radius,...
                                               'RadiusUnits',Args.RadiusUnits,...
                                               'AddIndInRef',true,...
                                               'IndInRefColName','IndInRef',...
                                               'IndInRefColPos',Inf,...
                                               'DeleteExistIndInRef',true,...
                                               'AddDistCol',true,...
                                               'DistUnits','arcsec',...
                                               'DistColName','Dist',...
                                               'DistColPos',Inf,...
                                               'AddNmatchCol',true,...
                                               'NmatchColPos',Inf,...
                                               'NmatchColName',Nmatch);
        
        % fit flux/mag to ref catalog magnitudes
        switch lower(Args.Method)
            case 'simple'
                % fit ZP and color term
                % FFU - add cleaning
                
                CatMag         = Cat.getCol(Args.CatColNameMag);
                CatMagErr      = Cat.getCol(Args.CatColNameMagErr);
                
                RefMag         = MatchedPhotCat.getCol(Args.RefColNameMag);
                RefMagErr      = MatchedPhotCat.getCol(Args.RefColNameMagErr);
                RefMagBands    = MatchedPhotCat.getCol(Args.RefColNameMagBands);
                RefMagBandsErr = MatchedPhotCat.getCol(Args.RefColNameMagBandsErr);
                
                % calculate all colors
                [Nsrc, Nband] = size(RefMagBands);
                
                Color = RefMag - RefMagBands;
                H     = [ones(Nsrc,1), Color];
                Y     = CatMag - RefMag;
                ErrY  = sqrt(CatMagErr.^2 + sum(RefMagBandsErr.^2, 2));
                
                ResFit(Iobj).Par   = H\Y;
                ResFit(Iobj).Resid = Y - H*ResFit(Iobj).Par;
                ResFit(Iobj).RMS   = std(ResFit(Iobj).Resid);
                ResFit(Iobj).Chi2  = sum((ResFit(Iobj).Resid./ErrY).^2);
                
            otherwise
                error('Unknown Method option');
        end
        
    end
end