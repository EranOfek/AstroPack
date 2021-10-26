function [Result, ResFit, PhotCat] = photometricZP(Obj, Args)
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
    % Example: [Result, ZP, PhotCat] = imProc.calib.photometricZP(SI(1))
    
    arguments
        Obj           % AstroCatalaog | AstroImage
        
        Args.Radius                   = 3;
        Args.RadiusUnits              = 'arcsec';
        Args.Method                   = 'simple';
        Args.UseOnlyMainSeq logical   = false;
        Args.MaxErr                   = 0.02;
        Args.MaxSN                    = 1000;  % if empty, do not use
        
        Args.CatColNameMag            = 'MAG_CONV_4';
        Args.CatColNameMagErr         = 'MAGERR_CONV_4';
        Args.CatColNameSN             = 'SN_4';
        
        Args.RefColNameMag            = 'Mag_BP';
        Args.RefColNameMagErr         = 'ErrMag_BP';
        Args.RefColNameMagBands       = {'Mag_RP'};
        Args.RefColNameMagBandsErr    = {'ErrMag_RP'};
        
        Args.CatName                  = 'GAIAEDR3';   % or AstroCatalog
        Args.CatOrigin                = 'catsHTM';
        Args.CatRadius                = [];   % if empty, use bounding_circle
        Args.CatRadiusUnits           = 'arcsec';
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
        
        Args.matchReturnIndicesArgs cell = {};
        
        Args.CreateNewObj logical      = false;
        Args.Plot logical              = false;
        
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
        
    %[Result, CreateNewObj] = Obj.createNewObj(Args.CreateNewObj, nargout, 0);
    
    % add RA/Dec to all catalogs
    %Result = addCoordinates2catalog(Result, Args.addCoordinates2catalogArgs{:},...
    %                                        'CreateNewObj',false);
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            error('Unknown input object type - first input arg must be AstroCatalog or AstroImage');
        end
        
        if isa(Args.CatName, 'AstroCatalog')
            % skip get astrometric cat
            PhotCat = Args.CatName;
            Npc     = numel(PhotCat);
            Ipc     = min(Npc, Iobj);
        else
            % RA/Dec bounding box
            if isempty(Args.CatRadius)
                
                [RA, Dec, CircleRadius] = boundingCircle(Cat, 'OutUnits','rad', 'CooType','sphere');
            else
                CircleRadius = Args.CatRadius;
                error('CatRadius is not yet supported, use empty');
            end
            
            % get photometric catalog
            Ipc = 1;
            [PhotCat] = imProc.cat.getAstrometricCatalog(RA, Dec, 'CatName',Args.CatName,...
                                                                  'CatOrigin',Args.CatOrigin,...
                                                                  'Radius',CircleRadius,...
                                                                  'CooUnits','rad',...
                                                                  'RadiusUnits','rad',...
                                                                  'OutUnits','rad',...
                                                                  'Con',Args.Con,...
                                                                  'UseIndex',Args.UseIndex,...
                                                                  'ColNameMag',Args.ColNameMag,...
                                                                  'RangeMag',Args.RangeMag,...
                                                                  'ColNamePlx',Args.ColNamePlx,...
                                                                  'RangePlx',Args.RangePlx);
        end
        
        
        if Args.UseOnlyMainSeq
            PhotCat = imProc.calib.selectMainSequenceFromGAIA(PhotCat, 'CreateNewObj',true);
        end
        
        % match Cat against reference (photometric) catalog
        PhotCat(Ipc).sortrows('Dec');
        %Cat.sortrows('Dec');
        
        ResMatch = imProc.match.matchReturnIndices(PhotCat(Ipc), Cat, 'Radius',Args.Radius,...
                                                                      'RadiusUnits',Args.RadiusUnits,...
                                                                      'CooType','sphere',...
                                                                      Args.matchReturnIndicesArgs{:});
        
        MatchedPhotCat = selectRows(PhotCat(Ipc), ResMatch.Obj2_IndInObj1, 'IgnoreNaN',false, 'CreateNewObj',true);
        
        
        
        % fit flux/mag to ref catalog magnitudes
        switch lower(Args.Method)
            case 'simple'
                % fit ZP and color term
                % FFU - add cleaning
                
                CatMag         = Cat.getCol(Args.CatColNameMag);
                CatMagErr      = Cat.getCol(Args.CatColNameMagErr);
                
                if isempty(Args.MaxSN)
                    SN = zeros(size(CatMag));      
                else
                    SN = Cat.getCol(Args.CatColNameSN);
                end
                
                CatXY2         = Cat.getCol({'X2','Y2'});
                W = sqrt(sum(CatXY2,2));
                MedW = median(W,1,'omitnan');
                
                RefMag         = MatchedPhotCat.getCol(Args.RefColNameMag);
                RefMagErr      = MatchedPhotCat.getCol(Args.RefColNameMagErr);
                RefMagBands    = MatchedPhotCat.getCol(Args.RefColNameMagBands);
                RefMagBandsErr = MatchedPhotCat.getCol(Args.RefColNameMagBandsErr);
                
                % calculate all colors
                [Nsrc, Nband] = size(RefMagBands);
                
                Color = RefMag - RefMagBands;
                H     = [ones(Nsrc,1), Color, Color.^2, W-MedW];
                Y     = CatMag - RefMag;
                ErrY  = sqrt(CatMagErr.^2 + sum(RefMagBandsErr.^2, 2));
                Flag  = ~isnan(Y) & CatMagErr < Args.MaxErr & SN<Args.MaxSN;
                
                ResFit(Iobj).Par    = H(Flag,:)\Y(Flag);
                ResFit(Iobj).Resid  = Y - H*ResFit(Iobj).Par;
                ResFit(Iobj).RefMag = RefMag;
                ResFit(Iobj).RefColor = Color;
                ResFit(Iobj).W      = W;
                ResFit(Iobj).MedW   = MedW;
                ResFit(Iobj).Flag   = Flag;
                ResFit(Iobj).RMS    = imUtil.background.rstd(ResFit(Iobj).Resid(Flag));
                ResFit(Iobj).Chi2   = sum((ResFit(Iobj).Resid(Flag)./ErrY(Flag)).^2);
                ResFit(Iobj).Nsrc   = sum(Flag);
                
            otherwise
                error('Unknown Method option');
        end
        
        if Args.Plot
            semilogy(ResFit(Iobj).RefMag, abs(ResFit(Iobj).Resid),'.')
            hold on;
            semilogy(ResFit(Iobj).RefMag(ResFit(Iobj).Flag), abs(ResFit(Iobj).Resid(ResFit(Iobj).Flag)),'.')
        end
    end
end