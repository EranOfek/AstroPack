function [Result, ResFit, PhotCat] = photometricZP(Obj, Args)
    % Calculate an absolute photometric calibration to AstroCatalog
    %       Given an AstroCatalog or AstroImage with a catalog, match the
    %       sources against a photometric catalog, and calculate the zero
    %       point (ZP) of the catalog.
    % Input  : - An AstroImage or AstroCatalog object.
    %          * ...,key,val,...
    %            'Radius' - Matching radius between the catalog and
    %                   reference catalog. Default is 3.
    %            'RadiusUnits' - Units for the 'Radius' argument.
    %                   Default is 'arcsec'.
    %            'Method' - Zero point calculation method:
    %                   'Simple' - Fit a ZP + color term + width term
    %                       model.
    %            'UseOnlyMainSeq' - A logical indicating if to use only
    %                   Main Sequence stars. Default is false.
    %            'MaxErr' - Max error of stars to use in solution.
    %                   Default is 0.02 mag.
    %            'MaxSN' - Max S/N of sources to use.
    %                   Default is 1000.
    %            'CatColNameMag' - Mag. column name in Catalog.
    %                   This magnitude will be calibrated.
    %                   Default is 'MAG_CONV_3'.
    %            'CatColNameMagErr' - Mag. error column name in Catalog.
    %                   Default is 'MAGERR_CONV_3'.
    %            'CatColNameSN' - S/N column name in Catalog.
    %                   Default is 'SN_3'.
    %            'LimMagSN' - S/N for lim. mag. calculation.
    %                   Default is 5.
    %            'LimMagColor' - Color in which to calculate the lim. mag.
    %                   Default is 1.
    %
    %            'RefColNameMag' - Mag. column name in reference catalog.
    %                   Default is 'Mag_BP'.
    %            'RefColNameMagErr' - Mag. error column name in ref. catalog.
    %                   Default is 'ErrMag_BP'.
    %            'RefColNameMagBands' - A cell array of mag column names
    %                   from which to calculate the colors.
    %                   If a single column then, Color is calculated
    %                   from 'RefColNameMag' - 'RefColNameMagBands'
    %                   If multiple columns than take the diff along the
    %                   second dimension. Default is {'Mag_RP','Mag_G'}.
    %            'RefColNameMagBandsErr' - Column names of mag. errors
    %                   corresponding to 'RefColNameMagBands'.
    %                   Default is {'ErrMag_RP','ErrMag_G'}.
    %   
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
    %            'UseIndex' - UseIndex paramter for catsHTM.
    %                   Default is false.
    %
    %            'RangeMag' - Magnitude range to retrieve.
    %                   Default is [12 19.5].
    %            'ColNamePlx' - Parallax column name.
    %                   Default is {'Plx'}.
    %            'RangePlx' - Parllax range to retrieve.
    %                   Default is [-Inf 50].
    %
    %            'UpdateHeader' - A logical indicating if to update header
    %                   with {'PH_ZP','PH_COL1','PH_COL2','PH_W','PH_MEDW','PH_RMS','PH_NSRC','PH_MAGSY','LIMMAG', 'BACKMAG'};
    %                   keywords. Applied only for AstroImage input.
    %                   Default is true.
    %
    %            'UpdateMagCols' - A logical indicating if to update the
    %                   magnbitude columns with the ZP.
    %                   NOTE: This will only add the ZP (without the other
    %                   terms) - we call this the telescope natural mag.
    %                   system. Default is true.
    %            'MagColName2update' - Either a char array containing a
    %                   string to match to all other columns, and columns containing
    %                   this string will be updated, or a cell array of
    %                   column names to update. Default is 'MAG_'.
    %
    %            'MagSys' - Magnitude system for photometric calibration.
    %                   'Vega'|'AB'. Default is 'AB',
    %            'MagZP' - Zero point that was used in order to convert
    %                   instrumental flux to instrumental mag.
    %                   Default is 25.
    %            'matchReturnIndicesArgs' - A cell array of additional
    %                   arguments to pass to imProc.match.matchReturnIndices
    %                   Default is {}.
    %            'CreateNewObj' - A logical indicating if to copy the input
    %                   object. Default is false.
    %            'Plot' - A logical indicating if to plot
    % Output : - The input object, with the (possible) mag. column names
    %            updated.
    %          - A structure array with the calibration fit results.
    %          - An AstroCatalog object containing all the retrieved
    %            photometric catalogs.
    % Author : Eran Ofek (Oct 2021)
    % Example: [Result, ZP, PhotCat] = imProc.calib.photometricZP(SI(1))
    %          [Result, ZP, PhotCat] = imProc.calib.photometricZP(SI,'UseOnlyMainSeq',0,'Plot',1, 'CreateNewObj',1);
    
    arguments
        Obj           % AstroCatalaog | AstroImage
        
        Args.Radius                   = 3;
        Args.RadiusUnits              = 'arcsec';
        Args.Method                   = 'simple';
        Args.UseOnlyMainSeq logical   = false;
        Args.MaxErr                   = 0.02;
        Args.MaxSN                    = 1000;  % if empty, do not use
        
        Args.CatColNameMag            = 'MAG_APER_3'; %'MAG_CONV_3';
        Args.CatColNameMagErr         = 'MAGERR_APER_3'; %'MAGERR_CONV_3';
        Args.CatColNameSN             = 'SN_3';
        Args.MagZP                    = 25;
        Args.MagSys                   = 'AB';  % 'AB' | 'Vega'
        Args.CatZP                    = 'GAIAEDR3';   % catalog origin of magnitudes
        
        Args.LimMagSN                 = 5;  % limiting mag for S/N calc
        Args.LimMagColor              = 1;  % Color for lim. mag calc
        
        Args.RefColNameMag            = 'Mag_BP';
        Args.RefColNameMagErr         = 'ErrMag_BP';
        Args.RefColNameMagBands       = {'Mag_RP','Mag_G'};   % red to blue...
        Args.RefColNameMagBandsErr    = {'ErrMag_RP','ErrMag_G'};
        
        Args.CatName                  = 'GAIAEDR3';   % or AstroCatalog
        Args.CatOrigin                = 'catsHTM';
        Args.CatRadius                = [];   % if empty, use bounding_circle
        Args.CatRadiusUnits           = 'arcsec';
        Args.OutUnits                 = 'rad';
        Args.Con cell                 = {};
        Args.UseIndex(1,1) logical    = false;
        
        Args.UpdateHeader logical     = true;
        
        % queryRange
        Args.RangeMag                  = [12 19.5];
        Args.ColNamePlx                = {'Plx'};
        Args.RangePlx                  = [0.1 100];  % remove galaxies
        
        % Update catalog
        Args.UpdateMagCols logical     = true;
        Args.MagColName2update         = 'MAG_';  % or e.g., {'MAG_APER_1','MAG_APER_2'}
        
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
    % ini ResFit struct
    ResFit = struct('Par',cell(Nobj,1),...
                    'ZP',cell(Nobj,1),...
                    'MagSys',cell(Nobj,1),...
                    'Resid',cell(Nobj,1),...
                    'RefMag',cell(Nobj,1),...
                    'InstMag',cell(Nobj,1),...
                    'RefColor',cell(Nobj,1),...
                    'Width',cell(Nobj,1),...
                    'MedW',cell(Nobj,1),...
                    'Flag',cell(Nobj,1),...
                    'RMS',cell(Nobj,1),...
                    'Chi2',cell(Nobj,1),...
                    'Nsrc',cell(Nobj,1),...
                    'LimMag',cell(Nobj,1),...
                    'BackMag',cell(Nobj,1));
                
          
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
                                                                  'ColNameMag',Args.RefColNameMag,...
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
                Width          = sqrt(sum(CatXY2,2));
                MedW           = median(Width,1,'omitnan');
                
                RefMag         = MatchedPhotCat.getCol(Args.RefColNameMag);
                RefMagErr      = MatchedPhotCat.getCol(Args.RefColNameMagErr);
                RefMagBands    = MatchedPhotCat.getCol(Args.RefColNameMagBands);
                RefMagBandsErr = MatchedPhotCat.getCol(Args.RefColNameMagBandsErr);
                
                %CatXY          = Cat.getCol({'X','Y'});
                
                switch lower(Args.MagSys)
                    case 'vega'
                        % do nothing GAIA is already in Vega sys
                    case 'ab'
                        %if 1==0
                        VegaToAB_Filters  = {'Mag_G','Mag_BP','Mag_RP'};
                        
                        GAIA_EDR3_ZP_VegaMinusAB = astro.mag.survey_ZP(Args.CatZP, 'VegaMinusAB');
                                                
                        I1 = find(strcmp(Args.RefColNameMag, VegaToAB_Filters));
                        RefMag = RefMag - GAIA_EDR3_ZP_VegaMinusAB(I1);
                        
                        I2 = find(ismember(VegaToAB_Filters, Args.RefColNameMagBands));
                        RefMagBands = RefMagBands - GAIA_EDR3_ZP_VegaMinusAB(I2);
                        %end
                    otherwise
                        error('Unknown MagSys option');
                end
                
                
                % calculate all colors
                [Nsrc, Nband] = size(RefMagBands);
                
                
                if size(RefMagBandsErr,2)==1
                    % Color has a single column
                    Color = RefMag - RefMagBands;
                else
                    % Color has multiple columns
                    Color = diff(RefMagBands, 1, 2);
                end
                    
                
                H     = [ones(Nsrc,1), Color, Color.^2, Width-MedW]; % CatXY];
                ResFit(Iobj).Fun = @(Par, InstMag, Color, Width, MedW) InstMag + Par(1) + Par(2).*Color + Par(3).*Color.^2 + Par(4).*(Width-MedW);
                                
                Y     = RefMag - CatMag;
                ErrY  = sqrt(CatMagErr.^2 + sum(RefMagBandsErr.^2, 2));
                Flag  = ~isnan(Y) & CatMagErr < Args.MaxErr & SN<Args.MaxSN;
                
                ResFit(Iobj).Par    = H(Flag,:)\Y(Flag);
                ResFit(Iobj).ZP     = ResFit(Iobj).Par(1) + Args.MagZP;
                ResFit(Iobj).MagSys = Args.MagSys;
                ResFit(Iobj).Resid  = Y - H*ResFit(Iobj).Par;
                ResFit(Iobj).RefMag = RefMag;
                ResFit(Iobj).InstMag = CatMag;
                ResFit(Iobj).RefColor = Color;
                ResFit(Iobj).Width  = Width;
                ResFit(Iobj).MedW   = MedW;
                ResFit(Iobj).Flag   = Flag;
                ResFit(Iobj).RMS    = imUtil.background.rstd(ResFit(Iobj).Resid(Flag));
                ResFit(Iobj).Chi2   = sum((ResFit(Iobj).Resid(Flag)./ErrY(Flag)).^2);
                ResFit(Iobj).Nsrc   = sum(Flag);
                
                % estimate limiting magnitude
                if isempty(Args.LimMagSN)
                    ResFit(Iobj).LimMag = NaN;
                else
                    ParLimMagFit = polyfit(log10(SN), ResFit(Iobj).Fun(ResFit(Iobj).Par, CatMag, Args.LimMagColor, MedW, MedW), 1);
                    ResFit(Iobj).LimMag = polyval(ParLimMagFit, log10(Args.LimMagSN));
                end
                
            otherwise
                error('Unknown Method option');
        end
        
        
        if Args.UpdateMagCols
            if ischar(Args.MagColName2update)
                MagColFlag = ~cellfun(@isempty, regexp(Cat.ColNames, Args.MagColName2update, 'match'));
            else
                MagColFlag = ismember(Cat.ColNames, Args.MagColName2update);
            end
            
            Cat.Catalog(:,MagColFlag) = Cat.Catalog(:,MagColFlag) + ResFit(Iobj).Par(1);  % donot add full ZP
             
            % This should happen automatically, but we are doing this for
            % readability and order
            if isa(Result, 'AstroImage')
                Result(Iobj).CatData = Cat;
            else
                Result(Iobj) = Cat;
            end
        end
        
        
        if Args.UpdateHeader && isa(Result, 'AstroImage')
            % write to header the following information:
            % PH_ZP
            % PH_COL1
            % PH_COL2
            % PH_W
            % PH_MEDW
            % PH_RMS
            % PH_NSRC
            % PH_MAGSY
            % LIMMAG
            % BACKMAG
            
            MedBack = fast_median(Result(Iobj).Back(:));   %, 'all', 'omitnan');
            ResFit(Iobj).BackMag = ResFit(Iobj).ZP - 2.5.*log10(MedBack);
            
            Keys = {'PH_ZP','PH_COL1','PH_COL2','PH_W','PH_MEDW','PH_RMS','PH_NSRC','PH_MAGSY','LIMMAG','BACKMAG'};
            Vals = {ResFit(Iobj).ZP, ResFit(Iobj).Par(2), ResFit(Iobj).Par(3), ResFit(Iobj).Par(4), ResFit(Iobj).MedW, ResFit(Iobj).RMS, ResFit(Iobj).Nsrc, ResFit(Iobj).MagSys, ResFit(Iobj).LimMag, ResFit(Iobj).BackMag};
            Result(Iobj).HeaderData.insertKey([Keys(:), Vals(:)], Inf);
            
        end
        
        if Args.Plot
            semilogy(ResFit(Iobj).RefMag, abs(ResFit(Iobj).Resid),'.')
            hold on;
            semilogy(ResFit(Iobj).RefMag(ResFit(Iobj).Flag), abs(ResFit(Iobj).Resid(ResFit(Iobj).Flag)),'.')
            H = xlabel('Mag');
            H.FontSize = 18;
            H.Interpreter = 'latex';
            H = ylabel('$\vert$Resid$\vert$');
            H.FontSize = 18;
            H.Interpreter = 'latex';
            
        end
    end
end