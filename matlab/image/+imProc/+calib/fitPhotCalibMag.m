function [Result, ResFit] = fitPhotCalibMag(Obj, Args)
    % To replace imProc.calib.photometricZP - but not ready!
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Feb) 
    % Example: R=imProc.calib.fitPhotCalibMag(AI);

    arguments
        Obj

        Args.IsGoodImage            = []; % use all

        % --- Reference Catalog ---
        Args.CatOrigin              = 'catsHTM';
        Args.CatName                = 'GAIADR3';
        Args.CatZP                  = 'GAIADR3'; % used for: imProc.calib.getGaiaMagColor
        Args.CatRadius              = [];   % if empty, use bounding_circle
        Args.CatRadiusUnits         = 'arcsec';
        Args.Con cell               = {};
        Args.UseIndex               = false;
        

        Args.RefColMag              = 'phot_bp_mean_mag'; %'Mag_BP';
        Args.RefColMagErr           = 'phot_bp_mean_flux_over_error'; %'ErrMag_BP';  
        Args.RefColColor            = {'phot_bp_mean_mag','phot_rp_mean_mag'};  %{'Mag_RP','Mag_G'};   % red to blue...
        Args.RefColColorErr         = {'phot_bp_mean_flux_over_error','phot_rp_mean_flux_over_error'};
        Args.RefIsErrSN             = true;
        Args.MaxErr                 = 0.02;
        Args.MagRange               = [13 18];
        Args.SelectCrit             = {'Plx',[0.05 1000], ''}
        Args.MagSys                 = 'AB';
        Args.UseOnlyMainSeq         = false;

        %--- Matching ---
        Args.Radius                 = 3;
        Args.RadiusUnits            = 'arcsec';
        Args.matchReturnIndicesArgs = {};

        %--- Input catalog ---
        Args.CatColMag              = 'MAG_APER_3'; %{'MAG_PSF', 'MAG_APER_3'}; %'MAG_APER_3'; %'MAG_CONV_3';
        Args.CatColMagErr           = 'MAGERR_APER_3'; %, 'MAGERR_APER_3'}; %'MAGERR_CONV_3';
        Args.CatIsErrSN             = false;
        Args.CatMom2                = {};  % {'X2','Y2','XY'};
        Args.CatPos                 = {'X','Y'};
        Args.CatAM                  = 'AIRMASS';

        %--- fit ---
        Args.MagZP                  = 25;
        Args.ColorOrder             = 1;
        Args.LimMagSN               = 5;   % estimate lim mag at this S/N
        Args.MinSN                  = 5;
        Args.MaxSN                  = 50;  % for estimating lim mag.
        Args.LimMagColor            = 1;
        Args.PosOrder               = [1 0; 0 1; 1 1];
        %--- General ---
        Args.UpdateHeader           = true;
        Args.Plot                   = true;
        Args.PixScale               = [];
        Args.CreateNewObj           = false;
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
                    'UsedColMag',cell(Nobj,1),...
                    'UsedColMagErr',cell(Nobj,1),...
                    'Resid',cell(Nobj,1),...
                    'RefMag',cell(Nobj,1),...
                    'InstMag',cell(Nobj,1),...
                    'RefColor',cell(Nobj,1),...
                    'Width',cell(Nobj,1),...
                    'MedC',cell(Nobj,1),...                
                    'MedW',cell(Nobj,1),...
                    'Flag',cell(Nobj,1),...
                    'RMS',cell(Nobj,1),...
                    'Chi2',cell(Nobj,1),...
                    'Nsrc',cell(Nobj,1),...
                    'LimMag',cell(Nobj,1),...
                    'BackMag',cell(Nobj,1));
          
    if isempty(Args.IsGoodImage)
        % Read IsGoodImage from WCS
        if isa(Obj, 'AstroCatalog')
            % set IsGoodImage to all true
            IsGoodImage = true(size(Obj));
        else
            % AstroImage, AstroZOGY, ...
            IsGoodImage = true(size(Obj));
            for Iobj=1:1:Nobj
                IsGoodImage(Iobj) = Result(Iobj).WCS.Success;
            end
        end
    else
        if isscalar(Args.IsGoodImage)
            IsGoodImage = repmat(Args.IsGoodImage, size(Obj));
        else
            IsGoodImage = Args.IsGoodImage;
        end
    end

    
    %PhotCat = [];
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
            %GoodAstrometry = true;   % assume astrometry is goog
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
            %GoodAstrometry = Result(Iobj).WCS.Success;
        else
            error('Unknown input object type - first input arg must be AstroCatalog or AstroImage');
        end
        
        %if GoodAstrometry
        if IsGoodImage(Iobj)
            if isa(Args.CatName, 'AstroCatalog')
                % skip get astrometric cat
                if isscalar(Args.CatName)
                    PhotCat = Args.CatName.copy;  % create a new copy, as this catalog is modified
                    Npc     = numel(PhotCat);
                    Ipc     = 1; %min(Npc, Iobj);
                else
                    if numel(Args.CatName)~=numel(Obj)
                        error('Number of catalogs in CatName is not consistent');
                    end
                end
            else
                % RA/Dec bounding box
                if isempty(Args.CatRadius)
                    [RA, Dec, CircleRadius] = boundingCircle(Cat, 'OutUnits','rad', 'CooType','sphere');
                else
                    CircleRadius = Args.CatRadius;
                    error('CatRadius is not yet supported, use empty');
                end

                if Iobj==1
                    PhotCat = AstroCatalog([Nobj 1]);
                end

                % get photometric catalog
                Ipc = Iobj;
                [PhotCat(Iobj)] = imProc.cat.getAstrometricCatalog(RA, Dec, 'CatName',Args.CatName,...
                                                                      'CatOrigin',Args.CatOrigin,...
                                                                      'Radius',CircleRadius,...
                                                                      'CooUnits','rad',...
                                                                      'RadiusUnits','rad',...
                                                                      'OutUnits','rad',...
                                                                      'Con',Args.Con,...
                                                                      'UseIndex',Args.UseIndex,...
                                                                      'ColNameMag',Args.RefColMag,...
                                                                      'RangeMag',Args.MagRange);
            end % if isa(Args.CatName, 'AstroCatalog')

            if Iobj==Ipc
                % no need to run this unless this is the first time we
                % treat the PhotCat:
                if Args.UseOnlyMainSeq
                    PhotCat(Ipc) = imProc.calib.selectMainSequenceFromGAIA(PhotCat(Ipc), 'CreateNewObj',false);
                end

                % match Cat against reference (photometric) catalog
                %PhotCat(Ipc).sortrows('Dec');
                PhotCat(Ipc).sortrows('Dec');
                %Cat.sortrows('Dec');
            end

            ResMatch = imProc.match.matchReturnIndices(PhotCat(Ipc), Cat, 'Radius',Args.Radius,...
                                                                          'RadiusUnits',Args.RadiusUnits,...
                                                                          'CooType','sphere',...
                                                                          Args.matchReturnIndicesArgs{:});

            MatchedPhotCat = selectRows(PhotCat(Ipc), ResMatch.Obj2_IndInObj1, 'IgnoreNaN',false, 'CreateNewObj',false);

            % Get information from catalogs
            % Get mag and color from MatchedPhotCat

            
            [RefMag, RefMagErr, RefColor, RefColorErr, RefSelectedInd] = imProc.calib.getGaiaMagColor(MatchedPhotCat, 'ColMag',Args.RefColMag,...
                                                                                                                      'ColMagErr',Args.RefColMagErr,...
                                                                                                                      'ColColor',Args.RefColColor,...
                                                                                                                      'ColColorErr',Args.RefColColorErr,...
                                                                                                                      'IsErrSN',Args.RefIsErrSN,...
                                                                                                                      'MaxErr',Args.MaxErr,...
                                                                                                                      'MagRange',Args.MagRange,...
                                                                                                                      'SelectCrit',Args.SelectCrit,...
                                                                                                                      'MagSys',Args.MagSys,...
                                                                                                                      'CatZP',Args.CatZP);
        


        
            %--- Input catalog ---
            [CatMag]       = Cat.getCol(Args.CatColMag);
            [CatMagErr]    = Cat.getCol(Args.CatColMagErr);
            ResFit(Iobj).UsedColMag       = Args.CatColMag; %UsedColMag{1};
            ResFit(Iobj).UsedColMagErr    = Args.CatColMagErr; %UsedColMagErr{1};

            CatMag    = CatMag(RefSelectedInd);
            CatMagErr = CatMagErr(RefSelectedInd);

            if Args.CatIsErrSN
                CatMagErr = 1.086./CatMagErr;
            end

            % 2nd moments
            if ~isempty(Args.CatMom2)
                Mom2 = Cat.getCol(Args.CatMom2);
                Mom2 = Mom2(SelectedInd,:);
                [PSF_AB] = imUtil.psf.mom2shape(Mom2(:,1), Mom2(:,2), Mom2(:,3));
                PSF_A = PSF_AB.A;
                PSF_B = PSF_AB.B;
                PSF_T = PSF_AB.Theta;
                Width = sqrt(PSF_A.^2 + PSF_B.^2);
            else
                PSF_A = [];
                PSF_B = [];
                PSF_T = [];
                Width = [];
            end
        
            if ~isempty(Args.CatPos)
                XY = Cat.getCol(Args.CatPos);
                X  = XY(RefSelectedInd,1);
                Y  = XY(RefSelectedInd,2);
            else
                X = [];
                Y = [];
            end

            % airmass
            if isempty(Args.CatAM)
                AM = [];
            else
                AM = Cat.getCol(Args.CatAM);
                AM = AM(RefSelectedInd);
            end

            %--- Use the collected data to solve the ZP ---


            [Rzp,~,VarY] = imUtil.calib.simplePhotometricZP([CatMag, CatMagErr],[RefMag,RefMagErr],'Color',RefColor,'ColorOrder',Args.ColorOrder,'Width',Width, 'MaxMagErr',Args.MaxErr, 'X',X, 'Y',Y, 'AM',AM);
            if Rzp.Ndof<2
                [Rzp,~,VarY] = imUtil.calib.simplePhotometricZP([CatMag, CatMagErr],[RefMag,RefMagErr],'Color',RefColor,'ColorOrder',Args.ColorOrder,'Width',Width, 'MaxMagErr',Args.MaxErr.*2);
            end
            ResFit(Iobj).Par = Rzp.Par;

            %ResFit(Iobj).ZP     = ResFit(Iobj).Par(1) + Args.MagZP;
            ResFit(Iobj).ZP     = Args.MagZP - ResFit(Iobj).Par(1);
            ResFit(Iobj).MagSys = Args.MagSys;
            ResFit(Iobj).Resid  = Rzp.AllResid; %Y - H*ResFit(Iobj).Par;
            ResFit(Iobj).RefMag = RefMag;
            ResFit(Iobj).InstMag = CatMag;
            ResFit(Iobj).RefColor = RefColor;
            ResFit(Iobj).Width  = Width;
            ResFit(Iobj).MedC   = Rzp.MeanVec(2);
            if ~isempty(Args.CatMom2)
                ResFit(Iobj).MedW   = Rzp.MeanVec(4);
            else
                ResFit(Iobj).MedW   = NaN;
            end
            ResFit(Iobj).Flag   = Rzp.FlagGood;
            ResFit(Iobj).RMS    = imUtil.background.rstd(ResFit(Iobj).Resid(ResFit(Iobj).Flag));
            ResFit(Iobj).Chi2   = sum(ResFit(Iobj).Resid(ResFit(Iobj).Flag).^2 ./VarY(ResFit(Iobj).Flag));
            ResFit(Iobj).Nsrc   = sum(ResFit(Iobj).Flag);

            if isempty(Args.CatMom2) && Args.ColorOrder==1
                ResFit(Iobj).Fun = @(Par, InstMag, Color, MedC) InstMag - Par(1) - Par(2).*(Color-MedC);
            else
                error('Unsupported option for ColorOrder');
            end

            % estimate limiting magnitude
            if isempty(Args.LimMagSN)
                ResFit(Iobj).LimMag = NaN;
            else
                %ParLimMagFit = polyfit(log10(SN), ResFit(Iobj).Fun(ResFit(Iobj).Par, CatMag, Args.LimMagColor, ResFit(Iobj).MedC, ResFit(Iobj).MedW, ResFit(Iobj).MedW), 1);
                % select only positive S/N:
                SN = 1./(1.086.*CatMagErr);
                Isn = find(SN>Args.MinSN & SN<Args.MaxSN);
                ParLimMagFit = polyfit(log10(SN(Isn)), ResFit(Iobj).Fun(ResFit(Iobj).Par, CatMag(Isn), Args.LimMagColor, ResFit(Iobj).MedC), 1);
                ResFit(Iobj).LimMag = polyval(ParLimMagFit, log10(Args.LimMagSN));
            end

            if Args.UpdateHeader && isa(Result, 'AstroImage')
                % write to header the following information:
                % PH_ZP
                % PH_COL1
                % PH_COL2
                % PH_W
                % PH_MEDC
                % PH_MEDW
                % PH_RMS
                % PH_NSRC
                % PH_MAGSY
                % LIMMAG
                % BACKMAG

                MedBack = fast_median(Result(Iobj).Back(:));   %, 'all', 'omitnan');
                if isempty(Args.PixScale)
                    % try to read pixel scale from WCS
                    if isa(Obj, 'AstroImage')
                        PixScale = Obj(Iobj).WCS.getScale('arcsec');
                    else
                        error('Can not get pixel scale - either provide it, or use AstroImage with WCS data');
                    end
                else
                    PixScale = Args.PixScale;
                end
                ResFit(Iobj).BackMag = ResFit(Iobj).ZP - 2.5.*log10(MedBack) + 5.*log10(PixScale);  % per aecsec^2
                
                
                if Args.ColorOrder==1 && isempty(Args.CatMom2)
                    Keys = {'PH_ZP','PH_COL1','PH_MEDC','PH_RMS','PH_NSRC','PH_MAGSY','LIMMAG','BACKMAG','PH_MAGT','PH_MAGTE'};
                    Vals = {ResFit(Iobj).ZP,...
                            ResFit(Iobj).Par(2),...
                            ResFit(Iobj).MedC,...
                            ResFit(Iobj).RMS,...
                            ResFit(Iobj).Nsrc,...
                            ResFit(Iobj).MagSys,...
                            ResFit(Iobj).LimMag,...
                            ResFit(Iobj).BackMag,...
                            ResFit(Iobj).UsedColMag,...
                            ResFit(Iobj).UsedColMagErr};
                else %if Args.UpdateHeader && isa(Result, 'AstroImage')

                    Keys = {'PH_ZP','PH_COL1','PH_COL2','PH_W','PH_MEDC','PH_MEDW','PH_RMS','PH_NSRC','PH_MAGSY','LIMMAG','BACKMAG','PH_MAGT','PH_MAGTE'};
                    Vals = {ResFit(Iobj).ZP,...
                            ResFit(Iobj).Par(2),...
                            ResFit(Iobj).Par(3),...
                            ResFit(Iobj).Par(4),...
                            ResFit(Iobj).MedC,...
                            ResFit(Iobj).MedW,...
                            ResFit(Iobj).RMS,...
                            ResFit(Iobj).Nsrc,...
                            ResFit(Iobj).MagSys,...
                            ResFit(Iobj).LimMag,...
                            ResFit(Iobj).BackMag,...
                            ResFit(Iobj).UsedColMag,...
                            ResFit(Iobj).UsedColMagErr};
                end
                    
                %Result(Iobj).HeaderData.insertKey([Keys(:), Vals(:)], Inf);
                Result(Iobj).HeaderData.replaceVal(Keys, Vals);
                

            end

            if Args.Plot
                figure(1)
                semilogy(ResFit(Iobj).RefMag, abs(ResFit(Iobj).Resid),'.')
                hold on;
                semilogy(ResFit(Iobj).RefMag(ResFit(Iobj).Flag), abs(ResFit(Iobj).Resid(ResFit(Iobj).Flag)),'.')
                H = xlabel('B$_{\rm p}$ [mag]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('$\vert$Resid$\vert$ [mag]');
                H.FontSize = 18;
                H.Interpreter = 'latex';


                % limiting magnitude plot
                figure(2)
                ColorVec = [0.6:0.2:1.4];
                NcV      = numel(ColorVec);
                Colors   = plot.generate_colors(NcV-1);
                for IcV=1:1:NcV-1
                    Icolor = RefColor>ColorVec(IcV) & RefColor<ColorVec(IcV+1);
                    %semilogy(RefMag(Icolor), SN(Icolor), 'k.','Color',Colors(IcV,:));
                    semilogy(RefMag(Icolor), SN(Icolor), 'k.'); %,'Color',Colors(IcV,:));
                    hold on;
                end
                H = xlabel('B$_{\rm p}$ [mag]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('$S/N$');
                H.FontSize = 18;
                H.Interpreter = 'latex';
            end % if Args.Plot


        end % if IsGoodImage(Iobj)
    end % for Iobj=1:1:Nobj

end
