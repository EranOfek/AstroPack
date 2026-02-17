function [Result] = fitPhotCalibMag(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Feb) 
    % Example: 

    arguments
        Obj

        Args.IsGoodImage            = []; % use all

        % --- Reference Catalog ---
        Args.CatName                = 'GAIADR3';
        Args.CatZP                  = 'GAIADR3'; % used for: imProc.calib.getGaiaMagColor
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

        %--- Input catalog ---
        Args.CatColMag            = 'MAG_APER_3'; %{'MAG_PSF', 'MAG_APER_3'}; %'MAG_APER_3'; %'MAG_CONV_3';
        Args.CatColMagErr         = 'MAGERR_APER_3'; %, 'MAGERR_APER_3'}; %'MAGERR_CONV_3';
        Args.CatIsErrSN           = false;
        Args.CatMom2              = {};  % {'X2','Y2','XY'};
        Args.CatPos               = {};  %{'X','Y'};
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
                                                                      'ColNameMag',Args.RefColNameMag,...
                                                                      'RangeMag',Args.RangeMag,...
                                                                      'ColNamePlx',Args.ColNamePlx,...
                                                                      'RangePlx',Args.RangePlx);
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
            [CatMag,~,~,UsedColMag]       = Cat.getCol(Args.CatColMag);
            [CatMagErr,~,~,UsedColMagErr] = Cat.getCol(Args.CatColMagErr);
            ResFit(Iobj).UsedColMag       = UsedColMag{1};
            ResFit(Iobj).UsedColMagErr    = UsedColMagErr{1};

            CatMag    = CatMag(SelectedInd);
            CatMagErr = CatMagErr(SelectedInd);

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
            else
                PSF_A = [];
                PSF_B = [];
                PSF_T = [];
            end
        
            if ~isempty(Args.CatPos)
                XY = Cat.getCol(Args.CatPos);
                X  = XY(SelectedInd,1);
                Y  = XY(SelectedInd,2);
            else
                X = [];
                Y = [];
            end

            %--- Use the collected data to solve the ZP ---


            [Rzp,~,VarY] = imUtil.calib.simplePhotometricZP([CatMag, CatMagErr],[RefMag,RefMagErr],'Color',Color,'ColorOrder',Args.ColorOrder,'Width',Width, 'MaxMagErr',Args.MaxErr);
                    if Rzp.Ndof<2
                        [Rzp,~,VarY] = imUtil.calib.simplePhotometricZP([CatMag, CatMagErr],[RefMag,RefMagErr],'Color',Color,'ColorOrder',Args.ColorOrder,'Width',Width, 'MaxMagErr',Args.MaxErr.*2);
                    end
                    ResFit(Iobj).Par = Rzp.Par;

                    %ResFit(Iobj).ZP     = ResFit(Iobj).Par(1) + Args.MagZP;
                    ResFit(Iobj).ZP     = Args.MagZP - ResFit(Iobj).Par(1);
                    ResFit(Iobj).MagSys = Args.MagSys;
                    ResFit(Iobj).Resid  = Rzp.AllResid; %Y - H*ResFit(Iobj).Par;
                    ResFit(Iobj).RefMag = RefMag;
                    ResFit(Iobj).InstMag = CatMag;
                    ResFit(Iobj).RefColor = Color;
                    ResFit(Iobj).Width  = Width;
                    ResFit(Iobj).MedC   = Rzp.MeanVec(2);
                    if Args.UseWidth
                        ResFit(Iobj).MedW   = Rzp.MeanVec(4);
                    else
                        ResFit(Iobj).MedW   = NaN;
                    end
                    ResFit(Iobj).Flag   = Rzp.FlagGood;
                    ResFit(Iobj).RMS    = imUtil.background.rstd(ResFit(Iobj).Resid(ResFit(Iobj).Flag));
                    ResFit(Iobj).Chi2   = sum(ResFit(Iobj).Resid(ResFit(Iobj).Flag).^2 ./VarY(ResFit(Iobj).Flag));
                    ResFit(Iobj).Nsrc   = sum(ResFit(Iobj).Flag);

                    if ~Args.UseWidth && Args.ColorOrder==1
                        ResFit(Iobj).Fun = @(Par, InstMag, Color, MedC) InstMag - Par(1) - Par(2).*(Color-MedC);
                    else
                        error('Unsupported option');
                    end

                    % estimate limiting magnitude
                    if isempty(Args.LimMagSN)
                        ResFit(Iobj).LimMag = NaN;
                    else
                        %ParLimMagFit = polyfit(log10(SN), ResFit(Iobj).Fun(ResFit(Iobj).Par, CatMag, Args.LimMagColor, ResFit(Iobj).MedC, ResFit(Iobj).MedW, ResFit(Iobj).MedW), 1);
                        % select only positive S/N:
                        Isn = find(SN>Args.MinSN & SN<Args.MaxSN);
                        ParLimMagFit = polyfit(log10(SN(Isn)), ResFit(Iobj).Fun(ResFit(Iobj).Par, CatMag(Isn), Args.LimMagColor, ResFit(Iobj).MedC), 1);
                        ResFit(Iobj).LimMag = polyval(ParLimMagFit, log10(Args.LimMagSN));
                    end




        end % if IsGoodImage(Iobj)
    end % for Iobj=1:1:Nobj

end
