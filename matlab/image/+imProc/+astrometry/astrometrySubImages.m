function [ResultRefineFit, ResultObj, AstrometricCat] = astrometrySubImages(Obj, Args)
    % Solve astrometry for sub images of a single contigious image
    %       The solution is done by executing astrometryCore for a limited
    %       number of sub images, and astrometryRefine for all the rest,
    %       based on the solution from astrometryCore.
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Aug 2021)
    % Example:
   
    arguments
        Obj AstroImage                    % AstroImage array with catalogs
        Args.CCDSEC(:,4)
        Args.CenterXY                            = [];  % [X,Y] pix Center of Full image, If empty, calculate from CCDSEC
        Args.RA
        Args.Dec
        Args.CooUnits                            = 'deg';
        Args.Scale                               = 1.0;
        Args.Tran                                = Tran2D('poly3');
        
        Args.EpochOut                            = [];
        
        Args.CreateNewObj                        = [];
        
        Args.CatName                             = 'GAIAEDR3';  % or AstroCatalog array
        Args.astrometryCoreArgs cell             = {};
        Args.astrometryRefineArgs cell           = {};
        
        Args.MinNumberCoreSolutions              = 1;
        Args.assessAstrometricQualityArgs cell   = {};
    end
    
    [ResultObj] = createNewObj(Obj, Args.CreateNewObj, nargout, 1);
    
    %Args.Tran.symPoly;   % will work only for handle class: copyObject/CreateNewObj
    
    Nobj = numel(Obj);
    if Nobj ~= size(Args.CCDSEC,1)
        error('Number of lines in CCDSEC must be equal to the number of elements in the input Obj');
    end
    
    % Center of sub images
    [SubCenterX, SubCenterY] = imUtil.ccdsec.center_ccdsec(Args.CCDSEC);
    SubCenterX = SubCenterX(:);
    SubCenterY = SubCenterY(:);
    if isempty(Args.CenterXY)
        % calculate CenterXY of full image from Args.CCDSEC
        Args.CenterXY = [0.5.*(min(Args.CCDSEC(:,1) + max(Args.CCDSEC(:,2)))),...
                         0.5.*(min(Args.CCDSEC(:,3) + max(Args.CCDSEC(:,4))))];
    end
    % Distance of sub images from full image center
    SubDistFromCenter = sqrt((SubCenterX - Args.CenterXY(1)).^2 + (SubCenterY - Args.CenterXY(2)).^2);
    % Sort SubImages by distance from image center (nominal position)
    [~,SI]  = sort(SubDistFromCenter);
    
    UseRefinment        = false;
    Sucess              = false(size(Obj));  % sucessful solution
    
    % do we need to define this if CatName is AstroCatalog???
    AstrometricCat      = AstroCatalog(size(Obj));
    for Iobj=1:1:Nobj
        % for each sub image
        
        if sum(Sucess)>=Args.MinNumberCoreSolutions
            % use astrometryCore
            UseRefinement = true;
        else
            % switch to refine mode
            UseRefinement = false;
        end
       
        if ~UseRefinement
            % Use core solution
            
            % select sub image index, after sorting by distance of sub image
            % from full image
            Iim = SI(Iobj);
            
            % astrometric solution
            if isa(Args.CatName,'AstroCatalog')
                CatName = Args.CatName(Iim);
            else
                CatName = Args.CatName;
            end
            
            % FFU: estimate RA/Dec for center of image
            
            %tic;
            [ResultFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryCore(ResultObj(Iim),...
                                                                                                     'Tran',Args.Tran,...
                                                                                                     'RA',Args.RA,...
                                                                                                     'Dec',Args.Dec,...
                                                                                                     'CooUnits',Args.CooUnits,...
                                                                                                     'EpochOut',Args.EpochOut,...
                                                                                                     'CatName',CatName,...
                                                                                                     'Scale',Args.Scale,...
                                                                                                     Args.astrometryCoreArgs{:});
            %toc
            % populate the WCS in the AstroImage
            %ResultObj(Iim).WCS = ResultFit(Iim).WCS;
            
            ResultRefineFit(Iim).ParWCS = ResultFit(Iim).ParWCS;
            ResultRefineFit(Iim).Tran   = ResultFit(Iim).Tran;
            ResultRefineFit(Iim).ResFit = ResultFit(Iim).ResFit;
            ResultRefineFit(Iim).WCS    = ResultFit(Iim).WCS;
            
            % check qulity of solution
            Sucess(Iim) = ResultFit(Iim).WCS.Success;
            %[Sucess(Iim), QualitySummary(Iim)] = imProc.astrometry.assessAstrometricQuality(ResultFit(Iim).ResFit, Args.assessAstrometricQualityArgs{:});
           
        else
            % run astrometryRefine
            % find a sub image which have nearby sucessful solution
            % matrix of distances : rows - sucssful; lines - ~sucessful
            DistSubMat   = sqrt((SubCenterX(Sucess) - SubCenterX(~Sucess).').^2 + (SubCenterY(Sucess) - SubCenterY(~Sucess).').^2);
            [~,IndMin]   = min(DistSubMat,[],'all','linear');
            [MinI,MinJ]  = imUtil.image.ind2sub_fast(size(DistSubMat), IndMin);
            FS           = find(Sucess);
            FNS          = find(~Sucess);
            IndSucess    = FS(MinI); 
            IndNotSucess = FNS(MinJ);
            % Index of image to solve
            Iim  = IndNotSucess;
            % index of image from which to take the WCS solution
            Iref = IndSucess;
            
            % shift solution to current CCDSEC
            % ?? This line doesn't copy the Tran object - is this ok?
            RefWCS = ResultObj(Iref).WCS.copy; %copyObject;
            
%             % the shift in CRPIX between the image and the ref
            ShiftX = Args.CCDSEC(Iim,1) - Args.CCDSEC(Iref,1);
            ShiftY = Args.CCDSEC(Iim,3) - Args.CCDSEC(Iref,3);
            % add shift to CRPIX - why??????
            RefWCS.CRPIX = RefWCS.CRPIX - [ShiftX, ShiftY];
            
            % call astrometryRefine with RefWCS and includeDistortion=false
            if isa(Args.CatName,'AstroCatalog')
                CatName = Args.CatName(Iim);
            else
                CatName = Args.CatName;
            end
            
            
            % DEBUGING
            % Test that the Iim image with the translated Iref WCS is ok
            %Tmp = ResultObj(Iim);
            %Tmp.WCS = RefWCS;
            %Tmp.HeaderData = wcs2header(RefWCS, Tmp.HeaderData);
            %ds9(Tmp)
          
                        
            % DEBUGING
            % ResultObj(Iim).WCS = RefWCS;
            % ResultObj(Iim).HeaderData = wcs2header(RefWCS);
            % ds9(ResultObj(Iim))
%             tic;

            %[Iim, Iref]
% if Iim==40 && Iref==31
%    'a'
% end
            UseRefine = true;
            if UseRefine
                %tic;
                [ResultRefineFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryRefine(ResultObj(Iim),...
                                                                                                           'WCS',RefWCS, ...
                                                                                                           'IncludeDistortions',false,...
                                                                                                           'Tran',Args.Tran,...
                                                                                                           'SearchRadius',5,...
                                                                                                           'Scale',Args.Scale,...
                                                                                                           'RA',[],...
                                                                                                           'Dec',[],...
                                                                                                           'CooUnits','deg',...
                                                                                                           'EpochOut',Args.EpochOut,...
                                                                                                           'CatName',CatName,...  
                                                                                                           Args.astrometryCoreArgs{:});
%toc
                                                                                                      
            else
              



                % Estimate RA/Dec of SubImage center
                CenterX = (Args.CCDSEC(Iim,2) - Args.CCDSEC(Iim,1)).*0.5;
                CenterY = (Args.CCDSEC(Iim,4) - Args.CCDSEC(Iim,3)).*0.5;
                [RA, Dec] = RefWCS.xy2sky(CenterX, CenterY, 'OutUnits',Args.CooUnits, 'includeDistortion',false);

                [ResultFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryCore(ResultObj(Iim),...
                                                                                                         'Tran',Args.Tran,...
                                                                                                         'RA',RA,...
                                                                                                         'Dec',Dec,...
                                                                                                         'RangeX',[-100 100],...
                                                                                                         'RangeY',[-100 100],...
                                                                                                         'CooUnits',Args.CooUnits,...
                                                                                                         'EpochOut',Args.EpochOut,...
                                                                                                         'CatName',CatName,...
                                                                                                         'Scale',Args.Scale,...
                                                                                                         Args.astrometryCoreArgs{:});

                % populate the WCS in the AstroImage
                %ResultObj(Iim).WCS = ResultFit(Iim).WCS;

                ResultRefineFit(Iim).ParWCS = ResultFit(Iim).ParWCS;
                ResultRefineFit(Iim).Tran   = ResultFit(Iim).Tran;
                ResultRefineFit(Iim).ResFit = ResultFit(Iim).ResFit;
                ResultRefineFit(Iim).WCS    = ResultFit(Iim).WCS;
            
            end
                
            % check qulity of solution
            %[Sucess(Iim), QualitySummary(Iim)] = imProc.astrometry.assessAstrometricQuality(ResultRefineFit(Iim).ResFit, Args.assessAstrometricQualityArgs{:});
            %ResultRefineFit(Iim).WCS.Success
            Sucess(Iim) = ResultRefineFit(Iim).WCS.Success;
            
            %[Sucess(Iim), QualitySummary(Iim)] = imProc.astrometry.assessAstrometricQuality(ResultFit(Iim).ResFit, Args.assessAstrometricQualityArgs{:});
            
        end
        
    
    end
    
end
    