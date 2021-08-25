function [ResultObj, ResultFit, AstrometricCat] = astrometrySubImages(Obj, Args)
    % Solve astrometry for sub images of a single contigious image
    %       The solution is done by executing astrometryCore for a limited
    %       number of sub images, and astrometryRefine for all the rest,
    %       based on the solution from astrometryCore.
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Aug 2021)
    % Example:
   
    arguments
        Obj                    % AstroImage array with catalogs
        Args.CCDSEC(:,4)
        Args.CenterXY                            = [];  % [X,Y] pix Center of Full image, If empty, calculate from CCDSEC
        Args.RA
        Args.Dec
        Args.CooUnits                            = 'deg';
        Args.Scale                               = 1.0;
        
        Args.CreateNewObj                        = [];
        
        Args.CatName                             = 'GAIAEDR3';  % or AstroCatalog array
        Args.astrometryCoreArgs cell             = {};
        Args.astrometryRefineArgs cell           = {};
        
        Args.MinNumberCoreSolutions = 1;
        Args.assessAstrometricQualityArgs cell   = {};
    end
    
    [ResultObj, CreateNewObj] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
    
    
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
            
            
            [ResultFit(Iim), ResultObj(Iim).CatData, AstrometricCat(Iim)] = imProc.astrometry.astrometryCore(ResultObj(Iim).CatData,...
                                                                                                     'RA',Args.RA,...
                                                                                                     'Dec',Args.Dec,...
                                                                                                     'CooUnits',Args.CooUnits,...
                                                                                                     'CatName',CatName,...
                                                                                                     'Scale',Args.Scale,...
                                                                                                     Args.astrometryCoreArgs{:});
            
            % populate the WCS in the AstroImage
            ResultObj(Iim).WCS = ResultFit(Iim).WCS;
            
            ResultRefineFit(Iim).ParWCS = ResultFit(Iim).ParWCS;
            ResultRefineFit(Iim).Tran   = ResultFit(Iim).Tran;
            ResultRefineFit(Iim).ResFit = ResultFit(Iim).ResFit;
            ResultRefineFit(Iim).WCS    = ResultFit(Iim).WCS;
            
            % check qulity of solution
            [Sucess(Iim), QualitySummary(Iim)] = imProc.astrometry.assessAstrometricQuality(ResultFit(Iim).ResFit, Args.assessAstrometricQualityArgs{:});
           
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
            RefWCS = ResultObj(Iref).WCS.copyObject;
            % the shift in CRPIX between the image and the ref
            ShiftX = Args.CCDSEC(Iim,1) - Args.CCDSEC(Iref,1);
            ShiftY = Args.CCDSEC(Iim,3) - Args.CCDSEC(Iref,3);
            % add shift to CRPIX
            RefWCS.CRPIX = RefWCS.CRPIX - [ShiftX, ShiftY];
            
            % call astrometryRefine with RefWCS and includeDistortion=false
            if isa(Args.CatName,'AstroCatalog')
                CatName = Args.CatName(Iim);
            else
                CatName = Args.CatName;
            end
            
            % Estimate RA/Dec of SubImage center
            CenterX = (Args.CCDSEC(Iim,2) - Args.CCDSEC(Iim,1)).*0.5;
            CenterY = (Args.CCDSEC(Iim,4) - Args.CCDSEC(Iim,3)).*0.5;
            
            [RA, Dec] = RefWCS.xy2sky(CenterX, CenterY, 'OutUnits','deg', 'includeDistortion',false);
            
            got here - there is a problem
            at seems that NsrcDep = 0????
            
            [ResultRefineFit(Iim), ResultObj(Iim).CatData, AstrometricCat(Iim)] = imProc.astrometry.astrometryRefine(ResultObj(Iim).CatData,...
                                                                                                       'WCS',RefWCS, ...
                                                                                                       'RA',RA,...
                                                                                                       'Dec',Dec,...
                                                                                                       'CooUnits','deg',...
                                                                                                       'CatName',CatName,...                     
                                                                                                       Args.astrometryCoreArgs{:});
            
            % check qulity of solution
            [Sucess(Iim), QualitySummary(Iim)] = imProc.astrometry.assessAstrometricQuality(ResultFit(Iim).ResFit, Args.assessAstrometricQualityArgs{:});
            
        end
    
    
    end
    
end
    